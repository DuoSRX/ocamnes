open Core
open Stdint

let all_palettes = [|
    0x666666; 0x002A88; 0x1412A7; 0x3B00A4; 0x5C007E;
    0x6E0040; 0x6C0600; 0x561D00; 0x333500; 0x0B4800;
    0x005200; 0x004F08; 0x00404D; 0x000000; 0x000000;
    0x000000; 0xADADAD; 0x155FD9; 0x4240FF; 0x7527FE;
    0xA01ACC; 0xB71E7B; 0xB53120; 0x994E00; 0x6B6D00;
    0x388700; 0x0C9300; 0x008F32; 0x007C8D; 0x000000;
    0x000000; 0x000000; 0xFFFEFF; 0x64B0FF; 0x9290FF;
    0xC676FF; 0xF36AFF; 0xFE6ECC; 0xFE8170; 0xEA9E22;
    0xBCBE00; 0x88D800; 0x5CE430; 0x45E082; 0x48CDDE;
    0x4F4F4F; 0x000000; 0x000000; 0xFFFEFF; 0xC0DFFF;
    0xD3D2FF; 0xE8C8FF; 0xFBC2FF; 0xFEC4EA; 0xFECCC5;
    0xF7D8A5; 0xE4E594; 0xCFEF96; 0xBDF4AB; 0xB3F3CC;
    0xB5EBF2; 0xB8B8B8; 0x000000; 0x000000;
|]

module Registers = struct
  type registers = {
    mutable control : int; (* $2000 *)
    mutable mask : int;    (* $2001 *)
    mutable status : int;  (* $2002 *)
    mutable oam : int;     (* $2003 *)
  }
  let make () = {
    status = 0; control = 0; mask = 0; oam = 0;
  }
end

type ppu = {
  mutable cycles : int;
  mutable scanline : int;
  mutable frames : int;
  mutable register : int;
  mutable new_frame : bool;

  (* https://wiki.nesdev.com/w/index.php/NMI *)
  mutable nmi_occured : bool;
  mutable nmi_output : bool;
  mutable nmi_triggered : bool;
  mutable nmi_previous : bool;
  mutable nmi_delay : int;


  (* Temp stuff *)
  mutable tile_data : Uint64.t;
  mutable name_table_b : int;
  mutable attr_table_b : int;
  mutable low_byte : int;
  mutable high_byte : int;
  mutable sprite_count : int;

  (* More temp stuff *)
  cur_nametable : int;
  cur_attribute : int;
  cur_pattern_low : int;
  cur_pattern_high : int;

  (* Internal registers *)
  mutable t : int;  (* temp vram address *)
  mutable v : int;  (* vram address *)
  mutable x : int;  (* final scroll *)
  mutable w : bool; (* Write latch *)
  mutable f : bool; (* Frame parity *)

  frame_content : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  registers: Registers.registers;
  palettes : int array;
  vram: int array;
  oam: int array;
  nametables : int array;
  rom : Cartridge.rom;
}

let make ~rom = {
  cycles = 340; scanline = 240; frames = 0;
  t = 0; v = 0; x = 0; w = true; f = false;
  registers = Registers.make ();
  register = 0; new_frame = false;
  nmi_occured = false; nmi_output = false; nmi_triggered = false; nmi_previous = false; nmi_delay = 0;
  sprite_count = 0;
  tile_data = Uint64.zero; name_table_b = 0; attr_table_b = 0; low_byte = 0; high_byte = 0;
  frame_content = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (256 * 240 * 3);
  palettes = Array.create ~len:32 0;
  vram = Array.create ~len:0x800 0;
  oam = Array.create ~len:0x100 0;
  nametables = Array.create ~len:0x800 0;
  rom;
  cur_nametable = 0; cur_attribute = 0; cur_pattern_low = 0; cur_pattern_high = 0;
}

let load ppu address =
  if address < 0x2000 then
    ppu.rom.chr.(address)
  else if address < 0x3F00 then (
    if address < 0x2800 then (
      ppu.nametables.(address land 0x3FF)
    ) else (
      ppu.nametables.(0x400 + (address land 0x3FF))
    )
  )
  else if address < 0x4000 then
    ppu.palettes.(address land 0x1F)
  else
    failwith @@ sprintf "Trying to read PPU VRAM @ %04X" address

let store ppu address value =
  if address < 0x2000 then
    ppu.rom.chr.(address) <- value
  else if address < 0x3F00 then
    if address < 0x2800 then (
      ppu.nametables.(address land 0x3FF) <- value
    ) else (
      ppu.nametables.(0x400 + (address land 0x3FF)) <- value
    )
  else if address < 0x4000 then (
    ppu.palettes.(address land 0x1F) <- value;
  )
  else
    failwith @@ sprintf "Trying to write PPU VRAM @ %04X" address

let nmi_change ppu =
  let nmi = ppu.nmi_output && ppu.nmi_occured in
	if nmi && not ppu.nmi_previous then
		ppu.nmi_delay <- 10;
	ppu.nmi_previous <- nmi

let address_increment ppu =
  match ppu.registers.control land 0x4 with
  | 0 -> 1
  | _ -> 32

let background_pattern_table_address ppu =
  match ppu.registers.control land 0x10 with
  | 0 -> 0
  | _ -> 0x1000

let read_register ppu = function
  | 0x2001 -> (* PPUMASK *)
    ppu.registers.mask
  | 0x2002 -> (* PPUSTATUS *)
    (* TODO: Sprite 0 and sprite overflow *)
    let result = (ppu.register land 0x1F) lor (if ppu.nmi_occured then 0x80 else 0) in
    ppu.nmi_occured <- false;
    nmi_change ppu;
    ppu.w <- true;
    result
  | 0x2004 -> (* OAMDATA *)
    ppu.oam.(ppu.registers.oam)
  | 0x2007 -> (* PPUDATA *)
    0
    (* let value = load ppu ppu.v in
    ppu.v <- (ppu.v + address_increment ppu);
    value *)
  | _ as r -> failwith @@ sprintf "Cannot read PPU Register @ %04X" r

let write_register ppu register value =
  ppu.register <- value;
  match register with
  | 0x2000 -> (* PPUCTRL *)
    ppu.nmi_output <- (value land 0x80) > 0;
    ppu.registers.control <- value;
    nmi_change ppu;
    ppu.t <- (ppu.t land 0xF3FF) lor ((value land 0x3) lsl 10);
  | 0x2001 -> (* PPUMASK *)
    ppu.registers.mask <- value
  | 0x2003 -> (* OAMADDR *)
    ppu.registers.oam <- value
  | 0x2004 -> (* OAMDATA *)
    let address = ppu.registers.oam in
    ppu.oam.(address) <- value;
    ppu.registers.oam <- (address + 1) % 0x100
  | 0x2005 -> (* PPUSCROLL *)
    if ppu.w then (
      ppu.t <- (ppu.t land 0xFFE0) lor (value lsr 3);
      ppu.x <- value land 0x7;
    ) else (
      ppu.t <- (ppu.t land 0x8FFF) lor ((value land 0x07) lsl 12);
      ppu.t <- (ppu.t land 0xFC1F) lor ((value land 0xF8) lsl 2);
    );
    ppu.w <- not ppu.w;
  | 0x2006 -> (* PPUADDR *)
    if ppu.w then (
      (* FIXME: 0x00FF or 0x80FF?? *)
      ppu.t <- ppu.t land 0x80FF;
      ppu.t <- ppu.t lor ((value land 0x3F) lsl 8);
    ) else (
      ppu.t <- ppu.t land 0xFF00;
      ppu.t <- ppu.t lor value;
      ppu.v <- ppu.t;
    );
    ppu.w <- not ppu.w;
  | 0x2007 -> (* PPUDATA *)
    store ppu ppu.v value;
    ppu.v <- (ppu.v + address_increment ppu)
  | _ as r -> failwith @@ sprintf "Cannot write PPU Register @ %04X" r

let show_sprites ppu = ppu.registers.mask land 0x10 > 0
let show_background ppu = ppu.registers.mask land 0x08 > 0

let load_nametable ppu =
  let address = 0x2000 lor (ppu.v land 0xFFF) in
  ppu.name_table_b <- load ppu address

let load_attribute_table ppu =
  let v = ppu.v in
  let address = 0x23C0 lor (v land 0x0C00) lor ((v lsr 4) land 0x38) lor ((v lsr 2) land 0x7) in
  let shift = ((v lsr 4) land 0x4) lor (v land 0x2) in
  ppu.attr_table_b <- (((load ppu address) lsr shift) land 0x3) lsl 2

let load_low_byte ppu =
  let y = (ppu.v lsr 12) land 0x7 in
  let address = (background_pattern_table_address ppu) + (ppu.name_table_b * 16) + y in
  ppu.low_byte <- load ppu (address)

let load_high_byte ppu =
  let y = (ppu.v lsr 12) land 0x7 in
  let address = (background_pattern_table_address ppu) + (ppu.name_table_b * 16) + y in
  ppu.high_byte <- load ppu (address + 8)

let store_tile_data ppu =
  let data = ref 0 in
  for _n = 1 to 8 do
    let a = ppu.attr_table_b in
    let p1 = (ppu.low_byte land 0x80) lsr 7 in
    let p2 = (ppu.high_byte land 0x80) lsr 6 in
    ppu.low_byte <- (ppu.low_byte lsl 1);
    ppu.high_byte <- (ppu.high_byte lsl 1);
    data := !data lsl 4;
    let res = a lor p1 lor p2 in
    data := !data lor res;
  done;
  ppu.tile_data <- Uint64.logor ppu.tile_data (Uint64.of_int !data)

let fetch_data ppu =
  ppu.tile_data <- Uint64.shift_left ppu.tile_data 4;
  match ppu.cycles % 8 with
  | 1 -> load_nametable ppu
  | 3 -> load_attribute_table ppu
  | 5 -> load_low_byte ppu
  | 7 -> load_high_byte ppu
  | 0 -> store_tile_data ppu
  | _ -> ()

let increment_x ppu =
  if ppu.v land 0x1F = 0x1F then (
    ppu.v <- ppu.v land (lnot 0x1F);
    ppu.v <- ppu.v lxor 0x0400;
  ) else (
    ppu.v <- (ppu.v + 1)
  )

let increment_y ppu =
  if ppu.v land 0x7000 <> 0x7000 then (
    ppu.v <- ppu.v + 0x1000
  ) else (
    ppu.v <- ppu.v land (lnot 0x7000);
    let y = match (ppu.v land 0x03E0) lsr 5 with
    | 29 -> ppu.v <- ppu.v lxor 0x0800; 0
    | 31 -> 0
    | other -> other + 1
    in
    ppu.v <- (ppu.v land (lnot 0x03E0)) lor (y lsl 5)
  )

let set_pixel ppu x y color =
  ppu.frame_content.{(y * 256 + x) * 3 + 0} <- color lsr 16;
  ppu.frame_content.{(y * 256 + x) * 3 + 1} <- color lsr 8;
  ppu.frame_content.{(y * 256 + x) * 3 + 2} <- color

let fetch_tile_data ppu =
  Uint64.shift_right_logical ppu.tile_data 32 |> Uint64.to_int

let background_pixel ppu =
  if show_background ppu then (
    let tile_data = fetch_tile_data ppu in
    let shift = (7 - ppu.x) * 4 in
    let data = tile_data lsr shift in
    data land 0xF
  ) else 0

let render_pixel ppu =
  let x = ppu.cycles - 1 in
  let y = ppu.scanline in
  let bg = background_pixel ppu in
  (* TODO: x < 8 and y < 8 *)
  let b = bg % 4 <> 0 in
  (* TODO: sprites *)
  let color = if b then bg else 0 in
  (* let palette = load ppu (0x3F00 + color) in *)
  let palette = ppu.palettes.(color) in
  set_pixel ppu x y (all_palettes.(palette land 0x3F))

let tick ppu =
  if ppu.nmi_delay > 0 then (
    ppu.nmi_delay <- ppu.nmi_delay - 1;
    if ppu.nmi_delay = 0 && ppu.nmi_output && ppu.nmi_occured then
      ppu.nmi_triggered <- true
  );

  let next_cycle = if (show_background ppu || show_sprites ppu) then (
    if ppu.f && ppu.scanline = 261 && ppu.cycles = 339 then (
      ppu.cycles <- 0;
      ppu.scanline <- 0;
      ppu.frames <- ppu.frames + 1;
      ppu.f <- not ppu.f;
      false
    ) else true
  ) else true in

  if next_cycle then (
    ppu.cycles <- ppu.cycles + 1;

    if ppu.cycles > 340 then (
      ppu.cycles <- 0;
      ppu.scanline <- ppu.scanline + 1;

      if ppu.scanline > 261 then (
        ppu.scanline <- 0;
        ppu.frames <- ppu.frames + 1;
        ppu.f <- not ppu.f
      )
    );
  )

let step ppu =
  tick ppu;

  let rendering_enabled = show_background ppu || show_sprites ppu in
  let pre_sl = ppu.scanline = 261 in
  let visible_sl = ppu.scanline < 240 in
  let render_sl = pre_sl || visible_sl in
  let prefetch = ppu.cycles >= 321 && ppu.cycles <= 336 in
  let visible_cy = ppu.cycles >= 1 && ppu.cycles <= 256 in
  let fetch_cy = prefetch || visible_cy in

  if rendering_enabled then (
    if visible_sl && visible_cy then (
      render_pixel ppu
    );
    if render_sl && fetch_cy then (
      fetch_data ppu
    );
    if pre_sl && ppu.cycles >= 280 && ppu.cycles <= 304 then (
      ppu.v <- (ppu.v land 0x841F) lor (ppu.t land 0x7BE0)
    );
    if render_sl then (
      if fetch_cy && ppu.cycles % 8 = 0 then (
        increment_x ppu
      );
      if ppu.cycles = 256 then (
        increment_y ppu
      );
      if ppu.cycles = 257 then (
        ppu.v <- (ppu.v land 0xFBE0) lor (ppu.t land 0x041F)
      );
    );

    if ppu.cycles = 257 then (
      if visible_sl then (
        () (* TODO: eval sprites *)
      ) else (
        () (* TODO: update sprite count *)
      )
    )
  );

  if ppu.scanline = 241 && ppu.cycles = 1 then (
    ppu.nmi_occured <- true;
    nmi_change ppu;
  );

  if pre_sl && ppu.cycles = 1 then (
    ppu.nmi_occured <- false;
    nmi_change ppu;
    (* TODO: sprite0 and overflow reset *)
  );

  (* printf "SL:%-3d CY:%-3d\n" ppu.cycles ppu.scanline *)
