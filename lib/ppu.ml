open Core

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
    mutable scroll : int;  (* $2005 *)
  }
  let make () = {
    status = 0; control = 0; mask = 0; scroll = 0; oam = 0;
  }
end

type ppu = {
  mutable cycles : int;
  mutable scanline : int;
  mutable frames : int;
  mutable register : int;
  mutable vblank : bool;
  mutable nmi : bool;
  mutable new_frame : bool;

  mutable scroll_x : int;
  mutable scroll_y : int;
  mutable sprite_count : int;

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
  cycles = 0; scanline = 0; frames = 0;
  t = 0; v = 0; x = 0; w = true; f = false;
  registers = Registers.make ();
  register = 0; vblank = true; nmi = false; new_frame = false;
  scroll_x = 0; scroll_y = 0; sprite_count = 0;
  frame_content = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (256 * 240 * 3);
  palettes = Array.create ~len:32 0;
  vram = Array.create ~len:0x800 0;
  oam = Array.create ~len:0x100 0;
  nametables = Array.create ~len:0x1000 0;
  rom;
}

let load ppu address =
  if address < 0x2000 then
    ppu.rom.chr.(address)
  else if address < 0x3F00 then
    if address < 0x2800 then
      ppu.nametables.(address land 0x3FF)
    else
      ppu.nametables.((address land 0x3FF) + 0x800)
    (* ppu.nametables.(address land 0x07FF) *)
  else if address < 0x4000 then
    ppu.palettes.(address land 0x1F)
  else
    failwith @@ sprintf "Trying to read PPU VRAM @ %04X" address

let store ppu address value =
  if address < 0x2000 then
    ppu.rom.chr.(address) <- value
  else if address < 0x3F00 then
    if address < 0x2800 then
      ppu.nametables.(address land 0x3FF) <- value
    else
      ppu.nametables.((address land 0x3FF) + 0x800) <- value
    (* ppu.nametables.(address land 0x07FF) <- value *)
  else if address < 0x4000 then (
    let a = address land 0x1F in
    ppu.palettes.(if a = 0x10 then 0 else a) <- value
  )
  else
    failwith @@ sprintf "Trying to write PPU VRAM @ %04X" address

let x_scroll_offset ppu =
  match ppu.registers.control land 0x1 with
  | 0 -> 0
  | _ -> 256

let y_scroll_offset ppu =
  match ppu.registers.control land 0x2 with
  | 0 -> 0
  | _ -> 240

let base_nametable_address ppu =
  match ppu.registers.control land 0x3 with
  | 0 -> 0x2000
  | 1 -> 0x2400
  | 2 -> 0x2800
  | 3 -> 0x2C00
  | _ -> failwith "unreachable"

let address_increment ppu =
  match ppu.registers.control land 0x4 with
  | 0 -> 1
  | _ -> 32

let sprite_address ppu =
  match ppu.registers.control land 0x8 with
  | 0 -> 0
  | _ -> 0x1000

let background_pattern_table_address ppu =
  match ppu.registers.control land 0x10 with
  | 0 -> 0
  | _ -> 0x1000

let read_register ppu = function
  | 0x2002 -> (* PPUSTATUS *)
    ppu.w <- true; (* TODO: NMI Change *)
    ppu.register <- ppu.register land 0x1F;
    (* TODO: Sprite 0 and sprite overflow *)
    ppu.registers.status
  | 0x2004 -> (* OAMADDR *)
    ppu.oam.(ppu.registers.oam)
  | 0x2007 -> (* PPUDATA *)
    let value = load ppu (ppu.v) in
    ppu.v <- (ppu.v + address_increment ppu) land 0x3FFF;
    value
  | _ as r -> failwith @@ sprintf "Cannot read PPU Register @ %04X" r

let write_register ppu register value =
  ppu.register <- value;
  match register with
  | 0x2000 -> (* PPUCONTROL *)
    ppu.registers.control <- value;
    ppu.scroll_x <- (ppu.scroll_x land 0xFF) lor (x_scroll_offset ppu);
    ppu.scroll_y <- (ppu.scroll_y land 0xFF) lor (y_scroll_offset ppu);
    ppu.t <- (ppu.t land 0xF3FF) lor (((value land 0x3) lsl 10) land 0xFFFF)
    (* TODO: Nmi change *)
  | 0x2001 -> (* PPUMASK *)
    ppu.registers.mask <- value
  | 0x2003 -> (* PPUOAMADDR *)
    ppu.registers.oam <- value
  | 0x2004 -> (* OAMDATA *)
    let address = ppu.registers.oam in
    ppu.oam.(address) <- value;
    ppu.registers.oam <- (address + 1) % 0x100
  | 0x2005 -> (* PPUSCROLL *)
    if ppu.w then (
      ppu.scroll_x <- (ppu.scroll_x land 0xFF00) lor value;
      ppu.w <- false;
      ppu.t <- (ppu.t land 0xFFE0) land (value lsr 3);
      ppu.x <- value land 0x7;
    ) else (
      ppu.scroll_y <- (ppu.scroll_y land 0xFF00) lor value;
      ppu.w <- true;
      ppu.t <- (ppu.t land 0x8FFF) lor (((value land 0x07) lsl 12) land 0xFFFF);
      ppu.t <- (ppu.t land 0xFC1F) lor (((value land 0xF8) lsl 2) land 0xFFFF);
    );
    ppu.registers.scroll <- value
  | 0x2006 -> (* PPUADDR *)
    if ppu.w then (
      ppu.w <- false;
      ppu.t <- (ppu.t land 0x80FF) lor (((value land 0x3F) lsl 8) land 0xFFFF);
    ) else (
      ppu.w <- true;
      ppu.t <- (ppu.t land 0xFF00) lor value;
      ppu.v <- ppu.t;
    )
  | 0x2007 -> (* PPUDATA *)
    store ppu ppu.v value;
    ppu.v <- (ppu.v + address_increment ppu) land 0x3FFF;
  | _ as r -> failwith @@ sprintf "Cannot write PPU Register @ %04X" r

let start_vblank ppu =
  if (ppu.registers.control land 0x80) > 0 then
    ppu.nmi <- true;
  ppu.vblank <- true;
  ppu.registers.status <- ppu.registers.status lor 0x80

let end_vblank ppu =
  ppu.vblank <- false;
  ppu.registers.status <- ppu.registers.status land (lnot 0x80)

let show_sprites ppu = ppu.registers.mask land 0x10 > 0
let show_background ppu = ppu.registers.mask land 0x08 > 0

(* let nametable_byte_byte ppu = ()
let attribute_table_byte ppu = ()
let low_tile_byte ppu = ()
let high_tile_byte ppu = ()
let store_tile_data ppu = () *)

let increment_x ppu =
  if ppu.v land 0x1F = 0x1F then (
    ppu.v <- ppu.v land 0xFFE0;
    ppu.v <- ppu.v lxor 0x0400;
  ) else (
    ppu.v <- ppu.v + 1
  )

let increment_y ppu =
  if ppu.v land 0x7000 <> 0x7000 then (
    ppu.v <- ppu.v + 0x1000
  ) else (
    ppu.v <- ppu.v land 0x8FFF;
    let y = match (ppu.v land 0x03E0) lsr 5 with
    | 29 -> ppu.v <- ppu.v lxor 0x0800; 0
    | 31 -> 0
    | other -> other + 1
    in
    ppu.v <- (ppu.v land 0xFC1F) lor (y lsl 5)
  )

let step ppu =
  if (show_background ppu || show_sprites ppu) && ppu.f && ppu.scanline = 261 && ppu.cycles = 339 then (
    ppu.cycles <- 0;
    ppu.scanline <- 0;
    ppu.frames <- ppu.frames + 1;
    ppu.f <- not ppu.f;
  ) else (
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
  );

  let pre_sl = ppu.scanline = 261 in
  let prefetch = ppu.cycles >= 321 && ppu.cycles <= 336 in
	let visible_sl = ppu.scanline < 240 in
	let visible_cy = ppu.cycles >= 1 && ppu.cycles <= 256 in
  let render_sl = pre_sl || visible_sl in
  let fetch_cy = prefetch || visible_cy in

  if show_background ppu || show_sprites ppu then (
    if visible_sl && visible_cy then (
      () (* TODO: Render pixel *)
    );
    if render_sl && fetch_cy then (
      () (* TODO: Fetch tile data and such *)
    );
    if pre_sl && ppu.cycles >= 280 && ppu.cycles <= 304 then (
      (* Copy Y *)
      ppu.v <- (ppu.v land 0x841F) lor (ppu.v land 0x7BE0)
    );
    if render_sl then (
      if fetch_cy && ppu.cycles % 8 = 0 then (
        increment_x ppu;
      );
      if ppu.cycles = 256 then (
        increment_y ppu;
      );
      if ppu.cycles = 257 then (
        (* Copy X *)
        ppu.v <- (ppu.v land 0xFBE0) lor (ppu.v land 0x041F)
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
    () (* TODO: set vblank *)
  );

  if pre_sl && ppu.cycles = 1 then (
    (* TODO: clear VBLANK, sprite0 and overflow *)
    ()
  )
