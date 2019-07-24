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

type sprite = {
  pattern : int;
  position : int;
  priority : int;
  index : int;
}

type ppu = {
  mutable cycles : int;
  mutable scanline : int;
  mutable frames : int;

  (* https://wiki.nesdev.com/w/index.php/NMI *)
  mutable nmi_occured : bool;
  mutable nmi_output : bool;
  mutable nmi_triggered : bool;
  mutable nmi_previous : bool;
  mutable nmi_delay : int;

  (* Registers *)
  mutable control : int;  (* PPUCTRL   $2000 *)
  mutable mask : int;     (* PPUMASK   $2001 *)
  mutable oam_addr : int; (* OAMADDR   $2003 *)

  (* Temp stuff *)
  mutable tile_data : Uint64.t;
  mutable name_table_b : int;
  mutable attr_table_b : int;
  mutable low_byte : int;
  mutable high_byte : int;
  mutable sprite_count : int;
  mutable buffer : int;
  mutable sprites : sprite array;
  mutable sprite0_hit : bool;
  mutable sprite_overflow : bool;
  mutable register : int; (* shift register when writing to other registers *)

  (* Internal registers *)
  mutable t : int;  (* temp vram address *)
  mutable v : int;  (* vram address *)
  mutable x : int;  (* final scroll *)
  mutable w : bool; (* Write latch *)
  mutable f : bool; (* Frame parity *)

  frame_content : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  oam: int array;
  vram : Vram.t
}

let make ~vram = {
  cycles = 340; scanline = 240; frames = 0;
  t = 0; v = 0; x = 0; w = true; f = false;
  oam_addr = 0; mask = 0; control = 0;
  register = 0; sprite0_hit = false; sprite_overflow = false;
  nmi_occured = false; nmi_output = false; nmi_triggered = false; nmi_previous = false; nmi_delay = 0;
  sprite_count = 0; buffer = 0; sprites = Array.create ~len:8 {pattern=0;position=0;priority=0;index=0};
  tile_data = Uint64.zero; name_table_b = 0; attr_table_b = 0; low_byte = 0; high_byte = 0;
  frame_content = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (256 * 240 * 3);
  oam = Array.create ~len:0x100 0;
  vram;
}

let load ppu address =
  Vram.load ppu.vram (address land 0x3FFF)

let store ppu address value =
  Vram.store ppu.vram (address land 0x3FFF) value

let nmi_change ppu =
  let nmi = ppu.nmi_output && ppu.nmi_occured in
	if nmi && not ppu.nmi_previous then
		ppu.nmi_delay <- 10;
	ppu.nmi_previous <- nmi

let address_increment ppu =
  match ppu.control land 0x4 with
  | 0 -> 1
  | _ -> 32

let background_pattern_table_address ppu =
  match ppu.control land 0x10 with
  | 0 -> 0
  | _ -> 0x1000

let sprite_address ppu =
  match ppu.control land 0x08 with
  | 0 -> 0
  | _ -> 0x1000

let sprite_size ppu =
  match ppu.control land 0x20 with
  | 0 -> 8
  | _ -> 16

let read_register ppu = function
  | 0x2001 -> (* PPUMASK *)
    ppu.mask
  | 0x2002 -> (* PPUSTATUS *)
    let result = (ppu.register land 0x1F)
      lor (if ppu.nmi_occured then 0x80 else 0)
      lor (if ppu.sprite0_hit then 0x40 else 0)
      lor (if ppu.sprite_overflow then 0x20 else 0)
    in
    ppu.nmi_occured <- false;
    nmi_change ppu;
    ppu.w <- true;
    result
  | 0x2004 -> (* OAMDATA *)
    ppu.oam.(ppu.oam_addr)
  | 0x2007 -> (* PPUDATA *)
    let value = load ppu ppu.v in
    let value = if (ppu.v % 0x4000 < 0x3F00) then (
      let buffer = ppu.buffer in
      ppu.buffer <- value;
      buffer
    ) else (
      ppu.buffer <- load ppu (ppu.v - 0x1000);
      value
    ) in
    ppu.v <- (ppu.v + address_increment ppu);
    value
  | _ as r -> failwith @@ sprintf "Cannot read PPU Register @ %04X" r

let write_register ppu register value =
  ppu.register <- value;
  match register with
  | 0x2000 -> (* PPUCTRL *)
    ppu.nmi_output <- (value land 0x80) > 0;
    ppu.control <- value;
    nmi_change ppu;
    ppu.t <- (ppu.t land 0xF3FF) lor ((value land 0x3) lsl 10);
  | 0x2001 -> (* PPUMASK *)
    ppu.mask <- value
  | 0x2003 -> (* OAMADDR *)
    ppu.oam_addr <- value
  | 0x2004 -> (* OAMDATA *)
    let address = ppu.oam_addr in
    ppu.oam.(address) <- value;
    ppu.oam_addr <- (address + 1) % 0x100
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

let show_sprites ppu =
  ppu.mask land 0x10 > 0

let show_background ppu =
  ppu.mask land 0x08 > 0

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
  let rec aux n data =
    let p1 = (ppu.low_byte land 0x80) lsr 7 in
    let p2 = (ppu.high_byte land 0x80) lsr 6 in
    ppu.low_byte <- (ppu.low_byte lsl 1);
    ppu.high_byte <- (ppu.high_byte lsl 1);
    let data = (data lsl 4) lor ppu.attr_table_b lor p1 lor p2 in
    if n > 0 then aux (n - 1) data else data
  in
  ppu.tile_data <- Uint64.logor ppu.tile_data (aux 7 0 |> Uint64.of_int )

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

let sprite_pattern ppu ~tile ~row ~attrs =
  let vflip = attrs land 0x80 > 0 in
  let hflip = attrs land 0x40 > 0 in

  let address = if sprite_size ppu = 8 then (
    let index = if vflip then 7 - row else row in
    sprite_address ppu + tile * 16 + index
  ) else (
    let row = if vflip then 15 - row else row in
    let offset = tile land 1 in
    let tile = tile land 0xFE in
    let (row, tile) = if row > 7 then (row - 8, tile + 1) else (row, tile) in
    sprite_address ppu * offset + tile * 16 + row
  ) in
  let lo = ref @@ load ppu address in
  let hi = ref @@ load ppu (address + 8) in
  let palette = (attrs land 3) lsl 2 in

  let rec aux n pattern =
    let pattern = if hflip then (
      let plane0 = (!lo land 0x1) in
      let plane1 = (!hi land 0x1) lsl 1 in
      lo := !lo lsr 1;
      hi := !hi lsr 1;
      (pattern lsl 4) lor (palette lor plane0 lor plane1)
    ) else (
      let plane0 = (!lo land 0x80) lsr 7 in
      let plane1 = (!hi land 0x80) lsr 6 in
      lo := !lo lsl 1;
      hi := !hi lsl 1;
      (pattern lsl 4) lor (palette lor plane0 lor plane1)
    ) in
    if n > 0 then aux (n - 1) pattern else pattern
  in aux 7 0

let make_sprites ppu =
  let height = sprite_size ppu in
  let n = ref 0 in
  for i = 0 to 63 do
    let y = ppu.oam.(i * 4) in
    let x = ppu.oam.(i * 4 + 3) in
    let attrs = ppu.oam.(i * 4 + 2) in
    let tile = ppu.oam.(i * 4 + 1) in
    let row = ppu.scanline - y in
    if row < 0 || row >= height then () else (
      if !n < 8 then (
        let position = x in
        let priority = (attrs lsr 5) land 1 in
        let index = !n in
        let pattern = sprite_pattern ppu ~tile ~row ~attrs in
        ppu.sprites.(!n) <- { position; priority; index; pattern }
      );
      n := !n + 1
    );
    ppu.sprite_count <- !n
  done;
  if !n > 8 then (
    ppu.sprite_count <- 8;
    ppu.sprite_overflow <- true
  )

let set_pixel ppu x y color =
  ppu.frame_content.{(y * 256 + x) * 3 + 0} <- color lsr 16;
  ppu.frame_content.{(y * 256 + x) * 3 + 1} <- color lsr 8;
  ppu.frame_content.{(y * 256 + x) * 3 + 2} <- color

let sprite_pixel ppu =
  let pixel = ref (0, 0) in
  if show_sprites ppu then (
    for n = 0 to ppu.sprite_count - 1 do
      let sprite = ppu.sprites.(n) in
      let offset = (ppu.cycles - 1) - sprite.position in
      if not (offset < 0 || offset > 7) then (
        let offset = 7 - offset in
        let color = (sprite.pattern lsr (offset * 4)) land 0xF in
        if not (color % 4 = 0) then
          pixel := n, color
      );
    done;
    );
  !pixel

let background_pixel ppu =
  if show_background ppu then (
    let tile_data = Uint64.shift_right_logical ppu.tile_data 32 |> Uint64.to_int in
    let shift = (7 - ppu.x) * 4 in
    let data = tile_data lsr shift in
    data land 0xF
  ) else 0

let render_pixel ppu =
  let x = ppu.cycles - 1 in
  let y = ppu.scanline in
  let bg_color = background_pixel ppu in
  let sprite_idx, sprite_color = sprite_pixel ppu in
  (* TODO: x < 8 and y < 8 *)
  let b = bg_color % 4 <> 0 in
  let s = sprite_color % 4 <> 0 in

  let color = match (b, s) with
  | false, false -> 0
  | true,  false -> bg_color
  | false, true  -> sprite_color lor 0x10
  | true,  true  -> (
    let sprite = ppu.sprites.(sprite_idx) in
    if sprite.index = 0 && x < 255 then
      ppu.sprite0_hit <- true;
    if sprite.priority = 0 then
      sprite_color lor 0x10
    else
      bg_color
  )
  in

  let palette = load ppu (0x3F00 + color) in
  let palette_offset = if palette >= 16 && palette % 4 = 0 then 16 else 0 in
  set_pixel ppu x y (all_palettes.((palette - palette_offset) land 0x3F))

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
        make_sprites ppu
      ) else (
        ppu.sprite_count <- 0
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
    ppu.sprite0_hit <- false;
    ppu.sprite_overflow <- false
  );

  (* printf "SL:%-3d CY:%-3d\n" ppu.cycles ppu.scanline *)
