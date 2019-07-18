open Core

module Registers = struct
  type registers = {
    mutable control : int;
    mutable mask : int;
    mutable status : int;
    mutable oam : int;
    mutable scroll : int;
    mutable address : int;
    mutable data : int;
  }
  let make () = {
    status = 0; address = 0; control = 0; mask = 0; scroll = 0; data = 0; oam = 0;
  }
end

type ppu = {
  mutable cycle : int;
  mutable scanline : int;
  mutable frame : int;
  mutable vram_rw_high : bool;
  mutable register : int;
  mutable vblank : bool;
  mutable nmi : bool;
  registers: Registers.registers;
  vram: int array;
  oam: int array;
  nametables : int array;
  rom : Cartridge.rom;
}

let make ~rom = {
  cycle = 0; scanline = 0; frame = 0; vram_rw_high = true;
  registers = Registers.make ();
  register = 0; vblank = true; nmi = false;
  vram = Array.create ~len:0x4000 0;
  oam = Array.create ~len:0x100 0;
  nametables = Array.create ~len:0x800 0;
  rom;
}

let load ppu address =
  if address < 0x2000 then
    ppu.rom.chr.(address) (* CHR *)
  else if address < 0x3F00 then
    ppu.nametables.(address land 0x07FF)
  else if address < 0x4000 then
    0xDEA2 (* Palettes *)
  else
    failwith @@ sprintf "Trying to read PPU VRAM @ %04X" address

let store ppu address value =
  if address < 0x2000 then
    () (* Can't write to CHR? *)
  else if address < 0x3F00 then
    ppu.nametables.(address land 0x07FF) <- value
  else if address < 0x4000 then
    ()
  else
    failwith @@ sprintf "Trying to write PPU VRAM @ %04X" address

let address_increment ppu =
  match ppu.registers.control land 0x04 with
  | 0 -> 1
  | _ -> 32

let read_register ppu = function
  | 0x2000 -> ppu.registers.control
  | 0x2001 -> ppu.registers.mask
  | 0x2002 ->
    ppu.registers.status <- ppu.registers.status lxor 0x80;
    ppu.vram_rw_high <- true;
    let result = ppu.register land 0x1F lor (if ppu.vblank then 0x80 else 0) in
    (* ppu.vblank <- false; *)
    result
  | 0x2004 -> ppu.oam.(ppu.registers.oam)
  | 0x2005 -> ppu.registers.scroll
  (* | 0x2006 -> ppu.registers.address *)
  | 0x2007 ->
    let address = ppu.registers.address in
    ppu.registers.address <- ppu.registers.address + address_increment ppu;
    load ppu address
  | _ as r -> failwith @@ sprintf "Cannot read PPU Register @ %04X" r

let write_register ppu register value =
  ppu.register <- value;
  match register with
  | 0x2000 -> ppu.registers.control <- value
  | 0x2001 -> ppu.registers.mask <- value
  | 0x2003 -> ppu.registers.oam <- value
  | 0x2005 ->
    ppu.vram_rw_high <- not ppu.vram_rw_high;
    ppu.registers.scroll <- value (* TODO: write to scroll horizontal or vertical *)
  | 0x2006 ->
    if ppu.vram_rw_high then (
      ppu.registers.address <- ((ppu.registers.address land 0xFF) lor (value lsl 8) land 0x3FFF);
      ppu.vram_rw_high <- false
    ) else (
      ppu.registers.address <- (ppu.registers.address land 0xFF00) lor value;
      ppu.vram_rw_high <- true
    )
  | 0x2007 ->
    let address = ppu.registers.address in
    store ppu address value;
    ppu.registers.address <- ((ppu.registers.address + address_increment ppu) land 0xFFFF) land 0x3FFF
  | _ as r -> failwith @@ sprintf "Cannot write PPU Register @ %04X" r

let step ppu =
  ppu.nmi <- false;
  let scanline = ppu.scanline in
  (* if scanline >= 0 && scanline < 240 then *)
    (* print_endline "making pixels"; DRAW STUFF *)

  (* if scanline = 241 && ppu.cycle = 1 then ( *)
  if scanline = 241 then (
    ppu.vblank <- true;
    if ppu.registers.control land 0x80 > 0 then
      ppu.nmi <- true
    (* print_endline "=============== VBLANK ==============" *)
  );

  ppu.cycle <- ppu.cycle + 1;

  if ppu.cycle >= 341 then (
    ppu.cycle <- 0;
    ppu.scanline <- ppu.scanline + 1);

  if ppu.scanline = 262 then (
    ppu.scanline <- 0;
    ppu.frame <- ppu.frame + 1;
    (* Render stuff here *)
    ppu.vblank <- false;
  )
