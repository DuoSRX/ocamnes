open Core

module Registers = struct
  type registers = {
    mutable control : int;
    mutable mask : int;
    mutable status : int;
    mutable scroll : int;
    mutable address : int;
    mutable data : int;
  }
  let make () = {
    status = 0; address = 0; control = 0; mask = 0; scroll = 0; data = 0;
  }
end

type ppu = {
  mutable cycle : int;
  mutable scanline : int;
  mutable frame : int;
  mutable vram_rw_high : bool;
  registers: Registers.registers;
  vram: int array;
}

let make () = {
  cycle = 0; scanline = 0; frame = 0; vram_rw_high = true;
  registers = Registers.make ();
  vram = Array.create ~len:0x4000 0;
}

let address_increment ppu =
  match ppu.registers.control land 0x04 with
  | 0 -> 1
  | _ -> 32

let read_register ppu = function
  | 0x2000 -> ppu.registers.control
  | 0x2001 -> ppu.registers.mask
  | 0x2002 ->
    let status = ppu.registers.status in
    ppu.registers.status <- status lxor 0x80;
    status
  | 0x2005 -> ppu.registers.scroll
  | 0x2006 -> ppu.registers.address
  (* | 0x2007 -> ppu.registers.data *)
  | _ as r -> failwith @@ sprintf "Cannot read PPU @ %04X" r

let write_register ppu register value = match register with
  | 0x2000 -> ppu.registers.control <- value
  | 0x2001 -> ppu.registers.mask <- value
  | 0x2005 -> ppu.registers.scroll <- value
  | 0x2006 ->
    if ppu.vram_rw_high then (
      ppu.registers.address <- (ppu.registers.address land 0x80FF) lor ((value land 0x3F) lsl 8);
      ppu.vram_rw_high <- false
    ) else (
      ppu.registers.address <- (ppu.registers.address land 0xFF00) lor value;
      ppu.vram_rw_high <- true
    )
  | 0x2007 ->
    let address = ppu.registers.address in
    ppu.vram.(address land 0x3FFF) <- value;
    ppu.registers.address <- ppu.registers.address + address_increment ppu
  | _ as r -> failwith @@ sprintf "Cannot write PPU @ %04X" r

(* module type PpuRegister = sig
  val value : int ref
end

module MakeRegister(M: PpuRegister) : PpuRegister = struct
  let value = M.value
end

module Ctrl = MakeRegister(struct
  let value = ref 0
  let vram_increment = match !value with
  | 0x04 -> 1
  | _ -> 32
end) *)
