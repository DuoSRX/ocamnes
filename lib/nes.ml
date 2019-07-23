open Core
module Cartridge = Cartridge
module Cpu = Cpu
module Input = Input
module Instructions = Instructions
module Ppu = Ppu

type t = {
  cpu : Cpu.cpu;
  ppu : Ppu.ppu;
  rom : Cartridge.rom;
  ram : int array;
}

type memmap = {
  ppu : Ppu.ppu;
  mapper : Mapper.t
}

let make ?(nestest=false) ?(tracing=false) rom =
  let ppu = Ppu.make ~rom in
  let ram = Array.create ~len:0x800 0 in
  let mapper = Mapper.mapper_from_rom rom in
  let cpu = Cpu.make ~rom ~ppu ~nestest ~tracing ~mapper in
  {
    ppu; rom; cpu; ram;
  }
