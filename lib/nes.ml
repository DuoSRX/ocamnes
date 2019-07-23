module Cartridge = Cartridge
module Cpu = Cpu
module Input = Input
module Instructions = Instructions
module Ppu = Ppu

type t = {
  cpu : Cpu.cpu;
  ppu : Ppu.ppu;
  rom : Cartridge.rom;
}

let make ?(nestest=false) ?(tracing=false) rom =
  let mapper = Mapper.mapper_from_rom rom in
  let ppu = Ppu.make ~rom ~mapper in
  let memory = Memory.make ~mapper ~ppu in
  let cpu = Cpu.make ~ppu ~nestest ~tracing ~memory in
  {
    ppu; rom; cpu;
  }
