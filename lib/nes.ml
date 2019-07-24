module Cartridge = Cartridge
module Cpu = Cpu
module Input = Input
module Instructions = Instructions
module Ppu = Ppu

type t = {
  cpu : Cpu.t;
  ppu : Ppu.t;
  rom : Cartridge.rom;
}

let make ?(nestest=false) ?(tracing=false) rom =
  let mapper = Mapper.mapper_for ~rom in
  let vram = Vram.make ~mapper in
  let ppu = Ppu.make ~vram in
  let memory = Memory.make ~mapper ~ppu in
  let cpu = Cpu.make ~ppu ~nestest ~tracing ~memory in
  {
    ppu; rom; cpu;
  }
