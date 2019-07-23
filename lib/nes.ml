module Cartridge = Cartridge
module Cpu = Cpu
module Input = Input
module Instructions = Instructions
module Ppu = Ppu

type t = {
  cpu : Cpu.cpu;
  ppu : Ppu.ppu;
  rom : Cartridge.rom;
  (* TODO: input mapper apu *)
}

let make ?(nestest=false) ?(tracing=false) rom =
  let ppu = Ppu.make ~rom in
  let cpu = Cpu.make ~rom ~ppu ~nestest ~tracing in
  {
    ppu; rom; cpu
  }
