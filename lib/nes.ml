open Core
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

module Debugger = struct
  let breakpoints = ref (Int.Set.of_list [])
  let break_on_step = ref false
  let break_after = ref (-2)

  let rec prompt (cpu:Cpu.t) =
    print_string "(DEBUG) ";
    Out_channel.(flush stdout);
    let command = In_channel.(input_line_exn stdin) in
    match (String.split command ~on:' ') with
    | [""] -> prompt cpu
    | ["s"] | ["step"] -> break_on_step := true
    | ["c"] | ["continue"] -> break_on_step := false
    | ["q"] | ["quit"] | ["exit"] -> exit 1;
    | ["steps"] -> printf "%d\n" cpu.steps; prompt cpu
    | "bp" :: "del" :: bp :: _ ->
      breakpoints := Set.remove !breakpoints (Int.of_string bp);
      prompt cpu
    | "bp" :: "add" :: bp :: _ ->
      breakpoints := Set.add !breakpoints (Int.of_string bp);
      prompt cpu
    | "bp" :: [] ->
      !breakpoints |> Set.to_list |> List.map ~f:(sprintf "%04X") |> String.concat ~sep:", " |> print_endline;
      prompt cpu
    | "byte" :: addr :: [] ->
      printf "%02X\n" (Cpu.load_byte cpu (Int.of_string addr));
      prompt cpu
    | "byte" :: a :: b :: [] ->
      Interval.Int.create (Int.of_string a) (Int.of_string b)
      |> Interval.Int.to_list
      |> List.map ~f:(Cpu.load_byte cpu)
      |> List.iter ~f:(printf "%02X ");
      print_endline "";
      prompt cpu
    | "ppu" :: addr :: [] ->
      printf "%02X\n" (Ppu.load cpu.ppu (Int.of_string addr));
      prompt cpu
    | "ppu" :: a :: b :: [] ->
      Interval.Int.create (Int.of_string a) (Int.of_string b)
      |> Interval.Int.to_list
      |> List.map ~f:(Ppu.load cpu.ppu)
      |> List.iter ~f:(printf "%02X ");
      print_endline "";
      prompt cpu
    | "oam" :: [] ->
      cpu.ppu.oam |> Array.iter ~f:(printf "%02X ");
      print_endline "";
      prompt cpu
    | "word" :: addr :: _ ->
      printf "%04X\n" (Cpu.load_word cpu (Int.of_string addr));
      prompt cpu
    | "pputrace" :: [] ->
      printf "T:%04X V:%04X X:%04X\n" cpu.ppu.t cpu.ppu.v cpu.ppu.x;
      prompt cpu
    | _ ->
      print_endline "Unknown command (q to quit, s to step, c to continue)";
      prompt cpu

    let should_break (cpu:Cpu.t) =
      !break_on_step || Set.mem !breakpoints cpu.pc || !break_after = cpu.steps
end

let on_step (cpu:Cpu.t) instruction opcode =
  if Debugger.should_break cpu then (
    print_endline @@ Cpu.trace cpu instruction opcode;
    Debugger.prompt cpu;
  )

let step ?(trace_fun=(fun _ _ _ -> "")) nes =
  let prev_cycles = nes.cpu.cycles in
  let log = Cpu.step nes.cpu ~on_step:on_step ~trace_fun in
  let elapsed_cycles = nes.cpu.cycles - prev_cycles in

  for _ = 1 to elapsed_cycles * 3 do
    Ppu.step nes.ppu;
  done;

  if nes.ppu.nmi_triggered then Cpu.trigger_nmi nes.cpu;
  log
