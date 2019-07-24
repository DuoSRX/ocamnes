open Core
open Instructions
module Cartridge = Cartridge
module Cpu = Cpu
module Input = Input
module Instructions = Instructions
module Ppu = Ppu
open Cpu.AddressingMode

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

let op_is_branch = function
  | JMP | JSR -> false
  | BCC | BCS | BEQ | BNE | BPL -> false
  | _ -> true

let args_to_string cpu (i:Cpu.instr) =
  let open Cpu in
  match i.mode with
  | Immediate -> sprintf "#$%02X" i.args
  | Absolute ->
    if op_is_branch i.op then
      sprintf "$%04X = %02X" (Option.value_exn i.target) i.args
    else
      sprintf "$%04X" (Option.value_exn i.target)
  | AbsoluteX ->
    sprintf "$%04X,X @ %04X = %02X" (load_word cpu (cpu.pc + 1)) (Option.value_exn i.target) i.args
  | AbsoluteY ->
    sprintf "$%04X,Y @ %04X = %02X" (load_word cpu (cpu.pc + 1)) (Option.value_exn i.target) i.args
  | ZeroPage -> sprintf "$%02X = %02X" (Option.value_exn i.target) i.args
  | ZeroPageX ->
    let byte = load_byte cpu (cpu.pc + 1) in
    sprintf "$%02X,X @ %02X = %02X" byte (Option.value_exn i.target) i.args
  | ZeroPageY ->
    let byte = load_byte cpu (cpu.pc + 1) in
    sprintf "$%02X,Y @ %02X = %02X" byte (Option.value_exn i.target) i.args
  | IndirectX ->
    let byte = load_byte cpu (cpu.pc + 1) in
    let sum = wrapping_add byte cpu.x in
    sprintf "($%02X,X) @ %02X = %04X = %02X" byte sum (Option.value_exn i.target) i.args
  | IndirectY ->
    let byte = load_byte cpu (cpu.pc + 1) in
    let sum = wrapping_sub_w (Option.value_exn i.target) cpu.y in
    sprintf "($%02X),Y = %04X @ %04X = %02X" byte sum (Option.value_exn i.target) i.args
  | Indirect -> sprintf "($%04X) = %04X" (load_word cpu (cpu.pc + 1)) (Option.value_exn i.target)
  | Relative ->
    sprintf "$%04X" (i.args);
  | Accumulator -> "A"
  | Implicit -> ""

let word_to_byte_string w =
  let lo = w land 0xFF in
  let hi = (w lsr 8) land 0xFF in
  sprintf "%02X %02X" lo hi

let args_to_hex_string cpu (i:Cpu.instr) =
  let open Cpu in
  match i.mode with
  | Immediate -> sprintf "%02X" i.args
  | IndirectX | IndirectY | ZeroPageX | ZeroPageY | Relative -> sprintf "%02X" (load_byte cpu (cpu.pc + 1))
  | Absolute -> word_to_byte_string (Option.value_exn i.target)
  | Indirect | AbsoluteX | AbsoluteY -> word_to_byte_string (load_word cpu (cpu.pc + 1))
  | ZeroPage -> sprintf "%02X" (Option.value_exn i.target)
  | Accumulator | Implicit -> ""

let trace (cpu : Cpu.t) instruction opcode =
  let cy = cpu.cycles * 3 mod 341 in
  let args = args_to_hex_string cpu instruction in
  let status = sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d" cpu.a cpu.x cpu.y (Cpu.flags_to_int cpu) cpu.s cy in
  let str_op = Instructions.show instruction.op in
  let instr = sprintf "%s %s" str_op (args_to_string cpu instruction) in
  (* let instr = str_op in *)
  sprintf "%04X  %02X %-6s%-32s %s" cpu.pc opcode args instr status

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
    print_endline @@ trace cpu instruction opcode;
    Debugger.prompt cpu;
  )

let step ?(trace_fun=trace) nes =
  let prev_cycles = nes.cpu.cycles in
  let log = Cpu.step nes.cpu ~on_step:on_step ~trace_fun in
  let elapsed_cycles = nes.cpu.cycles - prev_cycles in

  for _ = 1 to elapsed_cycles * 3 do
    Ppu.step nes.ppu;
  done;

  if nes.ppu.nmi_triggered then Cpu.trigger_nmi nes.cpu;
  log
