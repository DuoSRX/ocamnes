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
  let cpu = Cpu.make ~nestest ~tracing ~memory in
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
  | Immediate -> Printf.sprintf "#$%02X" i.args
  | Absolute ->
    if op_is_branch i.op then
      Printf.sprintf "$%04X = %02X" (Option.get i.target) i.args
    else
      Printf.sprintf "$%04X" (Option.get i.target)
  | AbsoluteX ->
    Printf.sprintf "$%04X,X @ %04X = %02X" (load_word cpu (cpu.pc + 1)) (Option.get i.target) i.args
  | AbsoluteY ->
    Printf.sprintf "$%04X,Y @ %04X = %02X" (load_word cpu (cpu.pc + 1)) (Option.get i.target) i.args
  | ZeroPage -> Printf.sprintf "$%02X = %02X" (Option.get i.target) i.args
  | ZeroPageX ->
    let byte = load_byte cpu (cpu.pc + 1) in
    Printf.sprintf "$%02X,X @ %02X = %02X" byte (Option.get i.target) i.args
  | ZeroPageY ->
    let byte = load_byte cpu (cpu.pc + 1) in
    Printf.sprintf "$%02X,Y @ %02X = %02X" byte (Option.get i.target) i.args
  | IndirectX ->
    let byte = load_byte cpu (cpu.pc + 1) in
    let sum = wrapping_add byte cpu.x in
    Printf.sprintf "($%02X,X) @ %02X = %04X = %02X" byte sum (Option.get i.target) i.args
  | IndirectY ->
    let byte = load_byte cpu (cpu.pc + 1) in
    let sum = wrapping_sub_w (Option.get i.target) cpu.y in
    Printf.sprintf "($%02X),Y = %04X @ %04X = %02X" byte sum (Option.get i.target) i.args
  | Indirect -> Printf.sprintf "($%04X) = %04X" (load_word cpu (cpu.pc + 1)) (Option.get i.target)
  | Relative ->
    Printf.sprintf "$%04X" (i.args);
  | Accumulator -> "A"
  | Implicit -> ""

let word_to_byte_string w =
  let lo = w land 0xFF in
  let hi = (w lsr 8) land 0xFF in
  Printf.sprintf "%02X %02X" lo hi

let args_to_hex_string cpu (i:Cpu.instr) =
  let open Cpu in
  match i.mode with
  | Immediate -> Printf.sprintf "%02X" i.args
  | IndirectX | IndirectY | ZeroPageX | ZeroPageY | Relative -> Printf.sprintf "%02X" (load_byte cpu (cpu.pc + 1))
  | Absolute -> word_to_byte_string (Option.get i.target)
  | Indirect | AbsoluteX | AbsoluteY -> word_to_byte_string (load_word cpu (cpu.pc + 1))
  | ZeroPage -> Printf.sprintf "%02X" (Option.get i.target)
  | Accumulator | Implicit -> ""

let trace (cpu : Cpu.t) instruction opcode =
  let cy = cpu.cycles * 3 mod 341 in
  let args = args_to_hex_string cpu instruction in
  let status = Printf.sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d" cpu.a cpu.x cpu.y (Cpu.flags_to_int cpu) cpu.s cy in
  let str_op = Instructions.show instruction.op in
  let instr = Printf.sprintf "%s %s" str_op (args_to_string cpu instruction) in
  Printf.sprintf "%04X  %02X %-6s%-32s %s" cpu.pc opcode args instr status


module Debugger = struct
  module IntSet = CCSet.Make(CCInt)
  let breakpoints = ref IntSet.empty
  let break_on_step = ref false
  let break_after = ref (-2)

  let rec prompt nes =
    let cpu = nes.cpu in
    let ppu = nes.ppu in
    print_string "(DEBUG) ";
    Out_channel.(flush stdout);
    match String.split_on_char ' ' (input_line stdin) with
    | [""] -> prompt nes
    | ["s"] | ["step"] -> break_on_step := true
    | ["c"] | ["continue"] -> break_on_step := false
    | ["q"] | ["quit"] | ["exit"] -> exit 1;
    | ["steps"] -> Printf.printf "%d\n" cpu.steps; prompt nes
    | "bp" :: "del" :: bp :: _ ->
      breakpoints := IntSet.remove (int_of_string bp) !breakpoints;
      prompt nes
    | "bp" :: "add" :: bp :: _ ->
      breakpoints := IntSet.add (int_of_string bp) !breakpoints;
      prompt nes
    | "bp" :: [] ->
      !breakpoints |> IntSet.to_list |> List.map (Printf.sprintf "%04X") |> String.concat ", " |> print_endline;
      prompt nes
    | "byte" :: addr :: [] ->
      Printf.printf "%02X\n" (Cpu.load_byte cpu (int_of_string addr));
      prompt nes
    | "byte" :: a :: b :: [] ->
      CCList.range (int_of_string a) (int_of_string b)
      |> List.map (Cpu.load_byte cpu)
      |> List.iter (Printf.printf "%02X ");
      print_endline "";
      prompt nes
    | "ppu" :: addr :: [] ->
      Printf.printf "%02X\n" (Ppu.load ppu (int_of_string addr));
      prompt nes
    | "ppu" :: a :: b :: [] ->
      CCList.range (int_of_string a) (int_of_string b)
      |> List.map (Ppu.load ppu)
      |> List.iter (Printf.printf "%02X ");
      print_endline "";
      prompt nes
    | "oam" :: [] ->
      ppu.oam |> Array.iter (Printf.printf "%02X ");
      print_endline "";
      prompt nes
    | "word" :: addr :: _ ->
      Printf.printf "%04X\n" (Cpu.load_word cpu (int_of_string addr));
      prompt nes
    | "pputrace" :: [] ->
      Printf.printf "T:%04X V:%04X X:%04X\n" ppu.t ppu.v ppu.x;
      prompt nes
    | _ ->
      print_endline "Unknown command (q to quit, s to step, c to continue)";
      prompt nes

    let should_break (cpu:Cpu.t) =
      !break_on_step || IntSet.mem cpu.pc !breakpoints || !break_after = cpu.steps

    let on_step nes instruction opcode =
      if should_break nes.cpu then (
        print_endline @@ trace nes.cpu instruction opcode;
        prompt nes
      )
end

let step ?(trace_fun=trace) nes =
  let prev_cycles = nes.cpu.cycles in
  let log = Cpu.step nes.cpu ~on_step:(Debugger.on_step nes) ~trace_fun in
  let elapsed_cycles = nes.cpu.cycles - prev_cycles in

  for _ = 1 to elapsed_cycles * 3 do
    Ppu.step nes.ppu;
  done;

  if nes.ppu.nmi_triggered then (
    nes.cpu.nmi_triggered <- true;
    nes.ppu.nmi_triggered <- false
  );
  log
