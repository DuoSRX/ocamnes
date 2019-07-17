open Core
open Nes
open Nes.Cpu
open Nes.Cartridge

let logs = Array.create ~len:10 ""
let steps = ref 0

let trace cpu instruction opcode =
  let str_op = Instructions.show_instruction instruction.op in
  let instr = sprintf "%s %s" str_op (args_to_string cpu instruction) in
  (* Horrible hack to comform to Nestest output logs... *)
  let instr2 = if (instruction.op = NOP) && opcode <> 0xEA then sprintf "*%s " instr else sprintf " %s" instr in
  let args = args_to_hex_string cpu instruction in
  let cy = cpu.cycles * 3 mod 341 in
  let status = sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d" cpu.a cpu.x cpu.y (flags_to_int cpu) cpu.s cy in
  let log = sprintf "%04X  %02X %-6s%-32s %s" cpu.pc opcode args instr2 status in
  logs.(!steps % 10) <- log

let main () =
  let nestest = In_channel.read_lines "./nestest.log" |> Array.of_list in
  let memory = Array.create ~len:0x10000 0 in
  let rom = load_rom "./nestest.nes" in
  let cpu = {
    rom = rom; ppu = Ppu.make (); cycles = 0;
    a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0; extra_cycles = 0;
    zero = false; negative = false; carry = false; decimal = false; interrupt = true; overflow = false
  } in
  cpu.pc <- 0xC000;

  let term = ref false in

  while not !term do
    step cpu ~trace_fun:trace;

    let log = logs.(!steps % 10) in
    let nestest_log = nestest.(!steps) in
    if not (String.equal log nestest_log) then (
      print_endline "";
      print_endline nestest_log;
      print_endline log;
      print_endline "\nPrevious 10 log lines:";
      Array.iter logs ~f:print_endline;
      failwith @@ sprintf "Nestest discrepancy detected @ PC = %04X, LOG = %d" cpu.pc !steps;
    );

    steps := !steps + 1;

    (* Stop before testing all the illegal opcodes (except NOPs) *)
    if cpu.pc = 0xE543 then term := true
  done;

  (* Array.iter logs ~f:print_endline; *)
  print_endline "Nestest run successful"

let () = main ()
