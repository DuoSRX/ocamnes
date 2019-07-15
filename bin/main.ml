open Core
open Nes
open Nes.Cpu
open Nes.Cartridge

let load_nestest_log () =
  In_channel.read_lines "./nestest.log" |> Array.of_list

let main () =
  let nestest = In_channel.read_lines "./nestest.log" |> Array.of_list in
  let memory = Array.create ~len:0x10000 0 in
  let rom = load_rom () in
  load_rom_into_memory memory rom;
  let cpu = {
    a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0; extra_cycles = 0;
    zero = false; negative = false; carry = false; decimal = false; interrupt = true; overflow = false } in
  (* let start = memory.(0xFFFC) lor (memory.(0xFFFD) lsl 8) in *)
  let start = 0xC000 in
  cpu.pc <- start;

  let cycles = ref 0 in
  let steps = ref 0 in
  while !steps < 1000 do
    let opcode = load_byte cpu cpu.pc in
    let instruction = decode_instruction cpu opcode in
    let str_op = Instructions.show_instruction instruction.op in
    let instr = sprintf "%s %s" str_op (args_to_string instruction) in
    let str_args = args_to_hex_string instruction in
    let cy = !cycles * 3 mod 341 in
    let status = sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d" cpu.a cpu.x cpu.y (flags_to_int cpu) cpu.s cy in
    let log = sprintf "%04X  %02X %-6s %-31s %s" cpu.pc opcode str_args instr status in
    print_endline log;

    let nestest_log = nestest.(!steps) in
    if not (String.equal log nestest_log) then (
      print_endline "";
      print_endline nestest_log;
      print_endline log;
      failwith "Nestest discrepancy detected";
    );

    execute_instruction cpu instruction;
    if should_change_pc instruction.op then cpu.pc <- cpu.pc + instruction.size;
    cycles := !cycles + instruction.cycles + cpu.extra_cycles;
    steps := !steps + 1;
    cpu.extra_cycles <- 0;
  done;

  ()

let () = main ()
