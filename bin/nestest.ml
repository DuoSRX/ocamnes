open Nes
open Nes.Cpu
open Nes.Cartridge

let logs = Array.make 10 ""
let log_length = 10

let trace cpu instruction opcode =
  let str_op = Instructions.show instruction.op in
  let instr = Printf.sprintf "%s %s" str_op (args_to_string cpu instruction) in
  (* Horrible hack to comform to Nestest output logs... *)
  let instr2 = if (instruction.op = Instructions.NOP) && opcode <> 0xEA then Printf.sprintf "*%s " instr else Printf.sprintf " %s" instr in
  let args = args_to_hex_string cpu instruction in
  let cy = cpu.cycles * 3 mod 341 in
  let status = Printf.sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d" cpu.a cpu.x cpu.y (flags_to_int cpu) cpu.s cy in
  Printf.sprintf "%04X  %02X %-6s%-32s %s" cpu.pc opcode args instr2 status

let main () =
  let nestest = Arg.read_arg "./roms/nestest.log" in
  let rom = load_rom "./roms/nestest.nes" in
  let nes = Nes.make ~tracing:true ~nestest:true rom in
  let cpu = nes.cpu in
  cpu.pc <- 0xC000;

  let term = ref false in

  while not !term do
    let log = Option.get (Nes.step nes ~trace_fun:trace) in
    logs.(cpu.steps mod log_length) <- log;

    let nestest_log = nestest.(cpu.steps) in
    if not (String.equal log nestest_log) then (
      Printf.printf "Nestest discrepancy detected @ PC = %04X, nestest.log line %d\n\n" cpu.pc cpu.steps;
      Printf.printf "Nestest: %s\n" nestest_log;
      Printf.printf "Ocamnes: %s\n" log;
      print_endline "\nBacktrace:";
      Array.iter print_endline logs;
      exit 1;
    );

    (* Stop before testing all the illegal opcodes (except NOPs) *)
    if cpu.pc = 0xE543 then term := true
  done;

  print_endline "Nestest run successful"

let () = main ()
