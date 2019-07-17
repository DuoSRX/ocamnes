open Core
open Nes
open Nes.Cpu
open Nes.Cartridge

let run_length = 10000000
let log_length = 60
let logs = Array.create ~len:log_length ""
let steps = ref 0
let term = ref false

let rec n_times f n =
  if n > 0 then (
    f (); n_times f (n - 1)
  )

let trace (cpu : cpu) instruction opcode =
  let cy = cpu.cycles * 3 mod 341 in
  let args = args_to_hex_string cpu instruction in
  let status = sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d" cpu.a cpu.x cpu.y (flags_to_int cpu) cpu.s cy in
  let str_op = Instructions.show_instruction instruction.op in
  let instr = sprintf "%s %s" str_op (args_to_string cpu instruction) in
  (* let instr = str_op in *)
  let log = sprintf "%04X  %02X %-6s%-32s %s" cpu.pc opcode args instr status in
  logs.(!steps % log_length) <- log

let main () =
  let memory = Array.create ~len:0x10000 0 in
  let rom = load_rom "./donkey.nes" in

  let cpu = {
    rom = rom; ppu = Ppu.make ~rom; cycles = 0;
    a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0; extra_cycles = 0;
    zero = false; negative = false; carry = false; decimal = false; interrupt = true; overflow = false;
    nestest = false
  } in

  cpu.pc <- Cpu.load_word cpu 0xFFFC;

  while !steps < run_length && not !term do
    let cycles = cpu.cycles in
    step cpu ~trace_fun:trace;
    (* try step cpu ~trace_fun:trace
    with Failure f -> (
      print_endline f;
      Array.iter logs ~f:print_endline;
    ); *)

    let elapsed = cpu.cycles - cycles in
    n_times (fun () -> Ppu.step cpu.ppu) (elapsed * 3);
    if cpu.ppu.nmi then Cpu.nmi cpu;
    print_endline logs.(!steps % log_length);
    steps := !steps + 1
  done;

  Array.iter logs ~f:print_endline;
  ()

let () = main ()
