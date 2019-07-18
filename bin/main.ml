open Core
open Nes
open Nes.Cpu
open Nes.Cartridge

let run_length = 10000000
let log_length = 60
let logs = Array.create ~len:log_length ""
let term = ref false

let rec n_times f n =
  if n > 0 then (
    f (); n_times f (n - 1)
  )

let main () =
  let memory = Array.create ~len:0x10000 0 in
  let rom = load_rom "./roms/donkey.nes" in

  let cpu = {
    rom = rom; ppu = Ppu.make ~rom; cycles = 0;
    a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0; extra_cycles = 0;
    zero = false; negative = false; carry = false; decimal = false; interrupt = true; overflow = false;
    nestest = false; steps = -1;
  } in

  cpu.pc <- Cpu.load_word cpu 0xFFFC;

  while cpu.steps < run_length && not !term do
    let cycles = cpu.cycles in
    logs.(cpu.steps % log_length) <- step cpu;

    let elapsed = cpu.cycles - cycles in
    n_times (fun () -> Ppu.step cpu.ppu) (elapsed * 3);
    if cpu.ppu.nmi then Cpu.nmi cpu;
    (* print_endline logs.(cpu.steps % log_length); *)
  done;

  Array.iter logs ~f:print_endline;
  ()

let () = main ()
