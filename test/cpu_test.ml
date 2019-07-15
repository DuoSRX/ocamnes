open Core
open Nes.Cpu

(* Yeah this doesn't work... *)

let make_cpu () =
  let memory = Array.create ~len:0x10000 0 in
  {
    a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0; extra_cycles = 0;
    zero = false; negative = false; carry = false; decimal = false; interrupt = true; overflow = false
  }

let jmp () =
  let cpu = make_cpu () in
  Alcotest.(check int) "same pc" 0x0 cpu.pc

let test_set = [
  "JMP", `Quick, jmp;
]

let () =
  Alcotest.run "CPU tests" [
    "test_set", test_set;
  ]
