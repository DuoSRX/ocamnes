open Core
open OUnit2
open Nes.Cpu

let make_cpu () =
  let memory = Array.create ~len:0x10000 0 in
  {
    a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0; extra_cycles = 0;
    zero = false; negative = false; carry = false; decimal = false; interrupt = true; overflow = false
  }

let push_byte_test _ =
  let cpu = make_cpu () in
  push_byte cpu 0x42;
  assert_equal 0x42 (pop_byte cpu) ~printer:string_of_int

let push_word_test _ =
  let cpu = make_cpu () in
  push_word cpu 0x1234;
  assert_equal 0x1234 (pop_word cpu) ~printer:string_of_int;
  push_byte cpu 0x12;
  push_byte cpu 0x34;
  assert_equal 0x1234 (pop_word cpu) ~printer:string_of_int

let tests = "test suite for CPU" >::: [
  "stack bytes" >:: push_byte_test;
  "stack words" >:: push_word_test;
]

let () = run_test_tt_main tests
