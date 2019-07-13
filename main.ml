open Core
open Cpu
open Instructions

type header = {
  prg_size: int;
  chr_size: int;
}

type rom = {
  headers : header;
  prg: int array;
  chr: int array;
}

let open_file name =
  let file = Stdio.In_channel.create name in
  let size = Int64.to_int_trunc(In_channel.length file) in
  let buffer = Bytes.create size in
  let _ = Stdio.In_channel.really_input file ~buf:buffer ~pos:0 ~len:size in
  let result = Array.create ~len:size 0 in
  for i = 0 to size - 1 do
    result.(i) <- int_of_char @@ Bytes.get buffer i
  done;
  result

let load_headers rom =
  { prg_size = rom.(4) * 0x4000; chr_size = rom.(5) * 0x2000 }

let load_rom () =
  let filename = "./nestest.nes" in
  let rom = open_file filename in
  let headers = load_headers rom in
  printf "Loaded rom %s\n" filename;
  printf "PRG:%04x CHR:%04x\n" headers.prg_size headers.chr_size;
  {
    headers;
    prg = Array.slice rom 0x10 (0x10 + headers.prg_size);
    chr = Array.slice rom (0x10 + headers.prg_size) (0x10 + headers.prg_size + headers.chr_size);
  }

let load_rom_into_memory mem rom =
  let dst_pos = 0x10000 - rom.headers.prg_size in
  Array.blit ~len:rom.headers.prg_size ~src:rom.prg ~src_pos:0 ~dst:mem ~dst_pos
  (* TODO: Load CHR into PPU *)
  (* Array.blit ~len:rom.headers.chr_size ~src:rom.chr ~src_pos:0 ~dst:cpu.memory ~dst_pos:0 *)

let main () =
  let memory = Array.create ~len:0x10000 0 in
  let rom = load_rom () in
  load_rom_into_memory memory rom;
  let cpu = {
    a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0;
    zero = false; sign = false; carry = false; decimal = false; interrupt = true; overflow = false } in
  (* let start = memory.(0xFFFC) lor (memory.(0xFFFD) lsl 8) in *)
  let start = 0xC000 in
  cpu.pc <- start;

  let cycles = ref 0 in
  while !cycles < 30 do
    let opcode = load_byte cpu cpu.pc in
    let instruction = decode_instruction cpu opcode in
    let str_op = op_to_string instruction.op in
    printf "%04X %02X %s %s A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%d\n" cpu.pc opcode str_op (args_to_string instruction) cpu.a cpu.x cpu.y (flags_to_int cpu) cpu.s !cycles;
    execute_instruction cpu instruction;
    if should_change_pc instruction.op then cpu.pc <- cpu.pc + instruction.size;
    cycles := !cycles + instruction.cycles;
  done;

  ()

let () = main ()
