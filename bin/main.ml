open Core
(* open Tsdl *)

module Flags = struct
  let carry = 0b00000001
  let zero  = 0b00000010
  let interrupt = 0b00000100
  let decimal = 0b00001000
  let break4 = 0b00010000
  let break5 = 0b00100000
  let overflow = 0b01000000
  let negative = 0b10000000
end

let wrapping_add a b = (a + b) land 0xFF

type cpu = {
  mutable a : int;
  mutable x : int;
  mutable y : int;
  mutable pc : int;
  mutable s : int;

  mutable sign : bool;
  mutable overflow : bool;
  mutable decimal : bool;
  mutable interrupt : bool;
  mutable zero : bool;
  mutable carry : bool;

  memory : int array;
}

let load_next_byte cpu = cpu.memory.(cpu.pc)

let load_byte_and_bump_pc cpu =
  let byte = cpu.memory.(cpu.pc) in
  cpu.pc <- cpu.pc + 1;
  byte

let load_word_and_bump_pc cpu =
  let a = load_byte_and_bump_pc cpu in
  let b = load_byte_and_bump_pc cpu in
  a lor b lsl 8

let load_word_zero_page cpu address =
  cpu.memory.(address) lor (cpu.memory.(address + 1) lsl 8)

let store_byte cpu address value =
  cpu.memory.(address) <- value

let store_word cpu address value =
  let lo = value land 0xFF in
  let hi = (value lsl 8) land 0xFF in
  store_byte cpu address lo;
  store_byte cpu (address + 1) hi

let push_byte cpu value =
  let sp = cpu.s in
  store_word cpu (0x100 + sp) value;
  cpu.s <- wrapping_add cpu.s (-1)

let push_word cpu value =
  let sp = wrapping_add cpu.s (-1) in
  store_word cpu (0x100 + sp) value;
  cpu.s <- wrapping_add cpu.s (-2)

let flags_to_int cpu =
  let p = ref (Flags.break4 land Flags.break5) in
  if cpu.sign then p := !p lor Flags.negative;
  if cpu.overflow then p := !p lor Flags.overflow;
  if cpu.decimal then p := !p lor Flags.decimal;
  if cpu.interrupt then p := !p lor Flags.interrupt;
  if cpu.zero then p := !p lor Flags.zero;
  if cpu.carry then p := !p lor Flags.carry;
  !p

module Location = struct
  type t =
    | Accumulator of cpu
    | Memory of cpu * int
    | Immediate of cpu
  let load = function
    | Accumulator cpu -> cpu.a
    | Memory (cpu, address) -> cpu.memory.(address)
    | Immediate cpu -> load_byte_and_bump_pc cpu
  let store mode value = match mode with
    | Accumulator cpu -> cpu.a <- value
    | Memory (cpu, address) -> cpu.memory.(address) <- value
    | _ -> ()
end

(* type addressing_mode =
  | Implicit | Absolute | AbsoluteX | AbsoluteY
  | ZeroPage | ZeroPageX | ZeroPageY
  | IndirectX | IndirectY | Immediate *)

(* module AddressingMode = struct
  type t =
    | Implicit | Absolute | AbsoluteX | AbsoluteY
    | ZeroPage | ZeroPageX | ZeroPageY
    | IndirectX | IndirectY | Immediate
  let size = function
    | Implicit -> 1
    | Immediate | ZeroPage | ZeroPageX | ZeroPageY -> 2
    | IndirectX | IndirectY -> 2
    | Absolute | AbsoluteX | AbsoluteY -> 3
end *)

let immediate cpu = Location.Immediate cpu

let absolute cpu =
  let address = load_word_and_bump_pc(cpu) in
  Location.Memory(cpu, address)

let absolute_x cpu =
  let address = wrapping_add (load_word_and_bump_pc cpu) cpu.x in
  Location.Memory(cpu, address)

let absolute_y cpu =
  let address = wrapping_add (load_word_and_bump_pc cpu) cpu.y in
  Location.Memory(cpu, address)

let zero_page cpu =
  let address = load_byte_and_bump_pc cpu in
  Location.Memory(cpu, address)

let zero_page_x cpu =
  let address = wrapping_add (load_byte_and_bump_pc cpu) cpu.x in
  Location.Memory(cpu, address)

let zero_page_y cpu =
  let address = wrapping_add (load_byte_and_bump_pc cpu) cpu.y in
  Location.Memory(cpu, address)

let indirect_x cpu =
  let target = wrapping_add (load_byte_and_bump_pc cpu) cpu.x in
  let address = load_word_zero_page cpu target in
  Location.Memory(cpu, address)

let indirect_y cpu =
  let target = load_byte_and_bump_pc cpu in
  let address = wrapping_add (load_word_zero_page cpu target) cpu.y in
  Location.Memory(cpu, address)

let set_nz_flags cpu value =
  cpu.zero <- value = 0;
  cpu.sign <- value land 0x80 <> 0;
  value

let tax c = c.a <- set_nz_flags c c.x
let txa c = c.x <- set_nz_flags c c.a
let inx c = c.x <- set_nz_flags c (wrapping_add c.x 1)
let dex c = c.x <- set_nz_flags c (wrapping_add c.x (-1))

let inc c am =
  let value = wrapping_add (Location.load(am)) 1 in
  Location.store(am) ((set_nz_flags c value) land 0xFF)

let dec c am =
  let value = wrapping_add (Location.load(am)) (-1) in
  Location.store(am) ((set_nz_flags c value) land 0xFF)

let lda c am = c.a <- set_nz_flags c (Location.load(am))
let ldx c am = c.x <- set_nz_flags c (Location.load(am))
let sta c am = Location.store(am) c.a
let stx c am = Location.store(am) c.x
let sty c am = Location.store(am) c.y

let jmp c = c.pc <- load_word_and_bump_pc c

let jsr cpu =
  let address = load_word_and_bump_pc cpu in
  let pc = cpu.pc - 1 in
  push_word cpu pc;
  cpu.pc <- address

let branch cpu cond =
  (* TODO: Figure out some i32 -> u16 magic here *)
  let byte = load_byte_and_bump_pc cpu in
  if cond then cpu.pc <- cpu.pc + byte

let bcs cpu = branch cpu cpu.carry
let bcc cpu = branch cpu (not cpu.carry)

let execute_instruction cpu instruction =
  match instruction with
  | 0xEA -> () (* NOP *)
  | 0x38 -> cpu.carry <- true
  | 0x18 -> cpu.carry <- false
  | 0xAA -> tax cpu
  | 0x8A -> txa cpu
  | 0xE8 -> inx cpu
  | 0xCA -> dex cpu
  | 0xCE -> dec cpu (absolute cpu)
  | 0xEE -> inc cpu (absolute cpu)
  | 0xA2 -> ldx cpu (immediate cpu)
  | 0xA9 -> lda cpu (immediate cpu)
  | 0xA5 -> lda cpu (zero_page cpu)
  | 0xB5 -> lda cpu (zero_page_x cpu)
  | 0xAD -> lda cpu (absolute cpu)
  | 0xBD -> lda cpu (absolute_x cpu)
  | 0xB9 -> lda cpu (absolute_y cpu)
  | 0xA1 -> lda cpu (indirect_x cpu)
  | 0xB1 -> lda cpu (indirect_y cpu)
  | 0x8D -> sta cpu (absolute cpu)
  | 0x86 -> stx cpu (zero_page cpu)
  | 0x84 -> sty cpu (zero_page cpu)
  | 0x4C -> jmp cpu
  | 0x20 -> jsr cpu
  | 0xB0 -> bcs cpu
  | 0x90 -> bcc cpu
  | _ -> failwith @@ sprintf "unknown instruction %#04x" instruction

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

let load_rom =
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
  let rom = load_rom in
  load_rom_into_memory memory rom;
  let cpu = {
    a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0;
    zero = false; sign = false; carry = false; decimal = false; interrupt = true; overflow = false } in
  (* let start = memory.(0xFFFC) lor (memory.(0xFFFD) lsl 8) in *)
  let start = 0xC000 in
  cpu.pc <- start;

  let cycles = ref 0 in
  while !cycles < 30 do
    let instruction = load_next_byte cpu in
    printf "%04X  %02X  A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%d\n" cpu.pc instruction cpu.a cpu.x cpu.y (flags_to_int cpu) cpu.s !cycles;
    cpu.pc <- cpu.pc + 1;
    execute_instruction cpu instruction;
    cycles := !cycles + 1;
  done;

  ()

let () = main ()
