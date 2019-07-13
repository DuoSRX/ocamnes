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

let load_byte cpu address = cpu.memory.(address)
let load_next_byte cpu = load_byte cpu cpu.pc

let load_word cpu address =
  let a = cpu.memory.(address) in
  let b = cpu.memory.(address + 1) in
  a lor b lsl 8

let load_next_word cpu = load_word cpu cpu.pc

let load_byte_and_bump_pc cpu =
  let byte = load_next_byte cpu in
  cpu.pc <- cpu.pc + 1;
  byte

let load_word_and_bump_pc cpu =
  let word = load_next_word cpu in
  cpu.pc <- cpu.pc + 2;
  word

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
    | Accumulator
    | Memory of int
    | MemoryW of int
    | Immediate of int
  let load cpu = function
    | Accumulator -> cpu.a
    | Memory address -> load_byte cpu address
    | MemoryW address -> load_word cpu address
    | Immediate value -> value
  let store cpu value = function
    | Accumulator -> cpu.a <- value
    | Memory address -> store_byte cpu address value
    | MemoryW address -> store_word cpu address value
    | Immediate _ -> failwith "trying to write to Immediate"
  let target = function
    | Accumulator -> None
    | Memory address -> Some address
    | MemoryW address -> Some address
    | Immediate n -> Some n
end

module AddressingMode = struct
  type t =
    | Implicit | Absolute | AbsoluteX | AbsoluteY
    | ZeroPage | ZeroPageX | ZeroPageY
    | IndirectX | IndirectY | Immediate
  let size = function
    | Implicit -> 1
    | Immediate | ZeroPage | ZeroPageX | ZeroPageY -> 2
    | IndirectX | IndirectY -> 2
    | Absolute | AbsoluteX | AbsoluteY -> 3
end

let decode_addressing_mode cpu am =
  let pc = cpu.pc + 1 in
  match am with
  | AddressingMode.Immediate -> Location.Immediate (load_byte cpu pc)
  | AddressingMode.Absolute -> Location.MemoryW (load_word cpu pc)
  | AddressingMode.ZeroPage -> Location.Memory (load_byte cpu pc)
  | AddressingMode.Implicit -> Location.Immediate 0xDEAD
  | _ -> failwith "unknown addressing mode"

let set_nz_flags cpu value =
  cpu.zero <- value = 0;
  cpu.sign <- value land 0x80 <> 0;
  value

let tax c = c.a <- set_nz_flags c c.x
let txa c = c.x <- set_nz_flags c c.a
let inx c = c.x <- set_nz_flags c (wrapping_add c.x 1)
let dex c = c.x <- set_nz_flags c (wrapping_add c.x (-1))

let inc cpu loc =
  let value = wrapping_add (Location.load cpu loc) 1 in
  Location.store cpu ((set_nz_flags cpu value) land 0xFF) loc

let dec cpu loc =
  let value = wrapping_add (Location.load cpu loc) (-1) in
  Location.store cpu ((set_nz_flags cpu value) land 0xFF) loc

let lda c loc = c.a <- set_nz_flags c (Location.load c loc)
let ldx c loc = c.x <- set_nz_flags c (Location.load c loc)
let sta c loc = Location.store c c.a loc
let stx c loc = Location.store c c.x loc
let sty c loc = Location.store c c.y loc

let jmp c loc = c.pc <- Option.value_exn (Location.target loc)

let jsr cpu loc =
  let address = Option.value_exn (Location.target loc) in
  let pc = cpu.pc - 1 in
  push_word cpu pc;
  cpu.pc <- address

let branch cpu loc cond =
  (* TODO: Figure out some i32 -> u16 magic here *)
  let address = Location.load cpu loc in
  if cond then cpu.pc <- cpu.pc + address

let bcs cpu loc = branch cpu loc cpu.carry
let bcc cpu loc = branch cpu loc (not cpu.carry)

type op = JMP | LDX | JSR | RTS | STX | NOP | SEC | BCS

type instruction = {
  op : op;
  mode : AddressingMode.t;
  location : Location.t;
  cycles : int;
  size: int;
}

let should_change_pc = function
  | JMP | JSR | RTS -> false
  | _ -> true

let decode opcode =
  match opcode with
  | 0x4C -> (JMP, AddressingMode.Absolute, 3)
  | 0xA2 -> (LDX, Immediate, 2)
  | 0x86 -> (STX, ZeroPage, 1)
  | 0x20 -> (JSR, Absolute, 5)
  | 0xEA -> (NOP, Implicit, 2)
  | 0x38 -> (SEC, Implicit, 2)
  | 0xB0 -> (BCS, Absolute, 2)
  | _ -> failwith @@ sprintf "Unknown opcode %#02x" opcode

(* (instruction, bytes read *)
let decode_instruction cpu instruction =
  let (op, mode, cycles) = decode instruction in
  let location = decode_addressing_mode cpu mode in
  { op; mode; cycles; location; size = AddressingMode.size mode}

let execute_instruction cpu instruction =
  let loc = instruction.location in
  match instruction.op with
  | JMP -> jmp cpu loc
  | LDX -> ldx cpu loc
  | STX -> stx cpu loc
  | JSR -> jsr cpu loc
  | SEC -> cpu.carry <- true
  | BCS -> bcs cpu loc
  | NOP -> ()
  | _ -> failwith "Unimplemented instruction"

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
    let opcode = load_next_byte cpu in
    let instruction = decode_instruction cpu opcode in
    printf "%04X  %02X  A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%d\n" cpu.pc opcode cpu.a cpu.x cpu.y (flags_to_int cpu) cpu.s !cycles;
    (* cpu.pc <- cpu.pc + 1; *)
    (* let arg = (Location.load cpu instruction.location) in *)
    (* let arg = Option.value_exn (Location.target instruction.location) in *)
    (* printf "%04x\n" (Location.load cpu @@ Location.MemoryW 0xC001); *)
    (* printf "%04x\n" arg; *)
    execute_instruction cpu instruction;
    if should_change_pc instruction.op then cpu.pc <- cpu.pc + instruction.size;
    (* printf "%04X  %02X  A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%d\n" cpu.pc opcode cpu.a cpu.x cpu.y (flags_to_int cpu) cpu.s !cycles; *)
    cycles := !cycles + 1;
  done;

  ()

let () = main ()
