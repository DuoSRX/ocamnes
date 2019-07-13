open Core
open Instructions

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

module AddressingMode = struct
  type t =
    | Implicit | Absolute | AbsoluteX | AbsoluteY
    | ZeroPage | ZeroPageX | ZeroPageY
    | Relative | IndirectX | IndirectY | Immediate
end

let decode_addressing_mode cpu am =
  let pc = cpu.pc + 1 in
  match am with
  | AddressingMode.Immediate ->
    (load_byte cpu pc, Some pc, 1)
  | AddressingMode.Absolute ->
    let address = load_word cpu pc in
    (load_byte cpu address, Some address, 2)
  | AddressingMode.ZeroPage ->
    let address = load_byte cpu pc in
    (load_byte cpu address, Some address, 1)
  | AddressingMode.Relative ->
    (load_byte cpu pc, Some pc, 1)
  | AddressingMode.Implicit ->
    (0, None, 0)
  | _ -> failwith "unimplemented addressing mode"

let set_nz_flags cpu value =
  cpu.zero <- value = 0;
  cpu.sign <- value land 0x80 <> 0;
  value

let tax c = c.a <- set_nz_flags c c.x
let txa c = c.x <- set_nz_flags c c.a
let inx c = c.x <- set_nz_flags c (wrapping_add c.x 1)
let dex c = c.x <- set_nz_flags c (wrapping_add c.x (-1))

(* let inc cpu loc =
  let value = wrapping_add (Location.load cpu loc) 1 in
  Location.store cpu ((set_nz_flags cpu value) land 0xFF) loc

let dec cpu loc =
  let value = wrapping_add (Location.load cpu loc) (-1) in
  Location.store cpu ((set_nz_flags cpu value) land 0xFF) loc

let lda c loc = c.a <- set_nz_flags c (Location.load c loc)
let ldx c loc = c.x <- set_nz_flags c (Location.load c loc)
let sta c loc = Location.store c c.a loc
let stx c loc = Location.store c c.x loc
let sty c loc = Location.store c c.y loc *)

let jmp c dst = c.pc <- dst
let stx c args = c.x <- args
let ldx c args = c.x <- set_nz_flags c args

let jsr cpu address =
  push_word cpu cpu.pc;
  cpu.pc <- address

let branch cpu offset cond =
  (* TODO: Figure out some i32 -> u16 magic here *)
  if cond then cpu.pc <- cpu.pc + offset

let bcs cpu address = branch cpu address cpu.carry
let bcc cpu address = branch cpu address @@ not cpu.carry

type instruction = {
  op : Instructions.instruction;
  mode : AddressingMode.t;
  args : int;
  target : int option;
  cycles : int;
  size: int;
}

let args_to_string i =
  match i.mode with
  | Absolute -> sprintf "$%04X" (Option.value_exn i.target)
  | Immediate -> sprintf "#$%02X" i.args
  | ZeroPage -> sprintf "$%02X = %02x" (Option.value_exn i.target) i.args
  | Relative -> sprintf "$%04X" ((Option.value_exn i.target) + i.args + 1)
  | _ -> ""

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
  | 0xB0 -> (BCS, Relative, 2)
  | _ -> failwith @@ sprintf "Unknown opcode %#02x" opcode

let decode_instruction cpu instruction =
  let (op, mode, cycles) = decode instruction in
  let (args, target, size) = decode_addressing_mode cpu mode in
  { op; mode; cycles; args; target; size = size + 1 }

let execute_instruction cpu instruction =
  let args = instruction.args in
  match instruction.op with
  | JMP -> jmp cpu (Option.value_exn instruction.target)
  | LDX -> ldx cpu args
  | STX -> stx cpu args
  | JSR -> jsr cpu (Option.value_exn instruction.target)
  | NOP -> ()
  | SEC -> cpu.carry <- true
  | BCS -> bcs cpu instruction.args
  (* | BCS -> bcs cpu (Option.value_exn instruction.target) *)
  (* | STX -> stx cpu loc
  | SEC -> cpu.carry <- true
  | NOP -> () *)
  | _ -> failwith @@ sprintf "Unimplemented instruction %s" (Instructions.show_instruction instruction.op)
