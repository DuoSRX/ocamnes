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
  let b = break4 lor break5
end

let wrapping_add a b = (a + b) land 0xFF
let wrapping_add_w a b = (a + b) land 0xFFFF

type cpu = {
  mutable a : int;
  mutable x : int;
  mutable y : int;
  mutable pc : int;
  mutable s : int;

  mutable negative : bool;
  mutable overflow : bool;
  mutable decimal : bool;
  mutable interrupt : bool;
  mutable zero : bool;
  mutable carry : bool;

  mutable extra_cycles: int;

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
  let hi = (value lsr 8) land 0xFF in
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

let pop_byte cpu =
  let byte = load_byte cpu (0x100 + cpu.s + 1) in
  cpu.s <- wrapping_add cpu.s 1;
  byte

let pop_word cpu =
  let word = load_word cpu (0x100 + cpu.s + 1) in
  cpu.s <- wrapping_add cpu.s 2;
  word

let flags_to_int cpu =
  let p = ref Flags.break5 in
  if cpu.negative then p := !p lor Flags.negative;
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
    [@@deriving show]
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
  cpu.negative <- value land 0x80 <> 0;
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
  Location.store cpu ((set_nz_flags cpu value) land 0xFF) loc *)

let jmp c dst = c.pc <- dst
let sta c addr = store_byte c addr c.a
let stx c addr = store_byte c addr c.x
let sty c addr = store_byte c addr c.y
let lda c args = c.a <- set_nz_flags c args
let ldx c args = c.x <- set_nz_flags c args

let brk cpu =
  push_word cpu (cpu.pc + 2);
  push_byte cpu @@ (flags_to_int cpu) lor Flags.b;
  cpu.interrupt <- true;
  cpu.pc <- load_word cpu 0xFFEE

let jsr cpu address =
  push_word cpu (wrapping_add_w cpu.pc 3);
  cpu.pc <- address

let branch cpu offset cond =
  if cond then
    (* Treat offset as a 8 bit signed integer and wrap around if necessary *)
    (* let off = if offset > 127 then -(((lnot offset) + 1) land 0xFF) else offset in *)
    let c = (cpu.pc + offset) land 0xFFFF in
    (* Add an extra cycle when cond is true and one more when crossing a page boundary *)
    let cycles = if (c / 256) <> (cpu.pc / 256) then 2 else 1 in
    cpu.extra_cycles <- cpu.extra_cycles + cycles;
    cpu.pc <- c

let bcs cpu offset = branch cpu offset cpu.carry
let bcc cpu offset = branch cpu offset (not cpu.carry)
let beq cpu offset = branch cpu offset cpu.zero
let bne cpu offset = branch cpu offset (not cpu.zero)
let bpl cpu offset = branch cpu offset (not cpu.negative)
let bvs cpu offset = branch cpu offset cpu.overflow
let bvc cpu offset = branch cpu offset (not cpu.overflow)

let bit cpu byte =
  let result = cpu.a land byte in
  cpu.negative <- Flags.negative land result <> 0;
  cpu.overflow <- Flags.overflow land result <> 0;
  cpu.zero <- result = 0

let php cpu =
  let flags = (flags_to_int cpu) lor (Flags.b) in
  push_byte cpu flags

let pla cpu = cpu.a <- set_nz_flags cpu (pop_byte cpu)

let andd cpu args = cpu.a <- set_nz_flags cpu (cpu.a land args)

type instr = {
  op : Instructions.instruction;
  mode : AddressingMode.t;
  args : int;
  target : int option;
  cycles : int;
  size: int;
} [@@deriving show]

let args_to_string i =
  match i.mode with
  | Immediate -> sprintf "#$%02X" i.args
  | Absolute -> sprintf "$%04X" (Option.value_exn i.target)
  | ZeroPage -> sprintf "$%02X = %02X" (Option.value_exn i.target) i.args
  | Relative -> sprintf "$%04X" ((Option.value_exn i.target) + i.args + 1)
  | Implicit -> ""
  | _ -> failwith @@ sprintf "can't print %s" @@ show_instr i

let word_to_byte_string w =
  let lo = w land 0xFF in
  let hi = (w lsr 8) land 0xFF in
  sprintf "%02X %02X" lo hi

let args_to_hex_string i =
  match i.mode with
  | Immediate -> sprintf "%02X" i.args
  | Absolute -> word_to_byte_string (Option.value_exn i.target)
  | ZeroPage -> sprintf "%02X" (Option.value_exn i.target)
  | Relative -> sprintf "%02X" i.args
  | Implicit -> ""
  | _ -> failwith @@ sprintf "can't print %s" @@ show_instr i

let should_change_pc = function
  | JMP | JSR | RTS -> false
  | _ -> true

open AddressingMode

let decode opcode =
  match opcode with
  | 0x00 -> (BRK, Implicit, 7)
  | 0x08 -> (PHP, Implicit, 3)
  | 0x10 -> (BPL, Relative, 2)
  | 0x18 -> (CLC, Implicit, 2)
  | 0x20 -> (JSR, Absolute, 6)
  | 0x24 -> (BIT, ZeroPage, 3)
  | 0x29 -> (AND, Immediate, 2)
  | 0x38 -> (SEC, Implicit, 2)
  | 0x4C -> (JMP, Absolute, 3)
  | 0x50 -> (BVC, Relative, 2)
  | 0x60 -> (RTS, Implicit, 6)
  | 0x68 -> (PLA, Implicit, 4)
  | 0x70 -> (BVS, Relative, 2)
  | 0x78 -> (SEI, Implicit, 2)
  | 0x85 -> (STA, ZeroPage, 3)
  | 0x86 -> (STX, ZeroPage, 3)
  | 0x90 -> (BCC, Relative, 2)
  | 0xA9 -> (LDA, Immediate, 2)
  | 0xA2 -> (LDX, Immediate, 2)
  | 0xB0 -> (BCS, Relative, 2)
  | 0xD0 -> (BNE, Relative, 2)
  | 0xEA -> (NOP, Implicit, 2)
  | 0xF0 -> (BEQ, Relative, 2)
  | 0xF8 -> (SED, Implicit, 2)
  | _ -> failwith @@ sprintf "Unknown opcode %#02x" opcode

let decode_instruction cpu instruction =
  let (op, mode, cycles) = decode instruction in
  let (args, target, size) = decode_addressing_mode cpu mode in
  { op; mode; cycles; args; target; size = size + 1 }

let execute_instruction cpu instruction =
  let args = instruction.args in
  match instruction.op with
  | AND -> andd cpu args
  | BCS -> bcs cpu args
  | BCC -> bcc cpu args
  | BEQ -> beq cpu args
  | BIT -> bit cpu args
  | BNE -> bne cpu args
  | BPL -> bpl cpu args
  | BRK -> brk cpu
  | BVS -> bvs cpu args
  | BVC -> bvc cpu args
  | CLC -> cpu.carry <- false
  | JMP -> jmp cpu (Option.value_exn instruction.target)
  | JSR -> jsr cpu (Option.value_exn instruction.target)
  | LDA -> lda cpu args
  | LDX -> ldx cpu args
  | NOP -> ()
  | PHP -> php cpu
  | PLA -> pla cpu
  | RTS -> cpu.pc <- pop_word cpu
  | SEC -> cpu.carry <- true
  | SED -> cpu.decimal <- true
  | SEI -> cpu.interrupt <- true
  | STA -> sta cpu (Option.value_exn instruction.target)
  | STX -> stx cpu (Option.value_exn instruction.target)
  | STY -> sty cpu (Option.value_exn instruction.target)
  | _ -> failwith @@ sprintf "Unimplemented instruction %s" (show_instruction instruction.op)
