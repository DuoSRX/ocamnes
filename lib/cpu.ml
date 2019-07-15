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
let wrapping_sub a b = wrapping_add a (-b)
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
  store_byte cpu (0x100 + cpu.s) value;
  cpu.s <- wrapping_sub cpu.s 1

let push_word cpu value =
  push_byte cpu ((value land 0xFF00) lsr 8);
  push_byte cpu (value land 0xFF)

let pop_byte cpu =
  cpu.s <- wrapping_add cpu.s 1;
  load_byte cpu (0x100 + cpu.s)

let pop_word cpu =
  let lo = pop_byte cpu in
  let hi = pop_byte cpu in
  (hi lsl 8) lor lo

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
    | ZeroPage | ZeroPageX | ZeroPageY | Accumulator
    | Relative | IndirectX | IndirectY | Immediate
    [@@deriving show]
end

open AddressingMode

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
  | AddressingMode.Accumulator ->
    (cpu.a, None, 0)
  | AddressingMode.Implicit ->
    (0, None, 0)
  | _ -> failwith "unimplemented addressing mode"

let write_target cpu target value =
  match target with
  | None -> cpu.a <- value
  | Some addr -> store_byte cpu addr value

let set_nz_flags cpu value =
  cpu.zero <- value = 0;
  cpu.negative <- value land 0x80 <> 0;
  value

let tax c = c.x <- set_nz_flags c c.a
let txa c = c.a <- set_nz_flags c c.x
let tay c = c.y <- set_nz_flags c c.a
let tya c = c.a <- set_nz_flags c c.y
let txs c = c.s <- c.x (* Not a bug. TXS is the only transfer that doesn't change NZ *)
let tsx c = c.x <- set_nz_flags c c.s

let dex c = c.x <- set_nz_flags c @@ wrapping_sub c.x 1
let dey c = c.y <- set_nz_flags c @@ wrapping_sub c.y 1
let inx c = c.x <- set_nz_flags c @@ wrapping_add c.x 1
let iny c = c.y <- set_nz_flags c @@ wrapping_add c.y 1

let jmp c dst = c.pc <- dst
let sta c addr = store_byte c addr c.a
let stx c addr = store_byte c addr c.x
let sty c addr = store_byte c addr c.y
let lda c args = c.a <- set_nz_flags c args
let ldx c args = c.x <- set_nz_flags c args
let ldy c args = c.y <- set_nz_flags c args

let brk _ = failwith "oh no BRK"
(* let brk cpu =
  push_word cpu (cpu.pc + 2);
  push_byte cpu @@ (flags_to_int cpu) lor Flags.b;
  cpu.interrupt <- true;
  cpu.pc <- load_word cpu 0xFFEE *)

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
let bmi cpu offset = branch cpu offset cpu.negative
let bpl cpu offset = branch cpu offset (not cpu.negative)
let bvs cpu offset = branch cpu offset cpu.overflow
let bvc cpu offset = branch cpu offset (not cpu.overflow)

let bit cpu byte =
  let result = cpu.a land byte in
  cpu.negative <- Flags.negative land byte > 0;
  cpu.overflow <- Flags.overflow land byte > 0;
  cpu.zero <- result = 0

let php cpu =
  let flags = (flags_to_int cpu) lor (Flags.b) in
  push_byte cpu flags

let plp cpu =
  let flags = pop_byte cpu in
  cpu.carry <- flags land Flags.carry > 0;
  cpu.zero <- flags land Flags.zero > 0;
  cpu.interrupt <- flags land Flags.interrupt > 0;
  cpu.decimal <- flags land Flags.decimal > 0;
  cpu.overflow <- flags land Flags.overflow > 0;
  cpu.negative <- flags land Flags.negative > 0

let pla cpu = cpu.a <- set_nz_flags cpu (pop_byte cpu)

let and_op cpu args = cpu.a <- set_nz_flags cpu (cpu.a land args)
let ora cpu args = cpu.a <- set_nz_flags cpu (cpu.a lor args)
let eor cpu args = cpu.a <- set_nz_flags cpu (cpu.a lxor args)

let lsr_op cpu args target =
  cpu.carry <- args land 1 > 0;
  let result = set_nz_flags cpu (args lsr 1) in
  write_target cpu target result

let asl_op cpu args target =
  cpu.carry <- args land 0x80 > 0;
  let result = set_nz_flags cpu ((args lsl 1) land 0xFF) in
  write_target cpu target result

let compare_op cpu a b =
  let result = wrapping_sub a b in
  cpu.carry <- a >= b;
  cpu.zero <- a = b;
  cpu.negative <- result > 127

let cmp cpu args = compare_op cpu cpu.a args
let cpx cpu args = compare_op cpu cpu.x args
let cpy cpu args = compare_op cpu cpu.y args

let adc cpu args =
  let sum = cpu.a + args + Bool.to_int cpu.carry in
  cpu.carry <- sum > 0xFF;
  (* Oh boy... *)
  cpu.overflow <- (lnot (cpu.a lxor args)) land (cpu.a lxor sum) land 0x80 > 0;
  cpu.a <- set_nz_flags cpu (sum mod 0x100)

let sbc cpu args = adc cpu (args lxor 0xFF)

let jsr cpu address =
  push_word cpu (wrapping_add_w cpu.pc 2);
  cpu.pc <- address

let rts cpu = cpu.pc <- (pop_word cpu) + 1

let rti cpu =
  plp cpu;
  cpu.pc <- pop_word cpu

type instr = {
  op : Instructions.instruction;
  mode : AddressingMode.t;
  args : int;
  target : int option;
  cycles : int;
  size: int;
} [@@deriving show]

let op_is_write = function
  | STA | STX | STY | LDA | LDX | LDY -> true
  | _ -> false

let should_change_pc = function
  | JMP | JSR | RTS | RTI -> false
  | _ -> true

let args_to_string i =
  match i.mode with
  | Immediate -> sprintf "#$%02X" i.args
  | Absolute ->
    if op_is_write i.op then
      sprintf "$%04X = %02X" (Option.value_exn i.target) i.args
    else
      sprintf "$%04X" (Option.value_exn i.target)
  | ZeroPage -> sprintf "$%02X = %02X" (Option.value_exn i.target) i.args
  | Relative -> sprintf "$%04X" ((Option.value_exn i.target) + i.args + 1)
  | Accumulator -> "A"
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
  | Accumulator | Implicit -> ""
  | _ -> failwith @@ sprintf "can't print %s" @@ show_instr i

let decode opcode =
  match opcode with
  | 0x00 -> (BRK, Implicit, 7)
  | 0x08 -> (PHP, Implicit, 3)
  | 0x09 -> (ORA, Immediate, 2)
  | 0x0A -> (ASL, Accumulator, 2)
  | 0x10 -> (BPL, Relative, 2)
  | 0x18 -> (CLC, Implicit, 2)
  | 0x20 -> (JSR, Absolute, 6)
  | 0x24 -> (BIT, ZeroPage, 3)
  | 0x28 -> (PLP, Implicit, 4)
  | 0x29 -> (AND, Immediate, 2)
  | 0x30 -> (BMI, Relative, 2)
  | 0x38 -> (SEC, Implicit, 2)
  | 0x40 -> (RTI, Implicit, 6)
  | 0x48 -> (PHA, Implicit, 3)
  | 0x49 -> (EOR, Immediate, 2)
  | 0x4A -> (LSR, Accumulator, 2)
  | 0x4C -> (JMP, Absolute, 3)
  | 0x50 -> (BVC, Relative, 2)
  | 0x60 -> (RTS, Implicit, 6)
  | 0x68 -> (PLA, Implicit, 4)
  | 0x69 -> (ADC, Immediate, 2)
  | 0x70 -> (BVS, Relative, 2)
  | 0x78 -> (SEI, Implicit, 2)
  | 0x85 -> (STA, ZeroPage, 3)
  | 0x86 -> (STX, ZeroPage, 3)
  | 0x88 -> (DEY, Implicit, 2)
  | 0x8A -> (TXA, Implicit, 2)
  | 0x8E -> (STX, Absolute, 4)
  | 0x90 -> (BCC, Relative, 2)
  | 0x9A -> (TXS, Implicit, 2)
  | 0x98 -> (TYA, Implicit, 2)
  | 0xA0 -> (LDY, Immediate, 2)
  | 0xA2 -> (LDX, Immediate, 2)
  | 0xA9 -> (LDA, Immediate, 2)
  | 0xA8 -> (TAY, Implicit, 2)
  | 0xAA -> (TAX, Implicit, 2)
  | 0xAD -> (LDA, Absolute, 4)
  | 0xAE -> (LDX, Absolute, 4)
  | 0xB0 -> (BCS, Relative, 2)
  | 0xB8 -> (CLV, Implicit, 2)
  | 0xBA -> (TSX, Implicit, 2)
  | 0xC0 -> (CPY, Immediate, 2)
  | 0xC8 -> (INY, Implicit, 2)
  | 0xC9 -> (CMP, Immediate, 2)
  | 0xCA -> (DEX, Implicit, 2)
  | 0xD0 -> (BNE, Relative, 2)
  | 0xD8 -> (CLD, Implicit, 2)
  | 0xE0 -> (CPX, Immediate, 2)
  | 0xE8 -> (INX, Implicit, 2)
  | 0xE9 -> (SBC, Immediate, 2)
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
  | ADC -> adc cpu args
  | AND -> and_op cpu args
  | ASL -> asl_op cpu args instruction.target
  | BCS -> bcs cpu args
  | BCC -> bcc cpu args
  | BEQ -> beq cpu args
  | BIT -> bit cpu args
  | BMI -> bmi cpu args
  | BNE -> bne cpu args
  | BPL -> bpl cpu args
  | BRK -> brk cpu
  | BVS -> bvs cpu args
  | BVC -> bvc cpu args
  | CLC -> cpu.carry <- false
  | CLD -> cpu.decimal <- false
  | CLV -> cpu.overflow <- false
  | CMP -> cmp cpu args
  | CPX -> cpx cpu args
  | CPY -> cpy cpu args
  | DEX -> dex cpu
  | DEY -> dey cpu
  | EOR -> eor cpu args
  | INX -> inx cpu
  | INY -> iny cpu
  | JMP -> jmp cpu (Option.value_exn instruction.target)
  | JSR -> jsr cpu (Option.value_exn instruction.target)
  | LDA -> lda cpu args
  | LDX -> ldx cpu args
  | LDY -> ldy cpu args
  | LSR -> lsr_op cpu args instruction.target
  | NOP -> ()
  | ORA -> ora cpu args
  | PHA -> push_byte cpu cpu.a
  | PHP -> php cpu
  | PLA -> pla cpu
  | PLP -> plp cpu
  | RTI -> rti cpu
  | RTS -> rts cpu
  | SBC -> sbc cpu args
  | SEC -> cpu.carry <- true
  | SED -> cpu.decimal <- true
  | SEI -> cpu.interrupt <- true
  | STA -> sta cpu (Option.value_exn instruction.target)
  | STX -> stx cpu (Option.value_exn instruction.target)
  | STY -> sty cpu (Option.value_exn instruction.target)
  | TAX -> tax cpu
  | TAY -> tay cpu
  | TSX -> tsx cpu
  | TXA -> txa cpu
  | TXS -> txs cpu
  | TYA -> tya cpu
  (* | _ -> failwith @@ sprintf "Unimplemented instruction %s" (show_instruction instruction.op) *)
