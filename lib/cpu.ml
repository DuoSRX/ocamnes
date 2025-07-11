open Instructions

module Flags = struct
  let carry     = 0b0000_0001
  let zero      = 0b0000_0010
  let interrupt = 0b0000_0100
  let decimal   = 0b0000_1000
  let break4    = 0b0001_0000
  let break5    = 0b0010_0000
  let overflow  = 0b0100_0000
  let negative  = 0b1000_0000
end

let wrapping_add a b = (a + b) land 0xFF
let wrapping_sub a b = wrapping_add a (-b)
let wrapping_add_w a b = (a + b) land 0xFFFF
let wrapping_sub_w a b = wrapping_add_w a (-b)

type t = {
  mutable a : int;
  mutable x : int;
  mutable y : int;
  mutable pc : int;
  mutable s : int;

  mutable carry : bool;
  mutable zero : bool;
  mutable interrupt : bool;
  mutable decimal : bool;
  mutable overflow : bool;
  mutable negative : bool;

  mutable cycles : int;
  mutable extra_cycles: int;
  mutable steps : int;
  mutable nmi_triggered : bool;

  mutable nestest : bool;
  mutable tracing : bool;

  memory : Memory.t;
}

let make ~nestest ~tracing ~memory = {
  cycles = 0; extra_cycles = 0;
  a = 0; x = 0; y = 0; s = 0xFD; pc = 0;
  zero = false; negative = false; carry = false; decimal = false; interrupt = true; overflow = false;
  steps = -1; nmi_triggered = false;
  tracing; nestest; memory
}

let load_byte cpu address =
  Memory.load cpu.memory address

let store_byte cpu address value =
  if address = 0x4014 then (* Extra cycles for DMA *)
    cpu.extra_cycles <- cpu.extra_cycles + 513 + Bool.to_int (cpu.cycles mod 2 = 1);

  Memory.store cpu.memory address value

let load_word cpu address =
  let a = load_byte cpu address in
  let b = load_byte cpu (address + 1) in
  a lor b lsl 8

let load_word_zero_page cpu address =
  let a = (address land 0xFF00) lor (wrapping_add (address land 0xFF) 1) in
  let lo = load_byte cpu address in
  let hi = load_byte cpu a in
  lo lor hi lsl 8

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

let page_crossed a b = (a land 0xFF00) <> (b land 0xFF00)

module AddressingMode = struct
  type t =
    | Implicit | Absolute | AbsoluteX | AbsoluteY
    | ZeroPage | ZeroPageX | ZeroPageY | Accumulator
    | Relative | IndirectX | IndirectY | Indirect | Immediate
    [@@deriving show]
end


let do_read = function
  | STA | STX | STY -> false
  | _ -> true

let decode_addressing_mode cpu am extra_page_cycles =
  let open AddressingMode in
  let pc = cpu.pc + 1 in
  match am with
  | Immediate ->
    (lazy (load_byte cpu pc), Some pc, 1)
  | Absolute ->
    let address = load_word cpu pc in
    (lazy (load_byte cpu address), Some address, 2)
  | AbsoluteX ->
    let arg = load_word cpu pc in
    let address = wrapping_add_w arg cpu.x in
    if page_crossed arg address then cpu.extra_cycles <- cpu.extra_cycles + extra_page_cycles;
    (lazy (load_byte cpu address), Some address, 2)
  | AbsoluteY ->
    let arg = load_word cpu pc in
    let address = wrapping_add_w arg cpu.y in
    if page_crossed arg address then cpu.extra_cycles <- cpu.extra_cycles + extra_page_cycles;
    (lazy (load_byte cpu address), Some address, 2)
  | ZeroPage ->
    let address = load_byte cpu pc in
    (lazy (load_byte cpu address), Some address, 1)
  | ZeroPageX ->
    let address = wrapping_add (load_byte cpu pc) cpu.x in
    (lazy (load_byte cpu address), Some address, 1)
  | ZeroPageY ->
    let address = wrapping_add (load_byte cpu pc) cpu.y in
    (lazy (load_byte cpu address), Some address, 1)
  | Relative ->
    let offset = lazy (
      let byte = load_byte cpu pc in
      if byte < 0x80 then pc + 1 + byte else pc + 1 + byte - 0x100
    ) in
    (offset, Some pc, 1)
  | IndirectX ->
    let zero_page = wrapping_add (load_byte cpu pc) cpu.x in
    let address = load_word_zero_page cpu zero_page in
    (lazy (load_byte cpu address), Some(address), 1)
  | IndirectY ->
    let zero_page = load_byte cpu pc in
    let word = load_word_zero_page cpu zero_page in
    let address = wrapping_add_w word cpu.y in
    if page_crossed word address then cpu.extra_cycles <- cpu.extra_cycles + extra_page_cycles;
    (lazy (load_byte cpu address), Some(address), 1)
  | Indirect ->
    let zero_page = load_word cpu pc in
    let dest = load_word_zero_page cpu zero_page in
    (lazy (load_word cpu dest), Some(dest), 2)
  | Accumulator ->
    (lazy (cpu.a), None, 0)
  | Implicit ->
    (lazy 0, None, 0)

type instr = {
  op : Instructions.t;
  mode : AddressingMode.t;
  args : int;
  target : int option;
  cycles : int;
  size: int;
} [@@deriving show]

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

let inc cpu args target =
  let result = wrapping_add args 1 in
  write_target cpu target (set_nz_flags cpu result)

let dec cpu args target =
  let result = wrapping_sub args 1 in
  write_target cpu target (set_nz_flags cpu result)

let dex c = c.x <- set_nz_flags c @@ wrapping_sub c.x 1
let dey c = c.y <- set_nz_flags c @@ wrapping_sub c.y 1
let inx c = c.x <- set_nz_flags c @@ wrapping_add c.x 1
let iny c = c.y <- set_nz_flags c @@ wrapping_add c.y 1

let sta c addr = store_byte c addr c.a
let stx c addr = store_byte c addr c.x
let sty c addr = store_byte c addr c.y
let lda c args = c.a <- set_nz_flags c args
let ldx c args = c.x <- set_nz_flags c args
let ldy c args = c.y <- set_nz_flags c args

let jmp cpu target = cpu.pc <- target

let brk cpu =
  push_word cpu (cpu.pc + 1);
  push_byte cpu @@ (flags_to_int cpu) lor Flags.break4;
  cpu.interrupt <- true;
  cpu.pc <- load_word cpu 0xFFFE

let branch cpu addr cond =
  if cond then
    let cycles = if page_crossed addr cpu.pc then 2 else 1 in
    cpu.extra_cycles <- cpu.extra_cycles + cycles;
    cpu.pc <- addr

let bcs cpu offset = branch cpu offset cpu.carry
let bcc cpu offset = branch cpu offset (not cpu.carry)
let beq cpu offset = branch cpu offset cpu.zero
let bne cpu offset = branch cpu offset (not cpu.zero)
let bmi cpu offset = branch cpu offset cpu.negative
let bpl cpu offset = branch cpu offset (not cpu.negative)
let bvs cpu offset = branch cpu offset cpu.overflow
let bvc cpu offset = branch cpu offset (not cpu.overflow)

let bit cpu byte =
  cpu.zero <- (cpu.a land byte) = 0;
  cpu.overflow <- byte land Flags.overflow <> 0;
  cpu.negative <- byte land Flags.negative <> 0

let php cpu =
  let flags = (flags_to_int cpu) lor Flags.break4 in
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

let ror cpu args target =
  let carry = if cpu.carry then 1 else 0 in
  cpu.carry <- args land 1 = 1;
  let result = (args lsr 1) lor (carry lsl 7) in
  write_target cpu target (set_nz_flags cpu result)

let rol cpu args target =
  let carry = if cpu.carry then 1 else 0 in
  cpu.carry <- args land 0x80 > 0;
  let result = ((args lsl 1) lor carry) land 0xFF in
  write_target cpu target (set_nz_flags cpu result)

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
  push_word cpu (wrapping_sub_w cpu.pc 1);
  cpu.pc <- address

let rts cpu = cpu.pc <- (pop_word cpu) + 1

let rti cpu =
  plp cpu;
  cpu.pc <- pop_word cpu

let nmi cpu =
  push_word cpu cpu.pc;
  let flags = flags_to_int cpu in
  push_byte cpu flags;
  cpu.nmi_triggered <- false;
  cpu.cycles <- cpu.cycles + 7;
  cpu.interrupt <- true;
  cpu.pc <- load_word cpu 0xFFFA

let decode opcode =
  let open AddressingMode in
  match opcode with
  | 0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xFA -> (NOP, Implicit, 2, 0)
  | 0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 -> (NOP, Immediate, 2, 0)
  | 0x04 | 0x44 | 0x64 -> (NOP, ZeroPage, 3, 0)
  | 0x0C -> (NOP, Absolute, 4, 0)
  | 0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 -> (NOP, ZeroPageX, 4, 0)
  | 0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC -> (NOP, AbsoluteX, 4, 1)
  | 0x00 -> (BRK, Implicit, 7, 0)
  | 0x01 -> (ORA, IndirectX, 6, 0)
  | 0x05 -> (ORA, ZeroPage, 3, 0)
  | 0x06 -> (ASL, ZeroPage, 5, 0)
  | 0x08 -> (PHP, Implicit, 3, 0)
  | 0x09 -> (ORA, Immediate, 2, 0)
  | 0x0A -> (ASL, Accumulator, 2, 0)
  | 0x0D -> (ORA, Absolute, 4, 0)
  | 0x0E -> (ASL, Absolute, 6, 0)
  | 0x10 -> (BPL, Relative, 2, 0)
  | 0x11 -> (ORA, IndirectY, 5, 0)
  | 0x15 -> (ORA, ZeroPageX, 4, 0)
  | 0x16 -> (ASL, ZeroPageX, 6, 0)
  | 0x18 -> (CLC, Implicit, 2, 0)
  | 0x19 -> (ORA, AbsoluteY, 4, 0)
  | 0x1D -> (ORA, AbsoluteX, 4, 0)
  | 0x1E -> (ASL, AbsoluteX, 7, 0)
  | 0x20 -> (JSR, Absolute, 6, 0)
  | 0x21 -> (AND, IndirectX, 6, 0)
  | 0x24 -> (BIT, ZeroPage, 3, 0)
  | 0x25 -> (AND, ZeroPage, 3, 0)
  | 0x26 -> (ROL, ZeroPage, 5, 0)
  | 0x28 -> (PLP, Implicit, 4, 0)
  | 0x29 -> (AND, Immediate, 2, 0)
  | 0x2A -> (ROL, Accumulator, 2, 0)
  | 0x2D -> (AND, Absolute, 4, 0)
  | 0x2C -> (BIT, Absolute, 4, 0)
  | 0x2E -> (ROL, Absolute, 6, 0)
  | 0x30 -> (BMI, Relative, 2, 1)
  | 0x31 -> (AND, IndirectY, 5, 0)
  | 0x35 -> (AND, ZeroPageX, 4, 0)
  | 0x36 -> (ROL, ZeroPageX, 6, 0)
  | 0x38 -> (SEC, Implicit, 2, 0)
  | 0x39 -> (AND, AbsoluteY, 4, 0)
  | 0x3D -> (AND, AbsoluteX, 4, 0)
  | 0x3E -> (ROL, AbsoluteX, 7, 0)
  | 0x40 -> (RTI, Implicit, 6, 0)
  | 0x41 -> (EOR, IndirectX, 6, 0)
  | 0x45 -> (EOR, ZeroPage, 3, 0)
  | 0x46 -> (LSR, ZeroPage, 5, 0)
  | 0x48 -> (PHA, Implicit, 3, 0)
  | 0x49 -> (EOR, Immediate, 2, 0)
  | 0x4A -> (LSR, Accumulator, 2, 0)
  | 0x4C -> (JMP, Absolute, 3, 0)
  | 0x4D -> (EOR, Absolute, 4, 0)
  | 0x4E -> (LSR, Absolute, 6, 0)
  | 0x50 -> (BVC, Relative, 2, 1)
  | 0x51 -> (EOR, IndirectY, 5, 0)
  | 0x55 -> (EOR, ZeroPageX, 4, 0)
  | 0x56 -> (LSR, ZeroPageX, 6, 0)
  | 0x58 -> (CLI, Implicit, 2, 0)
  | 0x59 -> (EOR, AbsoluteY, 4, 0)
  | 0x5D -> (EOR, AbsoluteX, 4, 0)
  | 0x5E -> (LSR, AbsoluteX, 7, 0)
  | 0x60 -> (RTS, Implicit, 6, 0)
  | 0x61 -> (ADC, IndirectX, 6, 0)
  | 0x65 -> (ADC, ZeroPage, 3, 0)
  | 0x66 -> (ROR, ZeroPage, 5, 0)
  | 0x68 -> (PLA, Implicit, 4, 0)
  | 0x69 -> (ADC, Immediate, 2, 0)
  | 0x6A -> (ROR, Accumulator, 2, 0)
  | 0x6C -> (JMP, Indirect, 5, 0)
  | 0x6D -> (ADC, Absolute, 4, 0)
  | 0x6E -> (ROR, Absolute, 6, 0)
  | 0x70 -> (BVS, Relative, 2, 1)
  | 0x71 -> (ADC, IndirectY, 5, 0)
  | 0x75 -> (ADC, ZeroPageX, 4, 0)
  | 0x76 -> (ROR, ZeroPageX, 6, 0)
  | 0x78 -> (SEI, Implicit, 2, 0)
  | 0x79 -> (ADC, AbsoluteY, 4, 0)
  | 0x7D -> (ADC, AbsoluteX, 4, 0)
  | 0x7E -> (ROR, AbsoluteX, 7, 0)
  | 0x81 -> (STA, IndirectX, 6, 0)
  | 0x84 -> (STY, ZeroPage, 3, 0)
  | 0x85 -> (STA, ZeroPage, 3, 0)
  | 0x86 -> (STX, ZeroPage, 3, 0)
  | 0x88 -> (DEY, Implicit, 2, 0)
  | 0x8A -> (TXA, Implicit, 2, 0)
  | 0x8C -> (STY, Absolute, 4, 0)
  | 0x8D -> (STA, Absolute, 4, 0)
  | 0x8E -> (STX, Absolute, 4, 0)
  | 0x90 -> (BCC, Relative, 2, 0)
  | 0x91 -> (STA, IndirectY, 6, 0)
  | 0x94 -> (STY, ZeroPageX, 4, 0)
  | 0x95 -> (STA, ZeroPageX, 4, 0)
  | 0x9A -> (TXS, Implicit, 2, 0)
  | 0x96 -> (STX, ZeroPageY, 4, 0)
  | 0x98 -> (TYA, Implicit, 2, 0)
  | 0x99 -> (STA, AbsoluteY, 5, 0)
  | 0x9D -> (STA, AbsoluteX, 5, 0)
  | 0xA0 -> (LDY, Immediate, 2, 0)
  | 0xA1 -> (LDA, IndirectX, 6, 0)
  | 0xA2 -> (LDX, Immediate, 2, 0)
  | 0xA4 -> (LDY, ZeroPage, 3, 0)
  | 0xA5 -> (LDA, ZeroPage, 3, 0)
  | 0xA6 -> (LDX, ZeroPage, 3, 0)
  | 0xA9 -> (LDA, Immediate, 2, 0)
  | 0xA8 -> (TAY, Implicit, 2, 0)
  | 0xAA -> (TAX, Implicit, 2, 0)
  | 0xAC -> (LDY, Absolute, 4, 0)
  | 0xAD -> (LDA, Absolute, 4, 0)
  | 0xAE -> (LDX, Absolute, 4, 0)
  | 0xB0 -> (BCS, Relative, 2, 1)
  | 0xB1 -> (LDA, IndirectY, 5, 1)
  | 0xB4 -> (LDY, ZeroPageX, 4, 0)
  | 0xB5 -> (LDA, ZeroPageX, 4, 0)
  | 0xB6 -> (LDX, ZeroPageY, 4, 0)
  | 0xB8 -> (CLV, Implicit, 2, 0)
  | 0xB9 -> (LDA, AbsoluteY, 4, 1)
  | 0xBA -> (TSX, Implicit, 2, 0)
  | 0xBD -> (LDA, AbsoluteX, 4, 1)
  | 0xBC -> (LDY, AbsoluteX, 4, 1)
  | 0xBE -> (LDX, AbsoluteY, 4, 1)
  | 0xC0 -> (CPY, Immediate, 2, 0)
  | 0xC1 -> (CMP, IndirectX, 6, 0)
  | 0xC4 -> (CPY, ZeroPage, 3, 0)
  | 0xC5 -> (CMP, ZeroPage, 3, 0)
  | 0xC6 -> (DEC, ZeroPage, 5, 0)
  | 0xC8 -> (INY, Implicit, 2, 0)
  | 0xC9 -> (CMP, Immediate, 2, 0)
  | 0xCA -> (DEX, Implicit, 2, 0)
  | 0xCC -> (CPY, Absolute, 4, 0)
  | 0xCD -> (CMP, Absolute, 4, 0)
  | 0xCE -> (DEC, Absolute, 6, 0)
  | 0xD0 -> (BNE, Relative, 2, 0)
  | 0xD1 -> (CMP, IndirectY, 5, 0)
  | 0xD5 -> (CMP, ZeroPageX, 4, 0)
  | 0xD6 -> (DEC, ZeroPageX, 6, 0)
  | 0xD8 -> (CLD, Implicit, 2, 0)
  | 0xD9 -> (CMP, AbsoluteY, 4, 0)
  | 0xDD -> (CMP, AbsoluteX, 4, 0)
  | 0xDE -> (DEC, AbsoluteX, 7, 0)
  | 0xE0 -> (CPX, Immediate, 2, 0)
  | 0xE1 -> (SBC, IndirectX, 6, 0)
  | 0xE4 -> (CPX, ZeroPage, 3, 0)
  | 0xE5 -> (SBC, ZeroPage, 3, 0)
  | 0xE6 -> (INC, ZeroPage, 5, 0)
  | 0xE8 -> (INX, Implicit, 2, 0)
  | 0xE9 -> (SBC, Immediate, 2, 0)
  | 0xEA -> (NOP, Implicit, 2, 0)
  | 0xEB -> (SBC, Immediate, 2, 0)
  | 0xEC -> (CPX, Absolute, 4, 0)
  | 0xED -> (SBC, Absolute, 4, 0)
  | 0xEE -> (INC, Absolute, 6, 0)
  | 0xF0 -> (BEQ, Relative, 2, 1)
  | 0xF1 -> (SBC, IndirectY, 5, 0)
  | 0xF5 -> (SBC, ZeroPageX, 4, 0)
  | 0xF6 -> (INC, ZeroPageX, 6, 0)
  | 0xF8 -> (SED, Implicit, 2, 0)
  | 0xF9 -> (SBC, AbsoluteY, 4, 0)
  | 0xFD -> (SBC, AbsoluteX, 4, 0)
  | 0xFE -> (INC, AbsoluteX, 7, 0)
  | _ -> failwith @@ Printf.sprintf "Unknown opcode %02X" opcode

let decode_instruction cpu instruction =
  let (op, mode, cycles, extra_page_cycles) = decode instruction in
  let (laz_args, target, size) = decode_addressing_mode cpu mode extra_page_cycles in
  let args = if do_read op || cpu.nestest then Lazy.force laz_args else 0x1337 in
  { op; mode; cycles; args; target; size = size + 1 }

let execute_instruction cpu instruction =
  let args = instruction.args in
  let target = instruction.target in
  match instruction.op with
  | ADC -> adc cpu args
  | AND -> and_op cpu args
  | ASL -> asl_op cpu args target
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
  | CLI -> cpu.interrupt <- false
  | CLV -> cpu.overflow <- false
  | CMP -> cmp cpu args
  | CPX -> cpx cpu args
  | CPY -> cpy cpu args
  | DEC -> dec cpu args target
  | DEX -> dex cpu
  | DEY -> dey cpu
  | EOR -> eor cpu args
  | INC -> inc cpu args target
  | INX -> inx cpu
  | INY -> iny cpu
  | JMP -> jmp cpu (Option.get target)
  | JSR -> jsr cpu (Option.get target)
  | LDA -> lda cpu args
  | LDX -> ldx cpu args
  | LDY -> ldy cpu args
  | LSR -> lsr_op cpu args target
  | NOP -> ()
  | ORA -> ora cpu args
  | PHA -> push_byte cpu cpu.a
  | PHP -> php cpu
  | PLA -> pla cpu
  | PLP -> plp cpu
  | ROL -> rol cpu args target
  | ROR -> ror cpu args target
  | RTI -> rti cpu
  | RTS -> rts cpu
  | SBC -> sbc cpu args
  | SEC -> cpu.carry <- true
  | SED -> cpu.decimal <- true
  | SEI -> cpu.interrupt <- true
  | STA -> sta cpu (Option.get target)
  | STX -> stx cpu (Option.get target)
  | STY -> sty cpu (Option.get target)
  | TAX -> tax cpu
  | TAY -> tay cpu
  | TSX -> tsx cpu
  | TXA -> txa cpu
  | TXS -> txs cpu
  | TYA -> tya cpu

let step ~trace_fun ~on_step cpu =
  if cpu.nmi_triggered then nmi cpu;
  let opcode = load_byte cpu cpu.pc in
  let instruction = decode_instruction cpu opcode in
  let log = if cpu.tracing then Some (trace_fun cpu instruction opcode) else None in
  on_step instruction opcode;
  cpu.pc <- cpu.pc + instruction.size;
  execute_instruction cpu instruction;
  cpu.cycles <- cpu.cycles + instruction.cycles + cpu.extra_cycles;
  cpu.extra_cycles <- 0;
  cpu.steps <- cpu.steps + 1;
  log
