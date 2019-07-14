type instruction =
    BCC
  | BCS
  | BEQ
  | CLC
  | JMP
  | JSR
  | LDA
  | LDX
  | NOP
  | RTS
  | SEC
  | STX
  [@@deriving show { with_path = false }]
