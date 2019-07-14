type instruction =
    BCS
  | CLC
  | JMP
  | JSR
  | LDX
  | NOP
  | RTS
  | SEC
  | STX
  [@@deriving show { with_path = false }]
