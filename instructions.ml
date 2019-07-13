type instruction =
    JMP
  | LDX
  | JSR
  | RTS
  | STX
  | NOP
  | SEC
  | BCS
  [@@deriving show { with_path = false }]
