type instruction =
    JMP
  | LDX
  | JSR
  | RTS
  | STX
  | NOP
  | SEC
  | BCS

let op_to_string = function
  | JMP -> "JMP"
  | LDX -> "LDX"
  | JSR -> "JSR"
  | RTS -> "RTS"
  | STX -> "STX"
  | NOP -> "NOP"
  | SEC -> "SEC"
  | BCS -> "BCS"
