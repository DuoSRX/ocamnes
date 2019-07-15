type instruction =
    ADC
  | AND
  | ASL
  | BCC
  | BCS
  | BEQ
  | BIT
  | BMI
  | BNE
  | BPL
  | BRK
  | BVC
  | BVS
  | CLC
  | JMP
  | JSR
  | LDA
  | LDX
  | NOP
  | PHP
  | PLA
  | RTS
  | SEC
  | SED
  | SEI
  | STA
  | STX
  | STY
  [@@deriving show { with_path = false }]
