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
  | CLD
  | CLV
  | CMP
  | JMP
  | JSR
  | LDA
  | LDX
  | NOP
  | ORA
  | PHA
  | PHP
  | PLA
  | PLP
  | RTS
  | SEC
  | SED
  | SEI
  | STA
  | STX
  | STY
  [@@deriving show { with_path = false }]
