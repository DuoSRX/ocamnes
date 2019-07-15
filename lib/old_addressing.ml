
(* let execute_instruction cpu instruction =
  match instruction with
  | 0xEA -> () (* NOP *)
  | 0x38 -> cpu.carry <- true
  | 0x18 -> cpu.carry <- false
  | 0xAA -> tax cpu
  | 0x8A -> txa cpu
  | 0xE8 -> inx cpu
  | 0xCA -> dex cpu
  | 0xCE -> dec cpu (absolute cpu)
  | 0xEE -> inc cpu (absolute cpu)
  | 0xA2 -> ldx cpu (immediate cpu)
  | 0xA9 -> lda cpu (immediate cpu)
  | 0xA5 -> lda cpu (zero_page cpu)
  | 0xB5 -> lda cpu (zero_page_x cpu)
  | 0xAD -> lda cpu (absolute cpu)
  | 0xBD -> lda cpu (absolute_x cpu)
  | 0xB9 -> lda cpu (absolute_y cpu)
  | 0xA1 -> lda cpu (indirect_x cpu)
  | 0xB1 -> lda cpu (indirect_y cpu)
  | 0x8D -> sta cpu (absolute cpu)
  | 0x86 -> stx cpu (zero_page cpu)
  | 0x84 -> sty cpu (zero_page cpu)
  | 0x4C -> jmp cpu
  | 0x20 -> jsr cpu
  | 0xB0 -> bcs cpu
  | 0x90 -> bcc cpu
  | _ -> failwith @@ sprintf "unknown instruction %#04x" instruction *)

(* let immediate _cpu = Location.Immediate

let absolute cpu =
  let address = load_word_and_bump_pc(cpu) in
  Location.Memory address

let absolute_x cpu =
  let address = wrapping_add (load_word_and_bump_pc cpu) cpu.x in
  Location.Memory address

let absolute_y cpu =
  let address = wrapping_add (load_word_and_bump_pc cpu) cpu.y in
  Location.Memory address

let zero_page cpu =
  let address = load_byte_and_bump_pc cpu in
  Location.Memory address

let zero_page_x cpu =
  let address = wrapping_add (load_byte_and_bump_pc cpu) cpu.x in
  Location.Memory address

let zero_page_y cpu =
  let address = wrapping_add (load_byte_and_bump_pc cpu) cpu.y in
  Location.Memory address

let indirect_x cpu =
  let target = wrapping_add (load_byte_and_bump_pc cpu) cpu.x in
  let address = load_word_zero_page cpu target in
  Location.Memory address

let indirect_y cpu =
  let target = load_byte_and_bump_pc cpu in
  let address = wrapping_add (load_word_zero_page cpu target) cpu.y in
  Location.Memory address *)
