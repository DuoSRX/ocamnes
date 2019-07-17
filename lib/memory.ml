(* let ram = Array.make 0x10000 0

let load address =
  if address < 0x2000 then
    ram.(address land 0x7FF)
  else if (address lsr 13) = 1 then
    0 (* TODO: PPU READ *)
  else
    ram.((address land 0x3FFF) + 0xC000)

let store address value =
  if address < 0x2000 then
    ram.(address land 0x7FF) <- value
  else if (address lsr 13) = 1 then
    () (* TODO: PPU WRITE *)
  else
    ram.((address land 0x3FFF) + 0xC000) <- value

module type Mapper = sig
  val prg_load : int array -> int -> int
  val chr_load : int array -> int -> int
end

module NRom = struct
  let prg = Array.make 10 0
  let chr = Array.make 10 0
  let prg_load address = prg.(address)
  let chr_load address = chr.(address)
end *)
