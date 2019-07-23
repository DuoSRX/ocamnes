open Core

type t = {
  rom : Cartridge.rom;
  load : int -> int;
  store : int -> int -> unit;
}

module NRom = struct
  let load_prg (rom : Cartridge.rom) address =
    if rom.headers.prg_size > 0x4000 then
      rom.prg.(address land 0x7FFF)
    else
      rom.prg.(address land 0x3FFF)

  let store_prg _rom address value =
    failwith @@ sprintf "Can't store to PRG @ %04X = %02X" address value

  let load_chr (rom : Cartridge.rom) address =
    rom.chr.(address)

  let store_chr (rom : Cartridge.rom) address value =
    rom.chr.(address) <- value

  let load (rom : Cartridge.rom) address =
    if address < 0x2000 then
      load_chr rom address
    else if address >= 0x6000 && address < 0x8000 then
      rom.ram.(address)
    else
      load_prg rom address

  let store (rom : Cartridge.rom) address value =
    if address < 0x2000 then
      store_chr rom address value
    else if address >= 0x6000 && address < 0x8000 then
      rom.ram.(address - 0x6000) <- value
    else
      store_prg rom address value
end

let nrom rom =
  { rom = rom
  ; load = (NRom.load rom)
  ; store = (NRom.store rom)
  }

let mapper_for ~(rom:Cartridge.rom) =
  match rom.headers.mapper with
  | 0 -> nrom rom
  | n -> failwith @@ sprintf "Unknwown mapper: %d" n
