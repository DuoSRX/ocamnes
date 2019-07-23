open Core

type t = {
  rom : Cartridge.rom;
  load_prg : int -> int;
  store_prg : int -> int -> unit;
  load_chr : int -> int;
  store_chr : int -> int -> unit;
}

module NRom = struct
  let load_prg (rom : Cartridge.rom) address =
    if address < 0x8000 then
      0
    else if rom.headers.prg_size > 0x4000 then
      rom.prg.(address land 0x7FFF)
    else
      rom.prg.(address land 0x3FFF)

  let store_prg _rom address value =
    failwith @@ sprintf "Can't store to PRG @ %04X = %02X" address value

  let load_chr (rom : Cartridge.rom) address =
    rom.chr.(address)

  let store_chr (rom : Cartridge.rom) address value =
    rom.chr.(address) <- value
end

let nrom rom =
  { rom = rom
  ; load_prg = (NRom.load_prg rom)
  ; store_prg = (NRom.store_prg rom)
  ; load_chr = (NRom.load_chr rom)
  ; store_chr = (NRom.store_chr rom)
  }

let mapper_for ~(rom:Cartridge.rom) =
  match rom.headers.mapper with
  | 0 -> nrom rom
  | n -> failwith @@ sprintf "Unknwown mapper: %d" n
