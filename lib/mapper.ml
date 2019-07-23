open Core

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

  let store_chr _rom address value =
    failwith @@ sprintf "Can't store to CHR @ %04X = %02X" address value
end

type t = {
  load_prg : int -> int;
  store_prg : int -> int -> unit;
}

let nrom rom =
  { load_prg = (NRom.load_prg rom)
  ; store_prg = (NRom.store_prg rom)
  }

let mapper_from_rom (rom:Cartridge.rom) =
  match rom.headers.mapper with
  | 0 -> nrom rom
  | n -> failwith @@ sprintf "Unknwown mapper: %d" n
