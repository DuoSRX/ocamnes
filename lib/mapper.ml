type t = {
  rom : Cartridge.rom;
  load : int -> int;
  store : int -> int -> unit;
}

module UxROM = struct
  let banks = ref 0
  let bank1 = ref 0
  let bank2 = ref 0

  let load (rom : Cartridge.rom) address =
    if address < 0x2000 then
      rom.chr.(address)
    else if address >= 0x6000 && address < 0x8000 then
      rom.ram.(address)
    else if address >= 0x8000 && address < 0xC000 then
      rom.prg.(!bank1 * 0x4000 + (address - 0x8000))
    else
      rom.prg.(!bank2 * 0x4000 + (address - 0xC000))

  let store (rom : Cartridge.rom) address value =
    if address < 0x2000 then
      rom.chr.(address) <- value
    else if address >= 0x6000 && address < 0x8000 then
      rom.ram.(address - 0x6000) <- value
    else
      bank1 := value mod !banks

  let make (rom : Cartridge.rom) =
    banks := rom.headers.prg_size / 0x4000;
    bank1 := 0;
    bank2 := !banks - 1;
    { rom = rom
    ; load = load rom
    ; store = store rom
    }
end

module CNRom = struct
  let bank = ref 0

  let load (rom : Cartridge.rom) address =
    if address > 0x2000 then
      rom.prg.(address land (rom.headers.prg_size - 1))
    else
      rom.chr.(!bank * 0x2000 + address)

  let store (rom : Cartridge.rom) address value =
    if address < 0x2000 then
      rom.chr.(address) <- value
    else
      bank := value land 3

  let make (rom : Cartridge.rom) =
    { rom = rom
    ; load = load rom
    ; store = store rom
    }
end

module NRom = struct
  let load (rom : Cartridge.rom) address =
    if address < 0x2000 then
      rom.chr.(address)
    else if address >= 0x6000 && address < 0x8000 then
      rom.ram.(address)
    else
      rom.prg.(address land (rom.headers.prg_size - 1))

  let store (rom : Cartridge.rom) address value =
    if address < 0x2000 then
      rom.chr.(address) <- value
    else if address >= 0x6000 && address < 0x8000 then
      rom.ram.(address - 0x6000) <- value
    else
      failwith @@ Printf.sprintf "Can't store to PRG @ %04X = %02X" address value

  let make (rom : Cartridge.rom) =
    { rom = rom
    ; load = load rom
    ; store = store rom
    }
end

let mapper_for ~(rom:Cartridge.rom) =
  match rom.headers.mapper with
  | 0 -> NRom.make rom
  | 2 -> UxROM.make rom
  | 3 -> CNRom.make rom
  | n -> failwith @@ Printf.sprintf "Unknwown mapper: %d" n
