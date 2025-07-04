type t = {
  mapper : Mapper.t;
  nametables : int array;
  palettes : int array;
}

let make ~mapper =
  {
    mapper;
    nametables = Array.make 0x800 0;
    palettes = Array.make 32 0;
  }

let mirrors = function
| Cartridge.Horizontal -> [|0; 0; 1; 1|]
| Cartridge.Vertical   -> [|0; 1; 0; 1|]

let mirror mode address =
  let address = (address - 0x2000) mod 0x1000 in
  let table = address / 0x400 in
  let offset = address mod 0x400 in
  (0x2000 + (mirrors mode).(table) * 0x400 + offset) mod 2048

let load v address =
  if address < 0x2000 then
    v.mapper.load address
  else if address < 0x3F00 then
    v.nametables.(mirror v.mapper.rom.headers.mirroring address)
  else if address < 0x4000 then
    let address = address land 0x1F in
    if address >= 16 && address mod 4 = 0 then
     v.palettes.(address - 16)
    else
     v.palettes.(address)
  else
    failwith @@ Printf.sprintf "can't read VRAM at %04X" address

let store v address value =
  if address < 0x2000 then
    v.mapper.store address value
  else if address < 0x3F00 then
    v.nametables.(mirror v.mapper.rom.headers.mirroring address) <- value
  else if address < 0x4000 then (
    let address = address land 0x1F in
    if address >= 16 && address mod 4 = 0 then
     v.palettes.(address - 16) <- value
    else
     v.palettes.(address) <- value
  )
  else
    failwith @@ Printf.sprintf "Trying to write PPU VRAM @ %04X" address
