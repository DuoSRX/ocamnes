open Core

type t = {
  mapper : Mapper.t;
  nametables : int array;
  palettes : int array;
}

let make ~mapper =
  {
    mapper;
    nametables = Array.create ~len:0x800 0;
    palettes = Array.create ~len:32 0;
  }

let load v address =
  if address < 0x2000 then
    v.mapper.load_chr address
  else if address < 0x3F00 then
      v.nametables.(address land 0x7FF)
  else if address < 0x4000 then
    v.palettes.(address land 0x1F)
  else
    failwith @@ sprintf "can't read VRAM at %04X" address

let store v address value =
  if address < 0x2000 then
    v.mapper.store_chr address value
  else if address < 0x3F00 then
    v.nametables.(address land 0x7FF) <- value
  else if address < 0x4000 then (
    v.palettes.(address land 0x1F) <- value;
  )
  else
    failwith @@ sprintf "Trying to write PPU VRAM @ %04X" address
