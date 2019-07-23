open Core

type t = {
  ppu : Ppu.ppu;
  mapper : Mapper.t;
  ram : int array;
}

let make ~mapper ~ppu =
  {
    ppu; mapper;
    ram = Array.create ~len:0x800 0;
  }

let load m address =
  if address < 0x2000 then
    m.ram.(address land 0x7FF)
  else if (address lsr 13) = 1 then
    Ppu.read_register m.ppu (0x2000 + address % 8)
  else if address < 0x4000 then
    Ppu.load m.ppu address
  else if address = 0x4016 then
    if Input.next_key () then 1 else 0
  else if address < 0x6000 then
    0
  else
    m.mapper.load address

let store m address value =
  if address < 0x2000 then
    m.ram.(address land 0x7FF) <- value
  else if (address lsr 13) = 1 then
    Ppu.write_register m.ppu (0x2000 + address % 8) value
  else if address < 0x4000 then
    Ppu.store m.ppu address value
  else if address = 0x4016 then
    Input.write value
  else if address < 0x6000 then
    ()
  else
    m.mapper.store address value
