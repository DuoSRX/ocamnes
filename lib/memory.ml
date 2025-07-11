type t = {
  ppu : Ppu.t;
  mapper : Mapper.t;
  ram : int array;
}

let make ~mapper ~ppu =
  {
    ppu; mapper;
    ram = Array.make 0x800 0;
  }

let load m address =
  if address < 0x2000 then
    m.ram.(address land 0x7FF)
  else if (address lsr 13) = 1 then
    Ppu.read_register m.ppu (0x2000 + address mod 8)
  else if address = 0x4016 then
    if Input.next_key () then 1 else 0
  else if address < 0x6000 then
    0
  else
    m.mapper.load address

let dma m address =
  let page = address * 0x100 in
  for x = 0 to 255 do
    Ppu.write_register m.ppu 0x2004 (load m (page + x))
  done

let store m address value =
  if address < 0x2000 then
    m.ram.(address land 0x7FF) <- value
  else if (address lsr 13) = 1 then
    Ppu.write_register m.ppu (0x2000 + address mod 8) value
  else if address < 0x4000 then
    Ppu.store m.ppu address value
  else if address = 0x4014 then
    dma m value
  else if address = 0x4016 then
    Input.write value
  else if address < 0x6000 then
    ()
  else
    m.mapper.store address value
