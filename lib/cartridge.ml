open Core

type mirroring = Vertical | Horizontal
  [@@deriving show { with_path = false }]

type header = {
  prg_size: int;
  chr_size: int;
  mapper : int;
  mirroring : mirroring
} [@@deriving show]

type rom = {
  headers : header;
  prg: int array;
  chr: int array;
  ram: int array;
} [@@deriving show]

let open_file name =
  name
  |> In_channel.read_all
  |> String.to_array
  |> Array.map ~f:Char.to_int

let load_headers rom =
  {
    prg_size = rom.(4) * 0x4000;
    chr_size = rom.(5) * 0x2000;
    mapper = (rom.(7) land 0xF0) lor (rom.(6) lsr 4);
    mirroring = if (rom.(6) land 1) = 1 then Vertical else Horizontal
  }

let load_rom path =
  let rom = open_file path in
  let headers = load_headers rom in
  printf "Loaded rom %s\n" path;
  printf "PRG:%04x CHR:%04x Mirroring:%s Mapper:%d\n"
    headers.prg_size headers.chr_size (show_mirroring headers.mirroring) headers.mapper;

  let chr = if headers.chr_size = 0 then
    Array.create ~len:0x2000 0
  else
    Array.slice rom (0x10 + headers.prg_size) (0x10 + headers.prg_size + headers.chr_size)
  in

  {
    headers;
    prg = Array.slice rom 0x10 (0x10 + headers.prg_size);
    chr;
    ram = Array.create ~len:0x2000 0;
  }
