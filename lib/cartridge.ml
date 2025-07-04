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
  let ic = open_in name in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  Array.init len (fun i -> Char.code s.[i])

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
  Printf.printf "Loaded rom %s\n" path;
  Printf.printf "PRG:%04x CHR:%04x Mirroring:%s Mapper:%d\n"
    headers.prg_size headers.chr_size (show_mirroring headers.mirroring) headers.mapper;

  let chr = if headers.chr_size = 0 then
    Array.make 0x2000 0
  else
    Array.sub rom (0x10 + headers.prg_size) headers.chr_size
  in

  {
    headers;
    prg = Array.sub rom 0x10 headers.prg_size;
    chr;
    ram = Array.make 0x2000 0;
  }
