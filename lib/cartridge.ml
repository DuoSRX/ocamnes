open Core

type header = {
  prg_size: int;
  chr_size: int;
  mapper : int;
} [@@deriving show]

type rom = {
  headers : header;
  prg: int array;
  chr: int array;
  ram: int array;
} [@@deriving show]

let open_file name =
  let file = Stdio.In_channel.create name in
  let size = Int64.to_int_trunc(In_channel.length file) in
  let buffer = Bytes.create size in
  let _ = Stdio.In_channel.really_input file ~buf:buffer ~pos:0 ~len:size in
  let result = Array.create ~len:size 0 in
  for i = 0 to size - 1 do
    result.(i) <- int_of_char @@ Bytes.get buffer i
  done;
  result

let load_headers rom =
  {
    prg_size = rom.(4) * 0x4000;
    chr_size = rom.(5) * 0x2000;
    mapper = (rom.(7) land 0xF0) lor (rom.(6) lsr 4)
  }

let load_rom path =
  let rom = open_file path in
  let headers = load_headers rom in
  (* printf "Loaded rom %s\n" path;
  printf "PRG:%04x CHR:%04x Mapper:%d\n" headers.prg_size headers.chr_size headers.mapper; *)
  {
    headers;
    prg = Array.slice rom 0x10 (0x10 + headers.prg_size);
    chr = Array.slice rom (0x10 + headers.prg_size) (0x10 + headers.prg_size + headers.chr_size);
    ram = Array.create ~len:0x2000 0;
  }
