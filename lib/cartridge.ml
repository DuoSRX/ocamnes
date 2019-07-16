open Core

type header = {
  prg_size: int;
  chr_size: int;
}

type rom = {
  headers : header;
  prg: int array;
  chr: int array;
}

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
  { prg_size = rom.(4) * 0x4000; chr_size = rom.(5) * 0x2000 }

let load_rom () =
  let filename = "./nestest.nes" in
  let rom = open_file filename in
  let headers = load_headers rom in
  (* printf "Loaded rom %s\n" filename;
  printf "PRG:%04x CHR:%04x\n" headers.prg_size headers.chr_size; *)
  {
    headers;
    prg = Array.slice rom 0x10 (0x10 + headers.prg_size);
    chr = Array.slice rom (0x10 + headers.prg_size) (0x10 + headers.prg_size + headers.chr_size);
  }

let load_rom_into_memory mem rom =
  let dst_pos = 0x10000 - rom.headers.prg_size in
  Array.blit ~len:rom.headers.prg_size ~src:rom.prg ~src_pos:0 ~dst:mem ~dst_pos;
  (* TODO: Mirror while reading instead of copying the whole data twice *)
  Array.blit ~len:rom.headers.prg_size ~src:rom.prg ~src_pos:0 ~dst:mem ~dst_pos:(dst_pos - rom.headers.prg_size)
  (* TODO: Load CHR into PPU *)
  (* Array.blit ~len:rom.headers.chr_size ~src:rom.chr ~src_pos:0 ~dst:cpu.memory ~dst_pos:0 *)
