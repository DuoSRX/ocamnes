# OCamNes

Yes. Yet another NES Emulator. Built in OCaml.

Currently passes the Nestest suite up to unofficial opcodes.

![screenshot](https://github.com/DuoSRX/ocamnes/blob/main/ocamnes-donkey.gif)

## Controls

| NES Controller | Keyboard  |
|----------------|-----------|
| Up             | Up        |
| Down           | Down      |
| Left           | Left      |
| Right          | Right     |
| A              | Z         |
| B              | X         |
| Start          | Return    |
| Select         | Left Shift|
| Quit           | Q         |
| Screenshot     | S         |
| Debugger       | '         |

## Games running mostly glitch-less:

* Super Mario Bros
* Donkey kong
* Balloon fight
* Lode runner
* Contra
* Gradius

## How to build

* `dune build`
* `opam install . --deps-only`
* `dune exec ./bin/main.exe`

## Debugger Controls

| Key        | Action                                 |
|------------|----------------------------------------|
| s          | Step                                   |
| c          | Continue                               |
| q          | Quit                                   |
| bp         | List breakpoints                       |
| bp add a   | Add a breakpoint at address a          |
| bp del a   | Delete a breakpoint at address a       |
| byte a     | Print byte at address a                |
| byte a b   | Print bytes from a to b                |
| word a     | Print word at address a                |
| ppu a      | Print PPU VRAM or Register at a        |
| ppu a b    | Print PPU VRAM or Register from a to b |
| pputrace   | Print PPU internal registers (t, v, x) |
| oam        | Print OAM data                         |

## TODO

* Support more mappers (only NROM/UxROM/CNROM are supported)
* PPU Special flags to stop rendering sprites at the edge
* IRQ (Not sure if they're used anywhere)
* APU (No sound for now)
* Second controller support
* Unofficial opcodes
* Load/Save state
* Better debugger and logging
* Remap controls
