# OCamNes

Yes. Yet another NES Emulator.

Currently passes the Nestest suite up to unofficial opcodes.

## Controls

* Q, Escape -> Quit
* Arrow keys -> Left, down, up, right
* Z: A button
* X: B Button
* Return -> Start
* Left Shift -> Select
* S -> Saves a screenshot as `screenshot.png`
* Apostrophe -> Start the debugger

## Games running mostly glitch-less:

* Super Mario Bros
* Donkey kong
* Balloon fight
* Lode runner
* Contra
* Gradius

## TODO

* Mappers (only NROM/UxROM/CNROM are supported)
* PPU
  * Special flags to stop rendering sprites at the edge
* IRQ (Not sure if they're used anywhere)
* APU
* Second controller
* Unofficial opcodes
* Load/Save state
* Better debugger and logging
* Remap controls
* CLI args
