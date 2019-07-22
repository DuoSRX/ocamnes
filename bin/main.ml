open Core
open Nes
open Nes.Cpu
open Nes.Cartridge
open Tsdl

let run_length = 10000000
let log_length = 20
let logs = Array.create ~len:log_length ""
let term = ref false

let memory = Array.create ~len:0x800 0
(* let rom = load_rom "./roms/ppu/palette_ram.nes" *)
(* let rom = load_rom "./roms/instr_test-v5/rom_singles/03-immediate.nes" *)
(* let rom = load_rom "./roms/instr_test-v5/rom_singles/02-implied.nes" *)
(* let rom = load_rom "./roms/instr_test-v5/rom_singles/01-basics.nes" *)
(* let rom = load_rom "./roms/nestress.nes" *)
(* let rom = load_rom "./roms/nestest.nes" *)
(* let rom = load_rom "./roms/color_test.nes" *)
(* let rom = load_rom "./roms/donkey.nes" *)
(* let rom = load_rom "./roms/mario.nes" *)
(* let rom = load_rom "./roms/ice_climber.nes" *)
let rom = load_rom "./roms/balloon_fight.nes"

let cpu = {
  rom = rom; ppu = Ppu.make ~rom; cycles = 0;
  a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0; extra_cycles = 0;
  zero = false; negative = false; carry = false; decimal = false; interrupt = true; overflow = false;
  nestest = false; steps = -1; nmi = false;
  tracing = false;
}

let update_input keycode ~down = match keycode with
| `Z -> Input.controller_state.a <- down
| `X -> Input.controller_state.b <- down
| `Return -> Input.controller_state.start <- down
| `Lshift -> Input.controller_state.select <- down
| `Down -> Input.controller_state.down <- down
| `Up -> Input.controller_state.up <- down
| `Left -> Input.controller_state.left <- down
| `Right -> Input.controller_state.right <- down
| _ -> ()

let sdl_try result = match result with
| Ok res -> res
| Error (`Msg m) -> Sdl.log "Create renderer error %s" m; exit 1

let event_loop ~window ~renderer ~texture =
  let e = Sdl.Event.create () in
  let last_time = ref (Unix.gettimeofday ()) in
  let frame_count = ref 0 in
  let prev_frames = ref 0 in
  let do_quit = ref false in

  while not !do_quit do
    let prev_cycles = cpu.cycles in
    step cpu |> Option.iter ~f:print_endline;
    let elapsed_cycles = cpu.cycles - prev_cycles in

    for _ = 1 to elapsed_cycles * 3 do
      Ppu.step cpu.ppu;
    done;

    if cpu.ppu.nmi_triggered then Cpu.trigger_nmi cpu;

    if !prev_frames <> cpu.ppu.frames then (
      prev_frames := cpu.ppu.frames;
      let now = Unix.gettimeofday () in

      if now >= (!last_time +. 1.0) then (
        ignore @@ Sdl.set_window_title window (sprintf "%d FPS - %d steps" !frame_count cpu.steps);
        frame_count := 0;
        last_time := now
      ) else (
        frame_count := !frame_count + 1;
      );

      Sdl.update_texture texture None cpu.ppu.frame_content (256 * 3) |> sdl_try;
      Sdl.render_clear renderer |> sdl_try;
      Sdl.render_copy renderer texture |> sdl_try;
      Sdl.render_present renderer;
      Out_channel.flush stdout;

      let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
      while Sdl.poll_event (Some e) do
        match Sdl.Event.(enum (get e typ)) with
          | `Quit -> do_quit := true
          | `Key_down when key_scancode e = `Escape -> do_quit := true
          | `Key_down when key_scancode e = `Space -> Cpu.Debugger.break_on_step := true
          | `Key_up -> update_input ~down:false (key_scancode e)
          | `Key_down -> update_input ~down:true (key_scancode e)
          | _ -> ()
      done;
    );
  done

let main () =
  cpu.pc <- Cpu.load_word cpu 0xFFFC;

  Sdl.init Sdl.Init.(video + events) |> sdl_try;
  Sdl.log "SDL Loaded";

  let flags = Sdl.Window.(shown + opengl) in
  let window = sdl_try @@ Sdl.create_window ~w:1024 ~h:960 "Ocamnes" flags in
  let renderer = sdl_try @@ Sdl.create_renderer window ~flags:(Sdl.Renderer.accelerated) in
  let texture = sdl_try @@ Sdl.create_texture renderer Sdl.Pixel.format_rgb24 Sdl.Texture.access_streaming ~w:256 ~h:240 in
  event_loop ~window ~renderer ~texture;
  Sdl.log "Bye!";
  Sdl.quit ();
  exit 0

let () = main ()

 (* (ocamlopt_flags (-O3 -unsafe -inlining-report)) *)