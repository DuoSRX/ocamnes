open Core
open Nes
open Nes.Cpu
open Nes.Cartridge
open Tsdl

let log_length = 20
let logs = Array.create ~len:log_length ""

(* let rom = load_rom "./roms/donkey.nes" *)
(* let rom = load_rom "./roms/nestest.nes" *)
(* let rom = load_rom "./roms/balloon_fight.nes" *)
(* let rom = load_rom "./roms/ice_climber.nes" *)
(* let rom = load_rom "./roms/instr_test-v5/official_only.nes" *)
(* let rom = load_rom "./roms/nestress.nes" *)
(* let rom = load_rom "./roms/mario.nes" *)
(* let rom = load_rom "./roms/1942.nes" *)
(* let rom = load_rom "./roms/megaman2.nes" *)
let rom = load_rom "./roms/contra.nes"

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

let render ~texture ~renderer ~pixels =
  Sdl.update_texture texture None pixels (256 * 3) |> sdl_try;
  Sdl.render_clear renderer |> sdl_try;
  Sdl.render_copy renderer texture |> sdl_try;
  Sdl.render_present renderer

let event_loop ~nes ~window ~renderer ~texture =
  let cpu = nes.cpu in
  let ppu = nes.ppu in
  let e = Sdl.Event.create () in
  let frame_count = ref 0 in
  let ticks = ref (Sdl.get_ticks () |> Int32.to_float) in
  let prev_frames = ref 0 in
  let do_quit = ref false in

  while not !do_quit do
    let prev_cycles = cpu.cycles in
    step cpu |> Option.iter ~f:print_endline;
    let elapsed_cycles = cpu.cycles - prev_cycles in

    for _ = 1 to elapsed_cycles * 3 do
      Ppu.step ppu;
    done;

    if ppu.nmi_triggered then Cpu.trigger_nmi cpu;

    if !prev_frames <> ppu.frames then (
      prev_frames := ppu.frames;

      let now = Sdl.get_ticks () |> Int32.to_float in
      if now >= (!ticks +. 1000.0) then (
        ignore @@ Sdl.set_window_title window (sprintf "%d FPS" !frame_count);
        frame_count := 0;
        ticks := now;
      ) else (
        frame_count := !frame_count + 1;
      );

      render ~renderer ~texture ~pixels:ppu.frame_content;
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
  let nes = Nes.make rom ~tracing:false in
  nes.cpu.pc <- Cpu.load_word nes.cpu 0xFFFC;

  Sdl.init Sdl.Init.(video + events) |> sdl_try;

  let flags = Sdl.Window.(shown + opengl + resizable) in
  let window = sdl_try @@ Sdl.create_window ~w:512 ~h:480 "Ocamnes" flags in
  let renderer = sdl_try @@ Sdl.create_renderer window ~flags:(Sdl.Renderer.(accelerated + presentvsync)) in
  let texture = sdl_try @@ Sdl.create_texture renderer Sdl.Pixel.format_rgb24 Sdl.Texture.access_streaming ~w:256 ~h:240 in
  event_loop ~nes ~window ~renderer ~texture;
  Sdl.quit ();
  exit 0

let () = main ()

 (* (ocamlopt_flags (-O3 -unsafe -inlining-report)) *)