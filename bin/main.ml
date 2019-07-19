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
(* let rom = load_rom "./roms/instr_test-v5/official_only.nes" *)
(* let rom = load_rom "./roms/instr_test-v5/rom_singles/01-basics.nes" *)
(* let rom = load_rom "./roms/instr_test-v5/rom_singles/03-immediate.nes" *)
(* let rom = load_rom "./roms/nestest.nes" *)
(* let rom = load_rom "./roms/color_test.nes" *)
(* let rom = load_rom "./roms/ice_climber.nes" *)
let rom = load_rom "./roms/donkey.nes"

let cpu = {
  rom = rom; ppu = Ppu.make ~rom; cycles = 0;
  a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0; extra_cycles = 0;
  zero = false; negative = false; carry = false; decimal = false; interrupt = true; overflow = false;
  nestest = false; tracing = false; steps = -1;
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

let event_loop window renderer texture =
  let e = Sdl.Event.create () in
  let last_time = ref (Unix.gettimeofday ()) in
  let frames = ref 0.0 in
  let cycles = ref 0 in
  let do_quit = ref false in

  while not !do_quit do
    ignore @@ step cpu;
    if cpu.ppu.nmi then Cpu.nmi cpu;
    Ppu.step cpu.ppu cpu.cycles;

    if cpu.ppu.new_frame then (
      let now = Unix.gettimeofday () in
      if now >= (!last_time +. 1.0) then (
        ignore @@ Sdl.set_window_title window (sprintf "%.1f FPS" !frames);
        frames := 0.0;
        cycles := 0;
        last_time := now
      ) else (
        frames := !frames +. 1.0;
        cycles := cpu.cycles - !cycles
      );

      cpu.ppu.new_frame <- false;

      let _ = Sdl.update_texture texture None cpu.ppu.frame_content (256 * 3) in
      let _ = Sdl.render_clear renderer in
      let _ = Sdl.render_copy renderer texture in
      let _ = Sdl.render_present renderer in

      let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
      while Sdl.poll_event (Some e) do
        match Sdl.Event.(enum (get e typ)) with
          | `Quit -> do_quit := true
          | `Key_down when key_scancode e = `Escape -> do_quit := true
          | `Key_down when key_scancode e = `Space -> Cpu.Debugger.break_on_step := true
          | `Key_down when key_scancode e = `V ->
            Cpu.Debugger.break_on_step := true;
            printf "%04X" cpu.ppu.oam.(31);
          | `Key_up -> update_input ~down:false (key_scancode e)
          | `Key_down -> update_input ~down:true (key_scancode e)
          | _ -> ()
      done;
    );
    (* Sdl.start_text_input(); *)
  done

let main () =
  cpu.pc <- Cpu.load_word cpu 0xFFFC;

  match Sdl.init Sdl.Init.(everything) with
  | Error (`Msg e) -> Sdl.log "Init error %s" e; exit 1
  | Ok () ->
    Sdl.log "SDL loaded...";
    let flags = Sdl.Window.(+) Sdl.Window.shown Sdl.Window.opengl in
    match Sdl.create_window ~w:512 ~h:480 "SDL OpenGL" flags with
    | Error (`Msg e) -> Sdl.log "Create window error %s" e; exit 1
    | Ok w ->
      match Sdl.create_renderer w ~flags:(Sdl.Renderer.accelerated) with
      | Error (`Msg e) -> Sdl.log "Create renderer error %s" e; exit 1
      | Ok renderer ->
        match Sdl.create_texture renderer Sdl.Pixel.format_rgb24 Sdl.Texture.access_streaming ~w:256 ~h:240 with
        | Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
        | Ok texture ->
          event_loop w renderer texture;
          (* Array.iter logs ~f:print_endline; *)
          printf "Cycles: %d Steps: %d" cpu.cycles cpu.steps;
          Sdl.log "Bye!";
          Sdl.quit ();
          exit 0

let () = main ()

 (* (ocamlopt_flags (-O3 -unsafe -inlining-report)) *)