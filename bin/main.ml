open Core
open Nes
open Nes.Cpu
open Nes.Cartridge
open Tsdl
open ImageLib

(* TODO Append and increment a counter for multiple screenshots *)
(* TODO Save animated gifs *)
let save_screenshot ?(filename="./screenshot.png") frame =
  let img = Image.create_rgb 256 240 in
  for y = 0 to 239 do
    for x = 0 to 255 do
      let offset = (y * 256 + x) * 3 in
      let r = Int.to_int frame.{offset} in
      let g = Int.to_int frame.{offset+1} in
      let b = Int.to_int frame.{offset+2} in
      Image.write_rgb img x y r g b
    done;
  done;
  let writer = ImageUtil_unix.chunk_writer_of_path filename in
  PNG.write writer img

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
  let e = Sdl.Event.create () in
  let frame_count = ref 0 in
  let ticks = ref (Sdl.get_ticks () |> Int32.to_int_exn) in
  let prev_frames = ref 0 in
  let do_quit = ref false in

  while not !do_quit do
    Nes.step nes |> Option.iter ~f:print_endline;

    if !prev_frames <> nes.ppu.frames then (
      prev_frames := nes.ppu.frames;

      let now = Sdl.get_ticks () |> Int32.to_int_exn in
      if now >= (!ticks + 1000) then (
        ignore @@ Sdl.set_window_title window (sprintf "%d FPS" !frame_count);
        frame_count := 0;
        ticks := now;
      ) else (
        frame_count := !frame_count + 1;
      );

      render ~renderer ~texture ~pixels:nes.ppu.frame_content;
      Out_channel.flush stdout;

      let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
      while Sdl.poll_event (Some e) do
        match Sdl.Event.(enum (get e typ)) with
          | `Quit -> do_quit := true
          | `Key_down -> (match key_scancode e with
            | `Escape -> do_quit := true
            | `Apostrophe -> Debugger.break_on_step := true
            | `S -> save_screenshot nes.ppu.frame_content
            | _ -> update_input ~down:true (key_scancode e))
          | `Key_up -> update_input ~down:false (key_scancode e)
          | _ -> ()
      done;
    );
  done

let main () =
  let rom = (Sys.get_argv()).(1) |> load_rom in

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
