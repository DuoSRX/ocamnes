open Core
open Nes
open Nes.Cpu
open Nes.Cartridge
open Tsdl

let run_length = 10000000
let log_length = 20
let logs = Array.create ~len:log_length ""
let term = ref false

let rec n_times f n =
  if n > 0 then (
    f (); n_times f (n - 1)
  )

let memory = Array.create ~len:0x10000 0
let rom = load_rom "./roms/donkey.nes"
(* let rom = load_rom "./roms/nestest.nes" *)

let cpu = {
  rom = rom; ppu = Ppu.make ~rom; cycles = 0;
  a = 0; x = 0; y = 0; memory; s = 0xFD; pc = 0; extra_cycles = 0;
  zero = false; negative = false; carry = false; decimal = false; interrupt = true; overflow = false;
  nestest = false; steps = -1;
}

let event_loop window renderer texture =
  let e = Sdl.Event.create () in
  let rec loop () =
    let cycles = cpu.cycles in
    (* logs.(cpu.steps % log_length) <- step cpu; *)
    ignore (step cpu);
    let elapsed = cpu.cycles - cycles in
    n_times (fun () -> Ppu.step cpu.ppu) (elapsed * 3);
    if cpu.ppu.nmi then Cpu.nmi cpu;

    if cpu.ppu.new_frame then (
      cpu.ppu.new_frame <- false;
      ignore @@ Sdl.set_window_title window (sprintf "%d" cpu.steps);
      let _ = Sdl.update_texture texture None cpu.ppu.frame_content (256 * 3) in
      let _ = Sdl.render_clear renderer in
      let _ = Sdl.render_copy renderer texture in
      let _ = Sdl.render_present renderer in
      ()
    );

    if Sdl.poll_event (Some e) then (
      let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in

      match Sdl.Event.(enum (get e typ)) with
        | `Key_down when key_scancode e = `Space ->
          Cpu.Debugger.break_on_step := true;
          loop ()
        | `Quit -> ()
        | _ -> loop ()
    ) else loop ()

  in
  Sdl.start_text_input();
  loop ()

let main () =
  cpu.pc <- Cpu.load_word cpu 0xFFFC;
  (* Bigarray.Array1.fill cpu.ppu.frame_content 0xFF; *)

  for y = 0 to 239 do
    for x = 0 to 255 do
      cpu.ppu.frame_content.{(y * 256 + x) * 3 + 0} <- 0x00;
      cpu.ppu.frame_content.{(y * 256 + x) * 3 + 1} <- 0xFF;
      cpu.ppu.frame_content.{(y * 256 + x) * 3 + 2} <- 0x00;
    done;
  done;

  match Sdl.init Sdl.Init.everything with
  | Error (`Msg e) -> Sdl.log "Init error %s" e; exit 1
  | Ok () ->
    Sdl.log "SDL loaded...";
    let flags = Sdl.Window.(+) Sdl.Window.shown Sdl.Window.opengl in
    match Sdl.create_window ~w:512 ~h:480 "SDL OpenGL" flags with
    | Error (`Msg e) -> Sdl.log "Create window error %s" e; exit 1
    | Ok w ->
      match Sdl.create_renderer w ~flags:Sdl.Renderer.accelerated with
      | Error (`Msg e) -> Sdl.log "Create renderer error %s" e; exit 1
      | Ok renderer ->
        match Sdl.create_texture renderer Sdl.Pixel.format_bgr24 Sdl.Texture.access_streaming ~w:256 ~h:240 with
        | Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
        | Ok texture ->
          event_loop w renderer texture;
          (* Array.iter logs ~f:print_endline; *)
          printf "Cycles: %d Steps: %d" cpu.cycles cpu.steps;
          Sdl.log "Bye!";
          Sdl.quit ();
          exit 0
(* let main () =
  cpu.pc <- Cpu.load_word cpu 0xFFFC;

  while cpu.steps < run_length && not !term do
    let cycles = cpu.cycles in
    logs.(cpu.steps % log_length) <- step cpu;
    let elapsed = cpu.cycles - cycles in
    n_times (fun () -> Ppu.step cpu.ppu) (elapsed * 3);
    if cpu.ppu.nmi then Cpu.nmi cpu;
    (* print_endline logs.(cpu.steps % log_length); *)
  done;

  Array.iter logs ~f:print_endline;
  ()*)

let () = main ()
