(* let event_loop () =
  let e = Sdl.Event.create () in
  let rec loop () = match Sdl.wait_event (Some e) with
  | Error (`Msg e) -> Sdl.log "Couldn't wait for event %s" e; exit 1
  | Ok () ->
    match Sdl.Event.(enum (get e typ)) with
    | `Quit -> ()
    | _ -> loop ()
  in
  Sdl.start_text_input();
  loop () *)

(* let main () = match Sdl.init Sdl.Init.everything with
  | Error (`Msg e) -> Sdl.log "Init error %s" e; exit 1
  | Ok () ->
    Sdl.log "SDL loaded...";
    let flags = Sdl.Window.(+) Sdl.Window.shown Sdl.Window.opengl in
    match Sdl.create_window ~w:620 ~h:387 "SDL OpenGL" flags with
    | Error (`Msg e) -> Sdl.log "Create window error %s" e; exit 1
    | Ok w ->
      match Sdl.create_renderer w ~flags:Sdl.Renderer.accelerated with
      | Error (`Msg e) -> Sdl.log "Create renderer error %s" e; exit 1
      | Ok renderer ->
        match Sdl.load_bmp "./grumpy-cat.bmp" with
        | Error (`Msg e) -> Sdl.log "BMP Load error %s" e; exit 1
        | Ok surface ->
          match Sdl.create_texture_from_surface renderer surface with
          | Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
          | Ok texture ->
            let _ = Sdl.render_clear renderer in
            let _ = Sdl.render_copy renderer texture in
            let _ = Sdl.render_present renderer in
            event_loop ();
            Sdl.log "Bye!";
            Sdl.quit ();
            exit 0 *)

        (* match Sdl.create_texture r Sdl.Pixel.format_bgr24 Sdl.Texture.access_target ~w:640 ~h:480 with
        | Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
        | Ok tex ->
          let _ = Sdl.render_clear r in
          let _ = Sdl.render_copy r tex in
          let _ = Sdl.render_present in
          Sdl.delay 3000l;
          Sdl.destroy_window w;
          Sdl.quit ();
          exit 0 *)
