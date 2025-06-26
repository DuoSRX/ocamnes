open Core

type pulse_channel = {
  mutable enabled : bool;
  mutable duty_cycle : int; (* 0–3 *)
  mutable timer : int;
  mutable timer_counter : int;
  mutable sequence_index : int;
  mutable volume : int;
}

let duty_table = [|
  [|0;1;0;0;0;0;0;0|]; (* 12.5% *)
  [|0;1;1;0;0;0;0;0|]; (* 25% *)
  [|0;1;1;1;1;0;0;0|]; (* 50% *)
  [|1;0;0;1;1;1;1;1|]; (* 75% *)
|]

let tick_pulse ch =
  if ch.timer_counter = 0 then begin
    ch.timer_counter <- ch.timer;
    ch.sequence_index <- (ch.sequence_index + 1) mod 8
  end else
    ch.timer_counter <- ch.timer_counter - 1

let pulse_sample ch =
  if not ch.enabled then 0
  else
    let duty = duty_table.(ch.duty_cycle).(ch.sequence_index) in
    ch.volume * duty (* 0 or volume *)

(* let make () =
  {
  } *)
