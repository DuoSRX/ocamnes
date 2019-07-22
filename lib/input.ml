type controller = {
 mutable a : bool;
 mutable b : bool;
 mutable select : bool;
 mutable start : bool;
 mutable up : bool;
 mutable down : bool;
 mutable left : bool;
 mutable right : bool;
}

let controller_state = {
  a = false;
  b = false;
  select = false;
  start = false;
  up = false;
  down = false;
  left = false;
  right = false;
}

let check_key = function
| 0 -> controller_state.a
| 1 -> controller_state.b
| 2 -> controller_state.select
| 3 -> controller_state.start
| 4 -> controller_state.up
| 5 -> controller_state.down
| 6 -> controller_state.left
| 7 -> controller_state.right
| _ -> false

let strobe = ref 0
let input_state = ref 0

let next_key () =
  let key = !input_state in
  input_state := (!input_state + 1) mod 8;
  if !strobe land 1 = 1 then
    input_state := 0;
  check_key key

let write value =
  strobe := value;
  if value land 1 = 1 then
    input_state := 0
