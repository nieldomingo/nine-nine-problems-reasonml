let rec last_two =
  fun
  | []
  | [_] => None
  | [a, b] => Some((a, b))
  | [_, ...rest] => last_two(rest);

let () = {
  last_two([1, 2, 3, 4]) |> Js.log;
  last_two([1]) |> Js.log;
  last_two([]) |> Js.log
};
