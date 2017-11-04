let rec last =
  fun
  | [] => None
  | [a] => Some(a)
  | [_, ...rest] => last(rest);

let () = {
  last([1, 2, 3, 4]) |> Js.log;
  last([]) |> Js.log
};
