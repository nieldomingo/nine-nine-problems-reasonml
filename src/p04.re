let rec length =
  fun
  | [] => 0
  | [_, ...rest] => 1 + length(rest);

let rec length_tail = (~acc=0, l) =>
  switch l {
  | [] => acc
  | [_, ...rest] => length_tail(rest, ~acc=acc + 1)
  };

let () = {
  length([]) |> Js.log;
  length([1, 2, 3]) |> Js.log;
  length_tail([]) |> Js.log;
  length_tail([1, 2, 3]) |> Js.log
};
