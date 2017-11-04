let rec at = (k, l) =>
  switch l {
  | [] => None
  | [a, ...rest] => k == 1 ? Some(a) : at(k - 1, rest)
  };

let () = {
  at(2, [1, 2, 3, 4]) |> Js.log;
  at(5, [1, 2, 3, 4]) |> Js.log;
  at(1, []) |> Js.log
};
