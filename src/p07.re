type node('a) =
  | One('a)
  | Many(list(node('a)));

let rec flatten =
  fun
  | [] => []
  | [a, ...rest] => aux(a) @ flatten(rest)
and aux =
  fun
  | One(a) => [a]
  | Many(a) => flatten(a);

let flatten2 = (l) => {
  let rec aux = (l, acc) =>
    switch l {
    | [] => acc
    | [One(a), ...rest] => aux(rest, [a, ...acc])
    | [Many(a), ...rest] => aux(rest, aux(a, acc))
    };
  List.rev(aux(l, []))
};

let () = {
  flatten([One("a"), Many([One("b"), Many([One("c"), One("d")]), One("e")])]) |> Js.log;
  flatten2([One("a"), Many([One("b"), Many([One("c"), One("d")]), One("e")])]) |> Js.log
};
