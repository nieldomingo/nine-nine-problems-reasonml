let replicate = (l, n) => {
  let rec dup = (a, n, accum) =>
    switch n {
    | 0 => accum
    | n => dup(a, n - 1, [a, ...accum])
    };
  let rec aux = (l, accum) =>
    switch l {
    | [] => accum
    | [a, ...rest] => aux(rest, dup(a, n, accum))
    };
  aux(List.rev(l), [])
};

let () = replicate(["a", "b", "c"], 2) |> Js.log;
