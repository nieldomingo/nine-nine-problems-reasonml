let drop = (l, n) => {
  let rec aux = (l, i, accum) =>
    switch l {
    | [] => accum
    | [a, ...rest] =>
      if (i == n) {
        aux(rest, 1, accum)
      } else {
        aux(rest, i + 1, [a, ...accum])
      }
    };
  List.rev(aux(l, 1, []))
};

let () = drop(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 3) |> Js.log;
