let duplicate = (l) => {
  let rec aux = (l, accum) =>
    switch l {
    | [] => accum
    | [a, ...rest] => aux(rest, [a, a, ...accum])
    };
  List.rev(aux(l, []))
};

let () = duplicate(["a", "b", "c", "c", "d"]) |> Js.log;
