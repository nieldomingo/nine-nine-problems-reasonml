let compress = (l) => {
  let rec aux = (l, acc) =>
    switch l {
    | [a, b, ...rest] =>
      if (a == b) {
        aux([b, ...rest], acc)
      } else {
        aux([b, ...rest], [a, ...acc])
      }
    | _ => acc
    };
  List.rev(aux(l, []))
};

let () =
  compress(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) |> Js.log;
