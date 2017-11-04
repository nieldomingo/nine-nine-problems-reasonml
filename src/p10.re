let encode = (l) => {
  let rec aux = (l, acc, cnt) =>
    switch l {
    | [] => []
    | [a] => [(cnt + 1, a), ...acc]
    | [a, b, ...rest] =>
      if (a == b) {
        aux([b, ...rest], acc, cnt + 1)
      } else {
        aux([b, ...rest], [(cnt + 1, a), ...acc], 0)
      }
    };
  List.rev(aux(l, [], 0))
};

let () = encode(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) |> Js.log;
