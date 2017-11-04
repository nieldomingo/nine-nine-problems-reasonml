type rle('a) =
  | One('a)
  | Many((int, 'a));

let encode = (l) => {
  let elem = (e, cnt) =>
    switch cnt {
    | 0 => One(e)
    | cnt => Many((cnt + 1, e))
    };
  let rec aux = (l, acc, cnt) =>
    switch l {
    | [] => []
    | [a] => [elem(a, cnt), ...acc]
    | [a, b, ...rest] =>
      if (a == b) {
        aux([b, ...rest], acc, cnt + 1)
      } else {
        aux([b, ...rest], [elem(a, cnt), ...acc], 0)
      }
    };
  List.rev(aux(l, [], 0))
};

let () = encode(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) |> Js.log;
