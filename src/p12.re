type rle('a) =
  | One('a)
  | Many((int, 'a));

let decode = (l) => {
  let rec elem = (e, acc) =>
    switch e {
    | One(c) => [c, ...acc]
    | Many((cnt, c)) =>
      if (cnt > 2) {
        elem(Many((cnt - 1, c)), [c, ...acc])
      } else {
        elem(One(c), [c, ...acc])
      }
    };
  let rec aux = (l, acc) =>
    switch l {
    | [] => acc
    | [a, ...rest] => aux(rest, elem(a, acc))
    };
  List.rev(aux(l, []))
};

let () =
  decode([Many((4, "a")), One("b"), Many((2, "c")), Many((2, "a")), One("d"), Many((4, "e"))])
  |> Js.log;
