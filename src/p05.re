let rev = (l) => {
  let rec rev_inner = (l, acc) =>
    switch l {
    | [] => acc
    | [a, ...rest] => rev_inner(rest, [a, ...acc])
    };
  rev_inner(l, [])
};

let () = {
  rev([1, 2, 3, 4]) |> Js.log;
  rev([1]) |> Js.log;
  rev([]) |> Js.log
};
