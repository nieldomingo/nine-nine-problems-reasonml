let split = (l, n) => {
  let rec aux = (l, n, accum) =>
    switch (n, l) {
    | (1, [a, ...rest]) => (List.rev([a, ...accum]), rest)
    | (n, [a, ...rest]) => aux(rest, n - 1, [a, ...accum])
    | (_, []) => (List.rev(accum), [])
    };
  aux(l, n, [])
};

let () = {
  split(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 3) |> Js.log;
  split(["a", "b", "c", "d"], 5) |> Js.log
};
