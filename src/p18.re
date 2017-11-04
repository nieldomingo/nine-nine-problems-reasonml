let prefix = (l, n) => {
  let rec aux = (l, n, accum) =>
    switch (n, l) {
    | (1, [a, ...rest]) => List.rev([a, ...accum])
    | (n, [a, ...rest]) => aux(rest, n - 1, [a, ...accum])
    | (_, []) => List.rev(accum)
    };
  aux(l, n, [])
};

let rec skip = (l, n) =>
  switch (l, n) {
  | ([], _) => []
  | (l, 0) => l
  | ([_, ...rest], n) => skip(rest, n - 1)
  };

let slice = (l, j, k) => prefix(skip(l, j), k - j + 1);

let () = slice(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 2, 6) |> Js.log;
