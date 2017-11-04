let remove_at = (k, l) => {
  let rec split = (k, l, accum) =>
    switch (l, k) {
    | ([], _) => (accum, [])
    | ([_, ...rest], 0) => (accum, rest)
    | ([a, ...rest], k) => split(k - 1, rest, [a, ...accum])
    };
  let rec into = (l, accum) =>
    switch l {
    | [] => accum
    | [a, ...rest] => into(rest, [a, ...accum])
    };
  let (l1, l2) = split(k, l, []);
  into(l1, l2)
};

let () = remove_at(1, ["a", "b", "c", "d"]) |> Js.log;
