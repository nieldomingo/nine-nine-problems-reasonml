let rotate = (l, n) => {
  let rec take = (l, n, accum) =>
    switch (l, n) {
    | ([], _) => (accum, [])
    | (l, 0) => (accum, l)
    | ([a, ...rest], n) => take(rest, n - 1, [a, ...accum])
    };
  let rec into = (l, accum) =>
    switch l {
    | [] => accum
    | [a, ...rest] => into(rest, [a, ...accum])
    };
  let normalize = (n) => {
    let len = List.length(l);
    if (n < 0) {
      len + n mod len
    } else {
      n mod len
    }
  };
  let (l1, l2) = take(l, normalize(n), []);
  into(List.rev(l2), into(l1, []))
};

let () = {
  rotate(["a", "b", "c", "d", "e", "f", "g", "h"], 3) |> Js.log;
  rotate(["a", "b", "c", "d", "e", "f", "g", "h"], (-2)) |> Js.log
};
