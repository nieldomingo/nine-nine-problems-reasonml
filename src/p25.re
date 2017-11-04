let pop = (k, l) => {
  let rec split = (k, l, accum) =>
    switch (l, k) {
    | ([], _) => (None, accum, [])
    | ([a, ...rest], 0) => (Some(a), accum, rest)
    | ([a, ...rest], k) => split(k - 1, rest, [a, ...accum])
    };
  let rec into = (l, accum) =>
    switch l {
    | [] => accum
    | [a, ...rest] => into(rest, [a, ...accum])
    };
  let (e, l1, l2) = split(k, l, []);
  (e, into(l1, l2))
};

let rand_select = (l, n) => {
  let rec aux = (l, n, accum) => {
    if (n == 0) {
      accum;
    } else {
      let c = Random.int(List.length(l));
      let (e, r) = pop(c, l);
      switch e {
      | None => accum
      | Some(v) => aux(r, n - 1, [v, ...accum])
      };
    }
  };
  aux(l, n, []);
};

let permutation = (l) => rand_select(l, List.length(l));

let () = permutation(["a", "b", "c", "d", "e", "f"]) |> Js.log;
