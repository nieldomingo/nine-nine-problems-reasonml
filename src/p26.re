let rec extract = (n, l) =>
  if (List.length(l) == n) {
    [l]
  } else if (n == 0) {
    [[]]
  } else {
    switch l {
    | [h, ...t] => List.map((e) => [h, ...e], extract(n - 1, t)) @ extract(n, t)
    | [] => raise(Not_found)
    }
  };

let () = extract(2, ["a", "b", "c", "d", "e"]) |> Js.log;
