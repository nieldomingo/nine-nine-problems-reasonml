let rec extract = (n, l) =>
  if (List.length(l) == n) {
    [(l, [])]
  } else if (n == 0) {
    [([], l)]
  } else {
    switch l {
    | [h, ...t] => List.map(
      (e) => {
        let (c, u) = e;
        ([h, ...c], u);
      }, extract(n - 1, t)) @ 
      List.map(
      (e) => {
        let (c, u) = e;
        (c, [h, ...u]);
      },
      extract(n, t))
    | [] => raise(Not_found)
    }
  };

let group = (l, g) => {
  let rec aux = (l, g) => {
    switch g {
    | [] => [([], l)]
    | [h, ...t] => {
        List.flatten(
          List.map(
            (e) => {
              let (cs, r) = e;
              List.map(
                (e2) => {
                  let (c, r2) = e2;
                  ([c, ...cs], r2);
                },
                extract(h, r));
            },
            aux(l, t)
          ));
      }
    }
  };
  List.map((e) => {
      let (g, _) = e;
      g;
    },
    aux(l, g));
};

let () = group(["a", "b", "c", "d"], [1, 1]) |> Js.log;
