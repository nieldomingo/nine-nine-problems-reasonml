let insert_at = (v, k, l) => {
  let rec aux = (k, l, accum) =>
    switch (k, l) {
    | (0, l) => aux(-1, l, [v, ...accum])
    | (_, []) => accum
    | (k, [h, ...rest]) => aux(k - 1, rest, [h, ...accum])
    };
  List.rev(aux(k, l, []));
};

let () = {
  insert_at("alfa", 1, ["a", "b", "c", "d"]) |> Js.log;
  insert_at("alfa", 3, ["a", "b", "c", "d"]) |> Js.log;
  insert_at("alfa", 4, ["a", "b", "c", "d"]) |> Js.log;
};
