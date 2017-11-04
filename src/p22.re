let range = (a, b) => {
  let rec aux = (a, accum) =>
    if (a == b) {
      [a, ...accum];
    } else {
      aux(a + 1, [a, ...accum]);
    };

  if (a <= b) {
    List.rev(aux(a, []));
  } else {
    aux(a, []);
  }
};

let () = {
  range(4, 9) |> Js.log;
  range(9, 4) |> Js.log;
};
