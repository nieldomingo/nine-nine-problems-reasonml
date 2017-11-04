let pack = (l) => {
  let rec aux = (l, acc, accSub) =>
    switch l {
    | [a, b, ...rest] =>
      if (a == b) {
        aux([b, ...rest], acc, [b, ...accSub])
      } else {
        aux([b, ...rest], [accSub, ...acc], [b])
      }
    | _ => [accSub, ...acc]
    };
  List.rev(
    switch l {
    | [] => []
    | [a, ...rest] => aux(l, [], [a])
    }
  )
};

let pack2 = (l) => {
  let rec aux = (l, acc, accSub) =>
    switch l {
    | [] => []
    | [a] => [[a, ...accSub], ...acc]
    | [a, b, ...rest] =>
      if (a == b) {
        aux([b, ...rest], acc, [a, ...accSub])
      } else {
        aux([b, ...rest], [[a, ...accSub], ...acc], [])
      }
    };
  List.rev(aux(l, [], []))
};

let () = {
  pack(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) |> Js.log;
  pack2(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) |> Js.log
};
