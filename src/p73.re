include Mult_tree;

let rec lispy = (tree) => {
  switch tree {
  | T(c, []) => Char.escaped(c)
  | T(c, children) =>
    "(" ++ Char.escaped(c) ++
    List.fold_left(
      (s, sub_tree) => s ++ " " ++ lispy(sub_tree),
      "",
      children) ++ ")"
  };
};

let () = {
  let t = T('a', [T('f',[T('g',[])]), T('c',[]),
              T('b',[T('d',[]), T('e',[])])]);
  lispy(T('a', [])) |> Js.log;
  lispy(T('a', [T('b', [])])) |> Js.log;
  lispy(t) |> Js.log;
};
