include Mult_tree;

let bottom_up = (tree) => {
  let rec aux = (tree, acc) => {
    let T(c, children) = tree;
      [c, ...List.fold_left(
        (acc, node) => aux(node, acc),
        acc,
        children)];
  };
  List.rev(aux(tree, []));
};

let () = {
  let t = T('a', [T('f',[T('g',[])]), T('c',[]),
              T('b',[T('d',[]), T('e',[])])]);
  bottom_up(t) |> Js.log;
};
