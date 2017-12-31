include Mult_tree;

let ipl = (tree) => {
  let rec aux = (tree, pl) => {
    let T(_, children) = tree;
    List.fold_left(
      (cnt, t) => cnt + aux(t, pl + 1),
      0,
      children) + pl;
  };
  aux(tree, 0);
};

let () = {
  let t = T('a', [T('f',[T('g',[])]), T('c',[]),
              T('b',[T('d',[]), T('e',[])])]);
  ipl(t) |> Js.log;
};
