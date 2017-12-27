include Mult_tree;

let string_of_tree = (tree) => {
  let rec aux = (tree, accum) => {
    switch tree {
    | T(c, []) => [c, ...accum]
    | T(c, children) =>
        List.fold_left(
          (accum, child) => ['^', ...aux(child, accum)],
          [c, ...accum],
          children)
    };
  };
  String.concat(
    "",
    List.map(
      (c) => Char.escaped(c),
      List.rev(aux(tree, []))
    )) ++ "^";
};

let tree_of_string = (encoded_tree) => {
  let rec aux = (pos, siblings) => {
    if (encoded_tree.[pos] == '^') {
      (pos + 1, siblings);
    } else {
      let (new_pos, children) = aux(pos + 1, []);
      let rev_children = List.rev(children);
      if (new_pos <  String.length(encoded_tree)) {
        aux(
          new_pos,
          [T(encoded_tree.[pos], rev_children), ...siblings]);
      } else {
        (
          new_pos,
          [T(encoded_tree.[pos], rev_children), ...siblings]);
      }
    }
  };
  switch (aux(0, [])) {
  | (_, []) => failwith("invalid state")
  | (_, [tree, ..._]) => tree
  };
};

let () = {
  let t = T('a', [T('f',[T('g',[])]), T('c',[]),
              T('b',[T('d',[]), T('e',[])])]);
  let encoded_tree = string_of_tree(t);
  encoded_tree |> Js.log;
  let rec_t = tree_of_string(encoded_tree);
  assert(rec_t == t);
  string_of_tree(rec_t) |> Js.log;
};
