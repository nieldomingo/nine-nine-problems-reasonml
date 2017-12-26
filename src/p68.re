include Binary_tree;

let preorder = (tree) => {
  let rec aux = (tree, accum) =>
    switch tree {
    | Empty => accum
    | Node(v, left, right) =>
      aux(right, aux(left, [v, ...accum]))
    };
  List.rev(aux(tree, []));
};

let inorder = (tree) => {
  let rec aux = (tree, accum) =>
    switch tree {
    | Empty => accum
    | Node(v, left, right) =>
      aux(right, [v, ...aux(left, accum)])
    };
  List.rev(aux(tree, []));
};

let split = (elements, root_val) => {
  let rec aux = (elements, prefix) => {
    switch elements {
    | [] => (List.rev(prefix), [])
    | [h, ...t] =>
      if (h == root_val) {
        (List.rev(prefix), t);
      } else {
        aux(t, [h, ...prefix]);
      }
    };
  };
  aux(elements, []);
};

let pre_in_tree = (p, i) => {
  let rec aux = (p, i) => {
    switch (p, i) {
    | (_, []) => (Empty, p)
    | ([h, ...t], i) => {
        let (left_i, right_i) = split(i, h);
        let (left_tree, new_p) = aux(t, left_i);
        let (right_tree, new_p) = aux(new_p, right_i);
        (Node(h, left_tree, right_tree), new_p);
      }
    };
  };
  let (tree, _) = aux(p, i);
  tree;
};

let () = {
  let leaf = (x) => Node (x, Empty, Empty);
  let example_layout_tree =
    Node("a", Node("b", leaf("d"), leaf("e")),
        Node("c", Empty, Node("f", leaf("g"), Empty)));
  let p = preorder(example_layout_tree);
  let i = inorder(example_layout_tree);
  let recreated_tree = pre_in_tree(p, i);
  recreated_tree |> Js.log;
  assert(recreated_tree == example_layout_tree);
};
