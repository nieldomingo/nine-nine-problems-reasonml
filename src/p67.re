include Binary_tree;

let rec string_of_tree = (tree) => {
  switch tree {
  | Empty => ""
  | Node(v, Empty, Empty) => v
  | Node(v, left, right) => {
      v ++ "(" ++ string_of_tree(left) ++ "," ++ string_of_tree(right) ++ ")";
    }
  };
};

let tree_of_string = (encoded_tree) => {
  let rec extract_children = (pos) => {
    if (pos == String.length(encoded_tree)) {
      (Empty, Empty, pos);
    } else {
      switch encoded_tree.[pos] {
      | ')' | ',' => (Empty, Empty, pos)
      | '(' => {
          let (left, new_pos) = extract_node(pos + 1);
          assert(new_pos < String.length(encoded_tree) - 1);
          assert(encoded_tree.[new_pos] == ',');
          let (right, new_pos) = extract_node(new_pos + 1);
          (left, right, new_pos + 1);
        }
      | v => failwith("expected '(', ',', ')', got " ++ Char.escaped(v));
      };
    }
  }
  and extract_node = (pos) => {
    if (pos == String.length(encoded_tree)) {
      (Empty, pos);
    } else {
      switch encoded_tree.[pos] {
      | ')' | ',' => (Empty, pos)
      | v => {
          let (left, right, new_pos) = extract_children(pos + 1);
          (Node(Char.escaped(v), left, right), new_pos);
        }
      };
    }
  };
  let (tree, _) = extract_node(0);
  tree;
};

let () = {
  let leaf = (x) => Node (x, Empty, Empty);
  let example_layout_tree =
    Node("a", Node("b", leaf("d"), leaf("e")),
        Node("c", Empty, Node("f", leaf("g"), Empty)));
  string_of_tree(example_layout_tree) |> Js.log;
  tree_of_string(string_of_tree(example_layout_tree)) |> Js.log;
};
