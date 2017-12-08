type binary_tree('a) =
  | Empty
  | Node('a, binary_tree('a), binary_tree('a));
