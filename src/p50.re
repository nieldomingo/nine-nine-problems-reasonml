type binary_tree('a) =
  | Leaf('a)
  | Node(binary_tree('a), binary_tree('a));

let push = (heap, e) => {
  List.sort(
    (a, b) => {
      let (w1, _) = a;
      let (w2, _) = b;
      w1 - w2;
    },
    [e, ...heap]
  );
};

let pop = (heap) =>
  switch heap {
  | [] => (None, [])
  | [h, ...t] => (Some(h), t)
  };

let form_tree = (fs) => {
  let heap = List.fold_left(
    (heap, e) => {
      let (c, w) = e;
      push(heap, (w, Leaf(c)));
    },
    [],
    fs
  );

  let rec aux = (heap) =>
    switch (pop(heap)) {
    | (None, _) => failwith("Something went wrong.")
    | (Some(_), []) => heap
    | (Some((w1, subtree1)), new_heap) =>
      switch (pop(new_heap)) {
      | (None, _) => failwith("Something went wrong.")
      | (Some((w2, subtree2)), new_new_heap) =>
          aux(push(new_new_heap, (w1 + w2, Node(subtree1, subtree2))))
      }
    };

  switch (pop(aux(heap))) {
  | (None, _) => failwith("Something went wrong.")
  | (Some(e), _) => {
      let (_, tree) = e;
      tree;
    }
  };
};

let huffman = (fs) => {
  let rec traverse = (tree, prefix, accum) => {
    switch tree {
    | Node(left, right) =>
        traverse(
          right,
          prefix ++ "1",
          traverse(left, prefix ++ "0", accum)
        )
    | Leaf(c) => [(c, prefix), ...accum]
    };
  };
  let tree = form_tree(fs);
  traverse(tree, "", []);
};

let () = {
  let fs = [("a", 45), ("b", 13), ("c", 12), ("d", 16),
     ("e", 9), ("f", 5)];
  huffman(fs) |> Js.log;
  huffman([("a", 10), ("b", 15), ("c", 30), ("d", 16), ("e", 29)]) |> Js.log;
};
