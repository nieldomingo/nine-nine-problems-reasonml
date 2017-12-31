include Graph_term;

let paths = (g, s, d) => {
  let neighbors = (s, path) =>
    List.map(
      (edge) => {
        let (u, v) = edge;
        u == s ? v : u;
      },
      List.filter(
        (edge) => {
          let (u, v) = edge;
          (u == s && !List.mem(v, path)) || (v == s && !List.mem(u, path));
        },
        g.edges));
  let rec traverse = (s, d, path, all_paths) => {
    let new_path = [s, ...path];
    if (s == d) {
      [List.rev(new_path), ...all_paths];
    } else {
      List.fold_left(
        (acc, neighbor) => traverse(neighbor, d, new_path, acc),
        all_paths,
        neighbors(s, new_path));
    }
  };
  traverse(s, d, [], []);
};

let () = {
  Array.of_list(
    List.map(
      (path) => Array.of_list(path),
      paths(Graph_term.example_graph, 'f', 'b'))) |> Js.log;
};
