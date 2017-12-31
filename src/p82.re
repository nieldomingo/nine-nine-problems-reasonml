include Graph_term;

let cycles = (g, start) => {
  let valid_edge = (s, u, v, path) => {
    (u == s && (!List.mem(v, path) || v == start))
  };
  let neighbors = (s, path) =>
    List.map(
      (edge) => {
        let (u, v) = edge;
        u == s ? v : u;
      },
      List.filter(
        (edge) => {
          let (u, v) = edge;
          valid_edge(s, u, v, path) || valid_edge(s, v, u, path);
        },
        g.edges));
  let rec traverse = (s, path, all_paths) => {
    let new_path = [s, ...path];
    if (s == start && List.length(path) != 0) {
      [List.rev(new_path), ...all_paths];
    } else {
      List.fold_left(
        (acc, neighbor) => traverse(neighbor, new_path, acc),
        all_paths,
        neighbors(s, new_path));
    }
  };
  traverse(start, [], []);
};

let () = {
  Array.of_list(
    List.map(
      (path) => Array.of_list(path),
      cycles(Graph_term.example_graph, 'f'))) |> Js.log;
};
