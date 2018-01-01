include Graph_term;

let s_tree = (g) => {
  let destination_node = (nodes, new_edge) => {
    let (u, v) = new_edge;
    List.mem(u, nodes) ? v : u;
  };
  let neighbors = (nodes, s) => {
    List.filter(
      (edge) => {
        let (u, v) = edge;
        if ((u == s) || (v == s)) {
          let other = u == s ? v : u;
          !List.mem(other, nodes);
        } else {
          false;
        }
      },
      g.edges);
  };
  let recalc_candidate_edges = (candidate_edges, nodes, new_edge) => {
    let dest = destination_node(nodes, new_edge);
    (
      [dest, ...nodes],
      List.rev_append(
        List.filter(
          (edge) => {
            let (u, v) = edge;
            (u != dest) && (v != dest);
          },
          candidate_edges),
        neighbors(nodes, dest)));
  };
  let rec traverse = (candidate_edges, nodes, edges, trees) => {
    switch candidate_edges {
    | [] => [{Graph_term.nodes: nodes, edges: edges}, ...trees]
    | _ =>
        List.fold_left(
          (trees, edge) => {
            let (new_nodes, new_candidate_edges) = recalc_candidate_edges(
              candidate_edges, nodes, edge);
            let new_edges = [edge, ...edges];
            traverse(new_candidate_edges, new_nodes, new_edges, trees);
          },
          trees,
          candidate_edges
        )
    };
  };
  switch g.nodes {
  | [s, ..._] => traverse(neighbors([s], s), [s], [], [])
  | [] => []
  };
};

let is_tree = (g) => {
  List.length(s_tree(g)) == 1;
};

let is_connected = (g) => {
  List.exists(
    (t) => List.length(t.nodes) == List.length(g.nodes),
    s_tree(g));
};

let () = {
  Array.of_list(
    List.map(
      (tree) => Array.of_list(tree.edges),
      s_tree(Graph_term.another_example))) |> Js.log;
  is_tree(Graph_term.another_example) |> Js.log;
  is_connected(Graph_term.another_example) |> Js.log;
};
