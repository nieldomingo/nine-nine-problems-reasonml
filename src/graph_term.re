type graph_term('a) = {
  nodes: list('a),
  edges: list(('a, 'a))
};

let example_graph = {
  nodes: ['b', 'c', 'd', 'f', 'g', 'h', 'k'],
  edges: [('h', 'g'),  ('k', 'f'),  ('f', 'b'),  ('f', 'c'),  ('c', 'b')]};
