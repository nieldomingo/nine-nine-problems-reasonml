type graph_term('a) = {
  nodes: list('a),
  edges: list(('a, 'a))
};

let example_graph = {
  nodes: ['b', 'c', 'd', 'f', 'g', 'h', 'k'],
  edges: [('h', 'g'),  ('k', 'f'),  ('f', 'b'),  ('f', 'c'),  ('c', 'b')]};

let another_example = {
  nodes: ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'],
  edges: [('a', 'b'), ('a', 'd'), ('b', 'c'), ('b', 'e'),
    ('c', 'e'), ('d', 'e'), ('d', 'f'), ('d', 'g'),
    ('e', 'h'), ('f', 'g'), ('g', 'h')]
};
