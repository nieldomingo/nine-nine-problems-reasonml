// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var List                          = require("bs-platform/lib/js/list.js");
var $$Array                       = require("bs-platform/lib/js/array.js");
var Caml_obj                      = require("bs-platform/lib/js/caml_obj.js");
var Graph_term$NinetyNineProblems = require("./graph_term.js");

function paths(g, s, d) {
  var neighbors = function (s, path) {
    return List.map((function (edge) {
                  var u = edge[0];
                  var match = Caml_obj.caml_equal(u, s);
                  if (match !== 0) {
                    return edge[1];
                  } else {
                    return u;
                  }
                }), List.filter((function (edge) {
                        var v = edge[1];
                        var u = edge[0];
                        if (Caml_obj.caml_equal(u, s) && !List.mem(v, path)) {
                          return /* true */1;
                        } else if (Caml_obj.caml_equal(v, s)) {
                          return 1 - List.mem(u, path);
                        } else {
                          return /* false */0;
                        }
                      }))(g[/* edges */1]));
  };
  var traverse = function (s, d, path, all_paths) {
    var new_path = /* :: */[
      s,
      path
    ];
    if (Caml_obj.caml_equal(s, d)) {
      return /* :: */[
              List.rev(new_path),
              all_paths
            ];
    } else {
      return List.fold_left((function (acc, neighbor) {
                    return traverse(neighbor, d, new_path, acc);
                  }), all_paths, neighbors(s, new_path));
    }
  };
  return traverse(s, d, /* [] */0, /* [] */0);
}

console.log($$Array.of_list(List.map($$Array.of_list, paths(Graph_term$NinetyNineProblems.example_graph, /* "f" */102, /* "b" */98))));

var example_graph = Graph_term$NinetyNineProblems.example_graph;

var another_example = Graph_term$NinetyNineProblems.another_example;

exports.example_graph   = example_graph;
exports.another_example = another_example;
exports.paths           = paths;
/*  Not a pure module */
