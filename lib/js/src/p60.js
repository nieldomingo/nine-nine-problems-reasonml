// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var Arg  = require("bs-platform/lib/js/arg.js");
var List = require("bs-platform/lib/js/list.js");

function gen_trees(left_sub_trees, right_sub_trees) {
  return List.fold_left((function (accum, left_sub_tree) {
                return List.fold_left((function (accum, right_sub_tree) {
                              return /* :: */[
                                      /* Node */[
                                        /* "x" */120,
                                        left_sub_tree,
                                        right_sub_tree
                                      ],
                                      accum
                                    ];
                            }), accum, right_sub_trees);
              }), /* [] */0, left_sub_trees);
}

function hbal_tree(n) {
  if (n !== 0) {
    if (n !== 1) {
      if (n < 0) {
        throw [
              Arg.Bad,
              "Invalid argument value."
            ];
      } else {
        var n$1 = n - 1 | 0;
        return List.flatten(/* :: */[
                    gen_trees(hbal_tree(n$1), hbal_tree(n$1)),
                    /* :: */[
                      gen_trees(hbal_tree(n$1), hbal_tree(n$1 - 1 | 0)),
                      /* :: */[
                        gen_trees(hbal_tree(n$1 - 1 | 0), hbal_tree(n$1)),
                        /* [] */0
                      ]
                    ]
                  ]);
      }
    } else {
      return /* :: */[
              /* Node */[
                /* "x" */120,
                /* Empty */0,
                /* Empty */0
              ],
              /* [] */0
            ];
    }
  } else {
    return /* :: */[
            /* Empty */0,
            /* [] */0
          ];
  }
}

console.log(hbal_tree(0));

console.log(hbal_tree(1));

console.log(List.length(hbal_tree(3)));

exports.gen_trees = gen_trees;
exports.hbal_tree = hbal_tree;
/*  Not a pure module */
