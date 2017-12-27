// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var List                    = require("bs-platform/lib/js/list.js");
var Caml_obj                = require("bs-platform/lib/js/caml_obj.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function preorder(tree) {
  var aux = function (_tree, _accum) {
    while(true) {
      var accum = _accum;
      var tree = _tree;
      if (tree) {
        _accum = aux(tree[1], /* :: */[
              tree[0],
              accum
            ]);
        _tree = tree[2];
        continue ;
        
      } else {
        return accum;
      }
    };
  };
  return List.rev(aux(tree, /* [] */0));
}

function inorder(tree) {
  var aux = function (_tree, _accum) {
    while(true) {
      var accum = _accum;
      var tree = _tree;
      if (tree) {
        _accum = /* :: */[
          tree[0],
          aux(tree[1], accum)
        ];
        _tree = tree[2];
        continue ;
        
      } else {
        return accum;
      }
    };
  };
  return List.rev(aux(tree, /* [] */0));
}

function split(elements, root_val) {
  var _elements = elements;
  var _prefix = /* [] */0;
  while(true) {
    var prefix = _prefix;
    var elements$1 = _elements;
    if (elements$1) {
      var t = elements$1[1];
      var h = elements$1[0];
      if (Caml_obj.caml_equal(h, root_val)) {
        return /* tuple */[
                List.rev(prefix),
                t
              ];
      } else {
        _prefix = /* :: */[
          h,
          prefix
        ];
        _elements = t;
        continue ;
        
      }
    } else {
      return /* tuple */[
              List.rev(prefix),
              /* [] */0
            ];
    }
  };
}

function pre_in_tree(p, i) {
  var aux = function (p, i) {
    if (i) {
      if (p) {
        var h = p[0];
        var match = split(i, h);
        var match$1 = aux(p[1], match[0]);
        var match$2 = aux(match$1[1], match[1]);
        return /* tuple */[
                /* Node */[
                  h,
                  match$1[0],
                  match$2[0]
                ],
                match$2[1]
              ];
      } else {
        throw [
              Caml_builtin_exceptions.match_failure,
              [
                "/home/niel/workspace/reasonml/ninety-nine-problems/src/p68.re",
                39,
                26
              ]
            ];
      }
    } else {
      return /* tuple */[
              /* Empty */0,
              p
            ];
    }
  };
  return aux(p, i)[0];
}

var example_layout_tree_001 = /* Node */[
  "b",
  /* Node */[
    "d",
    /* Empty */0,
    /* Empty */0
  ],
  /* Node */[
    "e",
    /* Empty */0,
    /* Empty */0
  ]
];

var example_layout_tree_002 = /* Node */[
  "c",
  /* Empty */0,
  /* Node */[
    "f",
    /* Node */[
      "g",
      /* Empty */0,
      /* Empty */0
    ],
    /* Empty */0
  ]
];

var example_layout_tree = /* Node */[
  "a",
  example_layout_tree_001,
  example_layout_tree_002
];

var p = preorder(example_layout_tree);

var i = inorder(example_layout_tree);

var recreated_tree = pre_in_tree(p, i);

console.log(recreated_tree);

if (!Caml_obj.caml_equal(recreated_tree, example_layout_tree)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "p68.re",
          63,
          2
        ]
      ];
}

exports.preorder    = preorder;
exports.inorder     = inorder;
exports.split       = split;
exports.pre_in_tree = pre_in_tree;
/* p Not a pure module */