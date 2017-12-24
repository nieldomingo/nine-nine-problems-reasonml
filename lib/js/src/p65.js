// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var Pervasives = require("bs-platform/lib/js/pervasives.js");

function layout_binary_tree_2(tree) {
  var depth = function (tree) {
    if (tree) {
      var left = tree[1];
      var exit = 0;
      if (left) {
        exit = 1;
      } else if (tree[2]) {
        exit = 1;
      } else {
        return 0;
      }
      if (exit === 1) {
        return 1 + Pervasives.max(depth(left), depth(tree[2])) | 0;
      }
      
    } else {
      return 0;
    }
  };
  var left_most_level = function (tree) {
    if (tree) {
      var left = tree[1];
      if (left) {
        return 1 + left_most_level(left) | 0;
      } else {
        return 0;
      }
    } else {
      return 0;
    }
  };
  var aux = function (tree, xpos, spacing, level) {
    if (tree) {
      return /* Node */[
              /* tuple */[
                tree[0],
                xpos,
                level
              ],
              aux(tree[1], xpos - spacing | 0, spacing / 2 | 0, level + 1 | 0),
              aux(tree[2], xpos + spacing | 0, spacing / 2 | 0, level + 1 | 0)
            ];
    } else {
      return /* Empty */0;
    }
  };
  var tree_depth = depth(tree);
  var offset = tree_depth - left_most_level(tree) | 0;
  return aux(tree, (1 << tree_depth) - offset | 0, (1 << (tree_depth - 1 | 0)), 1);
}

var example_layout_tree_001 = /* Node */[
  "k",
  /* Node */[
    "c",
    /* Node */[
      "a",
      /* Empty */0,
      /* Empty */0
    ],
    /* Node */[
      "e",
      /* Node */[
        "d",
        /* Empty */0,
        /* Empty */0
      ],
      /* Node */[
        "g",
        /* Empty */0,
        /* Empty */0
      ]
    ]
  ],
  /* Node */[
    "m",
    /* Empty */0,
    /* Empty */0
  ]
];

var example_layout_tree_002 = /* Node */[
  "u",
  /* Node */[
    "p",
    /* Empty */0,
    /* Node */[
      "q",
      /* Empty */0,
      /* Empty */0
    ]
  ],
  /* Empty */0
];

var example_layout_tree = /* Node */[
  "n",
  example_layout_tree_001,
  example_layout_tree_002
];

console.log(layout_binary_tree_2(example_layout_tree));

exports.layout_binary_tree_2 = layout_binary_tree_2;
/*  Not a pure module */