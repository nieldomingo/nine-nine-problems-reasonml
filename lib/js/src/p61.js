// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';


function count_leaves(tree) {
  if (tree) {
    var left = tree[1];
    if (left || tree[2]) {
      return count_leaves(left) + count_leaves(tree[2]) | 0;
    } else {
      return 1;
    }
  } else {
    return 0;
  }
}

console.log(count_leaves(/* Node */[
          /* "x" */120,
          /* Node */[
            /* "x" */120,
            /* Node */[
              /* "x" */120,
              /* Empty */0,
              /* Empty */0
            ],
            /* Empty */0
          ],
          /* Node */[
            /* "x" */120,
            /* Empty */0,
            /* Node */[
              /* "x" */120,
              /* Empty */0,
              /* Empty */0
            ]
          ]
        ]));

exports.count_leaves = count_leaves;
/*  Not a pure module */
