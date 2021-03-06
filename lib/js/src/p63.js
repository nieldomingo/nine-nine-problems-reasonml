// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';


function skip(_l, _n) {
  while(true) {
    var n = _n;
    var l = _l;
    if (n === 1) {
      return l;
    } else if (l) {
      _n = n - 1 | 0;
      _l = l[1];
      continue ;
      
    } else {
      return /* [] */0;
    }
  };
}

function complete_binary_tree(elements) {
  var aux = function (elements, n) {
    if (elements) {
      var left_children = skip(elements, ((n << 1) - n | 0) + 1 | 0);
      var right_children = skip(elements, ((n << 1) - n | 0) + 2 | 0);
      return /* Node */[
              elements[0],
              aux(left_children, (n << 1)),
              aux(right_children, (n << 1) + 1 | 0)
            ];
    } else {
      return /* Empty */0;
    }
  };
  return aux(elements, 1);
}

console.log(complete_binary_tree(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* :: */[
                  5,
                  /* :: */[
                    6,
                    /* [] */0
                  ]
                ]
              ]
            ]
          ]
        ]));

exports.skip                 = skip;
exports.complete_binary_tree = complete_binary_tree;
/*  Not a pure module */
