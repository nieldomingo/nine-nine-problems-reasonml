// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");

function prefix(l, n) {
  var _l = l;
  var _n = n;
  var _accum = /* [] */0;
  while(true) {
    var accum = _accum;
    var n$1 = _n;
    var l$1 = _l;
    var exit = 0;
    if (n$1 !== 1) {
      exit = 1;
    } else if (l$1) {
      return List.rev(/* :: */[
                  l$1[0],
                  accum
                ]);
    } else {
      exit = 1;
    }
    if (exit === 1) {
      if (l$1) {
        _accum = /* :: */[
          l$1[0],
          accum
        ];
        _n = n$1 - 1 | 0;
        _l = l$1[1];
        continue ;
        
      } else {
        return List.rev(accum);
      }
    }
    
  };
}

function skip(_l, _n) {
  while(true) {
    var n = _n;
    var l = _l;
    if (l) {
      if (n !== 0) {
        _n = n - 1 | 0;
        _l = l[1];
        continue ;
        
      } else {
        return l;
      }
    } else {
      return /* [] */0;
    }
  };
}

function slice(l, j, k) {
  return prefix(skip(l, j), (k - j | 0) + 1 | 0);
}

console.log(slice(/* :: */[
          "a",
          /* :: */[
            "b",
            /* :: */[
              "c",
              /* :: */[
                "d",
                /* :: */[
                  "e",
                  /* :: */[
                    "f",
                    /* :: */[
                      "g",
                      /* :: */[
                        "h",
                        /* :: */[
                          "i",
                          /* :: */[
                            "j",
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ], 2, 6));

exports.prefix = prefix;
exports.skip   = skip;
exports.slice  = slice;
/*  Not a pure module */
