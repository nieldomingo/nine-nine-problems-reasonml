// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");

function drop(l, n) {
  var aux = function (_l, _i, _accum) {
    while(true) {
      var accum = _accum;
      var i = _i;
      var l = _l;
      if (l) {
        var rest = l[1];
        if (i === n) {
          _i = 1;
          _l = rest;
          continue ;
          
        } else {
          _accum = /* :: */[
            l[0],
            accum
          ];
          _i = i + 1 | 0;
          _l = rest;
          continue ;
          
        }
      } else {
        return accum;
      }
    };
  };
  return List.rev(aux(l, 1, /* [] */0));
}

console.log(drop(/* :: */[
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
        ], 3));

exports.drop = drop;
/*  Not a pure module */