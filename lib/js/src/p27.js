// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var List                    = require("bs-platform/lib/js/list.js");
var Pervasives              = require("bs-platform/lib/js/pervasives.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function extract(n, l) {
  if (List.length(l) === n) {
    return /* :: */[
            /* tuple */[
              l,
              /* [] */0
            ],
            /* [] */0
          ];
  } else if (n) {
    if (l) {
      var t = l[1];
      var h = l[0];
      return Pervasives.$at(List.map((function (e) {
                        return /* tuple */[
                                /* :: */[
                                  h,
                                  e[0]
                                ],
                                e[1]
                              ];
                      }), extract(n - 1 | 0, t)), List.map((function (e) {
                        return /* tuple */[
                                e[0],
                                /* :: */[
                                  h,
                                  e[1]
                                ]
                              ];
                      }), extract(n, t)));
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  } else {
    return /* :: */[
            /* tuple */[
              /* [] */0,
              l
            ],
            /* [] */0
          ];
  }
}

function group(l, g) {
  var aux = function (l, g) {
    if (g) {
      var h = g[0];
      return List.flatten(List.map((function (e) {
                        var cs = e[0];
                        return List.map((function (e2) {
                                      return /* tuple */[
                                              /* :: */[
                                                e2[0],
                                                cs
                                              ],
                                              e2[1]
                                            ];
                                    }), extract(h, e[1]));
                      }), aux(l, g[1])));
    } else {
      return /* :: */[
              /* tuple */[
                /* [] */0,
                l
              ],
              /* [] */0
            ];
    }
  };
  return List.map((function (e) {
                return e[0];
              }), aux(l, g));
}

console.log(group(/* :: */[
          "a",
          /* :: */[
            "b",
            /* :: */[
              "c",
              /* :: */[
                "d",
                /* [] */0
              ]
            ]
          ]
        ], /* :: */[
          1,
          /* :: */[
            1,
            /* [] */0
          ]
        ]));

exports.extract = extract;
exports.group   = group;
/*  Not a pure module */
