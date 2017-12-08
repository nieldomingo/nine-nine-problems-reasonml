// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var List  = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");

function $$eval(_expr, env) {
  while(true) {
    var expr = _expr;
    switch (expr.tag | 0) {
      case 0 : 
          return List.assoc(expr[0], env);
      case 1 : 
          return 1 - $$eval(expr[0], env);
      case 2 : 
          if ($$eval(expr[0], env)) {
            _expr = expr[1];
            continue ;
            
          } else {
            return /* false */0;
          }
          break;
      case 3 : 
          if ($$eval(expr[0], env)) {
            return /* true */1;
          } else {
            _expr = expr[1];
            continue ;
            
          }
          break;
      
    }
  };
}

function table(vars, expr) {
  var create_input_table = function (_n, _input_table) {
    while(true) {
      var input_table = _input_table;
      var n = _n;
      if (n !== 1) {
        _input_table = List.append(List.map((function (vals) {
                    return /* :: */[
                            /* true */1,
                            vals
                          ];
                  }), input_table), List.map((function (vals) {
                    return /* :: */[
                            /* false */0,
                            vals
                          ];
                  }), input_table));
        _n = n - 1 | 0;
        continue ;
        
      } else {
        return input_table;
      }
    };
  };
  return List.map((function (vals) {
                var env = List.combine(vars, vals);
                return /* tuple */[
                        env,
                        $$eval(expr, List.combine(vars, vals))
                      ];
              }), create_input_table(List.length(vars), /* :: */[
                  /* :: */[
                    /* true */1,
                    /* [] */0
                  ],
                  /* :: */[
                    /* :: */[
                      /* false */0,
                      /* [] */0
                    ],
                    /* [] */0
                  ]
                ]));
}

console.log(table(/* :: */[
          "a",
          /* :: */[
            "b",
            /* [] */0
          ]
        ], /* And */Block.__(2, [
            /* Var */Block.__(0, ["a"]),
            /* Or */Block.__(3, [
                /* Var */Block.__(0, ["a"]),
                /* Var */Block.__(0, ["b"])
              ])
          ])));

exports.$$eval = $$eval;
exports.table  = table;
/*  Not a pure module */