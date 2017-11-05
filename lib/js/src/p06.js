// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj = require("bs-platform/lib/js/caml_obj.js");

function rev(l) {
  var _l = l;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var l$1 = _l;
    if (l$1) {
      _acc = /* :: */[
        l$1[0],
        acc
      ];
      _l = l$1[1];
      continue ;
      
    } else {
      return acc;
    }
  };
}

function is_palindrome(l) {
  var _l = l;
  var _r = rev(l);
  while(true) {
    var r = _r;
    var l$1 = _l;
    if (l$1) {
      if (r) {
        var match = Caml_obj.caml_equal(l$1[0], r[0]);
        if (match !== 0) {
          _r = r[1];
          _l = l$1[1];
          continue ;
          
        } else {
          return /* false */0;
        }
      } else {
        return /* false */0;
      }
    } else if (r) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  };
}

console.log(is_palindrome(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* :: */[
                  3,
                  /* :: */[
                    2,
                    /* :: */[
                      1,
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]));

console.log(is_palindrome(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]));

console.log(is_palindrome(/* [] */0));

exports.rev           = rev;
exports.is_palindrome = is_palindrome;
/*  Not a pure module */