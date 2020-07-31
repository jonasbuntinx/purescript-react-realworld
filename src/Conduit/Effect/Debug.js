"use strict";

exports.log = function(msg) {
  return function(data) {
    return function() {
      console.log("%c[DEBUG] %c%s: %o", "color: blue;", "color: currentColor;", msg, data);
    };
  };
};

exports.table = function(msg) {
  return function(data) {
    return function() {
      console.log("%c[DEBUG] %c%s:", "color: blue;", "color: currentColor;", msg);
      console.table(data);
    };
  };
};
