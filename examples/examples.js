/* global exports */
"use strict";

// module Main
exports.querySelectorImpl = function(r, f, s) {
   return function() {
      var result = document.querySelector(s);
      return result ? f(result) : r;
   };
}

exports.setText = function(text) {
   return function(node) {
      return function() {
         node.textContent = text;
         return node;
      };
   };
}