/* global exports */
"use strict";

// module DyGraphs 
exports.unsafeNullToMaybeImpl = function(just, nothing, val) {
  return val === null ? nothing : just(val);
}
