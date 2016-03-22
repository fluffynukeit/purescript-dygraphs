/* global exports */
"use strict";

// module DyGraphs 
exports.unsafeNullToMaybeImpl(just, nothing, val) {
  return val === null ? nothing : just(val);
}
