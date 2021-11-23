"use strict";

exports.setGradientStrokeStyle = function(ctx) {
  return function(gradient) {
      return function() {
          ctx.strokeStyle = gradient;
      };
  };
};

exports.setPatternStrokeStyle = function(ctx) {
  return function(pattern) {
      return function() {
          ctx.strokeStyle = pattern;
      };
  };
};

exports.getTransform = function(ctx) {
  const t = ctx.getTransform()
  return function() {
    return {m11: t.a, m12: t.b, m21: t.c, m22: t.d, m31: t.e, m32: t.f}
  }
}