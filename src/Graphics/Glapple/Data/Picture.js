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

exports.multiplyTransform = function(trans0) {
  return function(trans1) {
    const mat0 = new DOMMatrix([trans0.m11, trans0.m12, trans0.m21, trans0.m22, trans0.m31, trans0.m32])
    const mat1 = new DOMMatrix([trans1.m11, trans1.m12, trans1.m21, trans1.m22, trans1.m31, trans1.m32])
    const t = mat0.multiplySelf(mat1)
    return {m11: t.a, m12: t.b, m21: t.c, m22: t.d, m31: t.e, m32: t.f}
  }
}
