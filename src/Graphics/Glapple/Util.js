"use strict";

exports.createCanvasElement = () => document.createElement("canvas");

exports.multiplyTransform = (trans0) => (trans1) => {
  const mat0 = new DOMMatrix([
    trans0.m11,
    trans0.m12,
    trans0.m21,
    trans0.m22,
    trans0.m31,
    trans0.m32,
  ]);
  const mat1 = new DOMMatrix([
    trans1.m11,
    trans1.m12,
    trans1.m21,
    trans1.m22,
    trans1.m31,
    trans1.m32,
  ]);
  const t = mat0.multiplySelf(mat1);
  return { m11: t.a, m12: t.b, m21: t.c, m22: t.d, m31: t.e, m32: t.f };
};

exports.inverseTransform = (trans) => {
  const mat = new DOMMatrix([
    trans.m11,
    trans.m12,
    trans.m21,
    trans.m22,
    trans.m31,
    trans.m32,
  ]);
  const t = mat.inverse();
  return { m11: t.a, m12: t.b, m21: t.c, m22: t.d, m31: t.e, m32: t.f };
};

exports.transform =
  (trans) =>
  (translate) => {
    const point = new DOMPoint(translate.x, translate.y);
    const mat = new DOMMatrix([
      trans.m11,
      trans.m12,
      trans.m21,
      trans.m22,
      trans.m31,
      trans.m32,
    ]);
    const res = point.matrixTransform(mat);
    return { x: res.x, y: res.y };
  };
