"use strict"

exports.requestAnimationFramePromise = () => {
  return new Promise((resolve) => {
    window.requestAnimationFrame(resolve)
  })
}
