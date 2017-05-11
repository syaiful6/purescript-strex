'use strict';

exports.allocArrayBuffer = function (size) {
  return function () {
    return new ArrayBuffer(size);
  };
};

exports._transferArrayBuffer = function (newByteLength, old) {
  return function () {
    return ArrayBuffer.transfer(old, newByteLength);
  }
}

exports._slice = function (start, end, arrBuf) {
  return function () {
    return arrBuf.slice(start, end);
  };
};
