// module Data.ByteString.Internal
exports.copyBuffer = function (srcStart, srcEnd, src, targStart, targ) {
  return function () {
    return src.copy(targ, targStart, srcStart, srcEnd);
  };
}

exports.bufferCompare = function (buf1, buf2) {
  return function () {
    return buf1.compare(buf2);
  };
}

exports.bufferSlice = function (start, end, buffer) {
  return function () {
    return buffer.slice(start, end)
  }
}

exports.setAtOffset = function (ofs, v, buffer) {
  return function () {
    buffer[ofs] = v;
  }
}

exports.intersperse = function (srcStart, src, targStart, target, n, c) {
  return function () {
    if (n > 0) {
      for (var i = 0; i < n - 1; i++) {
        target[targStart]     = src[srcStart + i];
        target[targStart + i] = c;
        targStart += 2;
      }
    }
  };
};

exports.emptyBuf = Buffer.from([])
