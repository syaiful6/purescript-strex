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

exports.emptyBuf = Buffer.from([])
