'use strict';

exports._newUint8Array = function (byteOffset, length, buffer) {
  return function () {
    return new Uint8Array(buffer, byteOffset, length);
  };
};

exports._newInt8Array = function (byteOffset, length, buffer) {
  return function () {
    return new Int8Array(buffer, byteOffset, length);
  };
};

exports._newUint8ClampedArray = function (byteOffset, length, buffer) {
  return function () {
    return new Uint8ClampedArray(buffer, byteOffset, length);
  };
};

exports._newInt16Array = function (byteOffset, length, buffer) {
  return function () {
    return new Int16Arra(buffer, byteOffset, length);
  };
};

exports._newUint16Array = function (byteOffset, length, buffer) {
  return function () {
    return new Uint16Array(buffer, byteOffset, length);
  };
};

exports._newInt32Array = function (byteOffset, length, buffer) {
  return function () {
    return new Int32Array(buffer, byteOffset, length);
  };
};

exports._newUint32Array = function (byteOffset, length, buffer) {
  return function () {
    return new Uint8ClampedArray(buffer, byteOffset, length);
  };
};

exports._newFloat32Array = function (byteOffset, length, buffer) {
  return function () {
    return new Float32Array(buffer, byteOffset, length);
  };
};

exports._newFloat64Array = function (byteOffset, length, buffer) {
  return function () {
    return new Float64Array(buffer, byteOffset, length);
  };
};

exports._copyWithin = function (target, start, end, ta) {
  return function () {
    return ta.copyWithin(target, start, end);
  };
};

exports._subarray = function (start, end, ta) {
  return function () {
    return ta.subarray(start, end);
  };
};

exports._slice = function (start, end, ta) {
  return function () {
    return ta.slice(start, end);
  };
};

exports._getAtOffset = function (nothing, just, offset, ta) {
  return function () {
    var octet = ta[offset];
    return octet == null ? nothing
                         : just(octet);
  };
};

exports._unsafeGetAtOffset = function (offset, ta) {
  return function () {
    var octet = ta[offset];
    if (octet != null) return octet
    throw new Error('_unsafeGetAtOffset return null, its meant you request is out of bounds')
  }
}

exports.setAtOffset = function (value) {
  return function (offset) {
    return function (buff) {
      return function() {
        buff[offset] = value;
        return {};
      };
    };
  };
};

exports.set = function (src) {
  return function (offset) {
    return function (dest) {
      return function () {
        return dest.set(src, offset)
      }
    }
  }
}

exports.sizeOf = function (ta) {
  return ta.BYTES_PER_ELEMENT;
}

exports._arrayViewCompare = function (a, b) {
  if (a === b) return 0
  var x = a.length
  var y = b.length
  for (var i = 0, len = Math.min(x, y); i < len; ++i) {
    if (a[i] !== b[i]) {
      x = a[i]
      y = b[i]
      break
    }
  }

  if (x < y) return -1
  if (y < x) return 1
  return 0
}

exports._findIndex = function (nothing, just, f, xs) {
  for (var i = 0, len = xs.length; i < len; i++) {
    if (f(xs[i])) return just(i)
  }
  return nothing
}

exports._findLastIndex = function (nothing, just, f, xs) {
  for (var i = xs.length - 1; i >= 0; i--) {
    if (f(xs[i])) return just(i);
  }
  return nothing
}

exports.printArrayView = function (xs) {
  return "(TypedArray " + xs.toString() + " )"
}

exports._nullPtr = new Uint8Array(0)
