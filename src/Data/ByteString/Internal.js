'use strict';

exports._memcpy = function (dest, destOf, src, srcOf, n) {
  return function () {
    if (n < 1000) {
      for (var i = n - 1; i >= 0; i--) {
        dest[destOf + i] = src[srcOf + i]
      }
    } else {
      dest.set(src.subarray(srcOf, srcOf + n), destOf)
    }
  }
}

exports._memcpyArr = function (dest, ofs, src) {
  return function () {
    var len = src.length
    for (var i = 0; i < len; i++) {
      dest[ofs + i] = src[i]
    }
  }
}

exports._reverse = function (dest, destOf, src, srcOf, n) {
  return function () {
    if (n > 0) {
      for (var i = 0; i < n; i++) {
        dest[destOf + n - i - 1] = src[srcOf + i]
      }
    }
  }
}

exports._memmove = function (dest, destOf, src, srcOf, n) {
  return function () {
    var tmp = src.subarray(srcOf, srcOf + n)
    if (n > 0) {
      for(var i = 0; i < n; i++) {
        dest[destOf + i] = tmp[i]
      }
    }
  }
}

exports._memcmp = function (av, ao, bv, bo, n) {
  return function () {
    var a, b, c
    for (var i = 0; i < n; i++) {
      a = av[ao + i]
      b = bv[bo + i]
      c = a - b
      if(c !== 0) return c
    }
    return 0
  }
}

exports._memchr = function (av, ofs, c, n) {
  return function () {
    for (var i = 0; i < n; i++) {
      if (av[ofs + i] === c) {
        return ofs + i
      }
    }
    return -1
  }
}

exports._memset = function (av, ofs, chr, n) {
  return function () {
    var end = ofs + n;
    for (var i = 0; i < end; i++) {
      av[i] = chr
    }
  }
}

exports._fromArray = function (xs) {
  return function () {
    return Uint8Array.from(xs)
  }
}

exports._toArray = function (av, offset, n) {
  return function () {
    var xs = [];
    for (var i = 0; i < n; i++) {
      xs.push(av[offset + i])
    }
    return xs
  }
}

exports._intersperse = function (target, targStart, src, srcStart, c) {
  return function () {
    var ts = targStart, srcs = srcStart, len = src.length - srcStart;
    for (var i = 0; i < len; i++) {
      target[ts]     = src[srcs + i];
      target[ts + 1] = c;
      ts += 2;
    }
  };
};

exports._foldl = function (f, z, ofset, buf) {
  var r = z, len = buf.length - ofset;
  for (var i = ofset; i < len; ++i) {
    r = f(r)(buf[i]);
  }
  return r;
};

exports._foldr = function (f, z, ofset, buf) {
  var r = z, len = buf.length - ofset;
  for (var i = len - 1; i >= len; --i) {
    r = f(buf[i])(r)
  }
  return r
};

exports.assert = function (bool) {
  return function (a) {
    if (bool) return a
    throw new Error("Unsafe operation fail! Precondition assertion fail")
  };
};

exports._findSubstring = function (pattern, patOf, M, target, targOf, N) {
  var p = 0
  var t = 0
  var h = 1

  h = Math.pow(256, M - 1) % 101

  for (var i = 0; i < M ; i++ ) {
    p = (256 * p + pattern[patOf + i]) % 101
    t = (256 * t + target[targOf + i]) % 101
  }

  for (var i = 0; i <= N - M; i++) {
    if (p === t) {
      for (var j = 0; j < M ; j++) {
        if (target[targOf + i + j] !== pattern[patOf + j]) break
      }

      if (j === M) return i
    }

    if (i < N - M ) {
      t = (256 * (t - target[targOf + i] * h) + target[i + targOf + M]) % 101;
      t = (t < 0 ) ? (t + 101) : t
    }
  }
  return -1
}

exports.allocArrayBuffer = function (size) {
  return function () {
    return new ArrayBuffer(size)
  }
}

exports._newUint8Array = function (byteOffset, length, buffer) {
  return function () {
    return new Uint8Array(buffer, byteOffset, length);
  };
};

exports._subarray = function (start, end, ta) {
  return function () {
    return ta.subarray(start, end);
  };
};

exports._findLastIndex = function (nothing, just, f, xs) {
  for (var i = xs.length - 1; i >= 0; i--) {
    if (f(xs[i])) return just(i);
  }
  return nothing
}

exports._arrayViewLen = function (av) {
  return function () {
    return av.length
  }
}

exports._printArrayView = function (xs) {
  return "(TypedArray " + xs.toString() + " )"
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

exports._setAtOffset = function (value, offset, buff) {
  return function() {
    buff[offset] = value
  }
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

exports._nullPtr = new Uint8Array(0)
