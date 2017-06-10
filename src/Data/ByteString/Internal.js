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
  return function () {
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
}
