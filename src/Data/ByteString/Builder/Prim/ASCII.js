'use strict';

var atoa = function () {
  var hexdigits = [48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102]; // 0123456789abcdef

  function intDec (x, offset, buf) {
    var c, ptr = offset, nextFree, tmp;
    if (x < 0) {
      buf[ptr++] = 45; // -
      offset++;
      tmp = x;
      x = (x / 10) | 0;
      buf[ptr++] = hexdigits[x * 10 - tmp]
      if (x === 0) {
        return { buffer: buf, offset: ptr }
      } else {
        x = -x;
      }
    }

    do {
      tmp = x;
      x = (x / 10) | 0;
      buf[ptr++] = hexdigits[tmp - x * 10];
    } while (x);

    nextFree = ptr--;
    while(offset < ptr) {
      c            = buf[ptr];
      buf[ptr--]   = buf[offset];
      buf[offset++] = c;
    }
    return { buffer: buf, offset: nextFree }
  }

  function uintDec(x, offset, buf) {
    var c, ptr = offset, nextFree;
    var tmp;

    if (x < 0) x += 4294967296;

    do {
      tmp = x;
      x = (x / 10) | 0;
      buf[ptr++] = hexdigits[tmp - x * 10];
    } while(x);
    nextFree = ptr--;
    while(offset < ptr) {
      c            = buf[ptr];
      buf[ptr--]   = buf[offset];
      buf[offset++] = c;
    }
    return { buffer: buf, offset: nextFree }
  }

  // unsigned ints (32 bit words)
  function uintHex(x, offset, buf) {
    var c, ptr = offset, nextFree;
    do {
      buf[ptr++] = hexdigits[x & 0xf];
      x >>>= 4;
    } while(x);

    nextFree = ptr--;
    while(offset < ptr) {
        c            = buf[ptr];
        buf[ptr--]   = buf[offset];
        buf[offset++] = c;
    }
    return { buffer: buf, offset: nextFree }
  }

  return {
    intDec: intDec,
    uintDec: uintDec,
    uintHex: uintHex
  }
}()

exports._intDecEff = function (x, offset, buf) {
  return function () {
    return atoa.intDec(x, offset, buf)
  }
}

exports._uintDecEff = function (x, offset, buf) {
  return function () {
    return atoa.uintDec(x, offset, buf);
  }
}

exports._uintHexEff = function (x, offset, buf) {
  return function () {
    return atoa.uintHex(x, offset, buf)
  }
}
