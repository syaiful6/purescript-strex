'use strict';

var hexdigits = [48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102]

function intDec (x, offset, buf) {
  var c, ptr = offset, nextFree, tmp;
  if (x < 0) {
    buf[ptr++] = 45; // -
    offset++;
    tmp = x;
    x = (x / 10) | 0;
    bu8[ptr++] = hexdigits[x * 10 - tmp]
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
    c            = bu8[ptr];
    bu8[ptr--]   = bu8[offset];
    bu8[offset++] = c;
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

exports._intDecEff = function (x, offset, buf) {
  return function () {
    return intDec(x, buf, offset)
  }
}

exports._uintDecEff = function (x, offset, buf) {
  return function () {
    return uintDec(x, offset, buf);
  }
}
