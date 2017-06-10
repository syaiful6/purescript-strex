'use strict';

exports.newDataView = function (offset) {
  return function (len) {
    return function (buf) {
      return function () {
        return new DataView(buf, offset, len)
      }
    }
  }
}

exports.getImpl = function (type) {
  return function (ed) {
    return function (offset) {
      return function (dv) {
        return function () {
          return dv['get' + type](offset, ed)
        }
      }
    }
  }
}

exports.setImpl = function (value) {
  return function (type) {
    return function (ed) {
      return function (offset) {
        return function (dv) {
          return function () {
            dv['set' + type](offset, value, ed)
          }
        }
      }
    }
  }
}
