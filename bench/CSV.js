var fs = require('fs')

exports._sliceArrayBuffer = function (ofs, len, abuf) {
  return function () {
    return Buffer.from(abuf, ofs, len);
  }
}

exports.writeFile = function (path, buf) {
  return function () {
    fs.writeFile(path, buf, {encoding: 'utf8'}, function (err) {
      if (err) throw err;
      console.log('The file has been saved!');
    });
  }
}
