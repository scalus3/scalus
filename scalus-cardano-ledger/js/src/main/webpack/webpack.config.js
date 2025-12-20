module.exports = require('./scalajs.webpack.config');
module.exports.output.libraryTarget = 'commonjs2';
module.exports.output.filename = 'scalus.js';
module.exports.externals = {
  fs: 'commonjs fs'
};
// Suppress warnings to fix scalajs-bundler stats parsing issue with Webpack 5
// See: https://github.com/scalacenter/scalajs-bundler/issues/423
module.exports.ignoreWarnings = [/.*/];
