const path = require('path');

module.exports = {
  mode: 'production',
  output: {
    path: path.resolve(__dirname, 'js'),
    filename: 'visCommon.js',
  },
  module: {
    rules: [
      {
        test: require.resolve('clipboard'),
        loader: 'expose-loader',
        options: {
          exposes: 'ClipboardJS',
        },
      },
   ],
  },
};
