const path = require('path');

module.exports = {
  mode: 'production',
  entry: './index.js',
  output: {
    path: path.resolve(__dirname, ''),
    filename: 'prism.js',
  },
  module: {
    rules: [
      {
	test: /\.css$/i,
	use: ['style-loader', 'css-loader']
      }
    ]
  }
};
