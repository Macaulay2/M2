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
      {
        test: require.resolve('jquery'),
        loader: 'expose-loader',
        options: { exposes: ['$', 'jQuery'] },
      },
      {
        test: require.resolve('nouislider'),
        loader: 'expose-loader',
        options: {
          exposes: 'noUiSlider',
        },
      },
      {
        test: /\.css$/i,
        use: ['style-loader', 'css-loader'],
      },
      {
        test: /\.(woff2?|eot|ttf|otf)$/,
        type: 'asset/resource',
        generator: {
          filename: '../fonts/[name].[contenthash][ext]',
        },
      },
    ],
  },
};
