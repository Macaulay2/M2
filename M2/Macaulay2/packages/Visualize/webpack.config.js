const path = require('path');

module.exports = {
  mode: 'production',
  entry: {
    visCommon: './src/visCommon.js',
    visGraph2d: './src/visGraph2d.js',
    visDigraph2d: './src/visDigraph2d.js',
    visPoset: './src/visPoset.js',
    visSimplicialComplex2d: './src/visSimplicialComplex2d.js',
    visIdeal2d: './src/visIdeal2d.js',
    visIdeal3d: './src/visIdeal3d.js',
  },
  output: {
    path: path.resolve(__dirname, 'js'),
    filename: '[name].js',
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
