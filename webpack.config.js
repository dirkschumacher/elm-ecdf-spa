const path = require("path");
const webpack = require('webpack');

module.exports = {
  module: {
    rules: [{
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: "elm-webpack-loader",
          options: {
            debug: false
          }
        }
      },
      {
        test: /\.html$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: "file-loader?name=[name].[ext]"
      }
    ]
  },
  entry: {
    app: [
    "./index.js"
    ]
  },
  output: {
    path: path.resolve(__dirname + "/dist"),
    filename: "[name].js",
  },
  plugins: [
            new webpack.optimize.UglifyJsPlugin({
              minimize: true
            })
        ]
};
