/* eslint-disable no-unused-vars,no-console */
/* globals __dirname, require, process */

const CleanWebpackPlugin = require('clean-webpack-plugin');
const convert = require('koa-connect');
const ExtractTextPlugin = require("extract-text-webpack-plugin");
const history = require('connect-history-api-fallback');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const merge = require('webpack-merge');
const path = require('path');
const WriteFileWebpackPlugin = require('write-file-webpack-plugin');

const PRODUCTION = 'production';
const DEVELOPMENT = 'development';

const TARGET_ENV =
	path.basename(process.argv[1]) === 'webpack'
		? PRODUCTION
		: DEVELOPMENT;

const PATHS = {
	src: path.join(__dirname, 'src'),
	build: path.join(__dirname, 'public'),
	publicPath: '/'
};

console.log('Building for environment: ' + TARGET_ENV);

const parts = {
	entry: {
		resolve: {
			extensions: [".js", ".elm"]
		},
		entry: path.join(PATHS.src, "js", "index.js")
	},

	elm: {
		module: {
			noParse: /\.elm$/,
			rules: [
				{
					test: /\.elm$/,
					include: [path.join(PATHS.src, "elm")],
					exclude: [/elm-stuff/, /node_modules/],
					use: {
						loader: 'elm-webpack-loader',
						options: {
							cwd: __dirname,
							debug: TARGET_ENV === DEVELOPMENT,
							optimize: TARGET_ENV === PRODUCTION,
							verbose: true
						}
					}
				}
			]
		}
	},

	styles: TARGET_ENV === PRODUCTION
		? {
			module: {
				rules: [
					{
						test: /\.(css|scss)$/,
						use: ExtractTextPlugin.extract({
							fallback: "style-loader",
							use: [
								{
									loader: "css-loader"
								},
								{
									loader: "postcss-loader",
									options: {
										plugins: () => {
											require("autoprefixer");
										}
									}
								},
								{
									loader: "sass-loader"
								}
							]
						})
					}
				]
			},
			plugins: [
				new ExtractTextPlugin({
					filename: "[name].[chunkhash].css",
					allChunks: true
				})
			]
		}
		: {
			module: {
				rules: [
					{
						test: /\.(css|scss)$/,
						use: [
							{
								loader: "style-loader",
								options: {
									sourceMap: true
								}
							},
							{
								loader: "css-loader",
								options: {
									sourceMap: true
								}
							},
							{
								loader: "postcss-loader",
								options: {
									plugins: () => {
										require("autoprefixer");
									},
									sourceMap: true
								}
							},
							{
								loader: "sass-loader",
								options: {
									sourceMap: true
								}
							}
						]
					}
				]
			}
		},

	assets: {
	},

	html: {
		plugins: [
			new HtmlWebpackPlugin({
				template: path.join(PATHS.src, 'index.html')
			})
		]
	},

	build: {
		mode: 'production',
		output: {
			filename: '[name].[chunkhash].js',
			chunkFilename: '[chunkhash].js',
			path: PATHS.build,
			publicPath: PATHS.publicPath
		},
		optimization: {
			splitChunks: {
				cacheGroups: {
					commons: {
						test: /[\\/]node_modules[\\/]/,
						name: 'vendors',
						chunks: 'initial'
					}
				}
			}
		},
		plugins: [
			new CleanWebpackPlugin([PATHS.build], {
				verbose: true
			})
		]
	},

	watch: {
		mode: 'development',
		output: {
			filename: 'bundle.js',
			path: PATHS.build,
			publicPath: PATHS.publicPath
		},
		devtool: 'source-map',
		plugins: [
			new WriteFileWebpackPlugin()
		],
		serve: {
			hot: true,
			dev: {
				publicPath: PATHS.publicPath
			},
			add: (app, middleware, options) => {
				app.use(convert(history()));
			}
		}
	}
};


module.exports =
	merge([
		parts.entry,
		parts.elm,
		parts.assets,
		parts.styles,
		parts.html,
		TARGET_ENV === PRODUCTION ? parts.build : parts.watch
	]);
