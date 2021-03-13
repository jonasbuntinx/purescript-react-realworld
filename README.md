# Real World PureScript React

[![RealWorld Frontend](https://camo.githubusercontent.com/b507ac8f2ec6427bbef518193567c4ec6060c780/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f7265616c776f726c642d66726f6e74656e642d2532333738333537382e737667)](http://realworld.io)
![CI](https://github.com/jonasbuntinx/purescript-react-realworld/workflows/CI/badge.svg)
[![Netlify Status](https://api.netlify.com/api/v1/badges/f8b98224-3465-44f0-bd31-c4413a95953d/deploy-status)](https://app.netlify.com/sites/purescript-react-realworld/deploys)
[![Maintainer: jonasbuntinx](https://img.shields.io/badge/maintainer-jonasbuntinx-teal.svg)](http://github.com/jonasbuntinx)

This repository is a [Real World](https://github.com/gothinkster/realworld) implementation of Conduit, a Medium clone, using [PureScript](https://www.purescript.org/) and [React](https://reactjs.org/).

### [Demo](https://purescript-react-realworld.netlify.app/)

## Installation

Clone the repository:

```sh
git clone https://github.com/jonasbuntinx/purescript-react-realworld
cd purescript-react-realworld
```

Install the JavaScript and PureScript dependencies:

```sh
yarn
```

Build the project:

```sh
yarn build
```

You can bundle the JS for production:

```sh
yarn bundle
```

Or run a local development server (defaults to [port 1234](http://localhost:1234)):

```sh
yarn serve
```

## Noteworthy PureScript Libraries

#### [React Basic Hooks](https://github.com/spicydonuts/purescript-react-basic-hooks)

An implementation of React hooks on top of purescript-react-basic.

#### [React Halo](https://github.com/robertdp/purescript-react-halo)

A Halogen-inspired interface for React.

#### [Web Router](https://github.com/robertdp/purescript-web-router)

A basic web router with support for asynchronous routing logic.

#### [Routing Duplex](https://github.com/natefaubion/purescript-routing-duplex)

Unified parsing and printing for routes in PureScript.

## Recognition

I was inspired by [Thomas Honeyman](https://github.com/thomashoneyman)'s [implementation](https://github.com/thomashoneyman/purescript-halogen-realworld) of the Real World spec using [Halogen](https://github.com/slamdata/purescript-halogen).
