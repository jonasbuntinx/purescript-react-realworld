# Real World PureScript React

[![RealWorld Frontend](https://camo.githubusercontent.com/b507ac8f2ec6427bbef518193567c4ec6060c780/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f7265616c776f726c642d66726f6e74656e642d2532333738333537382e737667)](http://realworld.io)
[![Maintainer: jonasbuntinx](https://img.shields.io/badge/maintainer-jonasbuntinx-teal.svg)](http://github.com/jonasbuntinx)

This repository is a [Real World](https://github.com/gothinkster/realworld) implementation of Conduit, a Medium clone, using [PureScript](https://www.purescript.org/) and [React](https://reactjs.org/).

## Installation

Clone the repository:

```sh
git clone https://github.com/jonasbuntinx/purescript-react-realworld
cd purescript-react-realworld
```

Install the JavaScript and PureScript dependencies:

```sh
yarn install
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

#### [React Basic](https://github.com/lumihq/purescript-react-basic)

An opinionated set of bindings to the React library, optimizing for the most basic use cases

#### [React Basic Hooks](https://github.com/spicydonuts/purescript-react-basic-hooks)

An implementation of React hooks on top of purescript-react-basic

#### [React Basic Hooks Store](https://github.com/robertdp/purescript-react-basic-hooks-store)

Provides useStore, an asynchronous, scheduled reducer.

#### [Routing](https://github.com/purescript-contrib/purescript-routing)

A clean, type-safe routing library for PureScript.

#### [Routing Duplex](https://github.com/natefaubion/purescript-routing-duplex)

Unified parsing and printing for routes in PureScript

#### [Apiary](https://github.com/robertdp/purescript-apiary)

For the creation of type-level specs that can be queried against automatically.

#### [Wire](https://github.com/robertdp/purescript-wire)

Event/State library for reactive state.

## Recognition

I was inspired by [Thomas Honeyman](https://github.com/thomashoneyman)'s [implementation](https://github.com/thomashoneyman/purescript-halogen-realworld) of the Real World spec using [Halogen](https://github.com/slamdata/purescript-halogen).
