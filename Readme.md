# Wing
A modular framework for clojure applications

[![Build Status](https://img.shields.io/github/workflow/status/teknql/wing/CI.svg)](https://github.com/teknql/wing/actions)
[![Clojars Project](https://img.shields.io/clojars/v/wing/wing.svg)](https://clojars.org/wing/wing)
[![cljdoc badge](https://cljdoc.org/badge/wing/wing)](https://cljdoc.org/d/wing/wing/CURRENT)

## About and Status

This library is used internally at Teknql in many projects. It it includes multiple sub-libraries
that can be individually used, as well as a larger `wing/wing` project which includes all
sub-libraries for a larger, more robust and opinionated library.

This project has been used internally quite extensively and is pretty stable. Still, given the
somewhat internal nature, the library makes no stability guarantees as our best practices change
with the clojure ecosystem.

## Dependencies and Recommended Library Pairings

For the most part, wing tries to avoid using too many dependencies. Dependencies are almost
always broken out into sub-libraries to allow the modules of wing to stay focused. Still, wing
does have opinions on libraries for common needs.

### State Management

We recommend using Teknql's own [systemic](https://github.com/teknql/systemic) for state management.

### Validation and Coercion

We recommend using Metosin's wonderful [malli](https://github.com/metosin/malli).

### Time

We recommend using Juxt's outstanding [tick](https://github.com/juxt/tick).

## Sub-Libraries

As mentioned above, `wing/wing` will give you all of these, but if you want to use parts, here they
are.

### wing/core

[![Clojars Project](https://img.shields.io/clojars/v/wing/core.svg)](https://clojars.org/wing/core)

Extensions to the clojure standard library. Common functions that we frequently wish we had.

## wing/malli
[![Clojars Project](https://img.shields.io/clojars/v/wing/malli.svg)](https://clojars.org/wing/malli)

Extensions for working with Malli including a custom JSON transformer that encodes schemas to our
preferences.

## wing/repl
[![Clojars Project](https://img.shields.io/clojars/v/wing/repl.svg)](https://clojars.org/wing/repl)

Utilities for working in the Repl, including the ability to hot-load new libraries into a running
REPL without having to restart it.

## wing/test

[![Clojars Project](https://img.shields.io/clojars/v/wing/test.svg)](https://clojars.org/wing/test)

Utilities for testing including an `assert-match` macro.
