/**
 * XXX: ES6 generators are not quite performant.
 * This is quite naive indeed...
 */

'use strict'

require('babel-polyfill')

// Just a marker.
class NoneType {
  toString() {
    return 'None'
  }
}

const None = new NoneType()

class UnitType {
  toString() {
    return '()'
  }
}
const Unit = new UnitType()

/**
 * Just a computation-efficient way to treat a string as a list.
 */
function StringSlice(xs, opt_ix) {
  this.xs = xs
  this.ix = opt_ix || 0
}

StringSlice.prototype.head = function() {
  if (this.xs.length > this.ix) {
    return this.xs[this.ix]
  } else {
    return None
  }
}

StringSlice.prototype.tail = function() {
  if (this.xs.length > this.ix) {
    return new StringSlice(this.xs, this.ix + 1)
  } else {
    return None
  }
}

// Monadic parsers

function one(p) {
  return function* (xs) {
    let x
    if ((x = xs.head()) !== None) {
      if (p(x)) {
        yield [x, xs.tail()]
      }
    }
  }
}

function fmap(f, ma) {
  return function* (xs) {
    for (let [a, xs0] of ma(xs)) {
      yield [f(a), xs0]
    }
  }
}

function oneOf(s) {
  return one((c) => s.indexOf(c) != -1)
}

const letter = one((c) => ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
const digit = one((c) => '0' <= c && c <= '9')

const char = (c0) => one((c) => c == c0)

// *> :: m a -> m b -> m b
function starRight(p1, p2) {
  return function* (xs) {
    for (let [_, xs0] of p1(xs)) {
      yield* p2(xs0)
    }
  }
}

// XXX: Not very efficient.
function string(s) {
  var p = pure([])
  for (var i = 0; i < s.length; ++i) {
      p = andThen(p, char(s[i]))
  }
  return starRight(p, pure(s))
}

// (>>=) :: m a -> (a -> m b) -> m b
function andThen(p1, p2) {
  return function* (xs) {
    for (let [a1, xs0] of p1(xs)) {
      for (let [a2, xs1] of p2(xs0)) {
        yield [[a1, a2], xs1]
      }
    }
  }
}

// (<|>) :: m a -> m a -> m a
function orElse(p1, p2) {
  return function* (xs) {
    yield* p1(xs)
    yield* p2(xs)
  }
}

// pure :: a -> m a
function pure(x) {
  return function* (xs) {
    yield [x, xs]
  }
}

// cons :: a -> Array a -> Array a
function arrayCons(x, xs) {
  let res = [x]
  res.push.apply(res, xs)
  return res
}

// many1 :: m a -> m [a]
function many1(p) {
  return function* (xs) {
    for (let [a0, xs0] of p(xs)) {
      for (let [a1s, xs1] of orElse(many1(p), pure([]))(xs0)) {
        yield [arrayCons(a0, a1s), xs1]
      }
    }
  }
}

function many(p) {
  return orElse(many1(p), pure([]))
}

// sepBy1 :: m a -> m b -> m [a]
function sepBy1(p, sep) {
  return function* (xs) {
    for (let [a0, xs0] of p(xs)) {
      for (let [a1, xs1] of many1(starRight(sep, p))(xs0)) {
        yield [arrayCons(a0, a1), xs1]
      }
    }
  }
}

function sepBy(p, sep) {
  return orElse(sepBy1(p, sep), pure([]))
}

function joinChars(cs) {
  return cs.join('')
}

// joinCharsM :: m [Char] -> m String
const joinCharsM = (ma) => fmap(joinChars, ma)

// parse :: m a -> String -> Maybe a
function parse(p, xs) {
  for (let [a, _] of p(new StringSlice(xs))) {
    return a
  }
  return None
}

const pUrl = function* (xs) {
  let pScheme = joinCharsM(many1(letter))
  let pChar = orElse(letter, orElse(digit, oneOf('.-_')))
  let pChars = joinCharsM(many1(pChar))
  let pPathSeg = starRight(char('/'), joinCharsM(many1(pChar)))
  let pQuery = andThen(pChars, starRight(char('='), pChars))
  let pQueries = sepBy(pQuery, char('&'))

  for (let [scheme, xs0] of pScheme(xs)) {
    for (let [hostname, xs1] of starRight(string('://'), pChars)(xs0)) {
      for (let [pathSegs, xs2] of many(pPathSeg)(xs1)) {
        for (let [queries, xs3] of orElse(starRight(char('?'), pQueries), pure([]))(xs2)) {
          yield [{ scheme, hostname, pathSegs, queries }, xs2]
        }
      }
    }
  }
}


const pA = char('a')
const pB = char('b')
const pC = char('c')
const pAB = andThen(pA, pB)
const pAC = andThen(pA, pC)
console.log(parse(pUrl, 'http://www.google.com/foo/bar/baz/index.html?a=5&b=3'))
/*
$ node parsec-compiled.js
{ scheme: 'http',
  hostname: 'www.google.com',
  pathSegs: [ 'foo', 'bar', 'baz', 'index.html' ],
  queries: [ [ 'a', '5' ], [ 'b', '3' ] ] }
 */

//console.log(parse(sepBy1(many1(pA), char(',')), 'aa,a,aaa'))

module.exports = {
  one,
  andThen,
  parse,
}

