/**
 * XXX: ES6 generators are not quite performant.
 * This is quite naive indeed...
 */

'use strict'

import 'babel-polyfill'

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
class StringSlice {
  constructor(xs, opt_ix) {
    this.xs = xs
    this.ix = opt_ix || 0
  }

  head() {
    if (this.xs.length > this.ix) {
      return this.xs[this.ix]
    } else {
      return None
    }
  }

  tail() {
    if (this.xs.length > this.ix) {
      return new StringSlice(this.xs, this.ix + 1)
    } else {
      return None
    }
  }
}


// Monadic parsers

export function one(p) {
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

const named = (name, p) => fmap((a) => ({[name]: a}), p)

function foldlM(combine, init, ps) {
  ps.forEach((p) => {
    init = fmap(([a, b]) => combine(a, b), andThen(init, p))
  })
  return init
}

const combineNamed = (...ps) => foldlM((a, b) => Object.assign(a, b), pure({}), ps)

const sequentiallyNamed = (kw) => combineNamed(...Object.keys(kw).map((k) => named(k, kw[k])))

const letter = one((c) => ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
const digit = one((c) => '0' <= c && c <= '9')

const char = (c0) => one((c) => c == c0)

// *> :: m a -> m b -> m b
function ignoreFirst(p1, p2) {
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
  return ignoreFirst(p, pure(s))
}

// andThen :: m a -> m b -> m (a, b)
export function andThen(p1, p2) {
  return function* (xs) {
    for (let [a1, xs0] of p1(xs)) {
      for (let [a2, xs1] of p2(xs0)) {
        yield [[a1, a2], xs1]
      }
    }
  }
}

// (<|>) :: m a -> m a -> m a
export function orElse(p1, p2) {
  return function* (xs) {
    yield* p1(xs)
    yield* p2(xs)
  }
}

// pure :: a -> m a
export function pure(x) {
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

export function many(p) {
  return orElse(many1(p), pure([]))
}

// sepBy1 :: m a -> m b -> m [a]
export function sepBy1(p, sep) {
  return function* (xs) {
    for (let [a0, xs0] of p(xs)) {
      for (let [a1, xs1] of many1(ignoreFirst(sep, p))(xs0)) {
        yield [arrayCons(a0, a1), xs1]
      }
    }
  }
}

export function sepBy(p, sep) {
  return orElse(sepBy1(p, sep), pure([]))
}

function joinChars(cs) {
  return cs.join('')
}

// joinCharsM :: m [Char] -> m String
const joinCharsM = (ma) => fmap(joinChars, ma)

// parse :: m a -> String -> Maybe a
export function parse(p, xs) {
  for (let [a, _] of p(new StringSlice(xs))) {
    return a
  }
  return None
}

const pUrl = (() => {
  let pScheme = joinCharsM(many1(letter))
  let pChar = orElse(letter, orElse(digit, oneOf('.-_')))
  let pChars1 = joinCharsM(many1(pChar))
  let pChars = joinCharsM(many(pChar))
  let pPathSeg = ignoreFirst(char('/'), joinCharsM(many1(pChar)))
  let pQuery = andThen(pChars1, orElse(ignoreFirst(char('='), pChars1), pure(null)))
  let pQueries = sepBy(pQuery, char('&'))
  let pInt = fmap(Number, joinCharsM(many1(digit)))
  let pTagBodyOr = (tag, body, orValue) => orElse(ignoreFirst(string(tag), body), pure(orValue))

  // XXX: This relies on the fact that modern JS engines preserve the key
  // insertion order. combineNamed(named(..)..) is safer in this regard.
  return sequentiallyNamed({
    scheme: pScheme,
    hostname: pTagBodyOr('://', pChars),
    port: pTagBodyOr(':', pInt),
    pathSegs: many(pPathSeg),
    queries: pTagBodyOr('?', pQueries, []),
    hashQueries: pTagBodyOr('#', pQueries, []),
  })
})()


const pA = char('a')
const pB = char('b')
const pC = char('c')
const pAB = andThen(pA, pB)
const pAC = andThen(pA, pC)
console.log(parse(pUrl, 'http://www.google.com:80/foo/bar/baz/index.html?a=5&b=3&dothis#qq=666&dekai&ck'))
console.log(parse(pUrl, 'file:///foo/bar/baz/index.html?a=5&b=3&dothis#qq=666&dekai&ck'))
/*
$ node parsec-compiled.js
{ scheme: 'http',
  hostname: 'www.google.com',
  port: 80,
  pathSegs: [ 'foo', 'bar', 'baz', 'index.html' ],
  queries: [ [ 'a', '5' ], [ 'b', '3' ], [ 'dothis', null ] ],
  hashQueries: [ [ 'qq', '666' ], [ 'dekai', null ], [ 'ck', null ] ] }
 */

//console.log(parse(andThen(named('a', many1(pA)), named('b', many1(pB))), 'aabbb'))

