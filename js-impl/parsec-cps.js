/**
 * CPS could very possibly be faster than ES6 generators.
 */

'use strict'

import objectAssign from 'object-assign'

// Utils

// Just a marker.
class NoneType {
  toString() {
    return 'None'
  }
}

const None = new NoneType()

const CpsBreak = {}

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

// cons :: a -> Array a -> Array a
function arrayCons(x, xs) {
  let res = [x]
  res.push.apply(res, xs)
  return res
}

function joinChars(cs) {
  return cs.join('')
}

// Parser constructors and eliminators.

function one(p) {
  return (xs, k) => {
    let x
    if ((x = xs.head()) !== None) {
      if (p(x)) {
        return k(x, xs.tail())
      }
    }
  }
}

// parse :: m a -> String -> Maybe a
export function parse(p, xs) {
  var res = None
  p(new StringSlice(xs), (a, _) => {
    res = a
    return CpsBreak
  })
  return res
}

// Typeclass instances.

// andThen :: m a -> m b -> m (a, b)
export function andThen(p1, p2) {
  return (xs, k) => {
    return p1(xs, (a1, xs) => {
      return p2(xs, (a2, xs) => {
        return k([a1, a2], xs)
      })
    })
  }
}

export function orElse(p1, p2) {
  return (xs, k) => {
    var res
    if ((res = p1(xs, k)) === CpsBreak) {
      return res
    }
    if ((res = p2(xs, k)) === CpsBreak) {
      return res
    }
  }
}

// pure :: a -> m a
export function pure(x) {
  return (xs, k) => k(x, xs)
}

export function fmap(f, ma) {
  return (xs, k) => {
    return ma(xs, (a, xs) => k(f(a), xs))
  }
}

// *> :: m a -> m b -> m b
function ignoreFirst(p1, p2) {
  return (xs, k) => {
    return p1(xs, (_, xs) => {
      return p2(xs, k)
    })
  }
}

function foldlM(combine, init, ps) {
  ps.forEach((p) => {
    init = fmap(([a, b]) => combine(a, b), andThen(init, p))
  })
  return init
}

// Common parser combinators.

// many1 :: m a -> m [a]
export function many1(p) {
  return (xs, k) => {
    return p(xs, (a0, xs) => {
      return orElse(many1(p), pure([]))(xs, (a1s, xs) => {
        return k(arrayCons(a0, a1s), xs)
      })
    })
  }
}

export function many(p) {
  return orElse(many1(p), pure([]))
}

export function sepBy1(p, sep) {
  return function(xs, k) {
    return p(xs, (a0, xs) => {
      return many1(ignoreFirst(sep, p))(xs, (a1, xs) => {
        return k(arrayCons(a0, a1), xs)
      })
    })
  }
}

export function sepBy(p, sep) {
  return orElse(sepBy1(p, sep), pure([]))
}

// Some specific sugary parser combinators.

const named = (name, p) => fmap((a) => ({[name]: a}), p)
const combineNamed = (...ps) => foldlM((a, b) => objectAssign(a, b), pure({}), ps)
export const sequentiallyNamed = (kw) => combineNamed(...Object.keys(kw).map((k) => named(k, kw[k])))

// Concrete parsers / misc helpers.

export const oneOf = (s) => one((c) => s.indexOf(c) != -1)
export const char = (c0) => one((c) => c == c0)
export const letter = one((c) => ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
export const digit = one((c) => '0' <= c && c <= '9')

function string(s) {
  var p = pure([])
  for (var i = 0; i < s.length; ++i) {
      p = andThen(p, char(s[i]))
  }
  return ignoreFirst(p, pure(s))
}

const joinCharsM = (ma) => fmap(joinChars, ma)

// Tests

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

//console.log(parse(many1(char('a')), ''))
console.log(parse(pUrl, 'http://www.google.com:80/foo/bar/baz/index.html?a=5&b=3&dothis#qq=666&dekai&ck'))
