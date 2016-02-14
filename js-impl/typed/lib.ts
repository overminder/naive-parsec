abstract class Maybe<A> {
  isJust(): boolean {
    return false;
  }

  isNothing(): boolean {
    return false;
  }

  unwrap(): A {
    throw new Error("unwrap: isNothing");
  }

  abstract unwrapOr(a: A): A;
}

class Nothing<A> extends Maybe<A> {
  isNothing(): boolean {
    return true;
  }

  unwrapOr(a: A): A {
    return a;
  }
}

class Just<A> extends Maybe<A> {
  a: A;

  constructor(a: A) {
    super();
    this.a = a;
  }

  isJust(): boolean {
    return true;
  }

  unwrapOr(_: A): A {
    return this.a;
  }

  unwrap(): A {
    return this.a;
  }
}

export interface Pair<A, B> {
  fst: A;
  snd: B;
}

namespace Pair {
  export function of<A, B>(fst: A, snd: B) {
    return { fst, snd };
  }

  export function uncurry<A, B, C>(f: (a: A, b: B) => C): (p: Pair<A, B>) => C {
    return (p: Pair<A, B>): C => f(p.fst, p.snd);
  }
}

class StringSlice {
  xs: string;
  ix: number;

  constructor(xs: string, opt_ix?: number) {
    this.xs = xs;
    this.ix = opt_ix || 0;
  }

  head(): Maybe<string> {
    if (this.xs.length > this.ix) {
      return new Just(this.xs[this.ix]);
    } else {
      return new Nothing<string>();
    }
  }

  tail(): Maybe<StringSlice> {
    if (this.xs.length > this.ix) {
      return new Just(new StringSlice(this.xs, this.ix + 1));
    } else {
      return new Nothing<StringSlice>();
    }
  }
}

type Predicate<A> = (a: A) => boolean;

class CpsBreak {}
const cpsBreak = new CpsBreak();

type Parser<A> = Parser.Parser<A>;

export function parse<A>(p: Parser<A>, xs: string): A {
  let res: A;
  if (cpsBreak !== p(new StringSlice(xs), (a, _) => {
    res = a;
    return cpsBreak;
  })) {
    throw new Error("Nothing parsed.");
  }
  return res;
}

namespace Parser {
  // To my surprise TS has the universal quantifier!
  export type Parser<A> = <R>(xs: StringSlice, k: ParserCont<A, R>) => R;
  type ParserCont<A, R> = (a: A, xs: StringSlice) => R;

  export function one(p: Predicate<string>): Parser<string> {
    return <R>(xs: StringSlice, k: (a: string, xs: StringSlice) => R): R => {
      let x: Maybe<string>;
      if ((x = xs.head()).isJust()) {
        if (p(x.unwrap())) {
          return k(x.unwrap(), xs.tail().unwrap());
        }
      }
    };
  }

  export function andThen<A, B>(pa: Parser<A>, pb: Parser<B>): Parser<Pair<A, B>> {
    return <R>(xs: StringSlice, k: ParserCont<Pair<A, B>, R>) => {
      return pa(xs, (a: A, xs: StringSlice) => {
        return pb(xs, (b: B, xs: StringSlice) => {
          return k(Pair.of(a, b), xs);
        });
      });
    };
  }

  export function orElse<A>(p1: Parser<A>, p2: Parser<A>): Parser<A> {
    return <R>(xs: StringSlice, k: ParserCont<A, R>) => {
      let res: CpsBreak | R;
      if ((res = p1(xs, k)) === cpsBreak) {
        return res;
      }
      if ((res = p2(xs, k)) === cpsBreak) {
        return res;
      }
    };
  }

  export function pure<A>(x: A): Parser<A> {
    return <R>(xs: StringSlice, k: ParserCont<A, R>) => k(x, xs);
  }

  export function fmap<A, B>(f: (a: A) => B, ma: Parser<A>): Parser<B> {
    return <R>(xs: StringSlice, k: ParserCont<B, R>) => {
      return ma(xs, (a: A, xs: StringSlice) => k(f(a), xs));
    };
  }

  export function ignoreFirst<A, B>(pa: Parser<A>, pb: Parser<B>): Parser<B> {
    return <R>(xs: StringSlice, k: ParserCont<B, R>) => {
      return pa(xs, (_, xs) => {
        return pb(xs, k);
      });
    };
  }

  export function foldlM<A, B>(combine: (a: A, b: B) => B, init: Parser<B>, as: Parser<A>[]): Parser<B> {
    const combineP = Pair.uncurry(combine);
    function combineM(a: Parser<A>, b: Parser<B>): Parser<B> {
      return fmap<Pair<A, B>, B>(combineP, andThen(a, b));
    }

    return foldl<Parser<A>, Parser<B>>(combineM, init, as);
  }

  export function many1<A>(pa: Parser<A>): Parser<A[]> {
    return <R>(xs: StringSlice, k: ParserCont<A[], R>) => {
      return pa(xs, (a0, xs) => {
        return orElse(many1(pa), pure(Array.of<A>()))(xs, (a1s, xs) => {
          return k(arrayCons(a0, a1s), xs);
        });
      });
    };
  }
}

function arrayCons<A>(x: A, xs: A[]): A[] {
  let res = [x];
  res.push.apply(res, xs);
  return res;
}

export function foldl<A, B>(combine: (a: A, b: B) => B, init: B, as: A[]): B {
    as.forEach((a) => {
      init = combine(a, init);
    });
    return init;
}

export function test() {
  function is<A>(x: A): Predicate<A> {
    return (y: A) => x === y;
  }

  let a = Parser.one(is("a"));
  let b = Parser.one(is("b"));
  let ab = Parser.andThen(a, b);
  let aob = Parser.orElse(a, b);

  console.log(parse(Parser.many1(aob), "aabbb"));

}
