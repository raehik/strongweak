[lib-refined-hackage]: https://hackage.haskell.org/package/refined
[lib-barbies-hackage]: https://hackage.haskell.org/package/barbies

# strongweak
Purely convert between pairs of "weak" and "strong"/"validated" types, with
extensive failure reporting and powerful generic derivers. Alexis King's [Parse,
don't validate][parse-dont-validate] pattern as a library.

## What? Why?
[refined-blog]: http://nikita-volkov.github.io/refined/
[refined-hackage]: https://hackage.haskell.org/package/refined

Haskell is a wonderful language for accurate data modelling. Algebraic data
types (and GADTs as a fancy extension) enable defining highly restricted types
which prevent even *representing* invalid or unwanted values. Great! And for the
common case where you want to assert some predicate on a value but not change it
(i.e. validate), we have the powerful [refined][refined-blog] library to reflect
the existence of an asserted predicate in types. Fantastic!

Sadly I'm often grounded by "Reality", who insists that we don't use these
features everywhere because manipulating more complex types often means more
busywork on the term level. So I resort to less accurate data models, or
validating somewhat arbitrarily without assistance from the type system. I can
often feel Alexis King looking disapprovingly at me.

What if we defined two separate representations for a given model?

  * A **strong** representation, where no invalid values are permitted.
    (Promise.)
  * A **weak** representation, which doesn't necessarily enforce all the
    invariants that the strong representation does, but is easier to manipulate.

This way, we can use strong representations wherever possible e.g. passing
between subsystems, and shift to the weak representation for intensive
manipulation (and then back to strong at the end). Potential wins for
simplicity, brevity and performance, albeit for some conversion overhead.

Let's formalize the above as a pair of types `S` and `W`.

  * Given a `strong :: S`, we can always turn it into a `weak :: W`.
  * Given a `weak :: W`, we can only turn it into a `strong :: S` if it passes
    all the checks

We can write these as pure functions.

```haskell
weaken     :: S ->       W
strengthen :: W -> Maybe S
```

Oh! So this is like a parser-printer pair for arbitrary data. It seems like a
useful enough pattern. Let's think of some strongweak pairs:

  * `Refined p a` from the [refined][refined-hackage] library is an `a` where
    the predicate `p` has been asserted. This can be weakened into an `a` via
    `unrefine:: Refined p a -> a`.
  * `Word8` is a bounded natural number. `Natural` can represent any natural
    number. So `Natural` is a weak type, which can be strengthened into `Word8`
    (or `Word16`, `Word32`, ...) by asserting well-boundedness.
  * `[a]` doesn't have state any predicates. But we could weaken every `a` in
    the list. So `[a]` is a strong type, which can be weakened to `[Weak a]`.
  * `NonEmpty a` *does* have a predicate. For useability and other reasons, we
    only handle this predicate, and don't also weaken each `a` like above.
    `NonEmpty a` weakens to `[a]`.

But there's a hefty amount of boilerplate:

  * You need to model all the data types you want to use like this twice.
  * You need to write tons more definitions.

Aaaand it's already not worth it. Sigh.

## Library introduction
strongweak encodes the above strong/weak representation pattern for convenient
use, automating as much as possible. Some decisions restrict usage for nicer
behaviour. The primary definitions are below:

```haskell
class Weaken a where
    type Weak a :: Type
    weaken :: a :: Weak a

type Result = Validation Fails
type Fails = NeAcc Fail
class Weaken a => Strengthen a where
    strengthen :: Weak a -> Result a
```

Note that a strong type may have only one associated weak type. The same weak
type may be used for multiple strong types. This restriction guides the design
of "good" strong-weak type pairs, keeps them synchronized, and aids type
inference.

See the documentation on Hackage for further details.

## Cool points
### Extreme error clarity
strongweak is primarily a validation library. As such, strengthening failure
handling receives special attention:

  * Failures do not short-circuit; if a strengthening is made up of multiple
    smaller strengthenings, all are run and any failures collated.
  * Failures display the weak and strong (target) type.
  * Generic strengthening is scarily verbose: see below for details.

### One definition, strong + weak views
Using a type-level `Strength` switch and the `SW` type family, you can write a
single datatype definition and receive both a strong and a weak representation,
which the generic derivers can work with. See the `Strongweak.SW` module for
details.

### Powerful generic instances
There are generic derivers for generating `Strengthen` and `Weaken` instances
between *compatible* data types. The `Strengthen` instances annotate errors
extensively, telling you the datatype, constructor and field for which
strengthening failed!

Two types are *compatible* if

  * their generic SOP representations match precisely, and
  * every pair of leaf types is either identical or has the appropriate
    strengthen/weaken instance

The `SW` type family is here to help for accomplishing that. Otherwise, if your
types don't fit:

  * convert to a "closer" representation first
  * write your own instances (fairly simple with `ApplicativeDo`).

### Backdoors included
Sometimes you have can guarantee that a weak value can be safely strengthened,
but the compiler doesn't know - a common problem in parsing. In such cases, you
may use efficient unsafe strengthenings, which don't perform invariant checks.
Even better, they might explode your computer if you use them wrong!

## What this library isn't
### Not a convertible
This is not a `Convertible` library that enumerates transformations between
types into a dictionary. A strong type has exactly one weak representation, and
strengthening may fail while weakening cannot. For safe conversion enumeration
via typeclasses, consider Taylor Fausak's
[witch](https://hackage.haskell.org/package/witch) library.

### Not particularly speedy
The emphasis is on safety, which may come at the detriment of performance:

  * Strengthening and weakening might be slow. This depends on the type and the
    implementation. I try a little to ensure good performance, but not a lot.
  * Strong types can be more performant than their weak counterparts. For
    example, swapping all integrals for `Natural`s and `Integer`s will make your
    program slow.
    * You may avoid this fairly easily by simply not wrapping certain fields.

On the other hand, by only strengthening at the "edges" of your program and
knowing that between those you may transform the weak representation as you
like, you may find good performance easier to maintain.

## Related projects
### barbies
The [barbies][lib-barbies-hackage] library is an investigation into how far the
higher-kinded data pattern can be stretched. strongweak has some similar ideas:

  * Both treat a type definition as a "skeleton" for further types.
  * strongweak's `SW` type family looks a lot like barbies' `Wear`.

But I believe we're irreconcilable. strongweak is concerned with validation via
types. `SW` is just a convenience to reuse a definition for two otherwise
distinct types, and assist in handling common patterns. Due to the type family
approach, we can rarely be polymorphic over the strong and weak representations.
Whereas barbies wants to help you swap out functors over records, so it's very
polymorphic over those, and makes rules for itself that then apply to its users.

You could stack barbies on top of a `SW` type no problem. It would enable you to
split strengthening into two phases: strengthening each field, then gathering
via traverse (rather than doing both at once via applicative do). That thinking
helps reassure me that these ideas are separate. *(Note: I would hesitate to
write such a type, because the definition would start to get mighty complex.)*

## Other
### Can this be formalized or generalized in some useful way?
I note that this library is basically a couple of type classes and utilities for
automating writing parsers and printers for types which are "close". I can't
find anything in the literature that discusses this sort of thing. If you would
have some info there, please do let me know!
