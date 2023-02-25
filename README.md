[lib-refined-hackage]: https://hackage.haskell.org/package/refined
[lib-barbies-hackage]: https://hackage.haskell.org/package/barbies

# strongweak
Purely convert between pairs of "weak" and "strong"/"validated" types, with good
errors and generic derivers. Perhaps a pure approximation Alexis King's [Parse,
don't validate][parse-dont-validate] pattern as a library, focused on failure
reporting.

## Definition of strong and weak types
Take a pair of types `(strong, weak)`. We state the following:

  * You may safely convert ("weaken") any `strong` value to a `weak` value.
  * You can try to convert ("strengthen") any `weak` value to a `strong` value,
    but it may fail.

As a rule, a weak type should be *easier to use* than its related strong type.
That is, it should have fewer invariants to consider or maintain. One could
weaken an `a` to a `Maybe a`, but since a `Maybe a` is harder to use, I'm less
certain about adding it to this library. (I don't know, perhaps it should be.)

A strong type may have only one associated weak type. The same weak type may be
used for multiple strong types. This restriction guides the design of "good"
strong-weak type pairs, keeps them synchronized, and aids type inference.

### Examples
The [refined][lib-refined-hackage] library defines a `newtype Refined p a =
Refined a`. To get a `Refined`, you must test its associated predicate. You may
recover the unrefined value by removing the newtype wrapper. Thus, you may
strengthen `a`s into `Refined p a`s, and weaken vice versa.

The `WordX` family are like bounded `Natural`s. We can consider `Natural` as a
weak type, which can be strengthened into e.g. `Word8` by asserting
well-boundedness.

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
between arbitrary data types. The `Strengthen` instances annotate errors
extensively, telling you the datatype, constructor and field for which
strengthening failed!

Note that the generic derivers require your the generic SOP representation of
your strong and weak types to match precisely. The `SW` type family is here to
help for accomplishing that. Otherwise, if your types don't fit:

  * convert to a "closer" representation first, or
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
