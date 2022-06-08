[lib-refined-hackage]: https://hackage.haskell.org/package/refined
[lib-barbies-hackage]: https://hackage.haskell.org/package/barbies

# strongweak
Purely convert between pairs of "weak" and "strong"/"validated" types, with good
errors and generic derivers.

## Definition of strong and weak types
Take a pair of types `(strong, weak)`. We state the following:

  * You may safely convert ("weaken") any `strong` value to a `weak` value.
  * You can try to convert ("strengthen") any `weak` value to a `strong` value,
    but it may fail.

As a rule, a weak type should be *easier to use* than its related strong type.
That is, it should have fewer invariants to consider or maintain. You could
weaken an `a` to a `Maybe a`, but since a `Maybe a` is harder to use, it's not a
candidate for this library.

As an arbitrary limitation for ease of use, a strong type has only one
associated weak type. The same weak type may be used for multiple strong types.
This restriction guides the design of "good" strong-weak type pairs & keeps them
synchronized, plus helps type inference.

### Examples
The [refined][lib-refined-hackage] library defines a `newtype Refined p a =
Refined a`. To get a `Refined`, you must test its associated predicate. You may
recover the unrefined value by removing the newtype wrapper. Thus, you may
strengthen `a`s into `Refined p a`s, and weaken vice versa.

The `WordX` family are like bounded `Natural`s. We can consider `Natural` as a
weak type, which can be strengthened into e.g. `Word8` by asserting
well-boundedness.

## Cool points
### Validates as much as possible
This is primarily a validation library. Thus, we don't fail on the first error
-- we attempt to validate every part of a data type, and collate the errors into
a big list. (`ApplicativeDo` plus `Validation` is magical.)

### One definition, strong + weak views
Using a type-level `Strength` switch and the `SW` type family, you can write a
single datatype definition and receive both a strong and a weak representation,
which the generic derivers can work with. See the `Strongweak.SW` module for
details.

### Generic strengthening is extremely powerful
There are generic derivers for generating `Strengthen` and `Weaken` instances
between arbitrary data types. The `Strengthen` instances annotate errors
extensively, telling you the datatype & record for which strengthening failed -
recursively, for nested types!

Note that the generic derivers work with any pair of matching data types. But
they must match very closely: both types are traversed in tandem, so every pair
of fields must be compatible. If you need to do calculation to move between your
strong and weak types, consider splitting it into calculation -> strengthening
and using the generic derivers. Or write your own instances.

### Backdoors included
Sometimes you have can guarantee that a weak value can be safely strengthened,
but the compiler doesn't know - a common problem in parsing. In such cases, you
may use efficient unsafe strengthenings, which don't perform invariant checks.

## What this library isn't
### Not a convertible
This is not a `Convertible` library that enumerates transformations between
types into a dictionary. A strong type has exactly one weak representation, and
strengthening may fail while weakening cannot. For safe conversion enumeration
via typeclasses, consider Taylor Fausak's
[witch](https://hackage.haskell.org/package/witch) library.

### Not particularly speedy
The emphasis is on safety, possibly at the detriment of performance. However, my
expectation is that you only strengthen & weaken at the "edges" of your program,
and most of the time will be spent transforming weak representations. This may
improve performance if it means invariants don't have to be continually asserted
inline, but it also may slow things down e.g. `Natural`s are slower than
`Word`s.

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
