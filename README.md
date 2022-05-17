[lib-refined-hackage]: https://hackage.haskell.org/package/refined

# strongweak
Convert between pairs of "weak" and "strong"/"validated" types, with good
errors and generic derivers.

## Definition of strong and weak types
Take a pair of types `(strong, weak)`. We state the following:

  * You may safely convert ("weaken") any `strong` value to a `weak` value.
  * You can try to convert ("strengthen") any `weak` value to a `strong` value,
    but it may fail.

As an arbitrary limitation for ease of use, a strong type has only one
associated weak type. The same weak type may be used for multiple strong types.

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

### Generic strengthening is extremely powerful
There are generic derivers for generating `Strengthen` and `Weaken` instances
for arbitrary data types. The `Strengthen` instances annotate errors
extensively, telling you the datatype & record for which strengthening failed -
recursively, for nested types!

### One definition, strong + weak views
Using a type-level `Strength` switch and the `SW` type family, you can write a
single datatype definition and get a strong and a weak representation, which
the generic derivers can work with. See the `Strongweak.SW` module for details.

Note that this isn't required. But the generic derivers require data types to
line up very closely, so careful.

## What this library isn't
### Not a convertible
This is not a `Convertible` library that enumerates transformations between
types into a dictionary. A strong type has exactly one weak representation, and
strengthening may fail while weakening cannot.

### Not particularly speedy
The emphasis is on safety, possibly at the detriment of performance. However, my
expectation is that you only strengthen & weaken at the "edges" of your program,
and most of the time will be spent transforming weak representations. This may
improve performance if it means invariants don't have to be continually asserted
inline, but it also may slow things down e.g. `Natural`s are slower than
`Word`s.
