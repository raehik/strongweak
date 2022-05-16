# strongweak
Definitions for transforming between types.

  * `strong -> weak` drops invariants (e.g. going from a bounded to an unbounded
    numeric type)
  * `weak -> Maybe strong` introduces invariants

This is not a `Convertible` library that enumerates transformations between
types into a dictionary.

  * A "strong" type has exactly one "weak" representation.
  * Weakening a type is safe.
  * Strengthening a type may fail.

There are generic derivers for generating `Strengthen` and `Weaken` instances
for arbitrary data types. The `Strengthen` instances annotate errors
extensively, telling you the datatype & record for which strengthening failed -
recursively, for nested types.

This is a validation library. We don't fail on the first error -- we attempt to
validate every part of a data type, and collate the errors into a list. This
happens magically in the generic deriver, but if you're writing your own
instances, you may want `ApplicativeDo` so you can use do notation. So it'll
monadic, but actually everything will get checked.
