# strongweak to-dos
* split into base definitions and orphan instances?
  * base needs either, acc, text and prettyprinter

## Failures: is Typeable really the right choice?
I probably went with `TypeRep`s for filling out failure detail due to copying
refined's design. With rerefined, I've gone back on that, and I now
don't use `Typeable` at all. That said, is it the right pick here?

* The generics don't use it at all.
* Concrete instances could use an associated type family like `type Name =
  "Word32"`.
  * And strongweak largely deals in concrete instances.
* We can do the same type-level `Show` stuff that rerefined does.
  * And we can embed `PredicateName` for `Refined[1]` instances.

The user will have to fill out a bunch of associated type families when using
strongweak, which sucks. Or maybe we can do a shitty job automatically for
generics? Like we can get the datatype name-- no other info, but that's a good
start lol.

The `Show`ing feels a little weird too, but I think it's just that we get away
with it thanks to having lots of concrete instances. Non-concrete instances
would drop it.
