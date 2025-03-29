# strongweak to-dos
* split into base definitions and orphan instances?
* deleted generic failure tests because clumsy. kinda sad but idk :(

## Support generic `Weaken (a n)` where `type Weakened (a n) = a (n+1)`
This is a generalization of the `Strong`/`Weak` switching to the naturals.
It seems like a cool idea. Now, you can already do this just fine using
`WeakenedN` and writing manual instances for `0`, `1` etc. But I want to write
a single instance `n`, and have GHC figure out the rest.

I've started on `newtype Incremental n a = Incremental (WeakenedN n a)`, for
which I can write `Weaken` with a special `Refl` proof. This works! However, I
still need to add the `Weaken (WeakenedN n _)` context (for each field) in data
types that use this. This is slightly disappointing... but also to be expected?

Also, it's stuck in newtype land. Also disappointing. But I don't see a way
around that, since otherwise we can't track weakens. Maybe we can wire in
another coercion, that you would choose to do after weakening.

Yeah. We can add in _another_ type variable, something like `Bool`, where one
side uses `Incremental` (which we weaken/strengthen on) and the other side uses
`WeakenedN` (which has one fewer layers). We can't `coerce` between them... but
we can `gcoerce`. Good enough? But now with clumsy required contexts, type vars
everywhere, and a required `gcoerce`... I'm not very happy with this. At least
we gave it a good go. Honestly, I don't think it this idea was even that useful.

## Failures: Pretty refinement failures require exposing `Doc` early
rerefined refinement failures return an ADT, which is something. But we can't
embed that directly in a strengthen failure. We have to prettify to some degree.
But that will force us to perform all the indentation etc. upfront, which is
what we're trying to avoid (since it won't work properly).

Is this even sensible? When a strengthen failure indents, it has semantic
meaning: in this general failure, a more specific failure occurred. Using the
indent also for "in this specific failure, a refinement error occurred" seems
bad, somehow.

We _could_ fix this by going back on our new design, and making a failure
constructor specifically for refinement failures. But that seems like a big code
smell.

Alternatively, we store `Doc`s instead of `Builder`s. Then we can layout in the
`Strengthen Refined` instance. But that seems wrong, too.

Orrrr I know it's stupid, but we could have a `prettyRefineFailure :: E ->
[TBL.Builder]`, where the indents are prepared for us. Then we can add our own
base indent on top. No issue. OK, I guess we stick with the current setup.
