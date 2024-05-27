# strongweak to-dos
* split into base definitions and orphan instances?
  * base needs either, acc, text and prettyprinter
* deleted generic failure tests because clumsy. kinda sad but idk :(
* clean up Validation. I don't think we need it any more-- except it might be
  handy in generic strengthening. but probably we can and should just do some
  casing there to save on dependencies

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
