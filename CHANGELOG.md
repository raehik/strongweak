## 0.3.0 (2022-06-08)
  * switch to associated type family for `Weak` inside `Weaken` - `Strengthen`
    now has `Weaken` as a superclass
    * I'm fairly confident that things make more sense this way - we get to
      remove an open type family, improve type inference, and prevent users from
      writing potentially dangerous instances. For that, a bit of asymmetry is
      welcome.
  * better document generic derivers
  * clarify instance design, provide more decomposer instances
  * various refactoring

## 0.2.0 (2022-05-31)
Initial Hackage release (dependency issues prevented uploading).

  * fix field indexing in generic errors
  * add unsafe strengthening
  * add property and error tests

## 0.1.0 (2022-05-16)
Initial release.

  * basic instances (lists, numerics)
  * generic derivations
  * super explicit errors
