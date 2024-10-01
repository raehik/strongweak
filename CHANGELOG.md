## 0.9.0 (2024-10-01)
* fix `Strengthen [a]` instance reversing list
* support GHC 9.4 - GHC 9.8

## 0.8.0 (2024-06-11)
* no longer use `Validation`: failures now must wrap explicitly instead of being
  implicitly collated
* fix some bounds (`text` lower bound was too low)
* various tweaks

## 0.7.1 (2024-05-27)
* bump meta/dependencies

## 0.7.0 (2024-05-11)
* replaced refined1 with rerefined
* simplify failures

## 0.6.1 (2024-04-03)
  * tests: relax hspec upper bound

## 0.6.0 (2023-05-10)
  * add instances for `Refined1 p f a`; use refined1 library instead of refined
    (pending upstream)

## 0.5.0 (2023-05-05)
  * allow text-2.0
  * refactor strengthening code, rename some definitions
  * use NeAcc instead of NonEmpty for strengthening failures

## 0.4.1 (2023-02-22)
  * add `DerivingVia` wrapper for generic instances (like `Generically`)

## 0.4.0 (2023-02-22)
  * redesign some instances to avoid the decomposer style
    * alter `Identity`, `Const` instances
    * remove `Maybe` instance
  * expand sized vector instance

## 0.3.2 (2022-11-28)
  * support GHC 9.4

## 0.3.1 (2022-07-04)
  * update refined (polykind predicate)

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
