  * use ekmett's either library instead? also has Validation with Applicative
    instance. bit smaller validation stuff, bit larger otherwise (relies on
    profunctors), but it's a common dependency anyway
  * split into base definitions and orphan instances? base only needs
    prettyprinter and Validation.
