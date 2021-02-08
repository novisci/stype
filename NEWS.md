# `stype` 0.4.3

* Updates the function signature for `as_Surv` to take a new formal argument `multiple_endpoint` which defaults to `FALSE`. Previous versions always returned data in a _multiple endpoints_ format (see the object documentation for `survival::Surv` for a definition) when `censor_as_event` was `FALSE`, so this is a breaking change.
* Adds a function `pmin_v_rcensored` that can be used to combine multiple `v_rcensored` objects into a new `v_rcensored` object.

# `stype` 0.4.2

* Adds the following constructor synonyms:
    * `bnry` : `v_binary`
    * `cnt`  : `v_count`
    * `chr`  : `v_character`
    * `cont` : `v_continuous`
    * `nneg` : `v_continuous_nonneg`
    * `tmev` : `v_event_time`
    * `nom`  : `v_nominal`
    * `ord`  : `v_ordered`
    * `rcen` : `v_rcensored`

# `stype` 0.4.1

* Fixes bug in `*` and `+` where (e.g) `v_binary(c(FALSE, TRUE, FALSE, TRUE)) +  c(1, 3, 3, 5)` now returns an error.

# `stype` 0.4.0

* Adds an *experimental* `tbl_analysis` type, which includes the basic utilities of a `stype` such as a `context` and `data_summary`. It also includes a `modifiers` argument which is a `list` of functions sequentially modify the table. These functions are applied whenever the table is subset, and thus can act to define analytic workflows that should be run whenever a table is modified. This feature is experimental and will be tested internally at NoviSci before being further developed.

# `stype` 0.3.0

## Major Changes

* Depends on  `vctrs` 0.3.0.
* Constructors of `stype`s have an `extra_descriptors` argument for passing a `list`of `descriptor` function.
* The `purpose` class now has a `tags` slot, so `stype` vectors can be tagged with arbitrary `character` vectors. The `is_tagged` function can be used to check if a vector is tagged with a string; e.g. `is_tagged(x, tags = c("tag1", "tag2"))` checks if `x` has either `"tag1"` or `"tag2"`.
* Now featuring `lenses`! See the [lenses vignette](lenses.html) for examples of getting/setting parts of `stype` vector or getting/setting several `stype`s within a list-like structure.
* A `weight` function is available for updating the  `data_summary` of `stype`s with vector of weights. See the [weighting vignette](weighting.html) for example of usage.
* Adds a `get_data_summaries` function to access the `data_summary` from multiple stypes in a `list` or `data.frame`.

# `stype` 0.2.4

## Minor changes

* Adds a `sort` method for `v_rcensored`.

# `stype` 0.2.3

## Major changes

* The `v_binary` type now has logical or and and operators: `|`(or `+`) and `&` (or `*`). Unlike `|` and `&` for the `logical` type, logical operators for `v_binary`  do not recycle values. That is, for two vectors `x` and `y`, they must either have the same size or either `x` or `y` must a scalar.
