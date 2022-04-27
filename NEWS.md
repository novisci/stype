# `stype` 0.5.0

* Modifies the behavior of how the levels and labels for outcome and censors are created. The levels/labels follow the following rules:
    1. If a named list (all elements must be uniquely named) is passed to `outcomes` (or `censors`) and all the vectors have short labels, then the list names are the levels and the short labels become the labels.
    2. If a named list is passed to `outcomes` (or `censors`) and *any* of the vectors are missing short labels, then the list names become the levels and the labels.
    3. If a unnamed list is passed and all the vectors have internal names and all the vectors have short labels, then the internal names are the levels and the short labels become the labels. 
    4. If a unnamed list is passed and all the vectors have internal names *any* of the vectors are missing short labels, then the internal names become the levels and  labels. 
    5. Otherwise, `as.character(1:length(x))` become the levels and labels, where `length(x)` is the number of list elements.
* Makes using `v_continuous` and `v_continuous_nonneg` as the `w` argument to `weight` possible.
* Adds several assertions to `v_rcensored` to catch invalid inputs and provide more helpful error messages.
* Adds the package version of `stype` under which a `stype` vector was created as an attribute. This is included for future compatibility checking when comparing vectors in the case they were created under different versions of `stype` that don't play nice together.
* Adds an internal `new_stype_vctr` function to streamline the creation of new stype vectors, ensuring they all share a common set of attributes.
* Removes `v_character` type.
* Adds a `v_proportion` types with values in `[0. 1]`, which inherits from `v_continuous_nonneg`.
* Removes `v_event_time` type.
* The `v_rcensored` type now takes `v_continuous_nonneg` vectors instead of `v_event_time`. More importantly, `NA` values are no longer accepted as inputs to `v_rcensored`. To indicate that an observation *has not yet been observed* use `Inf`.
* Adds the `auto_compute_summary` argument to stype `v_*` constructors. When `TRUE`, data summaries are automatically computed when a vector is created or restored (e.g. on subset or `c()`). When `FALSE`, data summaries are only computed when the user asks for a data summary. The default is `TRUE`, though this may change in the future.
* Adds and updates Summary/Math/Arithmetic functions for `v_continous`, `v_continuous_nonneg`, `v_binary`, `v_proportion`, and `v_count`. See the `stype math` vignette for a complete list of all available functions and the respective domains/codomains of the functions.
* Adds the `ptableNoNA` descriptor to `categoricalDescriptors`
which summarizes a table excluding `NA` values.

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
