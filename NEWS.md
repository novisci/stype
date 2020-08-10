# `stype` 0.3.0

## Major Changes

* Depends on  `vctrs` 0.3.0.
* Constructors of `stype`s have an `extra_descriptors` argument for passing a `list`of `descriptor` function.
* The `purpose` class now has a `tags` slot, so `stype` vectors can be tagged with arbitrary `character` vectors. The `is_tagged` function can be used to check if a vector is tagged with a string; e.g. `is_tagged(x, tags = c("tag1", "tag2"))` checks if `x` has either `"tag1"` or `"tag2"`.
* Now featuring `lenses`! See the [lenses vignette](lenses.html) for examples of getting/setting parts of `stype` vector or getting/setting several `stype`s within a list-like structure.
* A `weight` function is available for updating the  `data_summary` of `stype`s with vector of weights. See the [weighting vignette](weighting.html) for example of usage.
* Adds a `get_data_summaries` function to access the `data_summary` from multiple stypes in a `list` or `data.frame`.
