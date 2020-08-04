# `stype` 0.3.0

## Major Changes

* Depends on  `vctrs` 0.3.0.
* `stype` constructors have an `extra_descriptors` argument for passing a `list`of `descriptor` function.
* Now featuring `lenses`! See the [lenses vignette](lenses.html) for examples of getting/setting parts of `stype` vector or getting/setting several `stype`s within a list-like structure.
* A `weight` function is available for updating the  `data_summary` of `stype`s with vector of weights. See the [weighting vignette](weighting.html) for example of usage.
