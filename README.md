# Statistical Data Types

`stype` (pronounced stipe) is an R package for statistical data types. It depends heavily upon the [`vctrs`](https://github.com/r-lib/vctrs) package to:

* implement S3 classes aligned with statistical language: binary, continuous, count, non-negative continuous, time to event, etc;
* create variable-level metadata useful for sharing information about a variable across applications;
* add automatically generated summary statistics to the variable metadata.

## Statistical types

Statisticians speak of analyzing "binary" data. This type data can be represented in R in at least three ways: a `logical`, a `factor` with two levels, or a `numeric` using just `0` and `1`. Which representation should one use? The latter two do not guarantee that certain operations are closed in a mathematical sense; e.g., you could easily do the following `c(0, 1, 0, 1) + 1` without error. Similarly, "count" data can be represented by an `integer` in `R` but without the restriction of being non-negative. The `stype` package creates [algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type) that enforce type safety (to the degree that one can in R) for type for many common statistical analyses such as `v_binary`, `v_continuous`, `v_count`, `v_nominal`, and `v_event_time`.  

## Contextual information

Each instance of `stype` objects contain 2 attributes that users may find useful: `context` and `data_summary`. A `context` can be used to specify study-specific metadata. It is an `S4` object containing slots like `short_label`, `long_label`, `description`, `security_type`, and `purpose`. A `purpose`, for example, can be used to define a variable's role in a study design such as "outcome", "identifier", "covariate", or "exposure". This kind of contextual information is valuable for selecting variables or modelling only certain variables.

## Summary statistics

A `stype` vector also contains a `data_summary` object, which is  *automatically* generated and contain summary statistics about the data. All objects contain the following statistics:

* `n`: number of observations 
* `has_missing`: an indicator of whether the variable has missing data
* `n_nonmissing`: the number of nonmissing
* `n_missing`: the number of missing
* `proportion_missing`: the proportion missing
* `is_constant`L an indicator of whether all the values are the same

Each type has additional summary statistics relevant to its data. For example, `v_continuous` contains the mean, standard deviation, min, max, and various quantiles. The `data_summary` is updated whenever a variable is subset or two vectors of the type are combined.

The package also prints certain attributes:

```
> stype::v_binary(c(TRUE, FALSE, TRUE))
<binary[3]>
[1] 1 0 1
Proportion = 0.667
```

```
> stype::v_binary(c(TRUE, FALSE, TRUE, NA))
<binary[4]>
[1]  1  0  1 NA
Proportion = 0.667; Missing = 1.000
```

