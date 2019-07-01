# Introduction

Introducing `stype` (pronounced stipe), an R package for statistical data types: https://gitlab.novisci.com/nsStat/stype. This package implements variable-level metadata useful for sharing information about a variable across application. 

It contains R S3 classes (data types) oriented around common statistical analyses such as `v_binary`, `v_continuous`, `v_count`, `v_nominal`, and `v_event_time`.  Each instance of these objects contain 2 attributes that users may find useful: `context` and `data_summary`. The `context` contains slots for things like `short_label`, `long_label`, `description`, `security_type`, and `purpose`. A `purpose` defines a variable's role in a study design such as "outcome", "identifier", "covariate", or "exposure".

The `data_summary` object is an *automatically* generated object containing summary statistics about the data. All objects contain the following statistics:

* `n`: number of observations 
* `has_missing`: an indicator of whether the variable has missing data
* `n_nonmissing`: the number of nonmissing
* `n_missing`: the number of missing
* `proportion_missing`: the proportion missing
* `is_constant`L an indicator of whether all the values are the same

Each type has additional summary statistics relevant to its data, e.g. `v_continuous` contains the mean, standard deviation, min, max, and various quantiles.

The `data_summary` is updated whenever a variable is subset or two vectors of the type are combined.

## Printing

The package prints certain attributes, for example:

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

