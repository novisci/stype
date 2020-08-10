# `stype` 0.2.4

## Minor changes

* Adds a `sort` method for `v_rcensored`.

# `stype` 0.2.3

## Major changes

* The `v_binary` type now has logical or and and operators: `|`(or `+`) and `&` (or `*`). Unlike `|` and `&` for the `logical` type, logical operators for `v_binary`  do not recycle values. That is, for two vectors `x` and `y`, they must either have the same size or either `x` or `y` must a scalar.