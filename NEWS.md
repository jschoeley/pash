pash 0.0.3
----------

### NEW FEATURES

- Conditional probabilities of death can be used as input via the new `Inputnqx()` function (@panchoVG).
- The crude death rate (cdr) has been added to the pace measures returned by `GetPace()`
- `GetShape()` now features the `harmonized` option. If set to `TRUE` the shape measures are rescaled so that they all have a similar interpretation (see Wrycza etal. 2015. Quantifying the Shape of Aging for details). Set to `FALSE` the measures are returned in their conventional form. (closes #14)

### FIXES

- all life-table shape measures now work on open ended life-tables (closes #16)
- the life-table variance and coefficient of variation measures now return consistent values for single year and abridged life-tables
- print method for pash object no longer produces incoherent values for ndx and lx due to integer truncation in last age groups (closes #15)

### OTHER CHANGES

- The life-table variance measure has been removed from the output of `GetShape()` because it is not standardized by pace and therefore no true shape measure. The life-table coefficient of variation is available as the true shape measure equivalent.
- Remove alternative Gini implementations
- Add additional data sets
- Implement additional tests