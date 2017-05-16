pash 0.0.4
----------

### NEW FEATURES

### FIXES

### OTHER CHANGES

pash 0.0.3
----------

### NEW FEATURES

- conditional probabilities of death can be used as input via the new `Inputnqx()` function (@panchoVG)
- `GetShape()` now features the `harmonized` option. if set to `TRUE` the shape measures are rescaled so that they all have a similar interpretation (see Wrycza etal. (2015). Quantifying the Shape of Aging for details). Set to `FALSE`, the measures are returned in their conventional form (closes #14)
- the life-table death rate (ldr) has been added to the pace measures returned by `GetPace()`

### FIXES

- all life-table shape measures now work on open ended life-tables (closes #16)
- the life-table variance and coefficient of variation measures now return consistent values for single year and abridged life-tables
- print method for pash object no longer produces incoherent values for ndx and lx due to integer truncation in last age groups (closes #15)

### OTHER CHANGES

- add vignette: "Shape measures and their implementation"
- add vignette: "Accuracy of discrete formulas for life-table shape measures"
- the life-table variance measure has been removed from the output of `GetShape()` because it is not standardized by pace and therefore not a true shape measure. the life-table coefficient of variation is available as the true shape measure equivalent.
- The cdr has been renamed ldr (life-table death rate) and moved from shape to pace measures
- remove alternative Gini implementations
- add additional data sets
- implement additional tests
- changed names of shape measures to lowercase