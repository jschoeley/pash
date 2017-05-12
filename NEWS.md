pash 0.0.3
----------

### NEW FEATURES

- Conditional probabilities of death can be used as input via the new `Inputnqx()` function (@panchoVG).
- The crude death rate (cdr) has been added to the pace measures returned by `GetPace()`

### FIXES

- the life-table Gini coefficient, variance and coefficient of variation now work with open ended life-tables (closes #16)
- the life-table variance and coefficient of variation measures now return consistent values for single year and abridged life-tables
- print method for pash object no longer produces incoherent values for ndx and lx due to integer truncation in last age groups (closes #15)

### OTHER CHANGES

- The life-table variance measure has been removed from the output of `GetShape()` because it is not standardized by pace and therefore no true shape measure. The life-table coefficient of variation is available as the true shape measure equivalent.
- Remove alternative Gini implementations

TODO
----

- add harmonize shape option to GetShape()
- add InputnLx()
- add time-unit transform
- fix bug: No Entropy when LastOpen = TRUE
- feature: add input checks for shape and pace selection