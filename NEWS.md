# JLutils (development version)

* `ggcross()` has been updated.
* `ggchisq_res()` is now deprecated. You could use instead `GGally::ggtable()` or `JLutils::ggcross()`.

# JLutils 1.20.0

* a `ggsurvey()` function for easy ggplot from a survey object
* an experimental `stat_weighted_mean()`

# JLutils 1.19.0

* `tidy.clm()`, `tidy.clmm()` and `tidy.svyolr()` are now part of `broom` and have
  been removed from JLutils.
* `tidy_chisq()` is deprecated. Use `augment()` from `broom` instead.

# JLutils 1.18.0

* Compatibility with `scales` version 1.1.0 (i.e. using `label_number()` instead of `number_format()`)

# JLutils 1.17.0

* Added `stat_prop()`  for computing proportions in `ggplot2`
