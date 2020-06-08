# JLutils (development version)

* `stat_prop()`, `stat_weigthed_mean()` and `stat_cross()` have
  been moved to `GGally` (>= 2.0.0)
* `stat_fill_labels()` & `stat_stack_labels()` are deprecated.
  Use `GGally::stat_prop()` instead.
* `ggcross()` has been updated.
* `ggchisq_res()` is now deprecated. You could use instead `GGally::ggtable()` or `JLutils::ggcross()`.
* `tidy_detailed()` keeps now the original order of terms.

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
