# JLutils 1.24.0

* New function `test_local_as_cran()`
* `tidy.msSurv()` removed as `msSurv` is not anymore on CRAN
* `signif_stars()` now available in `ggstats`
* `lm_right()`, `lm_left()`  and `lm_zero()` have been removed as not working
  anymore and considering that it is possible to pass `xseq` argument to `geom_smooth()`

# JLutils 1.23.0

* `long_to_periods()` has been updated. `.data` argument has been renamed `data`

# JLutils 1.22.0

* `ggsurvey()` has been moved to `questionr`.
* `ggcoef_model()` and `geom_stripped_rows()` has been moved to `GGally`.
* `tidy_detailed()` has been removed and could be replaced by `broom.helpers::tidy_plus_plus()`.
* `ggcross()` has been removed.

# JLutils 1.21.0

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
