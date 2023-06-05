
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aorsf-bench

<!-- badges: start -->
<!-- badges: end -->

The goal of aorsf-bench is to introduce and evaluate

1.  a method to increase the computational efficiency of (i.e.,
    accelerate) the oblique random survival forest (RSF)
2.  a method to estimate importance of individual predictor variables
    with the oblique RSF.

The entire project is summarized in a paper: `paper/arxix/main.pdf`.

## tl;dr

- We made oblique RSFs faster and also developed a new method to
  estimate importance of individual predictors with them. These methods
  are available in the `aorsf` R package.

- We find that the accelerated oblique RSF (`aorsf-fast` in the paper)
  is fast and accurate. In a benchmark with 35 different risk prediction
  tasks, `aorsf-fast` was faster than all of the learners we analyzed
  except for penalized Cox regression models (`glmnet-cox` in the
  paper). **Figure**: Distribution of time taken to fit a prediction
  model and compute predicted risk. The median time, in seconds, is
  printed and annotated for each learner by a vertical line.

<img src="README_files/figure-gfm/fig-bm-time-1.png" style="display: block; margin: auto;" />

- We find that `aorsf-fast` has the best index of prediction accuracy
  out of all the learners we evaluated. **Figure**: Expected differences
  in index of prediction accuracy between the accelerated oblique random
  survival forest and other learning algorithms. A region of practical
  equivalence is shown by purple dotted lines, and a boundary of
  non-zero difference is shown by an orange dotted line at the origin.

<img src="README_files/figure-gfm/fig-bm-ibs-1.png" style="display: block; margin: auto;" />

- We find that negation variable importance improves the chances of
  ranking a relevant variable as more important than an irrelevant
  variable when using an oblique RSF to estimate variable importance.
  **Figure**: Concordance statistic for assigning higher importance to
  relevant versus irrelevant variables. Text appears in rows where
  negation importance obtained the highest concordance, showing absolute
  and percent improvement over the second best technique.

<img src="README_files/figure-gfm/fig-bm-vi-1.png" style="display: block; margin: auto;" />

## Reproducing this paper

We use `targets` to coordinate our numerical experiments. Thus, our
results on publicly available data can be reproduced by cloning our repo
and running `tar_make()` with only publicly available data requested in
the `targets` pipeline. Below are the steps involved:

1.  Clone our repo and open the associated Rstudio project.

2.  Open `packages.R` and ensure you have installed the R packages
    listed in this file.

3.  In `_targets.R`, edit the vector of datasets passed into our
    `targets` pipeline so that it only contains publicly available data.
    Below is a code chunk showing which datasets you need to comment out
    to request only the publicly available ones:

``` r

data_source = c(
  "veteran",
  "colon_recur",
  "colon_acm",
  "pbc_orsf",
  "time_to_million",
  "gbsg2",
  "peakV02",
  "flchain",
  "nafld",
  "rotterdam_recur",
  "rotterdam_acm",
  "actg_aids",
  "actg_death",
  # "guide_it_cvd",    
  # "guide_it_hfhosp", 
  "breast",
  # "sprint_cvd",
  # "sprint_acm",
  "nki",
  "lung",
  "lung_ncctg",
  "follic_death",
  "follic_relapse",
  "mgus2_death",
  "mgus2_pcm"
  # "mesa_hf",
  # "mesa_chd",
  # "mesa_stroke",
  # "mesa_death",
  # "aric_hf",
  # "aric_chd",
  # "aric_stroke",
  # "aric_death",
  # "jhs_stroke",
  # "jhs_chd"
)
```

4.  Create the `nafld` data by running `nafld_build()`, which is an R
    function included in `R/nafld_build.R` that combines multiple
    datasets in the `survival` package into a dataset we analyzed in the
    current study. An example of how this can be done is below:

5.  Run `targets::tar_make()` to make the targets pipeline, but be aware
    that it will take a very long time to make the pipeline as-is.

To make the process run quickly, I recommend setting `run_seed` to be
`1:3` in `_targets.R` to request only 3 replications of Monte-Carlo
cross validation be completed. Also in `_targets.R`, use a subset of the
faster learners by commenting out the slower ones:

``` r

# slower model fitters are commented out

model_fitters <- c(
  'aorsf_fast',
  'aorsf_cph',
  # 'aorsf_random',
  # 'aorsf_net',
  # 'rotsf',
  # 'rsfse',
  # 'cif',
  'cox_net',
  # 'coxtime',
  # 'obliqueRSF',
  'xgb_cox',
  # 'xgb_aft',
  'randomForestSRC',
  'ranger'
)
```
