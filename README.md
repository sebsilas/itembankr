# itembankr

`itembankr` is an *R* package for creating useful item banks from raw, relatively unstructured data. These item banks are intended mainly to be used in psychological testing but can also be used for corpus analyses. The package is currently focused on musical, and particularly monophonic, melodic stimuli.

For convenience, we refer to a "corpus" as some raw input data, not in itself particularly useful in psychological testing, and an "item bank" as the useful output, which is typically a set of dataframes with the stimuli from the corpus organised in a useful way (and some new computed melodic representations).

Hence, the main function in `itembankr` is `create_item_bank`:

```r

# Create an item bank from MIDI and/or musicxml files

create_item_bank(name = 'Berkowitz',
                 midi_file_dir = 'berkowitz_midi_rhythmic_100bpm',
                 musicxml_file_dir = 'berkowitz_musicxml')
```

You can also create an item bank simply by inputting MIDI notes and durations directly to the `input_df` argument via an *R* tibble/dataframe. Minimally, you need `abs_melody` and `durations` columns, e.g,:


```r

test_item_bank <- tibble::tribble(
  ~abs_melody,              ~durations,
  "61,62,63,64,65,66",      "1,2,3,4,5,6",
  "72, 73, 75, 78",         "1, 1, 1, 1"
)


create_item_bank(name = "Test", input_df = test_item_bank)

```

The output of `create_item_bank` is one or more `.rda` files, depending on what you requested as `output`.

You can read these into R like this:

```r

# A fairly limited item bank consisting of just file references:

load('Berkowitz_file.rda')


Berkowitz_file <- item_bank

# (we rename the object from "item_bank")

rm(item_bank)


# A database of the melodic items split up into ngrams (perhaps most useful for arrhythmic usage)

load('Berkowitz_ngram.rda')

Berkowitz_ngram <- item_bank

rm(item_bank)


# Rather than splitting up the corpus into ngrams, split it up based on (a rather crude approximation of) phrase boundaries. This is perhaps more useful for rhythmic usage.

Berkowitz_phrase <- item_bank

rm(item_bank)

# Everything combined

load('Berkowitz_combined.rda')

Berkowitz_combined <- item_bank

rm(item_bank)

```


These various item banks can be used as inputs to tests created with the [musicassessr](https://github.com/syntheso/musicassessr) package.

Please note, these functions can take a long time to run.

By default, `create_item_bank` produces all item bank types as output. You can request only some of the database types to be created using the `output` argument.

You can also use the underlying functions directly, e.g., see: 

- `create_item_bank_from_files`
- `split_item_bank_into_ngrams`
- `count_freqs`
- `get_melody_features`
- `create_phrases_db`



## Analyse and compare item banks

`itembankr` now includes AI-generated helpers to *summarise*, *visualise*, and *dimension-reduce* item banks (numeric features), plus *compare categorical* features between item banks.

### What these functions do

- `analyse_item_bank()` (single bank)
  - Cleans/aligns numeric features (with optional `include`/`exclude`).
  - Descriptive statistics per feature.
  - Violin + boxplot panels of feature distributions.
  - Robust **Parallel Analysis** to estimate the number of components (PCs or factors).
  - **PCA via `psych::principal()`**, optional rotation (e.g., `"equamax"`, `"varimax"`, `"oblimin"`).
  - Names each component by its top loading features (configurable with `name_top`).
  - Optional **categorical summaries** (entropy, top levels) and a stacked bar plot.

- `compare_item_banks()` (two item banks)
  - Aligns overlapping numeric features and prints what’s used.
  - Descriptives per bank + **effect sizes** (Cohen’s *d*) and KS tests per feature.
  - Violin panels for side-by-side distributions.
  - **Parallel Analysis** per bank and jointly.
  - **Joint PCA** (on the combined rows of both item banks), with components named by top loadings.
  - PC score density plots by bank and per-component score summaries.
  - **Correlation structure similarity** (Mantel test of 1−cor matrices).
  - **Categorical comparisons** (chi-square/Fisher, Cramér’s *V*, Jensen–Shannon divergence) and stacked proportion plots.
  - By default, the following columns are **always excluded** from categorical analyses:
    `abs_melody`, `durations`, `item_type`, `melody`.

Both functions are **tidyverse-friendly** and print neat, colored progress/tables via `{cli}`.

> Heads-up: Under the hood we use median imputation and robust screening to avoid crashes from zero-variance or highly missing features. Parallel analysis uses `psych::fa.parallel()`, with a safe Kaiser fallback.

---

### Quick start: single bank

```r
# Analyse one item bank
res1 <- analyse_item_bank(
  item_bank  = WJD::phrase_item_bank,
  bank_name  = "WJD",
  standardise = TRUE,
  pa_type    = "pc",
  rotate     = "equamax",
  name_top   = 2L,
  max_plots  = 24,
  verbose    = TRUE
)

# Visuals
res1$violin_plot           # feature distributions
res1$pca$density_plot      # PC score densities

# Numeric summaries
res1$descriptives          # mean/sd/median/etc per feature
res1$pca$score_summary     # per-PC score summary

# --- Categorical outputs (single bank) ---
# 1) Long format rows (feature, level – with .bank for consistency)
res1$categoricals$long

# 2) Per-feature summary (n_levels, entropy, top level)
res1$categoricals$summary |>
  dplyr::arrange(dplyr::desc(entropy)) |>
  dplyr::slice_head(n = 10)

# 3) Stacked bar plot of top levels
res1$categoricals$plot
```

---

### Quick start: two item banks

```r
# Compare two item banks
res <- compare_item_banks(
  item_bank1  = WJD::phrase_item_bank,
  item_bank2  = Berkowitz::phrase_item_bank,
  item_bank_names  = c("WJD", "Berkowitz"),
  standardise = TRUE,
  pa_type     = "pc",
  rotate      = "equamax",
  name_top    = 3L,
  max_plots   = 24,
  verbose     = TRUE
)

# Visuals
res$violin_plot                 # distributions by bank
res$pca$joint_density_plot      # joint PC score densities by bank

# Numeric summaries
res$descriptives
res$differences |>
  dplyr::arrange(dplyr::desc(abs(cohen_d))) |>
  dplyr::slice_head(n = 20)

# Joint PCA score summaries
res$pca$joint_score_summary

# --- Categorical outputs (two item banks) ---
# 1) Long rows (feature, .bank, level)
res$categoricals$long

# 2) Tests per feature: test type (chi-square/Fisher), p, Cramér's V, JS divergence
res$categoricals$tests |>
  dplyr::arrange(dplyr::desc(cramers_v)) |>
  dplyr::slice_head(n = 10)

# 3) Per-bank, per-level proportions
res$categoricals$proportions

# 4) Stacked proportion bars
res$categoricals$plot
```

---

### Interpreting outputs

- **Parallel analysis** (`res$pa` / `res1$pa`): the suggested number of components for each bank and jointly.  
- **PCA loadings** (`res$pca$joint$rotation`, `res$pca$each[[i]]$rotation`, `res1$pca$model$rotation`):  
  rotated loadings matrix. Column names are auto-labels like  
  `PC 1: interval_var + contour_var` (based on the top `name_top` loadings).
- **Scores** (`$x`): component scores for each row (regression scores from `psych::principal()`).
- **Differences** (`res$differences`): numeric features ranked by absolute Cohen’s *d*; includes KS *p* for distributional differences.
- **Categoricals (two-bank)** (`res$categoricals$tests`):  
  - `cramers_v` ∈ [0,1]: strength of association between *feature* and *bank*.  
  - `js_divergence` ≥ 0: divergence between per-bank level distributions (0 = identical).  
- **Categoricals (single-bank)** (`res1$categoricals$summary`):  
  - `entropy`: diversity/uncertainty across levels (higher = more spread).

---

### Minimal dependencies

These helpers use: `{cli}`, `{dplyr}`, `{tidyr}`, `{purrr}`, `{tibble}`, `{ggplot2}`, `{janitor}`, `{psych}`, `{vegan}`.  
We use base R’s pipe `|>` internally, but you can happily use `%>%` in your scripts.

---

### Reproducibility & robustness

- Set `seed` for reproducibility.  
- We drop zero-variance or highly missing features before PA/PCA, and impute medians for stability.  
- If `fa.parallel()` fails (e.g., due to odd correlations), we fall back to a Kaiser rule on a cleaned correlation matrix.

---

### Tips

- Too many facets? Use `max_plots` to cap the number of violin panels.  
- Prefer rotated components for interpretability: `rotate = "varimax"` (orthogonal) or `"oblimin"` (oblique).  
- Change how components are labeled with `name_top` (e.g., `name_top = 3L`).  
- Exclude identifiers with `id_cols` (e.g., `"item_id"`).  
- For categoricals, we always exclude `abs_melody`, `durations`, `item_type`, `melody` to avoid giant level sets.

---

### Example session

```r
# Single bank
res1 <- analyse_item_bank(
  item_bank  = WJD::phrase_item_bank,
  bank_name  = "WJD",
  standardise = TRUE,
  pa_type    = "pc",
  rotate     = "oblimin",
  name_top   = 3L,
  verbose    = TRUE
)

# Two item banks
res2 <- compare_item_banks(
  item_bank1  = WJD::phrase_item_bank,
  item_bank2  = Berkowitz::phrase_item_bank,
  item_bank_names  = c("WJD", "Berkowitz"),
  standardise = TRUE,
  pa_type     = "pc",
  rotate      = "equamax",
  name_top    = 3L,
  verbose     = TRUE
)
```



## References

Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021). Expert musical improvisations contain sequencing biases seen in language production. Journal of Experimental Psychology: General. https://doi.org/10.1037/xge0001107

Berkowitz, S., Fontrier, G., Goldstein, P., & Smaldone, E. (2017). A new approach to sight singing (6th ed.). W. W. Norton & Company.

Crayencour, H.-C., Velichkina, O., Frieler, K., Höger, F., Pfleiderer, M., Henry, L., Solis, G., Wolff, D., Weyde, T., Peeters, G., Basaran, D., Smith, J., & Proutskova, P. (2021). The DTL1000 Jazz Solo Dataset (in preparation). Journal on Computing and Cultural Heritage.

Müllensiefen, D. (2009). FANTASTIC: Feature ANalysis Technology Accessing STatistics In a Corpus (Technical report No. 37).

Pfleiderer, M., Frieler, K., Abeßer, J., Zaddach, W.-G., & Burkhart, B. (Eds.). (2017). Inside the Jazzomat—New perspectives for jazz research. Schott Campus.

Slonimsky, N. (1947). Thesaurus of scales and melodic patterns. Literary Licensing, LLC.

