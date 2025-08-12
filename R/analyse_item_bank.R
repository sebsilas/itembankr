

# res1 <- analyse_item_bank(
#   item_bank = WJD::phrase_item_bank,
#   bank_name = "WJD",
#   standardise = TRUE,
#   pa_type = "pc",
#   rotate = "equamax",
#   name_top = 2L,
#   max_plots = 24,
#   verbose = TRUE
# )

# res1$violin_plot
# res1$pca$density_plot
# res1$descriptives
# res1$pca$score_summary

# # --- Categorical outputs ---
# # 1. Long form data
# res1$categoricals$long
#
# # 2. Summary table (n_levels, entropy, top level, top_prop)
# res1$categoricals$summary |>
#   dplyr::arrange(desc(entropy)) |>
#   dplyr::slice_head(n = 10)  # features with highest diversity
#
# # 3. Stacked bar plot for top levels
# res1$categoricals$plot


#' Analyse a single item bank (music corpus)
#'
#' Summarises feature distributions, runs a robust parallel analysis,
#' fits PCA via `psych::principal()`, and names components by their
#' top-loading features.
#'
#' @param item_bank Data frame with (mostly) numeric feature columns.
#' @param bank_name Character name for the bank (used in plots/tables).
#' @param include,exclude Optional tidyselect/character to filter features.
#' @param id_cols Character of columns to exclude from features.
#' @param standardise Logical; scale/center features before PA/PCA.
#' @param pa_type "pc" or "fa" for how many dimensions to retain from PA.
#' @param max_plots Cap the number of violin facets.
#' @param seed RNG seed.
#' @param verbose Print CLI progress/tables.
#' @param rotate Rotation for `psych::principal()`: one of
#'   "none","varimax","promax","oblimin","equamax".
#' @param name_top Integer; how many top-loading features to include in
#'   each component's label (default 2).
#'
#' @return A list: descriptives tibble, violin plot, PA selection,
#'         PCA loadings/scores (named), and PC score summaries/plot.
#' @export
analyse_item_bank <- function(
    item_bank,
    bank_name = "Bank",
    include = NULL,
    exclude = NULL,
    id_cols = "item_id",
    standardise = TRUE,
    pa_type = c("pc","fa"),
    max_plots = 36,
    seed = 123,
    verbose = TRUE,
    rotate = c("none", "varimax", "promax", "oblimin", "equamax"),
    name_top = 2
) {
  pa_type <- match.arg(pa_type)
  rotate  <- match.arg(rotate)
  set.seed(seed)

  if (verbose) {
    cli::cli_h1("Analysing item bank")
    cli::cli_text("{.strong Bank}: {.val {bank_name}}")
  }

  # -- Clean names, select numeric, tidy long (reuse bivariate style) ------
  sb <- prepare_single_bank(
    item_bank,
    bank_name = bank_name,
    include = include,
    exclude = exclude,
    id_cols  = id_cols,
    verbose  = verbose
  )
  x <- sb$x
  long <- sb$long

  if (verbose) cli::cli_rule("Descriptive statistics")
  desc_tbl <- summarise_descriptives(long)

  # Plot: we can reuse the violin helper by keeping a single '.bank'
  p_violin <- make_violin_plot(long, bank_names = bank_name, max_plots = max_plots)

  # categorical: single-bank
  cats1 <- prepare_single_categoricals(item_bank, bank_name,
                                       include = include, exclude = exclude,
                                       id_cols = id_cols, verbose = verbose)
  cat_long1 <- cats1$long
  cat_sum1  <- summarise_categoricals_single(cat_long1)
  p_cat1    <- cat_sum1$plot
  cat_summary1 <- cat_sum1$summary

  if (verbose && nrow(cat_long1) > 0) {
    cli::cli_rule("Categorical summaries")
    cli::cli_text(cli::col_cyan("Per-feature level counts/entropy (first 10):"))
    print(dplyr::slice_head(cat_summary1, n = 10))
  }


  # -- Robust prep for PA/PCA (reuse two-bank helper by passing x twice) ---
  prep <- prepare_for_components(x, x, bank_names = c(bank_name, bank_name),
                                 max_na_prop = 0.5, verbose = verbose)
  x_c <- prep$x1_clean
  x_i <- prep$x1_imp
  if (ncol(x_c) == 0L) stop("No features left after cleaning; cannot run PA/PCA.")

  # Standardise for PA input if requested
  scale_if <- function(df) if (standardise) as.data.frame(scale(df)) else df
  x_pa <- scale_if(x_i)

  # -- Parallel analysis (single) ------------------------------------------
  if (verbose) cli::cli_rule("Parallel analysis")
  n_pa <- safe_parallel_n(x_pa, pa_type = pa_type)
  if (verbose) cli::cli_text("{.strong PA components (type: {pa_type})}: {cli::col_green(n_pa)}")

  # -- PCA via psych::principal (on cleaned, non-imputed; impute inside) ---
  k <- min(n_pa, ncol(x_c), nrow(x_c) - 1L)
  pr <- run_pca_named(
    x_c, k = k, standardise = standardise, prefix = "PC",
    impute = "median", rotate = rotate, name_top = name_top
  )

  pcs <- summarise_single_pcs(pr, verbose = verbose)
  p_pc_density <- pcs$density_plot
  comp_summary <- pcs$summary

  # -- Pretty tables --------------------------------------------------------
  if (verbose) print_tables_cli_single(desc_tbl, comp_summary)

  # -- Return ---------------------------------------------------------------
  out <- list(
    bank = bank_name,
    kept_for_components    = prep$kept_features,
    dropped_for_components = prep$dropped_features,
    descriptives = desc_tbl,
    categoricals = list(
      long = cat_long1,
      summary = cat_summary1,
      plot = p_cat1
    ),
    violin_plot = p_violin,
    pa = list(n = n_pa, type = pa_type),
    pca = list(
      model = pr,                       # rotation (loadings) + x (scores)
      score_summary = comp_summary,
      density_plot  = p_pc_density
    )
  )
  class(out) <- c("itembank_single", class(out))
  invisible(out)
}


# Clean a single bank + make a long tibble compatible with make_violin_plot()
prepare_single_bank <- function(item_bank,
                                bank_name = "Bank",
                                include = NULL, exclude = NULL, id_cols = NULL,
                                verbose = TRUE) {
  ib <- janitor::clean_names(item_bank)

  num_cols <- function(df) names(df)[vapply(df, is.numeric, logical(1))]
  all <- num_cols(ib)

  if (!is.null(id_cols)) {
    id_cols <- intersect(id_cols, names(ib))
    all <- setdiff(all, id_cols)
  }

  pick <- all
  if (!is.null(include)) {
    if (is.character(include)) {
      pick <- intersect(pick, include)
    } else {
      pick <- names(dplyr::select(tibble::as_tibble(ib), {{ include }}))
      pick <- intersect(pick, all)
    }
  }
  if (!is.null(exclude)) {
    if (is.character(exclude)) {
      pick <- setdiff(pick, exclude)
    } else {
      drop <- names(dplyr::select(tibble::as_tibble(ib), {{ exclude }}))
      pick <- setdiff(pick, drop)
    }
  }

  common <- pick
  if (length(common) == 0) stop("No numeric features found after include/exclude.")

  if (verbose) {
    cli::cli_rule("Feature selection")
    cli::cli_text("{.strong Numeric}: {length(all)} | {.strong Used}: {length(common)}")
  }

  x <- tibble::as_tibble(ib[, common, drop = FALSE])
  long <- x |>
    dplyr::mutate(.bank = bank_name) |>
    tidyr::pivot_longer(cols = dplyr::all_of(common), names_to = "feature", values_to = "value")

  list(x = x, long = long, features = common)
}

# Summaries and density plot for a single-bank PCA (prcomp-like {rotation, x})
summarise_single_pcs <- function(pr, verbose = TRUE) {
  scores <- tibble::as_tibble(pr$x)
  if (ncol(scores) == 0L) {
    if (verbose) cli::cli_alert_warning("No principal components available after cleaning.")
    return(list(
      summary = tibble::tibble(),
      density_plot = ggplot2::ggplot() + ggplot2::labs(title = "No PCs to plot")
    ))
  }

  sc_long <- scores |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "component", values_to = "score")

  comp_summary <- sc_long |>
    dplyr::group_by(component) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(score, na.rm = TRUE),
      sd = stats::sd(score, na.rm = TRUE),
      median = stats::median(score, na.rm = TRUE),
      .groups = "drop"
    )

  p_pc_density <- ggplot2::ggplot(sc_long, ggplot2::aes(x = score)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~ component, scales = "free") +
    ggplot2::labs(title = "PC score densities", x = "Score", y = "Density") +
    ggplot2::theme_minimal(base_size = 12)

  if (verbose) cli::cli_alert_success("PC summaries computed.")
  list(summary = comp_summary, density_plot = p_pc_density)
}

# CLI printer tailored to the single-bank workflow
print_tables_cli_single <- function(desc_tbl, comp_summary) {
  cli::cli_rule("Tables")
  cli::cli_text(cli::col_cyan("Descriptives (first 20 rows):"))
  print(dplyr::slice_head(desc_tbl, n = 20))

  cli::cli_text(cli::col_cyan("PC score summaries:"))
  print(comp_summary)
}


# ---------- CATEGORICAL (factor/character) HELPERS ----------


# Single-bank categorical prep (long format with .bank)
prepare_single_categoricals <- function(item_bank, bank_name,
                                        include = NULL, exclude = NULL,
                                        id_cols = NULL, verbose = TRUE) {
  # always exclude these from categorical analyses
  always_exclude <- c("abs_melody", "durations", "item_type", "melody")

  ib <- janitor::clean_names(item_bank)
  all <- cat_cols(ib)  # factor/character/logical -> categorical

  # remove id columns if present
  if (!is.null(id_cols)) all <- setdiff(all, intersect(id_cols, names(ib)))

  pick <- all

  # include (character or tidyselect)
  if (!is.null(include)) {
    if (is.character(include)) {
      pick <- intersect(pick, include)
    } else {
      nm <- names(dplyr::select(tibble::as_tibble(ib), {{ include }}))
      pick <- intersect(pick, nm)
    }
  }

  # exclude (character or tidyselect)
  if (!is.null(exclude)) {
    if (is.character(exclude)) {
      pick <- setdiff(pick, exclude)
    } else {
      drop <- names(dplyr::select(tibble::as_tibble(ib), {{ exclude }}))
      pick <- setdiff(pick, drop)
    }
  }

  # finally: hard exclusions
  pre_len <- length(pick)
  pick <- setdiff(pick, always_exclude)
  hard_drop_n <- pre_len - length(pick)

  if (length(pick) == 0L) {
    if (verbose) {
      cli::cli_rule("Categorical features (single)")
      cli::cli_alert_warning("No categorical features after exclusions.")
    }
    return(list(long = tibble::tibble(), features = character()))
  }

  long <- tibble::as_tibble(ib[, pick, drop = FALSE]) |>
    dplyr::mutate(.bank = bank_name) |>
    tidyr::pivot_longer(cols = dplyr::all_of(pick),
                        names_to = "feature", values_to = "level") |>
    dplyr::mutate(level = ifelse(is.na(level), "(Missing)", as.character(level)))

  if (verbose) {
    cli::cli_rule("Categorical features (single)")
    cli::cli_text("{.strong Used}: {length(pick)}")
    if (hard_drop_n > 0) {
      cli::cli_text(cli::col_yellow("Hard-excluded: {hard_drop_n} ({.val {paste(always_exclude, collapse = ', ')}})"))
    }
  }

  list(long = long, features = pick)
}





# SINGLE-bank categorical summaries (with always-exclude filter)
summarise_categoricals_single <- function(long_cat, top_levels = 10) {
  # Always exclude these features from categorical analyses
  always_exclude <- c("abs_melody", "durations", "item_type", "melody")

  # Filter them out (only if columns present)
  if (!is.null(long_cat) && nrow(long_cat) > 0 &&
      all(c("feature", "level") %in% names(long_cat))) {
    long_cat <- dplyr::filter(long_cat, !(feature %in% always_exclude))
  }

  # Nothing left? Return empty-but-compatible outputs
  if (is.null(long_cat) || nrow(long_cat) == 0L) {
    return(list(
      summary = tibble::tibble(),
      plot    = ggplot2::ggplot() + ggplot2::labs(title = "No categorical features to show")
    ))
  }

  # per-feature counts & proportions
  sum_tbl <- long_cat |>
    dplyr::group_by(feature, level) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup()

  # Shannon entropy helper
  entropy_shannon <- function(p) { p <- p[p > 0]; -sum(p * log(p)) }

  feat_stats <- sum_tbl |>
    dplyr::group_by(feature) |>
    dplyr::summarise(
      n_levels = dplyr::n_distinct(level),
      entropy  = entropy_shannon(prop),
      top_level = level[which.max(prop)],
      top_prop  = max(prop),
      .groups = "drop"
    )

  # stacked proportions (collapse to top N levels per feature)
  plot_tbl <- sum_tbl |>
    dplyr::group_by(feature) |>
    dplyr::mutate(rank = dplyr::min_rank(dplyr::desc(n))) |>
    dplyr::mutate(level_plot = ifelse(rank <= top_levels, level, "Other")) |>
    dplyr::group_by(feature, level_plot) |>
    dplyr::summarise(n = sum(n), .groups = "drop") |>
    dplyr::group_by(feature) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup()

  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = feature, y = prop, fill = level_plot)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Categorical distributions (top levels)",
                  x = NULL, y = "Proportion", fill = "Level") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  list(summary = feat_stats, plot = p)
}


