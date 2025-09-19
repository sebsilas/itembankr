


#' Compare two item banks (music corpora) by feature distributions and components
#'
#' @param item_bank1,item_bank2 Data frames with (mostly) numeric feature columns.
#' @param item_bank_names Character length-2 naming the banks.
#' @param include,exclude Optional tidyselect/character to filter features.
#' @param id_cols Character of columns to exclude from features.
#' @param standardise Logical; scale/center features before PCA/PA.
#' @param pa_type "pc" or "fa" for how many dimensions to retain from PA.
#' @param max_plots Cap the number of violin facets.
#' @param seed RNG seed for reproducibility.
#' @param verbose Print CLI progress/tables.
#' @param rotate Rotation method passed to psych::principal(): One of "none", "varimax", "promax", "oblimin", "equamax" (default: "none").
#' @param name_top Integer; how many top-loading features to include in each component's label (default 2).
#' @return A list with tibbles, ggplots, and PA/PCA objects.
#' @examples
#' set.seed(123)
#'
#'
# res <- compare_item_banks(
#   item_bank1  = WJD::phrase_item_bank,
#   item_bank2  = Berkowitz::phrase_item_bank,
#   item_bank_names  = c("WJD", "Berkowitz"),
#   standardise = TRUE,
#   pa_type     = "pc",
#   rotate      = "varimax",
#   name_top    = 2,
#   verbose     = FALSE
# )
#'
#' # --- Numeric summaries & differences ---
#' head(res$descriptives)
#' res$differences |>
#'   dplyr::arrange(dplyr::desc(abs(cohen_d))) |>
#'   dplyr::slice_head(n = 5)
#'
#' # --- PCA outputs ---
#' # Named loadings (top-loadings in column names)
#' colnames(res$pca$joint$rotation)
#' # Score summaries by bank
#' res$pca$joint_score_summary
#'
#' # --- Categorical outputs ---
#' # Long rows (feature, .bank, level)
#' res$categoricals$long |> dplyr::distinct(feature) |> dplyr::arrange(feature)
#' # Tests per feature (chi-square/Fisher, Cramér's V, JS divergence)
#' res$categoricals$tests |>
#'   dplyr::arrange(dplyr::desc(cramers_v)) |>
#'   dplyr::slice_head(n = 5)
#' # Per-bank, per-level proportions
#' res$categoricals$proportions |> dplyr::slice_head(n = 10)
#'
#' # --- Plots (not run on CRAN) ---
#' \donttest{
#'   res$violin_plot
#'   res$categoricals$plot
#'   res$pca$joint_density_plot
#' }
#'
#'
#' @export
compare_item_banks <- function(
    item_bank1,
    item_bank2,
    item_bank_names = c("Bank A", "Bank B"),
    include = NULL,
    exclude = "ngrukkon",
    id_cols = c("item_id", "parent_durations", "parent_abs_melody"),
    standardise = TRUE,
    pa_type = c("pc", "fa"),
    max_plots = 36,
    seed = 123,
    verbose = TRUE,
    rotate = c("none", "varimax", "promax", "oblimin", "equamax"),
    name_top = 2
) {
  pa_type <- match.arg(pa_type)
  set.seed(seed)

  # helper to drop heavy plot data
  strip_ggplot_data <- function(p) {
    if (inherits(p, "gg")) {
      p$data <- NULL
      p$layers <- lapply(p$layers, function(layer) {
        layer$data <- NULL
        layer
      })
    }
    p
  }

  if (verbose) {
    cli::cli_h1("Comparing item banks")
    cli::cli_text("{.strong Banks}: {.val {item_bank_names[1]}} vs {.val {item_bank_names[2]}}")
  }

  # 1) Clean names, align features
  ib <- prepare_item_banks(
    item_bank1, item_bank2,
    include = include, exclude = exclude, id_cols = id_cols,
    item_bank_names = item_bank_names, verbose = verbose
  )
  x1 <- ib$x1; x2 <- ib$x2; xjoint <- ib$xjoint
  long <- ib$long

  # 2) Descriptives & differences (on raw overlap)
  if (verbose) cli::cli_rule("Descriptive statistics")
  desc_tbl <- summarise_descriptives(long)
  diffs_tbl <- compute_feature_differences(long, item_bank_names)

  # 3) Plots (strip data afterwards)
  p_violin <- make_violin_plot(long, item_bank_names, max_plots = max_plots)
  p_violin <- strip_ggplot_data(p_violin)

  # 3b) Categorical comparisons
  cats <- prepare_categoricals_both(item_bank1, item_bank2, item_bank_names,
                                    include = include, exclude = exclude,
                                    id_cols = id_cols, verbose = verbose)
  cat_long <- cats$long
  cat_cmp  <- compare_categoricals_both(cat_long, item_bank_names)
  cat_tests <- cat_cmp$tests
  p_cat <- strip_ggplot_data(cat_cmp$plot)

  if (verbose && nrow(cat_long) > 0) {
    cli::cli_rule("Categorical comparisons")
    cli::cli_text(cli::col_cyan("Top features by Cramér's V (first 10):"))
    print(dplyr::slice_head(cat_tests, n = 10))
  }

  # 4) Robust prep for PA/PCA (drop bad cols, impute)
  prep <- prepare_for_components(x1, x2, item_bank_names, max_na_prop = 0.5, verbose = verbose)
  x1_c <- prep$x1_clean; x2_c <- prep$x2_clean; xj_c <- prep$xjoint_clean
  x1_i <- prep$x1_imp;   x2_i <- prep$x2_imp;   xj_i <- prep$xjoint_imp

  if (ncol(x1_c) == 0L) stop("No features left after cleaning; cannot run PA/PCA.")

  # standardise for PA inputs if requested
  scale_if <- function(df) if (standardise) as.data.frame(scale(df)) else df
  x1_pa <- scale_if(x1_i); x2_pa <- scale_if(x2_i); xj_pa <- scale_if(xj_i)

  # 5) Parallel analysis (safe; with Kaiser fallback)
  if (verbose) cli::cli_rule("Parallel analysis")
  pa <- run_parallel_analysis(
    x1_imp = x1_pa, x2_imp = x2_pa, xjoint_imp = xj_pa,
    pa_type = pa_type, item_bank_names = item_bank_names, verbose = verbose
  )

  # 6) PCA on cleaned (non-imputed) data, k capped to available columns
  k1 <- min(pa$each[[item_bank_names[1]]]$n, ncol(x1_c), nrow(x1_c) - 1L)
  k2 <- min(pa$each[[item_bank_names[2]]]$n, ncol(x2_c), nrow(x2_c) - 1L)
  kj <- min(pa$joint$n,                 ncol(xj_c), nrow(xj_c) - 1L)

  pr1 <- run_pca_named(x1_c, k = k1, standardise = standardise, prefix = "PC",
                       impute = "median", rotate = rotate, name_top = name_top)
  pr2 <- run_pca_named(x2_c, k = k2, standardise = standardise, prefix = "PC",
                       impute = "median", rotate = rotate, name_top = name_top)
  prj <- run_pca_named(xj_c, k = kj, standardise = standardise, prefix = "Joint PC",
                       impute = "median", rotate = rotate, name_top = name_top)

  pcs <- summarise_joint_pcs(prj, n1 = nrow(x1_c), item_bank_names = item_bank_names, verbose = verbose)
  p_pc_density <- strip_ggplot_data(pcs$density_plot)
  comp_summary <- pcs$summary

  # 7) Correlation-structure similarity (use imputed-clean for stable R)
  if (verbose) cli::cli_rule("Correlation similarity")
  mantel_res <- mantel_similarity(x1_imp = x1_i, x2_imp = x2_i, verbose = verbose)

  # 8) Pretty console tables
  if (verbose) print_tables_cli(desc_tbl, diffs_tbl, comp_summary)

  # 9) Return
  out <- list(
    banks = item_bank_names,
    features_overlap = ib$feature_map,
    dropped_for_components = prep$dropped_features,
    kept_for_components = prep$kept_features,
    descriptives = desc_tbl,
    differences = diffs_tbl,
    categoricals = list(
      tests = cat_tests,
      proportions = cat_cmp$proportions,
      plot = p_cat
    ),
    violin_plot = p_violin,
    pa = pa,
    pca = list(
      each = setNames(list(pr1, pr2), item_bank_names),
      joint = prj,
      joint_score_summary = comp_summary,
      joint_density_plot = p_pc_density
    ),
    extras = list(mantel = mantel_res)
  )
  class(out) <- c("itembank_compare", class(out))
  invisible(out)
}

# ---------------------------- helpers ----------------------------

prepare_item_banks <- function(item_bank1, item_bank2,
                               include = NULL, exclude = NULL, id_cols = NULL,
                               item_bank_names = c("Bank A","Bank B"),
                               verbose = TRUE) {
  ib1 <- janitor::clean_names(item_bank1)
  ib2 <- janitor::clean_names(item_bank2)

  num_cols <- function(df) names(df)[vapply(df, is.numeric, logical(1))]
  all1 <- num_cols(ib1); all2 <- num_cols(ib2)

  if (!is.null(id_cols)) {
    id_cols <- intersect(id_cols, union(names(ib1), names(ib2)))
    all1 <- setdiff(all1, id_cols)
    all2 <- setdiff(all2, id_cols)
  }

  pick <- union(all1, all2)
  if (!is.null(include)) {
    if (is.character(include)) {
      pick <- intersect(pick, include)
    } else {
      pick <- names(dplyr::select(tibble::as_tibble(ib1[intersect(names(ib1), pick)]), {{ include }}))
    }
  }
  if (!is.null(exclude)) {
    if (is.character(exclude)) {
      pick <- setdiff(pick, exclude)
    } else {
      drop <- names(dplyr::select(tibble::as_tibble(ib1[intersect(names(ib1), pick)]), {{ exclude }}))
      pick <- setdiff(pick, drop)
    }
  }

  common <- intersect(intersect(all1, all2), pick)
  if (length(common) == 0) stop("No overlapping numeric features found.")

  if (verbose) {
    cli::cli_rule("Feature alignment")
    cli::cli_text("{.strong Numeric in {item_bank_names[1]}}: {length(all1)} | {.strong in {item_bank_names[2]}}: {length(all2)}")
    cli::cli_text("{.strong Overlap used}: {length(common)} features")
  }

  df1 <- tibble::as_tibble(ib1[, common, drop = FALSE]) |> dplyr::mutate(.bank = item_bank_names[1])
  df2 <- tibble::as_tibble(ib2[, common, drop = FALSE]) |> dplyr::mutate(.bank = item_bank_names[2])
  long <- dplyr::bind_rows(df1, df2) |>
    tidyr::pivot_longer(cols = dplyr::all_of(common), names_to = "feature", values_to = "value")

  feature_map <- tibble::tibble(
    feature = common,
    in_bank1 = feature %in% names(ib1),
    in_bank2 = feature %in% names(ib2)
  )

  list(
    x1 = tibble::as_tibble(ib1[, common, drop = FALSE]),
    x2 = tibble::as_tibble(ib2[, common, drop = FALSE]),
    xjoint = dplyr::bind_rows(
      tibble::as_tibble(ib1[, common, drop = FALSE]),
      tibble::as_tibble(ib2[, common, drop = FALSE])
    ),
    long = long,
    common = common,
    feature_map = feature_map
  )
}

summarise_descriptives <- function(long) {
  long |>
    dplyr::group_by(.bank, feature) |>
    dplyr::summarise(
      n      = dplyr::n(),
      mean   = mean(value, na.rm = TRUE),
      sd     = stats::sd(value, na.rm = TRUE),
      median = stats::median(value, na.rm = TRUE),
      mad    = stats::mad(value, na.rm = TRUE),
      min    = suppressWarnings(min(value, na.rm = TRUE)),
      max    = suppressWarnings(max(value, na.rm = TRUE)),
      .groups = "drop"
    )
}

compute_feature_differences <- function(long, item_bank_names) {
  pooled_sd <- function(x, y) {
    nx <- sum(!is.na(x)); ny <- sum(!is.na(y))
    sx <- stats::sd(x, na.rm = TRUE); sy <- stats::sd(y, na.rm = TRUE)
    sqrt(((nx - 1)*sx^2 + (ny - 1)*sy^2) / (nx + ny - 2))
  }

  purrr::map_dfr(unique(long$feature), function(f) {
    x <- long$value[long$feature == f & long$.bank == item_bank_names[1]]
    y <- long$value[long$feature == f & long$.bank == item_bank_names[2]]
    pd <- pooled_sd(x, y)
    d  <- (mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) / pd
    ks_p <- tryCatch(stats::ks.test(x, y)$p.value, error = function(e) NA_real_)
    tibble::tibble(
      feature = f,
      mean_diff = mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE),
      cohen_d = d,
      ks_p = ks_p
    )
  }) |>
    dplyr::mutate(sig = dplyr::case_when(
      is.na(ks_p) ~ "NA",
      ks_p < 0.001 ~ "***",
      ks_p < 0.01  ~ "**",
      ks_p < 0.05  ~ "*",
      TRUE ~ ""
    )) |>
    dplyr::arrange(dplyr::desc(abs(cohen_d)))
}

make_violin_plot <- function(long, item_bank_names, max_plots = 36, remove_features = NULL) {

  # Define feature families (with "General Information")
  feature_families <- tibble::tibble(
    feature = c(
      # Frequency-related
      "freq", "rel_freq", "log_freq", "IDF",
      # Pitch / interval
      "span", "mean_int_size", "int_range", "dir_change",
      "mean_dir_change", "int_variety", "i_entropy", "pitch_variety", "mean_run_length",
      # Tonality
      "tonalness", "tonal_clarity", "tonal_spike",
      # Contour
      "step_cont_glob_var", "step_cont_glob_dir",
      # Rhythm / duration
      "d_entropy", "d_eq_trans", "mean_duration",
      # General information
      "n", "mean_information_content"
    ),
    family = c(
      rep("Frequency", 4),
      rep("Pitch / Interval", 9),
      rep("Tonality", 3),
      rep("Contour", 2),
      rep("Rhythm / Duration", 3),
      rep("General Information", 2)
    )
  )

  # Filter features
  feats <- setdiff(sort(unique(long$feature)), remove_features %||% character())
  feats <- head(feats, min(length(feats), max_plots))
  plot_dat <- dplyr::filter(long, feature %in% feats)

  # Add family info
  plot_dat <- dplyr::left_join(plot_dat, feature_families, by = c("feature"))

  # Drop unmapped features
  plot_dat <- dplyr::filter(plot_dat, !is.na(family))

  # Rescale values to [0,1] within each feature
  plot_dat <- plot_dat %>%
    dplyr::group_by(feature) %>%
    dplyr::mutate(value = (value - min(value, na.rm = TRUE)) /
                    (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>%
    dplyr::ungroup()

  # Make feature a factor restricted to used features
  plot_dat <- plot_dat %>%
    dplyr::mutate(feature = factor(feature, levels = unique(feature)))

  ggplot2::ggplot(
    plot_dat,
    ggplot2::aes(x = feature, y = value, fill = .bank)
  ) +
    ggplot2::geom_violin(trim = TRUE, alpha = 0.6,
                         position = ggplot2::position_dodge(width = 0.8)) +
    ggplot2::geom_boxplot(width = 0.12, outlier.alpha = 0.25,
                          position = ggplot2::position_dodge(width = 0.8)) +
    ggplot2::facet_wrap(~ family, scales = "free_x") +
    ggplot2::labs(
      title = "Feature distributions by bank (rescaled 0–1)",
      subtitle = paste0("Showing up to ", length(feats), " features"),
      x = "Feature", y = "Rescaled value (0–1)", fill = "Bank"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold")
    )
}





# ---------- Robust PA/PCA prep & PA (safe) ----------

prepare_for_components <- function(x1, x2, item_bank_names, max_na_prop = 0.5, verbose = TRUE) {
  # replace Inf with NA
  fix_inf <- function(df) dplyr::mutate(df, dplyr::across(dplyr::everything(),
                                                          ~ ifelse(is.infinite(.x), NA_real_, .x)))
  x1 <- fix_inf(x1); x2 <- fix_inf(x2)

  feat_stats <- tibble::tibble(
    feature = colnames(x1),
    var1 = purrr::map_dbl(x1, ~ stats::var(.x, na.rm = TRUE)),
    var2 = purrr::map_dbl(x2, ~ stats::var(.x, na.rm = TRUE)),
    na1  = purrr::map_dbl(x1, ~ mean(is.na(.x))),
    na2  = purrr::map_dbl(x2, ~ mean(is.na(.x)))
  )

  drop_zero_var <- feat_stats$feature[feat_stats$var1 == 0 | feat_stats$var2 == 0 |
                                        is.na(feat_stats$var1) | is.na(feat_stats$var2)]
  drop_too_na   <- feat_stats$feature[feat_stats$na1 > max_na_prop | feat_stats$na2 > max_na_prop]
  drop <- union(drop_zero_var, drop_too_na)
  keep <- setdiff(colnames(x1), drop)

  if (verbose) {
    cli::cli_rule("Prepping for components")
    cli::cli_text("Keeping {.val {length(keep)}} features for PA/PCA (from {ncol(x1)}).")
    if (length(drop)) {
      why <- dplyr::filter(feat_stats, feature %in% drop) |>
        dplyr::mutate(reason = dplyr::case_when(
          var1 == 0 | var2 == 0 ~ "zero variance",
          is.na(var1) | is.na(var2) ~ "variance NA",
          na1 > max_na_prop | na2 > max_na_prop ~ paste0("> ", 100*max_na_prop, "% NA"),
          TRUE ~ "other"
        )) |>
        dplyr::select(feature, reason)
      cli::cli_text(cli::col_yellow("Dropped features:"))
      print(why)
    }
  }

  x1k <- x1[, keep, drop = FALSE]
  x2k <- x2[, keep, drop = FALSE]
  xjk <- dplyr::bind_rows(x1k, x2k)

  # median imputation for PA/PCA stability
  impute_med <- function(df) dplyr::mutate(df, dplyr::across(dplyr::everything(), ~ {
    m <- stats::median(.x, na.rm = TRUE)
    if (is.finite(m)) dplyr::if_else(is.na(.x), m, .x) else .x
  }))

  list(
    x1_clean = x1k,
    x2_clean = x2k,
    xjoint_clean = xjk,
    x1_imp = impute_med(x1k),
    x2_imp = impute_med(x2k),
    xjoint_imp = impute_med(xjk),
    kept_features = keep,
    dropped_features = drop
  )
}

safe_parallel_n <- function(df_imp, pa_type = c("pc","fa")) {
  pa_type <- match.arg(pa_type)
  out <- tryCatch({
    tmp <- utils::capture.output(
      psych::fa.parallel(df_imp, fm = "minres", fa = "both", plot = FALSE, error.bars = FALSE),
      type = "output"
    )
    # fa.parallel returns (invisibly) a list; capture.output just swallows prints
    psych::fa.parallel(df_imp, fm = "minres", fa = "both", plot = FALSE, error.bars = FALSE)
  }, error = function(e) NULL)

  if (!is.null(out)) {
    if (pa_type == "pc") out$ncomp else out$nfact
  } else {
    R <- stats::cor(df_imp, use = "pairwise.complete.obs")
    R[!is.finite(R)] <- 0
    ev <- eigen(R, only.values = TRUE)$values
    n <- sum(ev >= 1)
    max(n, 1L)
  }
}


run_parallel_analysis <- function(x1_imp, x2_imp, xjoint_imp,
                                  pa_type = c("pc","fa"),
                                  item_bank_names = c("Bank A","Bank B"),
                                  verbose = TRUE) {
  pa_type <- match.arg(pa_type)

  nc1 <- safe_parallel_n(x1_imp, pa_type)
  nc2 <- safe_parallel_n(x2_imp, pa_type)
  ncj <- safe_parallel_n(xjoint_imp, pa_type)

  if (verbose) {
    cli::cli_text("{.strong PA components (type: {pa_type})}:")
    cli::cli_text("• {item_bank_names[1]}: {cli::col_green(nc1)}  • {item_bank_names[2]}: {cli::col_green(nc2)}  • joint: {cli::col_green(ncj)}")
  }

  each_list <- setNames(
    list(list(results = NULL, n = nc1),
         list(results = NULL, n = nc2)),
    item_bank_names
  )

  list(
    each  = each_list,
    joint = list(results = NULL, n = ncj),
    type  = pa_type
  )
}


# ---------------------------- PCA & summaries ----------------------------

# PCA via psych::principal, returned in a prcomp-like shape
run_pca_named <- function(x, k = NULL, standardise = TRUE, prefix = "PC",
                          impute = c("median", "none"),
                          rotate = c("none", "varimax", "promax", "oblimin", "equamax"),
                          name_top = 2) {


  impute <- match.arg(impute)
  rotate <- match.arg(rotate)

  # 1) matrix + median imputation (keep this even if rotate != "none")
  X <- as.matrix(x)
  if (impute == "median") {
    col_meds <- apply(X, 2, function(col) stats::median(col[is.finite(col)], na.rm = TRUE))
    for (j in seq_len(ncol(X))) {
      bad <- !is.finite(X[, j])
      if (any(bad)) X[bad, j] <- col_meds[j]
    }
  }

  # 2) optional standardization
  if (standardise) {
    X <- scale(X)
  }

  # 3) drop zero-variance / non-finite cols (belt & suspenders)
  col_var <- apply(X, 2, stats::var, na.rm = TRUE)
  keep    <- is.finite(col_var) & (col_var > 0)
  X       <- X[, keep, drop = FALSE]

  if (ncol(X) == 0L) {
    return(list(rotation = matrix(numeric(0), nrow = 0, ncol = 0),
                x = matrix(numeric(0), nrow = nrow(X), ncol = 0)))
  }

  # 4) choose k safely
  if (!is.null(k)) {
    k <- max(1L, min(k, ncol(X), nrow(X) - 1L))
  } else {
    k <- min(ncol(X), nrow(X) - 1L)
  }

  # 5) run psych::principal
  #    - rotate = "none" keeps it comparable to prcomp; change if you want rotated components
  #    - scores = TRUE returns regression scores
  pc <- psych::principal(X, nfactors = k, rotate = rotate, scores = TRUE)

  # 6) coerce to your expected shape
  rot <- as.matrix(pc$loadings)              # p x k
  scr <- as.matrix(pc$scores)                # n x k (may be NULL if scores=FALSE)

  # 7) (re)name components by top-2 loading features
  name_components <- function(rot_mat) {
    if (nrow(rot_mat) == 0 || ncol(rot_mat) == 0) return(colnames(rot_mat))
    sapply(seq_len(ncol(rot_mat)), function(j) {
      # pick top-|name_top| features by absolute loading
      n_pick <- max(1L, min(name_top, nrow(rot_mat)))
      top_idx <- order(abs(rot_mat[, j]), decreasing = TRUE)[seq_len(n_pick)]
      top_feats <- rownames(rot_mat)[top_idx]
      paste0(prefix, " ", j, ": ", paste(top_feats, collapse = " + "))
    }, USE.NAMES = FALSE)
  }


  comp_names <- name_components(rot)
  if (length(comp_names)) {
    colnames(rot) <- comp_names
    if (!is.null(scr)) colnames(scr) <- comp_names
  }

  # 8) return prcomp-like minimal list
  list(rotation = rot, x = scr)
}




summarise_joint_pcs <- function(prj, n1, item_bank_names, verbose = TRUE) {
  scores <- tibble::as_tibble(prj$x)
  if (ncol(scores) == 0L) {
    if (verbose) cli::cli_alert_warning("No principal components available after cleaning.")
    return(list(
      summary = tibble::tibble(),
      density_plot = ggplot2::ggplot() + ggplot2::labs(title = "No PCs to plot")
    ))
  }

  scj_comp <- dplyr::bind_cols(
    tibble::tibble(.bank = c(rep(item_bank_names[1], n1), rep(item_bank_names[2], nrow(scores) - n1))),
    scores
  ) |>
    tidyr::pivot_longer(-.bank, names_to = "component", values_to = "score")

  comp_summary <- scj_comp |>
    dplyr::group_by(.bank, component) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(score, na.rm = TRUE),
      sd = stats::sd(score, na.rm = TRUE),
      median = stats::median(score, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(names_from = .bank, values_from = c(n, mean, sd, median))

  if (verbose) cli::cli_alert_success("Joint PC summaries computed.")

  p_pc_density <- ggplot2::ggplot(scj_comp, ggplot2::aes(x = score, color = .bank)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~ component, scales = "free") +
    ggplot2::labs(title = "Item Banks Compared on Joint Principal Components", x = "Score", y = "Density", color = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  list(summary = comp_summary, density_plot = p_pc_density)
}

# ---------------------------- Similarity & printing ----------------------------

mantel_similarity <- function(x1_imp, x2_imp, verbose = TRUE) {
  c1 <- stats::cor(x1_imp, use = "pairwise.complete.obs")
  c2 <- stats::cor(x2_imp, use = "pairwise.complete.obs")
  d1 <- stats::as.dist(1 - c1)
  d2 <- stats::as.dist(1 - c2)
  res <- vegan::mantel(d1, d2)
  if (verbose) cli::cli_text("Mantel r: {cli::col_green(round(res$statistic, 3))}  (p = {signif(res$signif, 3)})")
  res
}

print_tables_cli <- function(desc_tbl, diffs_tbl, comp_summary) {
  cli::cli_rule("Tables")
  cli::cli_text(cli::col_cyan("Descriptives (first 20 rows):"))
  print(dplyr::slice_head(desc_tbl, n = 20))

  cli::cli_text(cli::col_cyan("Feature differences (Cohen's d, KS p; first 20):"))
  print(dplyr::slice_head(diffs_tbl, n = 20))

  cli::cli_text(cli::col_cyan("Joint PC summaries:"))
  print(comp_summary)
}



# TWO-bank categorical comparison: chi-square/Fisher, Cramér's V, JS divergence (robust)
# TWO-bank categorical comparison: chi-square/Fisher, Cramér's V, JS divergence
compare_categoricals_both <- function(long_cat, item_bank_names) {
  # Always exclude these categorical features
  always_exclude <- c("abs_melody", "durations", "item_type", "melody")

  # Filter them out right away
  if (!is.null(long_cat) && nrow(long_cat) > 0 && "feature" %in% names(long_cat)) {
    long_cat <- dplyr::filter(long_cat, !(feature %in% always_exclude))
  }

  if (is.null(long_cat) || nrow(long_cat) == 0L) {
    return(list(tests = tibble::tibble(), plot = ggplot2::ggplot(), proportions = tibble::tibble()))
  }

  # counts and within-bank proportions
  cnt <- long_cat |>
    dplyr::count(feature, .bank, level, name = "n")

  prop_tbl <- cnt |>
    dplyr::group_by(feature, .bank) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup()

  # helper: Jensen–Shannon divergence between two named vectors
  js_div <- function(p, q) {
    support <- union(names(p), names(q))
    p <- p[support]; q <- q[support]
    p[is.na(p)] <- 0; q[is.na(q)] <- 0
    m <- 0.5 * (p + q)
    kl <- function(a, b) { idx <- a > 0; sum(a[idx] * log(a[idx] / b[idx])) }
    0.5 * kl(p, m) + 0.5 * kl(q, m)
  }

  tests <- cnt |>
    dplyr::group_by(feature) |>
    dplyr::group_map(~{
      fname <- .y$feature[[1]]

      # --- counts wide: ensure both bank columns exist, fill missing with 0
      w_n <- dplyr::filter(cnt, feature == fname) |>
        tidyr::pivot_wider(names_from = .bank, values_from = n, values_fill = 0)

      # make sure both bank columns are present even if one was absent
      missing_cols <- setdiff(item_bank_names, names(w_n))
      if (length(missing_cols)) for (mc in missing_cols) w_n[[mc]] <- 0L
      w_n <- w_n[, c("level", item_bank_names), drop = FALSE]

      mat <- as.matrix(w_n[item_bank_names])
      rownames(mat) <- w_n$level
      n_tot <- sum(mat); r <- nrow(mat); c <- ncol(mat)

      # test: Fisher if any small cell, else chi-square; catch failures
      htest <- tryCatch({
        if (any(mat < 5)) stats::fisher.test(mat) else stats::chisq.test(mat)
      }, error = function(e) tryCatch(stats::fisher.test(mat), error = function(e2) NULL))

      chi <- if (!is.null(htest$statistic)) as.numeric(htest$statistic) else NA_real_
      df  <- if (!is.null(htest$parameter)) as.integer(htest$parameter) else NA_integer_
      pv  <- if (!is.null(htest$p.value))   as.numeric(htest$p.value)   else NA_real_

      # Cramér's V (guard denom)
      denom <- n_tot * (min(max(r - 1, 0L), max(c - 1, 0L)))
      v <- if (!is.na(chi) && is.finite(denom) && denom > 0) sqrt(chi / denom) else NA_real_

      # proportions wide (for JS)
      w_p <- dplyr::filter(prop_tbl, feature == fname) |>
        tidyr::pivot_wider(names_from = .bank, values_from = prop, values_fill = 0)
      missing_cols_p <- setdiff(item_bank_names, names(w_p))
      if (length(missing_cols_p)) for (mc in missing_cols_p) w_p[[mc]] <- 0
      w_p <- w_p[, c("level", item_bank_names), drop = FALSE]

      p1 <- stats::setNames(w_p[[item_bank_names[1]]], w_p$level)
      p2 <- stats::setNames(w_p[[item_bank_names[2]]], w_p$level)
      js <- js_div(p1, p2)

      tibble::tibble(
        feature = fname,
        test = if (inherits(htest, "htest")) htest$method else NA_character_,
        statistic = chi,
        df = df,
        p_value = pv,
        cramers_v = v,
        js_divergence = js
      )
    }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(dplyr::desc(cramers_v))

  # stacked proportion bars (limit levels for readability)
  plot_tbl <- prop_tbl |>
    dplyr::group_by(feature) |>
    dplyr::mutate(rank = dplyr::min_rank(dplyr::desc(n))) |>
    dplyr::mutate(level_plot = ifelse(rank <= 10, level, "Other")) |>
    dplyr::group_by(feature, .bank, level_plot) |>
    dplyr::summarise(n = sum(n), .groups = "drop") |>
    dplyr::group_by(feature, .bank) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup()

  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .bank, y = prop, fill = level_plot)) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::facet_wrap(~ feature, scales = "free_y") +
    ggplot2::labs(title = "Categorical features by bank", x = NULL, y = "Proportion", fill = "Level") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  list(tests = tests, plot = p, proportions = prop_tbl)
}



# Two-bank categorical prep with level union/alignment
prepare_categoricals_both <- function(item_bank1, item_bank2, item_bank_names,
                                      include = NULL, exclude = NULL,
                                      id_cols = NULL, verbose = TRUE) {
  # local helper (factor/character/logical treated as categorical)
  cat_cols <- function(df) names(df)[vapply(df, function(x) is.factor(x) || is.character(x) || is.logical(x), logical(1))]

  # always exclude these from categorical analyses
  always_exclude <- c("abs_melody", "durations", "item_type", "melody")

  # clean names first so exclusions match
  ib1 <- janitor::clean_names(item_bank1)
  ib2 <- janitor::clean_names(item_bank2)

  # collect categorical candidates in each bank
  all1 <- cat_cols(ib1); all2 <- cat_cols(ib2)

  # drop id columns if provided
  if (!is.null(id_cols)) {
    id_cols <- intersect(id_cols, union(names(ib1), names(ib2)))
    all1 <- setdiff(all1, id_cols)
    all2 <- setdiff(all2, id_cols)
  }

  # start from overlapping categorical features
  pick <- intersect(all1, all2)

  # apply include (character or tidyselect)
  if (!is.null(include)) {
    if (is.character(include)) {
      pick <- intersect(pick, include)
    } else {
      pick <- names(dplyr::select(tibble::as_tibble(ib1[intersect(names(ib1), pick)]), {{ include }}))
    }
  }

  # apply exclude (character or tidyselect)
  if (!is.null(exclude)) {
    if (is.character(exclude)) {
      pick <- setdiff(pick, exclude)
    } else {
      drop <- names(dplyr::select(tibble::as_tibble(ib1[intersect(names(ib1), pick)]), {{ exclude }}))
      pick <- setdiff(pick, drop)
    }
  }

  # finally: hard categorical exclusions
  pre_len <- length(pick)
  pick <- setdiff(pick, always_exclude)
  hard_drop_n <- pre_len - length(pick)

  if (length(pick) == 0L) {
    if (verbose) {
      cli::cli_rule("Categorical features (both)")
      cli::cli_alert_warning("No overlapping categorical features after exclusions.")
    }
    return(list(long = tibble::tibble(), features = character()))
  }

  # build long with aligned levels per feature
  make_long <- function(df, bank) {
    tibble::as_tibble(df[, pick, drop = FALSE]) |>
      dplyr::mutate(.bank = bank) |>
      tidyr::pivot_longer(cols = dplyr::all_of(pick),
                          names_to = "feature", values_to = "level") |>
      dplyr::mutate(level = ifelse(is.na(level), "(Missing)", as.character(level)))
  }

  l1 <- make_long(ib1, item_bank_names[1])
  l2 <- make_long(ib2, item_bank_names[2])
  long <- dplyr::bind_rows(l1, l2)

  if (verbose) {
    cli::cli_rule("Categorical features (both)")
    cli::cli_text("{.strong Overlap used}: {length(pick)}")
    if (hard_drop_n > 0) {
      cli::cli_text(cli::col_yellow("Hard-excluded: {hard_drop_n} ({.val {paste(always_exclude, collapse = ', ')}})"))
    }
  }

  list(long = long, features = pick)
}


