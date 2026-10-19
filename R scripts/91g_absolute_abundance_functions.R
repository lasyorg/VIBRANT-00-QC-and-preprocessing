plot_timeline_for_pid <- function(se, mae, pid, features) {
  # Get the data for the participant
  uids_of_pid <-
    colData(mae) |>
    as_tibble() |>
    filter(pid == !!pid) |>
    pull(uid)
  tmp <-
    se |>
    filter(uid %in% uids_of_pid) |>
    as_tibble() |>
    left_join(
      colData(mae) |> as_tibble() |> select(uid, study_day, pid, visit_type),
      by = join_by(.sample == uid)
    ) |>
    filter(pid == !!pid)

  # selection of taxa
  selected_features <-
    tmp |>
    group_by(.feature) |>
    summarize(
      max_rel_ab = max(rel_ab, na.rm = TRUE),
      max_abs_ab = max(abs_ab, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(max_rel_ab > 0.1 | max_abs_ab > 1e6) |>
    arrange(-max_rel_ab) |>
    pull(.feature)

  # make the x-axis
  x_axis <-
    scale_x_continuous(
      "Study day",
      limits = c(
        (tmp$study_day - 1) |> min() |> floor(),
        (tmp$study_day + 1) |> max() |> ceiling()
      ),
      breaks = seq(-70, 700, by = 7),
      minor_breaks = seq(-70, 700, by = 1)
    )

  # exclude data
  excl <-
    tmp |>
    select(.sample, study_day, visit_type, exclude, exclude_reason) |>
    distinct() |>
    filter(exclude)

  # extraction plate data
  plates <-
    mae@metadata$sample_manifest |>
    filter(uid %in% uids_of_pid) |>
    left_join(
      colData(mae) |> as_tibble() |> select(uid, study_day, pid, visit_type),
      by = join_by(uid)
    )

  # cat(nrow(excl), "\n")

  # extraction plates
  g_plates <-
    plates |>
    arrange(ext_lib_plate_nb |> desc()) |>
    ggplot() +
    aes(x = study_day, y = visit_type |> fct_rev(), color = ext_lib_plate_id) +
    geom_label(aes(label = ext_lib_plate_nb), size = 2) +
    scale_y_discrete("", limits = c("Clinic", "Home")) +
    guides(color = "none") +
    x_axis

  # qPCR data
  g_qpcr <-
    tmp |>
    select(.sample, study_day, qpcr_16S, visit_type) |>
    distinct() |>
    ggplot() +
    aes(x = study_day, y = visit_type |> fct_rev(), color = qpcr_16S |> log10()) +
    geom_point() +
    geom_point(data = excl, color = "red", shape = 4, size = 2) +
    scale_color_gradient2(
      low = "red4",
      mid = "steelblue1",
      high = "purple",
      midpoint = 7,
      limits = c(0, 10),
      na.value = "red4"
    ) +
    theme(legend.direction = "horizontal") +
    ylab("") +
    x_axis

  # amplicon total reads
  g_ampl_tot_reads <-
    tmp |>
    select(.sample, study_day, ampl_total_reads, visit_type) |>
    distinct() |>
    ggplot() +
    aes(x = study_day, y = visit_type |> fct_rev(), color = ampl_total_reads |> log10()) +
    geom_point() +
    geom_point(data = excl, color = "red", shape = 4, size = 2) +
    scale_color_gradient(low = "red4", high = "steelblue1", limits = c(0, 6)) +
    theme(legend.direction = "horizontal") +
    ylab("") +
    x_axis

  # relative abundances
  g_rel_ab <-
    tmp |>
    filter(.feature %in% selected_features) |>
    ggplot() +
    facet_grid(visit_type ~ .) +
    aes(x = study_day, y = rel_ab, fill = .feature) +
    geom_col() +
    geom_point(
      data = excl,
      y = 0,
      color = "red",
      fill = "red",
      shape = 4,
      size = 2
    ) +
    ylab("Relative abundance") +
    x_axis +
    scale_fill_manual(
      breaks = selected_features,
      values = get_taxa_colors(selected_features),
      name = "Taxon"
    )

  # adjusted relative abundances
  g_rel_ab_adj <-
    tmp |>
    filter(.feature %in% selected_features) |>
    ggplot() +
    facet_grid(visit_type ~ .) +
    aes(x = study_day, y = rel_ab_adj, fill = .feature) +
    geom_col() +
    geom_point(
      data = excl,
      y = 0,
      color = "red",
      fill = "red",
      shape = 4,
      size = 2
    ) +
    ylab("Adjusted relative abundance") +
    x_axis +
    scale_fill_manual(
      breaks = selected_features,
      values = get_taxa_colors(selected_features),
      name = "Taxon"
    )

  # absolute abundances
  g_abs_ab <-
    tmp |>
    filter(.feature %in% selected_features) |>
    ggplot() +
    facet_grid(visit_type ~ .) +
    aes(x = study_day, y = abs_ab_0_imp, color = .feature) +
    geom_path(aes(group = .feature)) +
    geom_point() +
    geom_point(data = excl, y = 1, color = "red", shape = 4, size = 2) +
    ylab("Absolute abundance\n[genome copies per swab]") +
    x_axis +
    scale_color_manual(
      breaks = selected_features,
      values = get_taxa_colors(selected_features),
      name = "Taxon"
    ) +
    scale_y_log10()

  g_excl <-
    excl |>
    ggplot() +
    aes(x = study_day, y = visit_type |> fct_rev(), col = exclude_reason) +
    geom_point() +
    scale_y_discrete("", limits = c("Clinic", "Home")) +
    scale_color_discrete("Reason for exclusion") +
    x_axis

  # combine the plots
  g_combined <-
    g_plates +
    g_qpcr +
    g_ampl_tot_reads +
    g_rel_ab +
    guides(fill = "none") +
    g_rel_ab_adj +
    g_abs_ab +
    guides(col = "none") +
    g_excl
  g_combined <-
    g_combined +
    plot_annotation(
      title = str_c("Participant ", pid),
      theme = theme(plot.title = element_text(size = 16))
    ) +
    plot_layout(heights = c(0.25, 0.25, 0.25, 1, 1, 1, 0.25))

  g_combined
}


plot_extraction_plates <- function(mae, selected_pid) {
  # extraction plate data
  plates <-
    mae@metadata$sample_manifest |>
    left_join(
      colData(mae) |> as_tibble() |> select(uid, study_day, pid, visit_type),
      by = join_by(uid)
    ) |>
    filter(pid == !!selected_pid)

  plates |>
    arrange(ext_lib_plate_nb |> desc()) |>
    ggplot() +
    ggh4x::facet_nested(visit_type + ext_lib_plate_batch ~ ., scales = "free", space = "free") +
    aes(x = study_day, y = ext_lib_plate_nb |> fct_rev(), color = source_assay) +
    geom_label(aes(label = ext_lib_plate_nb), size = 2) +
    ylab("Extraction plate(s)") +
    # guides(color = "none") +
    scale_x_continuous(
      "Study day",
      limits = c(
        (plates$study_day - 1) |> min() |> floor(),
        (plates$study_day + 1) |> max() |> ceiling()
      ),
      breaks = seq(-70, 700, by = 7),
      minor_breaks = seq(-70, 700, by = 1)
    ) +
    theme(legend.position = "bottom") +
    ggtitle(str_c("Extraction plates for participant ", selected_pid))
}
