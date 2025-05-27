
plot_data_for_pid <- function(selected_pid, mae_sub){
  
  mae_selected_pid <- mae_sub[, mae_sub$pid == selected_pid]
  
  all_visits <- tibble(visit_code = mae_selected_pid$visit_code)
  
  mg_ <- 
    mae_selected_pid[["mg"]] |> 
    as_tibble() |> 
    group_by(.feature) |> 
    mutate(max_rel_ab = max(rel_abs_bact)) |> 
    ungroup() |> 
    arrange(LBP, ifelse(!is.na(LBP),.feature, 1), max_rel_ab * !is.na(LBP)) |>
    mutate(.feature = .feature |> fct_inorder()) |> 
    filter(as.numeric(.feature) <= 25) |> 
    dplyr::full_join(all_visits, by = join_by(visit_code))
  
  
  ampl_ <- 
    mae_selected_pid[["amplicon"]] |> 
    as_tibble() |> 
    dplyr::rename(.feature = feature) |> # CHECK WHY not .feature
    group_by(.feature) |>  
    mutate(max_rel_ab = max(rel_ab)) |> 
    ungroup() |> 
    mutate(.feature = .feature |> fct_inorder()) |> 
    filter(str_detect(.feature, "crispatus") | as.numeric(.feature) <= 10) |> 
    dplyr::full_join(all_visits, by = join_by(visit_code))
  
  qPCR_ <-
    mae_selected_pid[["qPCR"]] |> 
    as_tibble()
  
  if (any(qPCR_$copies_per_swab_med[!is.na(qPCR_$LBP)] > 0, na.rm = TRUE)) {
    qPCR_ <- 
      qPCR_ |> 
      bind_rows(
        qPCR_ |> 
          filter(!is.na(LBP)) |> 
          group_by(.sample, uid, pid, visit_code, location) |> 
          summarise(
            LBP = NA,
            copies_per_swab_med = sum(copies_per_swab_med, na.rm = TRUE),
            taxon_label = "LBP (all)",
            .feature = "LBP all",
            .groups = "drop"
          )
      )
  }
                                
                                
    
  
  qPCR_ <- 
    qPCR_ |> 
    arrange(LBP, .feature) |>
    mutate(.feature = .feature |> fct_inorder()) |> 
    dplyr::full_join(all_visits, by = join_by(visit_code))
  

  
  g_mg <- 
    mg_ |> 
    ggplot() +
    aes(x = visit_code, y = rel_abs_bact, fill = taxon_label) +
    geom_col() +
    scale_y_continuous(
      "Relative abundance\n(bacterial content only)", 
      labels = scales::label_percent(accuracy = 20)
        ) +
    scale_fill_manual(
      "Taxa (LBP + top taxa based on rel. ab.)",
      breaks = mg_$taxon_label |> unique(),
      values = get_taxa_colors(mg_$taxon_label |> unique()),
      guide = guide_legend(nrow = 5), na.translate = FALSE
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Metagenomics (kSanity + VIRGO2)")
  
  g_ampl <- 
    ampl_ |> 
    ggplot() +
    aes(x = visit_code, y = rel_ab, fill = .feature) +
    geom_col() +
    scale_y_continuous(
      "Relative abundance\n", 
      labels = scales::label_percent(accuracy = 20)
    ) +
    scale_fill_manual(
      "Taxa (top taxa based on rel. ab.)",
      breaks = ampl_$.feature |> unique(),
      values = get_taxa_colors(ampl_$.feature |> unique()),
      guide = guide_legend(nrow = 5), na.translate = FALSE
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("16S rRNA amplicon sequencing") 
  
  g_PCR <- 
    qPCR_ |> 
    mutate(copies_per_swab_med = ifelse(copies_per_swab_med == 0, NA, copies_per_swab_med)) |> 
    ggplot() +
    aes(x = visit_code, y = copies_per_swab_med, fill = taxon_label) +
    geom_col(position = "dodge") +
    scale_y_log10("Copies per swabs") +
    facet_grid(
      ifelse(is.na(.feature) | (.feature %in% c("16S", "LBP all")), "Total", "LBP strains") |> 
        factor(levels = c("LBP strains", "Total")) ~ ., 
      scales = "free_y"
      ) +
    scale_fill_manual(
      "Taxa (LBP + top taxa based on rel. ab.)",
      breaks = qPCR_$taxon_label |> unique(),
      values = get_taxa_colors(qPCR_$taxon_label |> unique()),
      guide = guide_legend(nrow = 5), na.translate = FALSE
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("qPCR") 
  
  g_ampl + g_mg + g_PCR +
    plot_annotation(
      title = str_c("Participant ", selected_pid)
    ) +
    plot_layout(ncol = 1) &
    theme(
      legend.justification = "left"
    )
  
  
}
