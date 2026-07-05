exclude_missing_analytes <- function(se) {
  NA_summary <-
    se |>
    as_tibble() |>
    group_by(.feature) |>
    summarize(all_NA = (raw_obs_conc == "***") |> all(na.rm = TRUE))

  included_cytokines <-
    NA_summary |>
    filter(!all_NA)

  cat(
    se@metadata$name,
    "\n\t",
    sum(NA_summary$all_NA),
    "excluded analytes: ",
    str_c(NA_summary$.feature[NA_summary$all_NA], collapse = ", "),
    "\n"
  )

  se <- se[included_cytokines$.feature, ]
  se
}
