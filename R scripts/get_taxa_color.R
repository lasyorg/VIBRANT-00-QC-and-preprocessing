get_taxa_colors_deprecated <- function(taxa) {
  tibble(
    taxa = taxa
  ) |>
    mutate(
      cat = case_when(
        taxa == "Other" ~ "Other",
        str_detect(taxa, "crispatus") ~ "crispatus",
        str_detect(taxa, "LBP") ~ "LBP",
        str_detect(taxa, "iner") ~ "iners",
        str_detect(taxa, "revotella") ~ "prevotella",
        str_detect(taxa, "ardnerella") ~ "gardnerella",
        TRUE ~ "non lacto"
      )
    ) |>
    group_by(cat) |>
    mutate(
      color = case_when(
        taxa == "Other" ~ "gray80",
        str_detect(taxa, "crispatus") ~ "orange",
        str_detect(taxa, "LBP") ~ colorRampPalette(c(
          "orangered1",
          "red4"
        ))(n()),
        str_detect(taxa, "iner") ~ "green3",
        str_detect(taxa, "jensenii") ~ "green4",
        str_detect(taxa, "ardnerella") ~ colorRampPalette(c(
          "dodgerblue1",
          "dodgerblue4"
        ))(n()),
        str_detect(taxa, "revotella") ~ colorRampPalette(c(
          "purple1",
          "purple4"
        ))(n()),
        TRUE ~ colorRampPalette(c("turquoise3", "black"))(n())
      )
    ) |>
    pull(color)
}


get_taxa_colors_deprecated <- function(taxa) {
  tibble(
    taxa = taxa
  ) |>
    mutate(
      cat = case_when(
        taxa == "Other" ~ "Other",
        str_detect(taxa, "crispatus") ~ "crispatus",
        str_detect(taxa, "LBP") ~ "LBP",
        str_detect(taxa, "iner") ~ "iners",
        str_detect(taxa, "revotella") ~ "prevotella",
        str_detect(taxa, "ardnerella") ~ "gardnerella",
        str_detect(taxa, "BVAB1") ~ "BVAB1",
        TRUE ~ "non lacto"
      )
    ) |>
    group_by(cat) |>
    mutate(
      color = case_when(
        taxa == "Other" ~ "gray80",
        str_detect(taxa, "crispatus") ~ "#FDCC45",

        str_detect(taxa, "C0022A1") ~ "#F7931E", # LC106-US
        str_detect(taxa, "C0059E1") ~ "#F6881F", # LC106-US
        str_detect(taxa, "C0175A1") ~ "#F57C20", # LC106-US
        str_detect(taxa, "FF00018") ~ "#F37122", #LC106-SA
        str_detect(taxa, "FF00051") ~ "#F26523", #LC106-SA
        str_detect(taxa, "UC101") ~ "#F15A24", #LC106-SA

        str_detect(taxa, "C0006A1") ~ "#D63200", #LC115-US
        str_detect(taxa, "C0028A1") ~ "#C83002", #LC115-US
        str_detect(taxa, "C0112A1") ~ "#B92E03", #LC115-US
        str_detect(taxa, "FF00004") ~ "#AB2B05", #LC115-SA
        str_detect(taxa, "FF00064") ~ "#9D2906", #LC115-SA
        str_detect(taxa, "FF00072") ~ "#8E2708", #LC115-SA
        str_detect(taxa, "UC119") ~ "#802509", #LC115-SA
        str_detect(taxa, "122010") ~ "#71220B", #LC115-SA
        str_detect(taxa, "185329") ~ "#63200C", #LC115-SA

        str_detect(taxa, "LBP") ~ "#F6881F",
        str_detect(taxa, "iner") ~ "green3",
        str_detect(taxa, "jensenii") ~ "green4",
        str_detect(taxa, "ardnerella") ~ colorRampPalette(c(
          "dodgerblue1",
          "dodgerblue4"
        ))(n()),
        str_detect(taxa, "revotella") ~ colorRampPalette(c(
          "purple1",
          "purple4"
        ))(n()),
        str_detect(taxa, "BVAB1") ~ colorRampPalette(c(
          "deeppink3",
          "deeppink1"
        ))(n()),
        TRUE ~ colorRampPalette(c("turquoise3", "black"))(n())
      )
    ) |>
    pull(color)
}
