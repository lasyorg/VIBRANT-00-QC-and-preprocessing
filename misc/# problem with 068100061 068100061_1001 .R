# problem with 068100061 068100061_1001 and  068200150_1302 068200150

samples_to_impute_rel_ab |>
  filter(pid == "068100061") |>
  ggplot() +
  aes(x = study_day, y = visit_code_imputed, col = impute) +
  geom_point(alpha = 0.4) +
  ggtitle("068100061")

samples_to_impute_rel_ab |>
  filter(pid == "068200150") |>
  ggplot() +
  aes(x = study_day, y = visit_code_imputed, col = impute) +
  geom_point(alpha = 0.4) +
  ggtitle("068200150")


mae@colData |>
  as_tibble() |>
  filter(pid == "068100061") |>
  ggplot() +
  aes(x = study_day, y = visit_code, col = visit_attended) +
  geom_point(alpha = 0.4)


mae@colData |>
  as_tibble() |>
  filter(pid == "068200150") |>
  ggplot() +
  aes(x = study_day, y = visit_code, col = visit_attended) +
  geom_point(alpha = 0.4)


mae_coldata |>
  filter(pid == "068100061") |>
  ggplot() +
  aes(x = study_day, y = visit_code, col = visit_attended) +
  geom_point(alpha = 0.4)


pid_with_missing_daily_study_days |>
  filter(pid == "068100061") |>
  select(pid, visit_code, study_day, fixed_study_day, exp_study_day, diff, diff_new) |>
  print(n = 40)


|>
  filter(pid == "068100061") |> print(n = 40)


plot_timeline_for_pid(
  se = se_imputed,
  mae = mae,
  pid = "068100061",
  features = c("ampl_total_reads", "rel_ab", "rel_ab_adj", "abs_ab_0_imp")
)