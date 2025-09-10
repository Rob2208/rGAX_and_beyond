library("tidyverse")
library("injurytools")

## 17/18
df_exposures1718 <- prepare_exp(
  df_exposures0 =
    raw_df_exposures |> filter(season == "17/18"),
  player = "player_name",
  date = "year",
  time_expo = "minutes_played"
) |>
  mutate(seasonb = date2season(date))
df_injuries1718 <- prepare_inj(
  df_injuries0 =
    raw_df_injuries |> filter(season == "17/18"),
  player = "player_name",
  date_injured = "from",
  date_recovered = "until"
)
injd1718 <- prepare_all(
  data_exposures = df_exposures1718,
  data_injuries = df_injuries1718,
  exp_unit = "matches_minutes"
)

injd1718 <- injd1718 |>
  mutate(seasonb = date2season(tstart)) |>
  ## join to have info such as position, age, citizenship etc.
  left_join(df_exposures1718, by = c(
    "player" = "player",
    "seasonb" = "seasonb"
  ))

## create injd1718_sub:
##  - time to first injury
##  - equivalent tstart and tstop in calendar days
injd1718_sub <- injd1718 |>
  mutate(tstop_day = as.numeric(difftime(tstop, tstart, units = "days"))) %>%
  group_by(player) |> ## important
  mutate(
    tstop_day = cumsum(tstop_day),
    tstart_day = lag(tstop_day, default = 0)
  ) |>
  ungroup() |>
  dplyr::select(player:tstop_minPlay, tstart_day, tstop_day, everything()) |>
  filter(enum == 1) ## time to first injury

## 18/19
df_exposures1819 <- prepare_exp(
  df_exposures0 =
    raw_df_exposures |> filter(season == "18/19"),
  player = "player_name",
  date = "year",
  time_expo = "minutes_played"
) |>
  mutate(seasonb = date2season(date))
df_injuries1819 <- prepare_inj(
  df_injuries0 =
    raw_df_injuries |> filter(season == "18/19"),
  player = "player_name",
  date_injured = "from",
  date_recovered = "until"
)
injd1819 <- prepare_all(
  data_exposures = df_exposures1819,
  data_injuries = df_injuries1819,
  exp_unit = "matches_minutes"
)

injd1819 <- injd1819 |>
  mutate(seasonb = date2season(tstart)) |>
  ## join to have info such as position, age, citizenship etc.
  left_join(df_exposures1819, by = c(
    "player" = "player",
    "seasonb" = "seasonb"
  ))

## create injd1819_sub:
##  - time to first injury
##  - equivalent tstart and tstop in calendar days
injd1819_sub <- injd1819 |>
  mutate(tstop_day = as.numeric(difftime(tstop, tstart, units = "days"))) %>%
  group_by(player) |> ## important
  mutate(
    tstop_day = cumsum(tstop_day),
    tstart_day = lag(tstop_day, default = 0)
  ) |>
  ungroup() |>
  dplyr::select(player:tstop_minPlay, tstart_day, tstop_day, everything()) |>
  filter(enum == 1) ## time to first injury

injd_sub <- bind_rows(
  "17-18" = injd1718_sub,
  "18-19" = injd1819_sub,
  .id = "season"
)

saveRDS(injd_sub, "data.rds")
