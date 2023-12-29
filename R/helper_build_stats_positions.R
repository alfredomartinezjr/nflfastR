build_stats_passing <- function(pbp, data, two_points, weekly) {
  pass_df <- data %>% dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(.data$passer_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarize(passing_yards_after_catch = sum((.data$passing_yards - .data$air_yards) * .data$complete_pass, na.rm = TRUE),
                     name_pass = dplyr::first(.data$passer_player_name),
                     team_pass = dplyr::first(.data$posteam),
                     opp_pass = dplyr::first(.data$defteam),
                     passing_yards = sum(.data$passing_yards, na.rm = TRUE),
                     passing_tds = sum(.data$touchdown == 1 &
                                         .data$td_team == .data$posteam &
                                         .data$complete_pass == 1),
                     interceptions = sum(.data$interception),
                     attempts = sum(.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1),
                     completions = sum(.data$complete_pass == 1),
                     sack_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$passer_player_id),
                     sack_fumbles_lost = sum(.data$fumble_lost == 1 &
                                               .data$fumbled_1_player_id == .data$passer_player_id &
                                               .data$fumble_recovery_1_team != .data$posteam),
                     passing_air_yards = sum(.data$air_yards, na.rm = TRUE),
                     sacks = sum(.data$sack),
                     sack_yards = -1 * sum(.data$yards_gained * .data$sack),
                     passing_first_downs = sum(.data$first_down_pass),
                     passing_epa = sum(.data$qb_epa, na.rm = TRUE),
                     pacr = .data$passing_yards / .data$passing_air_yards,
                     pacr = dplyr::case_when(is.nan(.data$pacr) ~ NA_real_,
                                             .data$passing_air_yards <= 0 ~ 0,
                                             TRUE ~ .data$pacr),
    ) %>%
    dplyr::rename(player_id = "passer_player_id") %>%
    dplyr::ungroup()
  if (isTRUE(weekly))
    pass_df <- add_dakota(pass_df, pbp = pbp, weekly = weekly)
  pass_two_points <- two_points %>%
    dplyr::filter(.data$pass_attempt ==  1) %>%
    dplyr::group_by(.data$passer_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarise(name_pass = custom_mode(.data$passer_player_name),
                     team_pass = custom_mode(.data$posteam),
                     opp_pass = custom_mode(.data$defteam),
                     passing_2pt_conversions = dplyr::n()) %>%
    dplyr::rename(player_id = "passer_player_id") %>%
    dplyr::ungroup()
  pass_df <- pass_df %>%
    dplyr::full_join(pass_two_points,
                     by = c("player_id", "week", "season", "gamescript",
                            "name_pass", "team_pass", "opp_pass")) %>%
    dplyr::mutate(passing_2pt_conversions = dplyr::if_else(is.na(.data$passing_2pt_conversions),
                                                           0L,
                                                           .data$passing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id))
  pass_df
}

build_stats_rushing <- function(data, two_points, mult_lats) {
  rushes <- data %>%
    dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(.data$rusher_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarize(name_rush = dplyr::first(.data$rusher_player_name),
                     team_rush = dplyr::first(.data$posteam),
                     opp_rush = dplyr::first(.data$defteam),
                     yards = sum(.data$rushing_yards, na.rm = TRUE),
                     tds = sum(.data$td_player_id == .data$rusher_player_id, na.rm = TRUE),
                     carries = dplyr::n(),
                     rushing_fumbles = sum(.data$fumble == 1 &
                                             .data$fumbled_1_player_id == .data$rusher_player_id &
                                             is.na(.data$lateral_rusher_player_id)),
                     rushing_fumbles_lost = sum(.data$fumble_lost == 1 &
                                                  .data$fumbled_1_player_id == .data$rusher_player_id &
                                                  is.na(.data$lateral_rusher_player_id) &
                                                  .data$fumble_recovery_1_team != .data$posteam),
                     rushing_first_downs = sum(.data$first_down_rush &
                                                 is.na(.data$lateral_rusher_player_id)),
                     rushing_epa = sum(.data$epa, na.rm = TRUE)) %>%
    dplyr::ungroup()
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_rusher_player_id)) %>%
    dplyr::group_by(.data$lateral_rusher_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarize(lateral_yards = sum(.data$lateral_rushing_yards,
                                         na.rm = TRUE),
                     lateral_fds = sum(.data$first_down_rush,
                                       na.rm = TRUE),
                     lateral_tds = sum(.data$td_player_id == .data$lateral_rusher_player_id,
                                       na.rm = TRUE),
                     lateral_att = dplyr::n(),
                     lateral_fumbles = sum(.data$fumble,
                                           na.rm = TRUE),
                     lateral_fumbles_lost = sum(.data$fumble_lost,
                                                na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rusher_player_id = "lateral_rusher_player_id") %>%
    dplyr::bind_rows(mult_lats %>%
                       dplyr::filter(.data$type == "lateral_rushing" &
                                       .data$season %in% data$season &
                                       .data$week %in% data$week) %>%
                       dplyr::select("season",
                                     "week",
                                     rusher_player_id = "gsis_player_id",
                                     lateral_yards = "yards") %>%
                       dplyr::mutate(lateral_tds = 0L,
                                     lateral_att = 1L)) %>%
    dplyr::group_by(.data$rusher_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarise_all(.funs = sum,
                         na.rm = TRUE) %>%
    dplyr::ungroup()
  rush_df <- rushes %>%
    dplyr::left_join(laterals,
                     by = c("rusher_player_id",
                            "week",
                            "season",
                            "gamescript")) %>%
    dplyr::mutate(lateral_yards = dplyr::if_else(is.na(.data$lateral_yards),
                                                 0,
                                                 .data$lateral_yards),
                  lateral_tds = dplyr::if_else(is.na(.data$lateral_tds),
                                               0L,
                                               .data$lateral_tds),
                  lateral_fumbles = dplyr::if_else(is.na(.data$lateral_fumbles),
                                                   0,
                                                   .data$lateral_fumbles),
                  lateral_fumbles_lost = dplyr::if_else(is.na(.data$lateral_fumbles_lost),
                                                        0,
                                                        .data$lateral_fumbles_lost),
                  lateral_fds = dplyr::if_else(is.na(.data$lateral_fds),
                                               0,
                                               .data$lateral_fds)) %>%
    dplyr::mutate(rushing_yards = .data$yards + .data$lateral_yards,
                  rushing_tds = .data$tds + .data$lateral_tds,
                  rushing_first_downs = .data$rushing_first_downs + .data$lateral_fds,
                  rushing_fumbles = .data$rushing_fumbles + .data$lateral_fumbles,
                  rushing_fumbles_lost = .data$rushing_fumbles_lost + .data$lateral_fumbles_lost) %>%
    dplyr::rename(player_id = "rusher_player_id") %>%
    dplyr::select("player_id", "week", "season", "gamescript", "name_rush",
                  "team_rush", "opp_rush", "rushing_yards", "carries",
                  "rushing_tds", "rushing_fumbles", "rushing_fumbles_lost",
                  "rushing_first_downs", "rushing_epa") %>%
    dplyr::ungroup()
  rush_two_points <- two_points %>%
    dplyr::filter(.data$rush_attempt == 1) %>%
    dplyr::group_by(.data$rusher_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarise(name_rush = custom_mode(.data$rusher_player_name),
                     team_rush = custom_mode(.data$posteam),
                     opp_rush = custom_mode(.data$defteam),
                     rushing_2pt_conversions = dplyr::n()) %>%
    dplyr::rename(player_id = "rusher_player_id") %>%
    dplyr::ungroup()
  rush_df <- rush_df %>%
    dplyr::full_join(rush_two_points,
                     by = c("player_id", "week", "season", "gamescript", "name_rush",
                            "team_rush", "opp_rush")) %>%
    dplyr::mutate(rushing_2pt_conversions = dplyr::if_else(is.na(.data$rushing_2pt_conversions),
                                                           0L,
                                                           .data$rushing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id))
  rush_df
}

build_stats_receiving <- function(data, mult_lats, two_points, racr_ids) {
  rec <- data %>% dplyr::filter(!is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(.data$receiver_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarize(name_receiver = dplyr::first(.data$receiver_player_name),
                     team_receiver = dplyr::first(.data$posteam),
                     opp_receiver = dplyr::first(.data$defteam),
                     yards = sum(.data$receiving_yards, na.rm = TRUE),
                     receptions = sum(.data$complete_pass == 1),
                     targets = dplyr::n(),
                     tds = sum(.data$td_player_id == .data$receiver_player_id,
                               na.rm = TRUE),
                     receiving_fumbles = sum(.data$fumble == 1 &
                                               .data$fumbled_1_player_id == .data$receiver_player_id &
                                               is.na(.data$lateral_receiver_player_id)),
                     receiving_fumbles_lost = sum(
                       .data$fumble_lost == 1 &
                         .data$fumbled_1_player_id == .data$receiver_player_id &
                         is.na(.data$lateral_receiver_player_id) &
                         .data$fumble_recovery_1_team != .data$posteam),
                     receiving_air_yards = sum(.data$air_yards, na.rm = TRUE),
                     receiving_yards_after_catch = sum(.data$yards_after_catch,
                                                       na.rm = TRUE),
                     receiving_first_downs = sum(.data$first_down_pass &
                                                   is.na(.data$lateral_receiver_player_id)),
                     receiving_epa = sum(.data$epa, na.rm = TRUE)) %>%
    dplyr::ungroup()
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_receiver_player_id)) %>%
    dplyr::group_by(.data$lateral_receiver_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarize(lateral_yards = sum(.data$lateral_receiving_yards, na.rm = TRUE),
                     lateral_tds = sum(.data$td_player_id == .data$lateral_receiver_player_id,
                                       na.rm = TRUE),
                     lateral_att = dplyr::n(), lateral_fds = sum(.data$first_down_pass, na.rm = T),
                     lateral_fumbles = sum(.data$fumble, na.rm = T),
                     lateral_fumbles_lost = sum(.data$fumble_lost,
                                                na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(receiver_player_id = "lateral_receiver_player_id") %>%
    dplyr::bind_rows(mult_lats %>%
                       dplyr::filter(.data$type == "lateral_receiving" &
                                       .data$season %in% data$season &
                                       .data$week %in% data$week) %>%
                       dplyr::select("season",
                                     "week",
                                     receiver_player_id = "gsis_player_id",
                                     lateral_yards = "yards") %>%
                       dplyr::mutate(lateral_tds = 0L,
                                     lateral_att = 1L)) %>%
    dplyr::group_by(.data$receiver_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarise_all(.funs = sum, na.rm = TRUE) %>%
    dplyr::ungroup()
  rec_team <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(.data$posteam,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarize(team_targets = dplyr::n(),
                     team_air_yards = sum(.data$air_yards, na.rm = TRUE), ) %>%
    dplyr::ungroup()
  rec_df <- rec %>%
    dplyr::left_join(laterals,
                     by = c("receiver_player_id", "week", "season", "gamescript")) %>%
    dplyr::left_join(rec_team,
                     by = c(team_receiver = "posteam", "week",
                            "season", "gamescript")) %>%
    dplyr::mutate(lateral_yards = dplyr::if_else(is.na(.data$lateral_yards),
                                                 0,
                                                 .data$lateral_yards),
                  lateral_tds = dplyr::if_else(is.na(.data$lateral_tds),
                                               0L, .data$lateral_tds),
                  lateral_fumbles = dplyr::if_else(is.na(.data$lateral_fumbles),
                                                   0, .data$lateral_fumbles),
                  lateral_fumbles_lost = dplyr::if_else(is.na(.data$lateral_fumbles_lost),
                                                        0, .data$lateral_fumbles_lost),
                  lateral_fds = dplyr::if_else(is.na(.data$lateral_fds),
                                               0, .data$lateral_fds)) %>%
    dplyr::mutate(receiving_yards = .data$yards + .data$lateral_yards,
                  receiving_tds = .data$tds + .data$lateral_tds,
                  receiving_yards_after_catch = .data$receiving_yards_after_catch + .data$lateral_yards,
                  receiving_first_downs = .data$receiving_first_downs + .data$lateral_fds,
                  receiving_fumbles = .data$receiving_fumbles + .data$lateral_fumbles,
                  receiving_fumbles_lost = .data$receiving_fumbles_lost + .data$lateral_fumbles_lost,
                  racr = .data$receiving_yards/.data$receiving_air_yards,
                  racr = dplyr::case_when(is.nan(.data$racr) ~ NA_real_,
                                          .data$receiving_air_yards == 0 ~ 0,
                                          .data$receiving_air_yards < 0 &
                                            !.data$receiver_player_id %in% racr_ids$gsis_id ~ 0,
                                          TRUE ~ .data$racr),
                  target_share = .data$targets/.data$team_targets,
                  air_yards_share = .data$receiving_air_yards/.data$team_air_yards,
                  wopr = 1.5 * .data$target_share + 0.7 * .data$air_yards_share) %>%
    dplyr::rename(player_id = "receiver_player_id") %>%
    dplyr::select("player_id", "week", "season", "gamescript","name_receiver",
                  "team_receiver", "opp_receiver", "receiving_yards",
                  "receiving_air_yards", "receiving_yards_after_catch",
                  "receptions", "targets", "receiving_tds", "receiving_fumbles",
                  "receiving_fumbles_lost", "receiving_first_downs",
                  "receiving_epa", "racr", "target_share", "air_yards_share",
                  "wopr")
  rec_two_points <- two_points %>% dplyr::filter(.data$pass_attempt ==  1) %>%
    dplyr::group_by(.data$receiver_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarise(name_receiver = custom_mode(.data$receiver_player_name),
                     team_receiver = custom_mode(.data$posteam),
                     opp_receiver = custom_mode(.data$defteam),
                     receiving_2pt_conversions = dplyr::n()) %>%
    dplyr::rename(player_id = "receiver_player_id") %>%
    dplyr::ungroup()
  rec_df <- rec_df %>%
    dplyr::full_join(rec_two_points,
                     by = c("player_id", "week", "season", "gamescript",
                            "name_receiver", "team_receiver",
                            "opp_receiver")) %>%
    dplyr::mutate(receiving_2pt_conversions = dplyr::if_else(is.na(.data$receiving_2pt_conversions),
                                                             0L,
                                                             .data$receiving_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id),
                  !is.na(.data$name_receiver))
  rec_df
}

build_stats_specialteams <- function(pbp) {
  st_tds <- pbp %>%
    dplyr::filter(.data$special == 1 & !is.na(.data$td_player_id)) %>%
    dplyr::group_by(.data$td_player_id,
                    .data$week,
                    .data$season,
                    .data$gamescript) %>%
    dplyr::summarise(name_st = custom_mode(.data$td_player_name),
                     team_st = custom_mode(.data$td_team),
                     opp_st = custom_mode(.data$defteam),
                     special_teams_tds = sum(.data$touchdown, na.rm = TRUE)) %>%
    dplyr::rename(player_id = "td_player_id")
  st_tds
}
