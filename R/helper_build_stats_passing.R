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
}
