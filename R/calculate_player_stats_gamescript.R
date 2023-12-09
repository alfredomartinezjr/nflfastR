#' Get player stats broken down by gamescript.
#'
#' @param pbp A data frame of play-by-play data obtained by running load_pbp().
#' @param weekly TRUE/FALSE value indicating if you want your stats broken down by week.
#' @param simple Selects only a small amount of columns in the return data frame.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' pbp_2023 <- nflfastR::load_pbp(2023) %>%
#'   dplyr::filter(week <= 16)
#' offplayer_pts <- pbp_2023 %>%
#'   nflfastR::calculate_player_stats()
calculate_player_stats_gamescript <- function(pbp, weekly = FALSE, simple = FALSE) {
  rlang::check_installed("nflreadr (>= 1.3.0)", "to join player information.")
  # custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
  custom_mode <- function(x, na.rm = TRUE) {
    if(na.rm){x <- x[!is.na(x)]}
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }

  add_dakota <- function(add_to_this, pbp, weekly) {
    dakota_model <- NULL
    con <- url("https://github.com/nflverse/nflfastR-data/blob/master/models/dakota_model.Rdata?raw=true")
    try(load(con), silent = TRUE)
    close(con)

    if (is.null(dakota_model)) {
      user_message("This function needs to download the model data from GitHub. Please check your Internet connection and try again!", "oops")
      return(add_to_this)
    }

    if (!"id" %in% names(pbp)) pbp <- clean_pbp(pbp)
    if (!"qb_epa" %in% names(pbp)) pbp <- add_qb_epa(pbp)

    suppressMessages({
      df <- pbp %>%
        dplyr::filter(.data$pass == 1 | .data$rush == 1) %>%
        dplyr::filter(!is.na(.data$posteam) & !is.na(.data$qb_epa) & !is.na(.data$id) & !is.na(.data$down)) %>%
        dplyr::mutate(epa = dplyr::if_else(.data$qb_epa < -4.5, -4.5, .data$qb_epa)) %>%
        decode_player_ids()
    })

    if (isTRUE(weekly)) {
      relevant_players <- add_to_this %>%
        dplyr::filter(.data$attempts >= 5) %>%
        dplyr::mutate(filter_id = paste(.data$player_id, .data$season, .data$week, sep = "_")) %>%
        dplyr::pull(.data$filter_id)

      model_data <- df %>%
        dplyr::group_by(.data$id, .data$week, .data$season) %>%
        dplyr::summarize(
          n_plays = n(),
          epa_per_play = sum(.data$epa) / .data$n_plays,
          cpoe = mean(.data$cpoe, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cpoe = dplyr::if_else(is.na(.data$cpoe), 0, .data$cpoe)) %>%
        dplyr::rename("player_id" = "id") %>%
        dplyr::mutate(filter_id = paste(.data$player_id, .data$season, .data$week, sep = "_")) %>%
        dplyr::filter(.data$filter_id %in% relevant_players)

      model_data$dakota <- mgcv::predict.gam(dakota_model, model_data) %>% as.vector()

      out <- add_to_this %>%
        dplyr::left_join(
          model_data %>%
            dplyr::select("player_id", "week", "season", "dakota"),
          by = c("player_id", "week", "season")
        )
    } else if (isFALSE(weekly)) {
      relevant_players <- add_to_this %>%
        dplyr::filter(.data$attempts >= 5) %>%
        dplyr::pull(.data$player_id)

      model_data <- df %>%
        dplyr::group_by(.data$id) %>%
        dplyr::summarize(
          n_plays = n(),
          epa_per_play = sum(.data$epa) / .data$n_plays,
          cpoe = mean(.data$cpoe, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cpoe = dplyr::if_else(is.na(.data$cpoe), 0, .data$cpoe)) %>%
        dplyr::rename("player_id" = "id") %>%
        dplyr::filter(.data$player_id %in% relevant_players)

      model_data$dakota <- mgcv::predict.gam(dakota_model, model_data) %>% as.vector()

      out <- add_to_this %>%
        dplyr::left_join(
          model_data %>%
            dplyr::select("player_id", "dakota"),
          by = "player_id"
        )
    }
    return(out)
  }

  #adds gamescript info
  # "1 - Trailing Big" : Team is down by at least 9 pts
  # "2 - Trailing": Team is dowm between 4-8 pts
  # "3 - Neutral": Team is either up or down 3 pts
  # "4 - Leading": Team is up between 4-8 pts
  # "5 - Leading Big": Team is up by at least 9 pts
  calculateGameScript <- function(score_differential_col) {
    gamescript <- NA
    i <- 1
    for (j in score_differential_col) {
      if (is.na(j)) {
        gamescript[i] <- NA
      } else if (j > 8) {
        gamescript[i] <- "5 - Leading Big"
      } else if(j > 3 & j < 9) {
        gamescript[i] <- "4 - Leading"
      } else if(j > -4 & j < 4) {
        gamescript[i] <- "3 - Neutral"
      } else if(j > -9 & j < -3) {
        gamescript[i] <- "2 - Trailing"
      } else if (j < -8) {
        gamescript[i] <- "1 - Trailing Big"
      }
      i <- i + 1
    }
    return(gamescript)
  }
  pbp$gamescript <- calculateGameScript(pbp$score_differential)

  #accounts for lateral plays
  mult_lats <- nflreadr::rds_from_url("https://github.com/nflverse/nflverse-data/releases/download/misc/multiple_lateral_yards.rds") %>%
    dplyr::mutate(season = substr(.data$game_id, 1, 4) %>% as.integer(),
                  week = substr(.data$game_id, 6, 7) %>% as.integer()) %>%
    dplyr::filter(.data$yards != 0) %>%
    dplyr::group_by(.data$game_id,
                    .data$play_id) %>%
    dplyr::slice(seq_len(dplyr::n() - 1)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$season,
                    .data$week,
                    .data$type,
                    .data$gsis_player_id) %>%
    dplyr::summarise(yards = sum(.data$yards)) %>%
    dplyr::ungroup()
  #grabs only football plays
  suppressMessages({
    data <- pbp %>%
      dplyr::filter(!is.na(.data$down),
                    .data$play_type %in% c("pass", "qb_kneel",
                                           "qb_spike", "run")) %>%
      decode_player_ids()
    if (!"qb_epa" %in% names(data))
      data <- add_qb_epa(data)
    two_points <- pbp %>%
      dplyr::filter(.data$two_point_conv_result == "success") %>%
      dplyr::select("week", "season", "posteam", "gamescript", "defteam", "pass_attempt",
                    "rush_attempt", "passer_player_name", "passer_player_id",
                    "rusher_player_name", "rusher_player_id",
                    "lateral_rusher_player_name", "lateral_rusher_player_id",
                    "receiver_player_name", "receiver_player_id", "lateral_receiver_player_name",
                    "lateral_receiver_player_id") %>%
      decode_player_ids()
  })
  #special teams info
  if (!"special" %in% names(pbp)) {
    pbp <- pbp %>%
      dplyr::mutate(special = dplyr::if_else(.data$play_type %in%
                                               c("extra_point", "field_goal",
                                                 "kickoff", "punt"),
                                             1, 0))
  }
  s_type <- pbp %>% dplyr::select("season", "season_type",
                                  "week") %>% dplyr::distinct()

  # creates data frame of players
  player_info <- nflreadr::load_players() %>%
    dplyr::select(player_id = "gsis_id", player_display_name = "display_name",
                  player_name = "short_name", "position", "position_group",
                  headshot_url = "headshot")
  # dataframe of info for running backs & related
  racr_ids <- player_info %>%
    dplyr::filter(.data$position %in% c("RB", "FB", "HB")) %>%
    dplyr::select(gsis_id = "player_id")

  # creates stats table for passers
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
  pass_df_nas <- is.na(pass_df)
  epa_index <- which(dimnames(pass_df_nas)[[2]] %in% c("passing_epa",
                                                       "dakota", "pacr"))
  pass_df_nas[, epa_index] <- c(FALSE)
  pass_df[pass_df_nas] <- 0

  # creates stats table for rushers
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
  rush_df_nas <- is.na(rush_df)
  epa_index <- which(dimnames(rush_df_nas)[[2]] == "rushing_epa")
  rush_df_nas[, epa_index] <- c(FALSE)
  rush_df[rush_df_nas] <- 0

  #creates stats tables for receivers
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
  rec_df_nas <- is.na(rec_df)
  epa_index <- which(dimnames(rec_df_nas)[[2]] %in%
                       c("receiving_epa", "racr", "target_share",
                         "air_yards_share", "wopr"))
  rec_df_nas[, epa_index] <- c(FALSE)
  rec_df[rec_df_nas] <- 0

  # creates stats table for special teams scorers
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

  # creates the player table
  player_df <- pass_df %>%
    dplyr::full_join(rush_df, by = c("player_id", "week", "season", "gamescript")) %>%
    dplyr::full_join(rec_df, by = c("player_id", "week", "season", "gamescript")) %>%
    dplyr::full_join(st_tds, by = c("player_id", "week", "season", "gamescript")) %>%
    dplyr::left_join(s_type, by = c("season", "week")) %>%
    dplyr::mutate(
      player_name = dplyr::case_when(
        !is.na(.data$name_pass) ~
          .data$name_pass,
        !is.na(.data$name_rush) ~ .data$name_rush,
        !is.na(.data$name_receiver) ~ .data$name_receiver,
        TRUE ~
          .data$name_st
      ),
      recent_team = dplyr::case_when(
        !is.na(.data$team_pass) ~
          .data$team_pass,!is.na(.data$team_rush) ~ .data$team_rush,
        !is.na(.data$team_receiver) ~ .data$team_receiver,
        TRUE ~
          .data$team_st
      ),
      opponent_team = dplyr::case_when(
        !is.na(.data$opp_pass) ~
          .data$opp_pass,!is.na(.data$opp_rush) ~ .data$opp_rush,
        !is.na(.data$opp_receiver) ~ .data$opp_receiver,
        TRUE ~
          .data$opp_st
      )
    ) %>%
    dplyr::select(tidyselect::any_of(
      c(
        "player_id",
        "player_name",
        "recent_team",
        "season",
        "week",
        "season_type",
        "opponent_team",
        "gamescript",
        "completions",
        "attempts",
        "passing_yards",
        "passing_tds",
        "interceptions",
        "sacks",
        "sack_yards",
        "sack_fumbles",
        "sack_fumbles_lost",
        "passing_air_yards",
        "passing_yards_after_catch",
        "passing_first_downs",
        "passing_epa",
        "passing_2pt_conversions",
        "pacr",
        "dakota",
        "carries",
        "rushing_yards",
        "rushing_tds",
        "rushing_fumbles",
        "rushing_fumbles_lost",
        "rushing_first_downs",
        "rushing_epa",
        "rushing_2pt_conversions",
        "receptions",
        "targets",
        "receiving_yards",
        "receiving_tds",
        "receiving_fumbles",
        "receiving_fumbles_lost",
        "receiving_air_yards",
        "receiving_yards_after_catch",
        "receiving_first_downs",
        "receiving_epa",
        "receiving_2pt_conversions",
        "racr",
        "target_share",
        "air_yards_share",
        "wopr",
        "special_teams_tds"
      )
    )) %>%
    dplyr::filter(!is.na(.data$player_id), !is.na(.data$player_name))
  player_df_nas <- is.na(player_df)
  epa_index <-
    which(
      dimnames(player_df_nas)[[2]] %in% c(
        "passing_epa",
        "rushing_epa",
        "receiving_epa",
        "dakota",
        "racr",
        "target_share",
        "air_yards_share",
        "wopr",
        "pacr"
      )
    )
  player_df_nas[, epa_index] <- c(FALSE)
  player_df[player_df_nas] <- 0
  player_df <- player_df %>% dplyr::mutate(
    fantasy_points_std = 1 / 25 *
      .data$passing_yards + 4 * .data$passing_tds+-2 * .data$interceptions +
      1 / 10 * (.data$rushing_yards + .data$receiving_yards) +
      6 * (
        .data$rushing_tds + .data$receiving_tds + .data$special_teams_tds
      ) +
      2 * (
        .data$passing_2pt_conversions + .data$rushing_2pt_conversions +
          .data$receiving_2pt_conversions
      ) +-2 * (
        .data$sack_fumbles_lost +
          .data$rushing_fumbles_lost + .data$receiving_fumbles_lost
      ),
    fantasy_points_ppr = .data$fantasy_points_std + .data$receptions,
    fantasy_points_halfppr = .data$fantasy_points_std + (0.5 * .data$receptions),
    fantasy_points_custom = fantasy_points_halfppr +
      (0.5 * .data$rushing_first_downs) + (0.5 * .data$receiving_first_downs)
  ) %>%
    dplyr::arrange(.data$player_id, .data$season, .data$week, .data$gamescript)
  if (isFALSE(weekly)) {
    player_df <- player_df %>%
      dplyr::mutate(tgts = .data$targets,
                    rec_air_yds = .data$receiving_air_yards) %>%
      dplyr::group_by(.data$player_id, .data$gamescript) %>%
      dplyr::summarise(
        player_name = custom_mode(.data$player_name),
        games = dplyr::n(),
        recent_team = dplyr::last(.data$recent_team),
        gamescript = custom_mode(.data$gamescript),
        completions = sum(.data$completions),
        attempts = sum(.data$attempts),
        passing_yards = sum(.data$passing_yards),
        passing_tds = sum(.data$passing_tds),
        interceptions = sum(.data$interceptions),
        sacks = sum(.data$sacks),
        sack_yards = sum(.data$sack_yards),
        sack_fumbles = sum(.data$sack_fumbles),
        sack_fumbles_lost = sum(.data$sack_fumbles_lost),
        passing_air_yards = sum(.data$passing_air_yards),
        passing_yards_after_catch = sum(.data$passing_yards_after_catch),
        passing_first_downs = sum(.data$passing_first_downs),
        passing_epa = dplyr::if_else(all(is.na(
          .data$passing_epa
        )),
        NA_real_, sum(.data$passing_epa, na.rm = TRUE)),
        passing_2pt_conversions = sum(.data$passing_2pt_conversions),
        pacr = .data$passing_yards / .data$passing_air_yards,
        carries = sum(.data$carries),
        rushing_yards = sum(.data$rushing_yards),
        rushing_tds = sum(.data$rushing_tds),
        rushing_fumbles = sum(.data$rushing_fumbles),
        rushing_fumbles_lost = sum(.data$rushing_fumbles_lost),
        rushing_first_downs = sum(.data$rushing_first_downs),
        rushing_epa = dplyr::if_else(all(is.na(
          .data$rushing_epa
        )),
        NA_real_, sum(.data$rushing_epa, na.rm = TRUE)),
        rushing_2pt_conversions = sum(.data$rushing_2pt_conversions),
        receptions = sum(.data$receptions),
        targets = sum(.data$targets),
        receiving_yards = sum(.data$receiving_yards),
        receiving_tds = sum(.data$receiving_tds),
        receiving_fumbles = sum(.data$receiving_fumbles),
        receiving_fumbles_lost = sum(.data$receiving_fumbles_lost),
        receiving_air_yards = sum(.data$receiving_air_yards),
        receiving_yards_after_catch = sum(.data$receiving_yards_after_catch),
        receiving_first_downs = sum(.data$receiving_first_downs),
        receiving_epa = dplyr::if_else(all(is.na(
          .data$receiving_epa
        )),
        NA_real_, sum(.data$receiving_epa, na.rm = TRUE)),
        receiving_2pt_conversions = sum(.data$receiving_2pt_conversions),
        racr = .data$receiving_yards / .data$receiving_air_yards,
        target_share = dplyr::if_else(
          all(is.na(.data$target_share)),
          NA_real_,
          sum(.data$tgts, na.rm = TRUE) / sum(.data$tgts / .data$target_share,
                                              na.rm = TRUE)
        ),
        air_yards_share = dplyr::if_else(
          all(is.na(.data$air_yards_share)),
          NA_real_,
          sum(.data$rec_air_yds, na.rm = TRUE) / sum(.data$rec_air_yds / .data$air_yards_share,
                                                     na.rm = TRUE)
        ),
        wopr = 1.5 * .data$target_share +
          0.7 * .data$air_yards_share,
        special_teams_tds = sum(.data$special_teams_tds),
        fantasy_points_std = sum(.data$fantasy_points_std),
        fantasy_points_ppr = sum(.data$fantasy_points_ppr),
        fantasy_points_halfppr = sum(.data$fantasy_points_halfppr),
        fantasy_points_custom = sum(.data$fantasy_points_custom)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        racr = dplyr::case_when(
          is.nan(.data$racr) ~
            NA_real_,
          .data$receiving_air_yards == 0 ~ 0,
          .data$receiving_air_yards <
            0 &
            !.data$player_id %in% racr_ids$gsis_id ~ 0,
          TRUE ~ .data$racr
        ),
        pacr = dplyr::case_when(
          is.nan(.data$pacr) ~
            NA_real_,
          .data$passing_air_yards <= 0 ~ 0,
          TRUE ~
            .data$pacr
        )
      ) %>%
      add_dakota(pbp = pbp, weekly = weekly) %>%
      dplyr::select("player_id":"pacr",
                    tidyselect::any_of("dakota"),
                    dplyr::everything())
  }
  player_df <- player_df %>% dplyr::select(-"player_name") %>%
    dplyr::left_join(player_info, by = "player_id")
  if (simple == TRUE) {
    player_df <- player_df %>%
      dplyr::select(
        "player_id",
        "player_display_name",
        "position",
        "gamescript",
        "games",
        "recent_team",
        "attempts",
        "carries",
        "targets",
        "passing_air_yards",
        "fantasy_points_custom"
      )
  }
  return(player_df)
}
