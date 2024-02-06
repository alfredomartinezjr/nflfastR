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
#' \donttest{
#' pbp_2023 <- nflfastR::load_pbp(2023) %>%
#'   dplyr::filter(week <= 17)
#' offplayer_pts <- pbp_2023 %>%
#'   nflfastR::calculate_player_stats_gamescript()
#'   }
calculate_player_stats_gamescript <- function(pbp, weekly = FALSE, simple = FALSE) {
  rlang::check_installed("nflreadr (>= 1.3.0)", "to join player information.")


# Clean up pbp data frame -----------------------------------------------------
  # adds column to pbp with special teams info, if missing
  if (!"special" %in% names(pbp)) {
    pbp <- pbp %>%
      dplyr::mutate(special = dplyr::if_else(.data$play_type %in%
                                               c("extra_point", "field_goal",
                                                 "kickoff", "punt"),
                                             1, 0))
  }

  pbp$gamescript <- calculateGameScript(pbp$score_differential)

  s_type <- pbp %>% dplyr::select("season", "season_type",
                                  "week") %>% dplyr::distinct()
# Creates the data frame to be used for aggregating game stats ----------------
  # filters pbp data frame down to only football plays
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

  # accounts for lateral plays
  mult_lats <- load_multi_laterals()
  # creates data frame of players
  player_info <- nflreadr::load_players() %>%
    dplyr::select(player_id = "gsis_id", player_display_name = "display_name",
                  player_name = "short_name", "position", "position_group",
                  headshot_url = "headshot")
  # creates data frame of info for running backs & related
  racr_ids <- player_info %>%
    dplyr::filter(.data$position %in% c("RB", "FB", "HB")) %>%
    dplyr::select(gsis_id = "player_id")



# Create stats tables for each position ---------------------------------------
  # creates stats table for passers
  pass_df <- build_stats_passing(pbp, data, two_points, weekly)
  pass_df_nas <- is.na(pass_df)
  epa_index <- which(dimnames(pass_df_nas)[[2]] %in% c("passing_epa",
                                                        "dakota", "pacr"))
  pass_df_nas[, epa_index] <- c(FALSE)
  pass_df[pass_df_nas] <- 0

  # creates stats table for rushers
  rush_df <- build_stats_rushing(data, two_points, mult_lats)
  rush_df_nas <- is.na(rush_df)
  epa_index <- which(dimnames(rush_df_nas)[[2]] == "rushing_epa")
  rush_df_nas[, epa_index] <- c(FALSE)
  rush_df[rush_df_nas] <- 0

  #creates stats tables for receivers
  rec_df <- build_stats_receiving(data, mult_lats, two_points, racr_ids)
  rec_df_nas <- is.na(rec_df)
  epa_index <- which(dimnames(rec_df_nas)[[2]] %in%
                       c("receiving_epa", "racr", "target_share",
                         "air_yards_share", "wopr"))
  rec_df_nas[, epa_index] <- c(FALSE)
  rec_df[rec_df_nas] <- 0

  # creates stats table for special teams scorers
  st_tds <- build_stats_specialteams(pbp)

# Merges together position stats tables into one player table -----------------
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
    fantasy_points_custom = .data$fantasy_points_halfppr +
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
