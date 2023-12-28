#' Load and aggregate multi-lateral plays
#'
#' @return A data frame aggregating yards gained per player per multi-lateral play on record.
#' @export
#'
#' @examples
#' \donttest{
#' mult_lats <- load_multi_laterals()
#' }
load_multi_laterals <- function() {
  nflreadr::rds_from_url("https://github.com/nflverse/nflverse-data/releases/download/misc/multiple_lateral_yards.rds") %>%
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
}
