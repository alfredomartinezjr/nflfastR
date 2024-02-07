
#adds gamescript info
# "1_trailingbig" : Team is down by at least 9 pts
# "2_trailing": Team is dowm between 4-8 pts
# "3_neutral": Team is either up or down 3 pts
# "4_leading": Team is up between 4-8 pts
# "5_leadingbig": Team is up by at least 9 pts
calculateGameScript <- function(score_differential_col) {
  gamescript <- NA
  i <- 1
  for (j in score_differential_col) {
    if (is.na(j)) {
      gamescript[i] <- NA
    } else if (j > 8) {
      gamescript[i] <- "5_leadingbig"
    } else if(j > 3 & j < 9) {
      gamescript[i] <- "4_leading"
    } else if(j > -4 & j < 4) {
      gamescript[i] <- "3_neutral"
    } else if(j > -9 & j < -3) {
      gamescript[i] <- "2_trailing"
    } else if (j < -8) {
      gamescript[i] <- "1_trailingbig"
    }
    i <- i + 1
  }
  return(gamescript)
}
