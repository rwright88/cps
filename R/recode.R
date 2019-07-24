# age topcode = 80 is lowest
# education = unsure?
# hours topcode = 99

rec_age <- function(x) {
  out <- rep(NA_integer_, length(x))
  out[x %in% 0:79] <- x[x %in% 0:79]
  out
}

rec_education <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x %in% 0:69]    <- "less-hs"
  out[x %in% 70:79]   <- "hs-ged"
  out[x %in% 80:109]  <- "assoc-some"
  out[x %in% 110:119] <- "bachelor"
  out[x %in% 120:129] <- "advanced"
  out
}

rec_married <- function(x) {
  out <- rep(NA, length(x))
  out[x %in% 1:2] <- TRUE
  out[x %in% 3:7] <- FALSE
  out
}

rec_race <- function(race, hisp, year) {
  if (length(race) != length(hisp) || length(race) != length(year)) {
    stop("Lengths of inputs must be the same", call. = FALSE)
  }

  good <- (year >= 1970)
  aspi <- (race %in% 650:652)

  out <- rep(NA_character_, length(race))
  out[good & race == 100 & hisp == 0] <- "white"
  out[good & race == 200 & hisp == 0] <- "black"
  out[good & aspi & hisp == 0]        <- "asian-pi"
  out[good & hisp %in% 100:500]       <- "hispanic"
  out
}

rec_sex <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x == 1] <- "male"
  out[x == 2] <- "female"
  out
}

rec_wage <- function(x) {
  x[x == 9999998] <- NA
  x[x == 9999999] <- 0
  x
}

rec_work_class <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x %in% 10:14] <- "self-employed"
  out[x %in% 20:29] <- "wage-salary"
  out
}

rec_work_hours <- function(x) {
  out <- rep(NA_integer_, length(x))
  out[x %in% 0:98] <- x[x %in% 0:98]
  out[x == 999]    <- 0L
  out
}

rec_year <- function(x) {
  x - 1
}
