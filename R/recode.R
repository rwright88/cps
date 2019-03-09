rec_earn <- function(wage, bus, farm) {
  wage[wage == 9999998] <- NA
  bus[bus   == 9999998] <- NA
  farm[farm == 9999998] <- NA
  wage[wage == 9999999] <- 0
  bus[bus   == 9999999] <- 0
  farm[farm == 9999999] <- 0

  earn <- wage + bus + farm
  earn
}

rec_hours <- function(x) {
  x[x == 99] <- NA
  x[x == 999] <- 0
  x
}

rec_year <- function(x) {
  x - 1
}
