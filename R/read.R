#' Read IPUMS CPS database table.
#'
#' @param file_db Path of database file to read.
#' @param years Years in data to query (where).
#' @param vars Variables in data to query (select).
#' @return Data frame.
#' @export
cps_db_read <- function(file_db, years, vars) {
  stopifnot(is.numeric(years))
  stopifnot(is.character(vars))

  vars <- toupper(vars)
  YEAR <- NULL

  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)
  on.exit(DBI::dbDisconnect(con))

  data <- dplyr::tbl(src = con, "cps")
  data <- dplyr::filter(data, YEAR %in% years)
  data <- dplyr::select(data, vars)
  data <- dplyr::collect(data)

  fix_int <- c(
    "WKSWORK1",
    "UHRSWORKLY"
  )
  fix_int <- vars[vars %in% fix_int]
  for (var in fix_int) {
    data[[var]] <- as.integer(data[[var]])
  }

  data
}

#' List variables in IPUMS CPS database table
#'
#' @param file_db Path of database file to read.
#' @return Character vector of variable names from database table.
#' @export
cps_db_list <- function(file_db) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbListFields(con, "cps")
}
