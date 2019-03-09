#' Read IPUMS CPS database table.
#'
#' @param file_db Path of database file to read.
#' @param years Years in data to query (where).
#' @param vars Variables in data to query (select).
#' @return Data frame.
#' @export
cps_db_read <- function(file_db, years, vars) {
  if (!all(years >= 1962)) {
    stop("`years` values must be greater than or equal to 1962", call. = FALSE)
  }
  if (!is.character(vars)) {
    stop("`vars` must be a character vector", call. = FALSE)
  }

  vars <- toupper(vars)
  YEAR <- NULL

  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)

  data <- con %>%
    dplyr::tbl("cps") %>%
    dplyr::filter(YEAR %in% years) %>%
    dplyr::select(vars) %>%
    dplyr::collect()

  # # fix data types
  # if ("INCEARN" %in% vars) {
  #   data[["INCEARN"]] <- as.integer(data[["INCEARN"]])
  # }

  DBI::dbDisconnect(con)
  data
}

#' List variables in IPUMS CPS database table
#'
#' @param file_db Path of database file to read.
#' @return Character vector of variable names from database table.
#' @export
cps_db_list <- function(file_db) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)
  vars <- DBI::dbListFields(con, "cps")
  DBI::dbDisconnect(con)
  vars
}
