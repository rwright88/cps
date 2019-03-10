# TODO:
# Add more variables

#' Clean IPUMS CPS data
#'
#' Currently, this only cleans/recodes a selection of variables.
#'
#' @param data Data frame of IPUMS CPS data.
#' @return Data frame.
#' @export
cps_clean <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  params <- dplyr::tribble(
    ~var,         ~fun,
    "age",        rec_age,
    "educ",       rec_education,
    "inctot",     rec_income,
    "marst",      rec_married,
    "sex",        rec_sex,
    "classwly",   rec_work_class,
    "uhrsworkly", rec_work_hours,
    "year",       rec_year
  )

  data <- setNames(data, tolower(names(data)))
  vars <- names(data)
  params <- params[params$var %in% vars, ]

  for (i in seq_len(nrow(params))) {
    var <- params[["var"]][[i]]
    fun <- params[["fun"]][[i]]
    data[[var]] <- fun(data[[var]])
  }

  if (all(c("race", "hispan", "year") %in% names(data))) {
    data[["race"]] <- rec_race(data[["race"]], data[["hispan"]], data[["year"]])
    data[["hispan"]] <- NULL
  }

  if (all(c("incwage", "incbus", "incfarm") %in% names(data))) {
    data[["earn"]] <- rec_earn(data[["incwage"]], data[["incbus"]], data[["incfarm"]])
    data[["incwage"]] <- NULL
    data[["incbus"]]  <- NULL
    data[["incfarm"]] <- NULL
  }

  data
}
