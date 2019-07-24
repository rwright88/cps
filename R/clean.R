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
  stopifnot(is.data.frame(data))

  params <- dplyr::tribble(
    ~var,         ~fun,
    "age",        rec_age,
    "educ",       rec_education,
    "marst",      rec_married,
    "sex",        rec_sex,
    "incwage",    rec_wage,
    "classwly",   rec_work_class,
    "uhrsworkly", rec_work_hours,
    "year",       rec_year
  )

  names(data) <- tolower(names(data))
  vars <- names(data)
  params <- params[params$var %in% vars, ]

  for (i in seq_along(params[["var"]])) {
    var <- params[["var"]][[i]]
    fun <- params[["fun"]][[i]]
    data[[var]] <- fun(data[[var]])
  }

  if (all(c("race", "hispan", "year") %in% vars)) {
    data$race <- rec_race(data$race, data$hispan, data$year)
    data$hispan <- NULL
  }

  data
}
