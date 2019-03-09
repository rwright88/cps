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
    "year",       rec_year,
    "uhrsworkly", rec_hours
  )

  data <- setNames(data, tolower(names(data)))
  vars <- names(data)
  vars <- vars[vars %in% params$var]

  for (i in seq_along(vars)) {
    var <- params[["var"]][[i]]
    fun <- params[["fun"]][[i]]
    data[[var]] <- fun(data[[var]])
  }

  if (all(c("incwage", "incbus", "incfarm") %in% names(data))) {
    data[["earn"]] <- rec_earn(data[["incwage"]], data[["incbus"]], data[["incfarm"]])
    data[["incwage"]] <- NULL
    data[["incbus"]] <- NULL
    data[["incfarm"]] <- NULL
  }

  data
}
