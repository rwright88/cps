# analysis

library(tidyverse)
library(cps)

file_db <- "~/data/cps/cpsdb"
years <- 2018

# funs --------------------------------------------------------------------

get_data <- function(file_db, years) {
  vars <- c(
    "year", "asecwt", "sex", "age", "race", "hispan", "educ",
    "wkswork1", "uhrsworkly", "incwage"
  )
  data <- cps::cps_db_read(file_db = file_db, years = years, vars = vars)
  data <- cps::cps_clean(data)
  data
}

calc_stats <- function(data, by, probs = seq(0.1, 0.9, 0.01)) {
  by_ <- syms(by)
  n_years <- length(unique(data$year))

  out <- data %>%
    group_by(!!!by_) %>%
    summarise(
      n = n(),
      pop = sum(asecwt) / !!n_years,
      p = list(!!probs),
      q = list(Hmisc::wtd.quantile(incwage, asecwt, probs = !!probs))
    ) %>%
    unnest() %>%
    ungroup()

  out$q[out$n < 100] <- NA
  out
}

plot_stats <- function(data, color = NULL) {
  if (!is.null(color)) {
    p <- ggplot(data, aes(p, q, color = !!sym(color)))
  } else {
    p <- ggplot(data, aes(p, q))
  }

  p +
    geom_point(size = 1.5, alpha = 0.2) +
    geom_smooth(span = 0.5, se = FALSE, size = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), minor_breaks = NULL) +
    scale_y_continuous(limits = c(0, NA), breaks = seq(0, 5e5, 2e4), labels = scales::comma) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)

res <- data %>%
  filter(age %in% 25:55, incwage > 0) %>%
  calc_stats(by = "sex")

res %>%
  plot_stats(color = "sex")
