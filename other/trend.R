# trend

library(tidyverse)
library(cps)

file_db <- "~/data/cps/cpsdb"
years <- 1971:2018

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

calc_stats <- function(data, by, probs = seq(0.25, 0.75, 0.25)) {
  by_ <- syms(by)

  out <- data %>%
    group_by(!!!by_) %>%
    summarise(
      n = n(),
      pop = sum(asecwt),
      p = list(str_c("p", probs * 100)),
      q = list(round(Hmisc::wtd.quantile(incwage, asecwt, probs = !!probs), -3))
    ) %>%
    unnest() %>%
    ungroup()

  out$q[out$n < 100] <- NA
  out <- spread(out, p, q)
  out
}

plot_ratio <- function(data, y) {
  y_ <- sym(y)

  data %>%
    ggplot(aes(year, !!y_)) +
    geom_point(size = 2, color = "#1f77b4") +
    geom_smooth(span = 1, se = FALSE, size = 0.5, color = "#1f77b4") +
    scale_y_continuous(limits = c(1, NA)) +
    theme_bw()
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)

res <- data %>%
  filter(sex == "male", age %in% 25:55, incwage > 0) %>%
  calc_stats(by = "year")

res %>%
  arrange(desc(year))
