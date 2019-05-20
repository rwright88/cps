# earnings quantiles ratio

library(tidyverse)
library(cps)
library(rwmisc)

file_db <- "~/data/cps/cpsdb"
years <- 1971:2018
vars <- c(
  "YEAR", "ASECWT", "AGE", "SEX", "RACE", "HISPAN",
  "WKSWORK1", "UHRSWORKLY", "INCWAGE", "INCBUS", "INCFARM"
)

# funs --------------------------------------------------------------------

calc_earn <- function(data, by) {
  by <- syms(by)
  probs <- seq(0.1, 0.9, 0.1)
  probsn <- str_c("p", probs * 100)

  out <- data %>%
    group_by(!!!by) %>%
    summarise(
      p = list(!!probsn),
      q = list(Hmisc::wtd.quantile(earn, asecwt, probs = !!probs))
    ) %>%
    unnest() %>%
    ungroup() %>%
    spread(p, q) %>%
    mutate(r9050 = p90 / p50)

  out
}

plot_earn <- function(data, y) {
  y_ <- sym(y)

  data %>%
    ggplot(aes(year, !!y_)) +
    geom_point(size = 2, color = "#1f77b4") +
    geom_smooth(span = 1, se = FALSE, size = 0.5, color = "#1f77b4") +
    scale_y_continuous(limits = c(1, NA), breaks = seq(1, 5, 0.5)) +
    theme_rw()
}

# run ---------------------------------------------------------------------

dat <- cps_db_read(file_db, years, vars)
dat <- cps_clean(dat)

# temp fix
dat$wkswork1 <- as.numeric(dat$wkswork1)
dat$uhrsworkly <- as.numeric(dat$uhrsworkly)

dat2 <- dat %>%
  filter(sex == "male", age %in% 25:54)

summary2_by(dat2, by = "year", vars = "earn") %>%
  arrange(desc(year))

res <- calc_earn(dat2, by = "year")

arrange(res, desc(year))

plot_earn(res, y = "r9050")
