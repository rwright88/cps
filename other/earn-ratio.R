# earnings quantiles ratio

library(tidyverse)
library(cps)
library(rwmisc)

file_data <- "~/data/cps/cps.csv"
file_db <- "~/data/cps/cpsdb"
years <- 1971:2018
vars <- c(
  "YEAR", "ASECWT", "AGE", "SEX", "RACE", "HISPAN",
  "WKSWORK1", "UHRSWORKLY", "INCWAGE", "INCBUS", "INCFARM"
)

# funs --------------------------------------------------------------------

plot_weeks_hours <- function(data) {
  d <- data %>%
    mutate_at(c("wkswork1", "uhrsworkly"), ~ round(. / 10) * 10) %>%
    group_by(sex, wkswork1, uhrsworkly) %>%
    summarise(n = sum(asecwt)) %>%
    group_by(sex) %>%
    mutate(d = n / sum(n)) %>%
    ungroup()

  d %>%
    ggplot(aes(wkswork1, uhrsworkly, fill = d)) +
    geom_tile() +
    geom_text(aes(label = format(round(d * 100, 1), nsmall = 1)), size = 3.5) +
    scale_fill_gradient(low = "#ffffff", high = "#1f77b4") +
    facet_wrap("sex") +
    coord_cartesian(ylim = c(0, 80)) +
    theme_rw()
}

calcq <- function(data, by) {
  by <- syms(by)
  probs <- seq(0.1, 0.9, 0.1)

  data %>%
    group_by(!!!by) %>%
    summarise(
      p = list(probs),
      q = list(Hmisc::wtd.quantile(earn, asecwt, probs = probs))
    ) %>%
    tidyr::unnest() %>%
    ungroup()
}

# run ---------------------------------------------------------------------

dat <- cps_db_read(file_db, years, vars)
dat <- cps_clean(dat)
dat <- filter(dat, age %in% 25:54, race == "white")

rwmisc::summary2(dat)
rwmisc::summary2_by(dat, "year", "earn")

plot_weeks_hours(dat)

res <- dat %>%
  filter(wkswork1 >= 40, uhrsworkly >= 30) %>%
  calcq(by = c("year", "sex")) %>%
  spread(p, q) %>%
  mutate(r9050 = `0.9` / `0.5`)

res %>%
  arrange(desc(year))

res %>%
  ggplot(aes(year, r9050)) +
  geom_point(size = 1.5, color = "#1f77b4") +
  geom_smooth(span = 1, se = FALSE, size = 0.5, color = "#1f77b4") +
  scale_y_continuous(limits = c(1, NA)) +
  facet_wrap("sex") +
  theme_rw()
