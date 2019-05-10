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

  data %>%
    group_by(!!!by) %>%
    summarise(
      p = list(probs),
      q = list(Hmisc::wtd.quantile(earn, asecwt, probs = probs))
    ) %>%
    unnest() %>%
    ungroup() %>%
    spread(p, q) %>%
    mutate(r9050 = `0.9` / `0.5`)
}

calc_earn2 <- function(data, by) {
  probs <- seq(0.1, 0.9, 0.1)
  uniques <- unique(data[[by]])
  out <- vector("list", length(uniques))

  out <- lapply(uniques, function(.x) {
    x <- data[data[[by]] == .x, ]
    as.list(Hmisc::wtd.quantile(x$earn, x$asecwt, probs = probs))
  })

  out <- dplyr::bind_rows(out)
  out[[by]] <- uniques
  out$r9050 <- out$`90%` / out$`50%`
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

dat2 <- dat %>%
  filter(sex == "male", age %in% 25:54)

summary2_by(dat2, by = "year", vars = "earn") %>%
  arrange(desc(year))

system.time({
  res <- calc_earn(dat2, by = "year")
})
system.time({
  res2 <- calc_earn2(dat2, by = "year")
})

res %>%
  arrange(desc(year))

plot_earn(res, y = "r9050")
