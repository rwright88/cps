# analysis

library(cps)
library(dplyr)
library(ggplot2)
library(rwmisc)
library(tidyr)

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

calc_stats <- function(data, by) {
  by_ <- syms(by)
  probs <- seq(0.1, 0.9, 0.01)

  out <- data %>%
    group_by(!!!by_) %>%
    summarise(
      n = n(),
      pop = sum(asecwt),
      p = list(!!probs),
      q = list(rwmisc::wtd_quantile(incwage, asecwt, probs = !!probs))
    ) %>%
    unnest() %>%
    ungroup()

  out$q[out$n < 100] <- NA
  out
}

plot_latest <- function(data, color = NULL) {
  if (!is.null(color)) {
    color_ <- sym(color)
    out <- ggplot(data, aes(p, q, color = !!color_))
  } else {
    out <- ggplot(data, aes(p, q))
  }

  out +
    geom_point(size = 1.5, alpha = 0.2) +
    geom_smooth(span = 0.5, se = FALSE, size = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), minor_breaks = NULL) +
    scale_y_log10(breaks = seq(1e4, 2e5, 1e4), minor_breaks = NULL, labels = scales::comma) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()
}

plot_trend <- function(data, color = NULL, facet = NULL) {
  if (!is.null(color)) {
    color_ <- sym(color)
    out <- ggplot(data, aes(year, q, color = !!color_))
  } else {
    out <- ggplot(data, aes(year, q))
  }

  out <- out +
    geom_point(size = 1.5, alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE, size = 1) +
    scale_y_log10(breaks = seq(1e4, 2e5, 1e4), minor_breaks = NULL, labels = scales::comma) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()

  if (!is.null(facet)) {
    out <- out + facet_wrap(facet, nrow = 1)
  }

  out
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)

data %>%
  filter(year == max(year), age %in% 25:55, incwage > 0, uhrsworkly >= 20) %>%
  calc_stats(by = "sex") %>%
  plot_latest(color = "sex")

data %>%
  filter(year >= 1990, age %in% 25:55, incwage > 0, uhrsworkly >= 20) %>%
  calc_stats(by = c("year", "sex")) %>%
  filter(round(p * 100) %in% c(10, 30, 50, 70, 90)) %>%
  plot_trend(color = "sex", facet = "p")
