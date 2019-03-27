# trends

library(tidyverse)
library(cps)
library(rwmisc)

file_db <- "~/data/cps/cpsdb"
years   <- 1990:2018

# funs --------------------------------------------------------------------

get_data <- function(file_db, years) {
  vars <- c("YEAR", "ASECWT", "AGE", "SEX", "RACE", "HISPAN", "EDUC")
  data <- cps_db_read(file_db, years, vars)
  data <- cps_clean(data)
  data <- filter(data, age %in% 30:49, race == "white")
  data$educ <- rec_edu2(data$educ)
  data
}

rec_edu2 <- function(x) {
  x[x %in% c("less-hs", "hs-ged")] <- "hs-or-less"
  x
}

calc_trend <- function(data) {
  out <- data %>%
    group_by(year, sex, educ) %>%
    summarise(n = n(), pop = sum(asecwt)) %>%
    group_by(year, sex) %>%
    mutate(d = pop / sum(pop)) %>%
    ungroup()

  out
}

plot_trend <- function(data, y, color, facet = NULL, n_col = NULL) {
  y <- sym(y)
  color <- sym(color)

  p <- data %>%
    ggplot(aes(year, !!y, color = !!color)) +
    geom_line(size = 1.1) +
    ggrepel::geom_text_repel(
      mapping = aes(label = format(round(!!y * 100, 1), nsmall = 1)),
      data = filter(data, year %in% c(min(year), max(year))),
      size = 3.5,
      box.padding = 0.5,
      direction = "y",
      show.legend = FALSE
    ) +
    scale_x_continuous(minor_breaks = NULL) +
    scale_y_continuous(limits = c(0, NA), minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    rwmisc::theme_rw()

  if (!is.null(facet)) {
    p <- p + facet_wrap(facet, ncol = n_col)
  }

  p
}

# run ---------------------------------------------------------------------

dat <- get_data(file_db, years)
res <- calc_trend(dat)

rwmisc::summary2(res)

res %>%
  filter(year >= 1991, !is.na(educ)) %>%
  mutate(educ = factor(educ, c("hs-or-less", "assoc-some", "bachelor", "advanced"))) %>%
  plot_trend(y = "d", color = "sex", facet = "educ", n_col = 5)
