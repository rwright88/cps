# trends

library(cps)
library(dplyr)
library(ggplot2)
library(rwmisc)
library(vroom)

file_db <- "~/data/cps/cpsdb"
file_pcepi <- "other/pcepi.csv"
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

calc_stats <- function(data, probs, by1, by2) {
  out <- group_by(data, !!!syms(by1))
  out <- summarise(out, n = n(), pop = sum(asecwt))
  out <- group_by(out, !!!syms(by2))
  out <- mutate(out, percent = pop / sum(pop) * 100)
  out <- ungroup(out)

  wage <- data[which(data$incwage > 0), ]
  wage <- group_by(wage, !!!syms(by1))
  wage <- summarise(wage,
    wage = mean(rwmisc::wtd_quantile(incwage, asecwt, probs = !!probs))
  )
  wage <- ungroup(wage)

  out <- left_join(out, wage, by = by1)
  out$wage[out$n < 100] <- NA
  out
}

inflate <- function(x, year, file_pcepi) {
  pcepi <- suppressMessages(vroom(file_pcepi))
  pcepi_val <- pcepi$pcepi[match(year, pcepi$year)]
  pcepi_cur <- max(pcepi_val)
  x * pcepi_cur / pcepi_val
}

plot_trend <- function(data, y, color = NULL, facet = NULL) {
  if (!is.null(color)) {
    out <- ggplot(data, aes(year, !!sym(y), color = !!sym(color)))
  } else {
    out <- ggplot(data, aes(year, !!sym(y)))
  }

  out <- out +
    geom_line(size = 1.1) +
    scale_x_continuous(breaks = seq(1900, 2100, 10), minor_breaks = NULL) +
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
  filter(age %in% 25:54) %>%
  mutate(age = round((age + 0.1) / 10) * 10) %>%
  calc_stats(probs = seq(0.45, 0.55, 0.01), by1 = c("year", "sex", "age"), by2 = c("year", "sex", "age")) %>%
  mutate(wage = inflate(wage, year, file_pcepi = file_pcepi)) %>%
  plot_trend(y = "wage", color = "sex", facet = "age") +
  scale_y_log10(breaks = seq(1e4, 5e5, 1e4), minor_breaks = NULL, label = scales::comma)
