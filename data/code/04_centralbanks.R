library(tidyverse)
library(showtext)   # install.packages("showtext")

# slide fonts (downloaded from Google Fonts at render time)
font_add_google("Ubuntu", "ubuntu", regular.wt = 300, bold.wt = 500)
font_add_google("Roboto Slab", "robotoslab", regular.wt = 300, bold.wt = 700)
font_add_google("Yanone Kaffeesatz", "yanone", regular.wt = 400, bold.wt = 700)
knitr::opts_chunk$set(fig.showtext = TRUE)

# slide colors (econpol_slides.scss)
pal <- c(blue = "#5d8198", red = "#e76254", dred = "#d03161", yellow = "#ffd500",
         green = "#218500", brown = "#d37b00",
         grey200 = "#e9ecef", grey400 = "#ced4da", grey500 = "#adb5bd",
         grey600 = "#868e96", grey800 = "#343a40", grey900 = "#212529")

# ordered palette from blue to red, for many ordered categories
pal_ramp <- function(n) colorRampPalette(c(pal[["blue"]], pal[["red"]]))(n)

# ggplot theme matching the slides:
# Ubuntu for text, Roboto Slab for numbers, Yanone Kaffeesatz in red for titles
theme_econpol <- function(base_size = 12,
                          text_x = TRUE, text_y = TRUE,      # axis text (tick labels)
                          title_x = TRUE, title_y = TRUE) {  # axis titles
  th <- theme_minimal(base_size = base_size, base_family = "ubuntu") +
    theme(
      text          = element_text(colour = pal[["grey800"]]),
      panel.grid    = element_blank(),
      axis.text     = element_text(family = "robotoslab", colour = pal[["grey600"]],
                                   size = rel(0.85)),
      axis.title.x  = element_text(margin = margin(t = 10)),
      axis.title.y  = element_text(margin = margin(r = 10)),
      plot.title    = element_text(family = "yanone", colour = pal[["red"]],
                                   size = rel(1.8), margin = margin(b = 6)),
      plot.subtitle = element_text(colour = pal[["grey600"]]),
      plot.caption  = element_text(family = "robotoslab", colour = pal[["grey600"]],
                                   size = rel(0.7), hjust = 0),
      legend.position = "bottom",
      legend.title    = element_blank(),
      legend.key.width = unit(1, "null")
    )
  if (!text_x)  th <- th + theme(axis.text.x  = element_blank())
  if (!text_y)  th <- th + theme(axis.text.y  = element_blank())
  if (!title_x) th <- th + theme(axis.title.x = element_blank())
  if (!title_y) th <- th + theme(axis.title.y = element_blank())
  th
}


library(tidyverse)   # includes lubridate
library(eurostat)    # install.packages("eurostat")

# euro area members (ECB country codes)
ea    <- c("AT","BE","BG","CY","DE","EE","ES","FI","FR","GR","HR",
           "IE","IT","LT","LU","LV","MT","NL","PT","SI","SK")
cc    <- paste(ea, collapse = "+")
start <- "2015-01"

# helper: ECB Data Portal API -> tibble
ecb_get <- function(flow, key, start = "2015-01") {
  url <- paste0("https://data-api.ecb.europa.eu/service/data/", flow, "/", key,
                "?format=csvdata&startPeriod=", start)
  read_csv(url, show_col_types = FALSE) |>
    select(REF_AREA, TIME_PERIOD, OBS_VALUE)
}

# 1. ECB deposit facility rate (daily -> monthly average)
dfr <- ecb_get("FM", "D.U2.EUR.4F.KR.DFR.LEV", start) |>
  mutate(date = floor_date(as.Date(TIME_PERIOD), "month")) |>
  group_by(date) |>
  summarise(dfr = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop")

# 2. HICP inflation, annual rate of change
inflation <- ecb_get("HICP", paste0("M.", cc, ".N.000000.4D0.ANR"), start) |>
  transmute(country = REF_AREA, date = ym(TIME_PERIOD), inflation = OBS_VALUE)

# 3. Cost of borrowing for house purchase, new business
mortgage <- ecb_get("MIR", paste0("M.", cc, ".B.A2C.AM.R.A.2250.EUR.N"), start) |>
  transmute(country = REF_AREA, date = ym(TIME_PERIOD), mortgage_rate = OBS_VALUE)

# 4. Unemployment rate, seasonally adjusted (Eurostat uses EL for Greece)
unemployment <- get_eurostat(
  "une_rt_m",
  filters = list(geo = recode(ea, GR = "EL"), s_adj = "SA",
                 age = "TOTAL", sex = "T", unit = "PC_ACT"),
  time_format = "date") |>
  rename_with(~ "date", any_of(c("time", "TIME_PERIOD"))) |>
  transmute(country = recode(geo, EL = "GR"), date, unemployment = values) |>
  filter(date >= ym(start))

# combine into one panel
panel <- inflation |>
  left_join(mortgage,     by = c("country", "date")) |>
  left_join(unemployment, by = c("country", "date")) |>
  left_join(dfr,          by = "date") |>
  mutate(period = case_when(
    date < as.Date("2022-07-01") ~ "Negative rates",   # first hike July 2022
    date < as.Date("2024-06-01") ~ "Tightening",       # first cut June 2024
    TRUE                         ~ "Easing"
  ) |> factor(levels = c("Negative rates", "Tightening", "Easing"))) |>
  select(date, country, period, dfr, inflation, mortgage_rate, unemployment) |>
  arrange(country, date)

panel |> 
  group_by(country) |> 
  summarize(inflation=mean(inflation, na.rm=TRUE)) |> 
  mutate(country=fct_reorder(country, inflation)) |> 
  ggplot() +
  geom_col(aes(y=country, x=inflation, 
               fill=(country=="AT"))) +
  geom_text(aes(y=country, 
                x=inflation + .5, 
                label=sprintf("%.2f%%", inflation)),
            family="robotoslab",
            color=pal[["grey800"]],
            size=3.2) +
  geom_vline(xintercept=mean(panel$inflation, na.rm=TRUE), 
             linetype=3, 
             color=pal[["grey600"]]) +
  scale_fill_manual(values=c("FALSE"=pal[["blue"]], "TRUE"=pal[["dred"]]),
                    guide="none") +
  theme_econpol(text_x=FALSE, text_y=FALSE, title_x=FALSE, title_y=FALSE)

panel |> 
  filter(country=="AT") |> 
  ggplot() +
  geom_line(aes(x=date, y=dfr), color=pal[["blue"]], linewidth = 1) +
  geom_line(aes(x=date, y=inflation), color=pal[["red"]], linewidth = 1) +
  annotate("text",
           x=as.Date("2020-01-01"), 
           y=3, 
           label="Inflation", 
           color=pal[["red"]],
           family="ubuntu") +
  annotate("text", 
           x=as.Date("2024-10-01"), 
           y=1, 
           label="Deposit Facility Rate", 
           color=pal[["blue"]],
           family="ubuntu") +
  annotate("text", 
           y=(panel |> 
                filter(country=="AT") |> 
                filter(inflation==max(inflation)) |> 
                pull(inflation) |> 
                max()) 
           + .5,
           x=(panel |>
                filter(country=="AT") |> 
                filter(inflation==max(inflation)) |>
                pull(date) |> 
                min()),
           label=sprintf("%.1f%%", panel |> 
                           filter(country=="AT") |> 
                           filter(inflation==max(inflation)) |> 
                           pull(inflation) |> 
                           max()),
           color=pal[["red"]],
           family="robotoslab"
  ) +
  annotate("text", 
           y=(panel |> 
                filter(country=="AT") |> 
                filter(date==min(date)) |> 
                pull(inflation) |> 
                max()) 
           + 1.5,
           x=(panel |>
                filter(country=="AT") |> 
                filter(date==min(date)) |>
                pull(date) |> 
                min()),
           label=sprintf("%.1f%%", panel |> 
                           filter(country=="AT") |> 
                           filter(date==min(date)) |> 
                           pull(inflation) |> 
                           max()),
           color=pal[["red"]],
           family="robotoslab"
  ) +
  annotate("text", 
           y=(panel |> 
                filter(country=="AT") |> 
                filter(dfr==max(dfr)) |> 
                pull(dfr) |> 
                max()) + .5,
           x=(panel |> 
                filter(country=="AT") |> 
                filter(dfr==max(dfr)) |> 
                pull(date) |> 
                max()),
           label=sprintf("%.1f%%", 
                         panel |> 
                           filter(country=="AT") |> 
                           filter(dfr==max(dfr)) |> 
                           pull(dfr) |> 
                           max()),
           color=pal[["blue"]],
           family="robotoslab"
  ) +
  annotate("text", 
           y=(panel |> 
                filter(country=="AT") |> 
                filter(date==min(date)) |> 
                pull(dfr) |> 
                max()) + .5,
           x=(panel |> 
                filter(country=="AT") |> 
                filter(date==min(date)) |> 
                pull(date) |> 
                max()),
           label=sprintf("%.1f%%", 
                         panel |> 
                           filter(country=="AT") |> 
                           filter(date==min(date)) |> 
                           pull(dfr) |> 
                           max()),
           color=pal[["blue"]],
           family="robotoslab"
  ) +
  theme_econpol(text_y=FALSE, title_x=FALSE, title_y=FALSE) +
  theme(axis.line.x=element_line(color=pal[["grey500"]]))

panel |> 
  mutate(country=fct_reorder(country, mortgage_rate, .fun=mean, na.rm=TRUE)) |> 
  ggplot() +
  geom_point(aes(x=dfr, y=mortgage_rate, color=country), 
             alpha=0.3, 
             size=4) +
  scale_color_manual(values=pal_ramp(n_distinct(panel$country))) +
  guides(color = guide_legend(nrow = 2,
                              override.aes = list(alpha = 1)
  )
  ) +
  labs(x="Deposit Facility Rate", y="Mortgage Rate") +
  scale_y_continuous(breaks=c(1,3,5)) +
  theme_econpol()

panel |> 
  ggplot() +
  geom_density(aes(x=inflation, fill=period), alpha=0.6, color=NA) +
  scale_fill_manual(values=c("Negative rates"=pal[["blue"]],
                             "Tightening"=pal[["red"]],
                             "Easing"=pal[["grey500"]])) +
  labs(x="Inflation") +
  theme_econpol(text_y=FALSE, title_y=FALSE)
