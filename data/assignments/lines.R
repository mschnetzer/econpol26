library(tidyverse)
library(ggrepel)

# Download data from OeNB website
# raw <- read_csv2("rates.csv", col_select = c("Jahr", "Monat", "Indikator", "Werte"), 
#           locale = locale(encoding = "ISO-8859-1"))
# raw |> select(year = Jahr, month = Monat, country = Indikator, value = Werte) |> 
#   mutate(country = case_when(
#     str_detect(country, "Euroraum") ~ "EA",
#     str_detect(country, "Königreich") ~ "GB",
#     str_detect(country, "USA") ~ "USA",
#     str_detect(country, "Japan") ~ "JP"
#   )) |> 
#      filter(country %in% c("EA", "GB", "USA", "JP")) -> rates
# write.csv(rates, "lines.csv")

raw <- read_csv("lines.csv") |> 
     mutate(date = make_date(year, month))

raw |> 
  ggplot(aes(x = date, y = value, color = country)) + 
  geom_line(linewidth = 0.9) +
  geom_text_repel(aes(label = country), size = 2.8, hjust = 0, direction = "y",
            position = position_nudge(x = 30),
            data = raw |> slice_max(date, by = country)) +
  scale_y_continuous(labels = scales::number_format(suffix = "%")) +
  scale_x_date(limits = c(as.Date("2010-01-01"), NA), expand = c(0.05,0.05)) +
  labs(x = NULL, y = NULL,
       title = "Evolution of interest rates",
       subtitle = "Base interest rates of four central banks, 2010-2026",
       caption = "Source: OeNB. Figure: @matschnetzer") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(legend.position = "none", 
        plot.title.position = "plot",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12, margin = margin(b = 1, unit = "lines")),
        plot.caption = element_text(size = 8, margin = margin(t = 1, unit = "lines")),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.1))

ggsave("lines.png", width = 8, height = 4.5, dpi = 320, bg = "white")
