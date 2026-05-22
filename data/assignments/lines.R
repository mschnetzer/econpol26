library(tidyverse)
library(gghighlight)
library(ggrepel)

# dwa <- read_csv("~/Daten/Datasets/DWA/2025_09_24_Data.csv")
# raw <- dwa |> filter(str_detect(TITLE, "Top 5%"), !REF_AREA == "I9") |> 
#  select(country = REF_AREA, quarter = TIME_PERIOD, top5share = OBS_VALUE) 
# write_csv(raw, file = "lines.csv")

raw <- read_csv("lines.csv")

findat <- raw |> 
  mutate(date = yq(quarter),
         country = countrycode::countrycode(country, origin = "iso2c", 
                                            destination = "country.name.en"))

findat |> 
  ggplot() + 
  geom_line(aes(x = date, y = top5share, group = country), 
            linewidth = 0.9, color = "firebrick") +
  gghighlight(country == "Austria", line_label_type = "text_path",
              label_params = list(size = 3, family = "Roboto Condensed", 
                                  hjust = 0.03, vjust = -0.2),
              unhighlighted_params = list(linewidth = 0.4, color = "gray70")) +
  geom_text(aes(x = date, y = top5share, label = country), color = "gray40",
            size = 3, nudge_y = -1.5, nudge_x = -150, family = "Roboto Condensed",
            data = findat |> slice_max(date) |> slice_min(top5share, n = 1)) +
  geom_text(aes(x = date, y = top5share, label = country), color = "gray40",
            size = 3, nudge_y = 1.5, nudge_x = -150, family = "Roboto Condensed",
            data = findat |> slice_max(date) |> slice_max(top5share, n = 1)) +
  scale_y_continuous(labels = scales::number_format(suffix = "%")) +
  scale_x_date(limits = c(as.Date("2014-01-01"), NA), expand = c(0,0)) +
  labs(x = NULL, y = NULL,
       title = "Wealth inequality in the Eura Area",
       subtitle = "Net wealth share of the top 5%, Q1/2014-Q1/2025",
       caption = "Source: Distributional Wealth Accounts, ECB. Figure: @matschnetzer") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12, margin = margin(b = 1, unit = "lines")),
        plot.caption = element_text(size = 8, margin = margin(t = 1, unit = "lines")),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.1))

ggsave("lines.png", width = 8, height = 4.5, dpi = 320, bg = "white")
