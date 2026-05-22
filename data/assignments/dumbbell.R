library(tidyverse)

# Load and save data
# raw <- eurostat::get_eurostat(id = "tessi190", time_format = "num")

# Load data
raw <- read_csv("dumbbell.csv")

# Show all available countries, but we don't want country groups (EA18, EU15, etc.) 
unique(raw$geo)
filtered <- raw |> filter(str_length(geo) < 3, TIME_PERIOD %in% c(2015,2024))

# Not all countries have both years available -> filter those with both values!
filtered |> count(geo)
plotdata <- filtered |> filter(n() > 1, .by = geo) |> 
  mutate(geo = fct_reorder2(geo, TIME_PERIOD, values, .desc = T))

plotdata |> 
  ggplot(aes(x = geo, y = values)) +
  geom_line(aes(group = geo), linewidth = 3, color = "gray90") +
  geom_point(aes(color = factor(TIME_PERIOD)), size = 3) +
  scale_color_manual(name = NULL, values = c("goldenrod1","midnightblue"),
                     guide = guide_legend(direction = "horizontal")) + 
  labs(x = NULL, y = "Gini index", title = "Change in income inequality in Europe",
       subtitle = "Gini coefficient of disposable household income, 2015-2024",
       caption = "Source: Eurostat [tessi190]. Figure: @matschnetzer") +
  theme_minimal() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.75,0.85),
        legend.text = element_text(size = 12),
        plot.title.position = "plot",
        plot.caption = element_text(size = 7,
                                    margin = margin(t = 10, b= 0, unit = "pt")),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

ggsave("dumbbell.png", width = 8, height = 5, dpi = 320, bg = "white")
