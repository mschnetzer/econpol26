library(tidyverse)

# Load and save data
raw <- eurostat::get_eurostat(id = "prc_hicp_minr",
    filters = list(unit = "RCH_A", coicop18 = "TOTAL", 
                    geo = eurostat::ea_countries$code))
write_csv(raw, file = "dumbbell.csv")

# Load data
raw <- read_csv("dumbbell.csv")

# Filter minimum and maximum value by country
filtered <- raw |> 
  summarise(mininf = min(values, na.rm = T), maxinf = max(values, na.rm = T), 
            .by = geo) |> 
  pivot_longer(cols = -geo, names_to = "position", values_to = "values")

# Not all countries have both years available -> filter those with both values!
plotdata <- filtered |> 
  mutate(geo = fct_reorder2(geo, position, values, .desc = F))

plotdata |> 
  ggplot(aes(x = geo, y = values)) +
  geom_line(aes(group = geo), linewidth = 3, color = "gray90") +
  geom_point(aes(color = factor(position)), size = 3) +
  geom_text(aes(label = values), nudge_y = 1, size = 2.5,
            data = plotdata |> filter(position == "maxinf")) +
  geom_text(aes(label = values), nudge_y = -1, size = 2.5,
            data = plotdata |> filter(position == "mininf")) +
  scale_color_manual(name = NULL, values = c("goldenrod1","midnightblue"),
                     labels = c("Maximum", "Minimum"),
                     guide = guide_legend(direction = "horizontal")) + 
  labs(x = NULL, y = "HCPI", title = "Range of inflation rates in Europe",
       subtitle = "Harmonized index of consumer prices (HCPI), 1996-2026",
       caption = "Source: Eurostat [prc_hicp_minr]. Figure: @matschnetzer") +
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
