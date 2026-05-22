##############################
## 08 REDISTRIBUTION · MAPS ##
##############################

librarian::shelf(tidyverse, eurostat, giscoR, sf)

# Map: Disposable income on NUTS-2 level

# Load data via eurostat package or locally: load("08_redistribution.RData")
raw_inc <- get_eurostat("nama_10r_2hhinc", time_format = "raw", 
                         filters = list(unit = "PPS_EU27_2020_HAB", time = "2021", 
                                        na_item = "B6N", direct = "BAL"))

# Load EU map with NUTS 2 level
eumap_nuts2 <- gisco_get_nuts(nuts_level = "2", resolution = "10", year = 2021, epsg = "3035")

# Load EU map with countries (for background) 
bgmap <- gisco_get_countries(resolution = "10", year = 2024, epsg = "3035")

# Cut disposable income data into 8 classes (style: "equal" for equal distance between thresholds, "quantile" for equal group sizes, etc.)
inc_data <- raw_inc |> drop_na() |>   
  mutate(cat = cut_to_classes(values, n = 8, style = "quantile"),
         cat = str_replace_all(cat, "~< ", "- <"),
         cat = fct_reorder(cat, values))

# Transform to ETRS89 projection
plotdat <- left_join(inc_data, eumap_nuts2) |> 
  st_as_sf()

plotdat |> 
  ggplot() + 
  geom_sf(data = bgmap, fill = "gray90", color = "white", linewidth = 0.1) +
  geom_sf(aes(fill = cat), linewidth = 0.05, color = "white") +
  annotate("text", x = 3200000, y = 4400000, label = "Income differences in\nthe European Union",
           family = "Alfa Slab One", size = 5, hjust = 0.5, lineheight = 1) +
  scale_fill_brewer(palette = "YlOrRd", na.translate = F,
                    name = "Disposable income\nin PPP Euro",
                    guide = guide_legend(keywidth = 0.5, keyheight = 1.2)) +
  coord_sf(xlim = c(2500000, 6500000), ylim = c(1600000, 5200000)) +
  labs(caption = "Data: Eurostat (nama_10r_2hhinc)") +
  theme_void(base_family = "Barlow Condensed") +
  theme(panel.background = element_rect(fill = "aliceblue", color = NA),
        legend.position = "inside", 
        legend.position.inside = c(0.9, 0.75),
        plot.caption = element_text(color = "gray40", size = 8))

ggsave("plots/08_redistribution.png", width = 6.5, height = 6, dpi = 320, bg = "aliceblue")
