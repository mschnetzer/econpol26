##########################
## 06 MOBILITY · COLORS ##
##########################

librarian::shelf(tidyverse, countrycode, ggrepel, RColorBrewer, 
                 MetBrewer, wesanderson, futurevisions, viridis)

load("06_mobility.RData")

# Use countrycode package for country names and contintent
gatsby <- gatsby |> 
  filter(!iso3c == "OECD24") |> 
  mutate(mobility = 1-mobility, 
         iso2c = countrycode(iso3c, origin = "iso3c", destination = "iso2c"),
         country = countrycode(iso3c, origin = "iso3c", destination = "country.name.en"),
         continent = countrycode(iso3c, origin = "iso3c", destination = "continent"))

gatsby |>
  ggplot(aes(x = inequality, y = mobility)) +
  geom_smooth(method = "lm", se = F, color = "darkred") +
  geom_point() +
  geom_text_repel(aes(label=country), size = 3.2, family = "Roboto Condensed", 
                  segment.size = 0.2) +
  labs(x = "Gini coefficient (more inequality →)", 
       y = "Intergenerational earnings elasticity (less mobility →)") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        axis.title.y = element_text(size = 12, margin = margin(r = 0.5, unit="lines")),
        axis.title.x = element_text(size = 12, margin = margin(t = 0.5, unit="lines")),
        axis.text = element_text(size = 10))

# Let's have small multiples (facets): 2 x 2, so we have do drop 1 continent for this excersise
final <- 
gatsby |>
  filter(!continent == "Oceania") |> 
  ggplot(aes(x = inequality, y = mobility, color = continent)) +
  geom_point(color = "gray85", data = gatsby |> select(-continent)) +
  geom_point() +
  geom_text_repel(aes(label=iso2c), size = 3.5, family = "Roboto Condensed", 
                  segment.size = 0.2, box.padding = unit(2, "pt")) +
  facet_wrap(~continent, nrow =2) +
  labs(x = "Gini coefficient (more inequality →)", 
       y = "Intergenerational earnings elasticity (less mobility →)") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.border = element_rect(linewidth = 0.2, color = "gray50", fill = NA),
        strip.text = element_text(size = 12),
        axis.title.y = element_text(size = 12, margin = margin(r = 0.5, unit="lines")),
        axis.title.x = element_text(size = 12, margin = margin(t = 0.5, unit="lines")),
        axis.text = element_text(size = 10))


# Now let's try different color schemes
## RColorBrewer: https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
final + scale_color_manual(values = brewer.pal(name="Dark2", n=4))

## MetBrewer: https://github.com/BlakeRMills/MetBrewer
final + scale_color_manual(values = met.brewer("Juarez")[-3])

## wesanderson: https://github.com/karthik/wesanderson
final + scale_color_manual(values = wes_palette(name="Darjeeling1"))

## futurevisions: https://github.com/JoeyStanley/futurevisions
final + scale_color_manual(values = futurevisions("mars"))

