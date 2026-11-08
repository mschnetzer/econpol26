###############
## 05 SCALES ##
###############

librarian::shelf(tidyverse, countrycode)

load("05_scales.RData")

# Data into long format with "pivot_longer", capitalize country codes 
# with "toupper" and merge country names from countrycode package 
# with "left_join"
findat <- hfcs |> 
  pivot_longer(-measure, names_to = "country", values_to = "value") |> 
  mutate(cname = countrycode(country, 
                      origin = "iso2c", destination = "country.name.en", 
                      nomatch = "Euro Area")) |> 
  mutate(cname = fct_reorder(cname, value)) 

final <-findat |> 
  ggplot(aes(x = value, y = cname, color = measure)) +
  geom_line(color = "gray80", alpha = 0.15, linewidth = 2) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(NA, 1200),
                     breaks = seq(0, 1200, 400),
                     labels = scales::number_format(prefix = "€", suffix = "K", big.mark = "")) +
  labs(x = NULL, y = NULL, color = NULL, 
       title = "How is net wealth distributed in Europe?",
       subtitle = "Percentiles of net wealth distributions in 1,000 Euros",
       caption = "Data: HFCS, ECB. Figure: @matschnetzer") + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title.position = "plot",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2, color = "gray80"),
        legend.position = c(0.8, 0.3))


# Now let's try different color schemes

librarian::shelf(RColorBrewer, MetBrewer, wesanderson, futurevisions)

## Ugly manual:
final + scale_color_manual(values = c("red", "blue", "green", "brown"))

## RColorBrewer: https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
scales::show_col(brewer.pal(name="Dark2", n=8))
final + scale_color_manual(values = brewer.pal(name="Dark2", n=4))

## MetBrewer: https://github.com/BlakeRMills/MetBrewer
final + scale_color_manual(values = met.brewer("Juarez")[-3])

## wesanderson: https://github.com/karthik/wesanderson
final + scale_color_manual(values = wes_palette(name="Darjeeling1"))

## futurevisions: https://github.com/JoeyStanley/futurevisions
final + scale_color_manual(values = futurevisions("mars"), 
        guide = guide_legend(override.aes = list(size = 4)))
