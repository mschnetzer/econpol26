########################
## 07 GROWTH · LABELS ##
########################

# We learn a new method to load multiple packages with librarian
librarian::shelf(tidyverse, eurostat, MetBrewer, countrycode, ggrepel)

# Here are the new packages we will use
install.packages("ggflags", repos = "https://jimjam-slam.r-universe.dev")
librarian::shelf(ggtext, sysfonts, showtext, ggflags)

# Load data directly from Eurostat or load("07_growth.RData")
rawdat <- get_eurostat("namq_10_gdp", 
                       filters = list(geo = c("AT","DE","FR","ES","IT"), 
                                      na_item = "B1GQ", # GDP at market prices 
                                      s_adj = "SCA", # Season and calendar day adjustment 
                                      unit = "CLV_PCH_SM"), # %-Change to last year period
                       type = "label",       # Get labels instead of Eurostat codes
                       time_format = "date") # Get date in date format

# Add ISO 2 character codes that we will use later
rawdat <- rawdat |> 
  mutate(iso2c = countrycode(geo, origin = "country.name.en", destination = "iso2c"))

# Add Google Font for the labels
sysfonts::font_add_google("Roboto Condensed", family = "Roboto Condensed")
sysfonts::font_add_google("Roboto Slab", family = "Roboto Slab")
# When loading fonts from Google, don't forget the following command!
showtext_auto()
showtext_opts(dpi = 320)

rawdat |>
  filter(time > "2010-01-01") |> 
  ggplot(aes(x = time, y= values, color = geo)) +
  geom_hline(yintercept = 0, linewidth = 0.1, color = "gray40") +
  geom_area(aes(fill = geo)) +
  geom_line(linewidth = 0.6, aes(group = geo)) +
# We take only the minimum values per country (slice_min by geo) and create non-overlapping labels with the ggrepel package. If you do not like the glue-package you can create the labels with paste0: label = paste0("Min:", round(values,1), "%").
  geom_label_repel(data = rawdat |> slice_min(values, n=1, by = geo), 
             size = 2.5, nudge_y = -2, label.padding = unit(0.15,"lines"), 
             family = "Roboto Condensed",
             aes(label = glue::glue("Min: {round(values,1)}%")),
             min.segment.length = unit(2, unit="cm")) +
# Let's do the same with the maximum values. The segment options define the line between the label and the data point. We also include an arrow!
  geom_label_repel(data = rawdat |> slice_max(values, n=1, with_ties = F ,by = geo), 
             size = 2.5, nudge_y = 2, label.padding = unit(0.15, "lines"), 
             family = "Roboto Condensed",
             aes(label = glue::glue("Max: {round(values,1)}%")),
             min.segment.length = unit(2, unit="cm")) +
# ggflags needs 2-digit country codes (that we take from countrycode::codelist above). These should be in small letters, so we execute tolower(iso2c)!
  geom_flag(data = rawdat |> slice_min(time, n=1), size = 6,
            aes(x = as.Date("2011-01-01"), y = 32, country = tolower(iso2c))) +
# Now we add the country names. We need each name only once, that's why we slice the dataframe. As we want the country in capital letters, we execute toupper(geo)
  geom_text(data = rawdat |> slice_min(time, n=1), size = 4, hjust = 0, 
            family = "Roboto Condensed",
            aes(x = as.Date("2013-01-01"), y = 32, label = toupper(geo))) +
# Have percentage symbols on the y-axis
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
# With the colorspace package we can "darken" or "lighten" color palettes! Here the fill scale of the area chart is lighter, the color of the lines are darker.
  scale_color_manual(values = colorspace::darken(met.brewer("Nattier")[5:1], 0.2)) +
  scale_fill_manual(values = colorspace::lighten(met.brewer("Nattier")[5:1], 0.2)) +
# Create small multiples by country
  facet_wrap(~geo, nrow = 1) +
# We can even add Markdown in our labels. The * in the subtitles mean italics. But we have to tell ggplot that subtitle should be rendered as markdown in the theme below!!
  labs(x = NULL, y = NULL,
       title = "Ups and downs",
       subtitle = "Quarterly real GDP growth 2010-2024, *seasonally adjusted*",
       caption = "Data: Eurostat. Figure: @matschnetzer") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(legend.position = "none",
        strip.text = element_blank(),
        plot.title = element_text(family = "Roboto Slab", size = 24),
# Subtitle should be rendered as markdown (from ggtext package) rather than element_text!
        plot.subtitle = element_markdown(color = "gray40", size = 14),
        plot.caption = element_text(color = "gray40", size = 7,
                                    margin = margin(t = 1, unit = "lines")),
        plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing.x = unit(1, "lines"))

ggsave(filename = "gdp.png", width = 10, height = 4.5, dpi = 320)
 