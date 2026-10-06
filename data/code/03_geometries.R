###########################
## 03 DATA VISUALIZATION ##
###########################

# Load general packages
library(tidyverse)
library(lubridate) # for dates and times
library(scales) # for scale layouts (breaks and labels)

##############
## PENGUINS ##
##############

# Load and assign data
data <- penguins

# Take brief look
head(data)

# Calculate median and standard deviation for bill length and depth
data_summary <- data |>
  group_by(species) |>
  summarise(across(c(bill_len, bill_dep),
                   list(median = ~median(., na.rm = TRUE), 
                        sd = ~sd(., na.rm = TRUE))))

# Scatter plot with error bars by species
# Idea by Cedric Scherer: https://www.behance.net/gallery/101517403/Bill-Dimensions-of-Penguins
data |> ggplot(aes(x = bill_len, 
                   y = bill_dep, 
                   color = species)) +
  # Error bars at the median with the standard deviations
  # Attention: we take other data here with new aesthetics, so inherit.aes = F
  geom_errorbar(
    data = data_summary,
    aes(x = bill_len_median,
        ymin = bill_dep_median - bill_dep_sd,
        ymax = bill_dep_median + bill_dep_sd,
        color = species,
        color = after_scale(colorspace::darken(color, .2, space = "combined"))
    ),
    inherit.aes = F, width = .8, linewidth = .8
  ) +
  geom_errorbar(
    data = data_summary,
    aes(y = bill_dep_median,
        xmin = bill_len_median - bill_len_sd,
        xmax = bill_len_median + bill_len_sd,
        color = species,
        color = after_scale(colorspace::darken(color, .2, space = "combined"))
    ),
    inherit.aes = F, width = .8, linewidth = .8
  ) +
  geom_point(size = 1.5, alpha = 0.5) +
  scale_color_manual(name = NULL,
                     values = MetBrewer::met.brewer("Lakota")) +
  scale_x_continuous(labels = scales::number_format(suffix="mm")) +
  scale_y_continuous(labels = scales::number_format(suffix="mm", accuracy = 1)) +
  # Add labels in the plot rather than in legend
  annotate("text", x = c(34.7, 55.7, 50.7), y = c(20.7, 19, 13.6), 
           color = MetBrewer::met.brewer("Lakota")[1:3], 
           label = c("Adélie","Chinstrap","Gentoo"), fontface = "bold", size = 4) +
  labs(x = "Bill length", y = "Bill depth",
       title = "Penguins are awesome",
       subtitle = "Depth and length of bills") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 13),
        panel.grid.minor = element_blank())
  



##############
## EUROSTAT ##
##############

library(eurostat)
library(geomtextpath)
library(MetBrewer)

# Search datasets for median income
search_eurostat("median income") |> View()

# Get data for ilc_di03
rawinc <- get_eurostat("ilc_di03", time_format = "num", type = "label", filters = list(geo = c("AT","FR","IT","DE","ES")))

# Alternatively, load local RData file
# load("03_geometries.RData")

View(rawinc)

inc <- rawinc |> 
  filter(age == "Total", sex == "Total", 
         unit == "Purchasing power standard (PPS)") |>  
  filter(time %in% 2005:2023)


## Let's try different geometries
# 1. Line plot with evolution of median income
inc |> filter(str_starts(indic_il, "Median")) |>  
  ggplot(aes(x = time, y = values, group = geo, color = geo)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = NULL, values = met.brewer("Juarez")) +
  scale_y_continuous(labels = scales::number_format(prefix = "€", big.mark = ",")) +
  labs(x = NULL, y = NULL,  title = "Evolution of median household income 2005-2023", 
       subtitle = "Median income in € (PPS)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")
ggsave("plots/inc_evolution.png", width = 6, height = 4, dpi = 320)

# Annotation within the plot; for casual style, try "stat = 'smooth'"
library(geomtextpath)
inc |> filter(str_starts(indic_il, "Median")) |>
  ggplot(aes(x = time, y = values, group = geo, color = geo)) +
  geomtextpath::geom_textline(aes(label = geo), hjust = 0.7, vjust = 0.5, 
                              size = 3, fontface = "bold", linewidth = 0.8) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_y_continuous(labels = scales::number_format(prefix = "€", big.mark = ",")) +
  labs(x = NULL, y = NULL, title = "Evolution of median household income 2005-2023", 
       subtitle = "Median income in € (PPS)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("plots/inc_evolution_label.png", width = 6, height = 4, dpi = 320)
  

# 2. Barplot with 2023 mean values
inc |> filter(str_starts(indic_il, "Mean")) |> 
  slice_max(time, by = geo) |> 
  ggplot(aes(x = geo, y = values)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = NULL, title = "Mean household income 2023", 
       subtitle = "Mean income in € (PPS)") +
  theme_minimal()

# 3. Facets of these barplots
inc |> filter(str_starts(indic_il, "Mean"), time > 2018) |> 
  ggplot(aes(x = time, y = values, fill = geo)) +
  geom_bar(stat = "identity") +
  facet_wrap(~geo) +
  scale_fill_manual(values = met.brewer("Juarez")) +
  labs(x = NULL, y = NULL, title = "Mean household income 2018-2023", 
       subtitle = "Mean income in € (PPS)") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")


# 4. Lollipop chart 
inc |> filter(str_starts(indic_il, "Mean"), time > 2018) |> 
  ggplot(aes(x = time, y = values)) + 
  geom_segment(aes(xend = time, yend = 0), color = "gray80", linewidth = 2.5) + 
  geom_hline(yintercept = 0, color = "black", size = 0.3) + 
  geom_point(aes(color = geo), size = 2.5) +
  facet_wrap(~geo, nrow = 1) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_y_continuous(labels = scales::number_format(scale = 1/1000, prefix = "€",
                                                    suffix ="K")) +
  labs(x = NULL, y = NULL, title = "Mean household income 2018-2023") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, unit = "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = .5))
