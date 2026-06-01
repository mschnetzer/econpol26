############################
## 04 INCOME · GEOMETRIES ##
############################

# load packages
library(tidyverse)
library(eurostat)
library(MetBrewer)

# Search datasets for median income
search_eurostat("median income") |> View()

# Get data for ilc_di03
rawinc <- get_eurostat("ilc_di03", time_format = "num", type = "label", filters = list(geo = c("AT","FR","IT","DE","ES")))

# Alternatively, load local RData file
# load("04_income.RData")

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
