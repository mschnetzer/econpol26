#######################
## 09 POLICY · THEME ##
#######################

librarian::shelf(tidyverse, eurostat)

# Load sectoral balances data for Austria
rawdat <- get_eurostat("tipsnf10", 
                       filters = list(geo = "AT"), 
                       time = "date", type = "label")

# Load GDP data
gdpdat <- get_eurostat("nama_10_gdp", 
                       filters = list(geo = "AT", unit = "CP_MEUR", na_item = "B1GQ"), 
                       time = "date", type = "label")

# Alternatively, load prepared data 
#load("09_policy.RData")

findat <- rawdat |> 
  mutate(values = ifelse(sector == "Total economy", values*-1, values),
         sector = case_match(sector,
                "Total economy" ~ "Rest of World",
                "Households; non-profit institutions serving households" ~ "Households/NPISH",
                             .default = sector)) |> 
  left_join(gdpdat |> select(time, gdp = values)) |> 
  mutate(shares = values/gdp*100,
         sector = factor(sector, 
                         levels=c("General government","Non-financial corporations",
                            "Financial corporations", "Households/NPISH", "Rest of World")))

findat |> 
  ggplot(aes(x = time, y = shares, fill = sector)) +
  geom_bar(stat = "identity", position = "stack") + 
  geom_hline(yintercept = 0, color = "black", linewidth = 0.9) +
  scale_fill_manual(name = NULL, values = MetBrewer::met.brewer("Lakota")) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  scale_x_date(expand = c(0.01,0.01)) +
  labs(x = NULL, y = NULL, title = "Sectoral balances in % of GDP") +
  theme_minimal(base_family = "Barlow Condensed", base_size = 14) +
  theme(plot.background = element_rect(fill = "bisque"),
        plot.margin = margin(t = 1, b = 1, l = 1, r = 1, unit = "lines"),
        plot.title.position = "plot",
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(margin = margin(b = 1, unit = "lines")),
        plot.caption = element_text(margin = margin(t = 2, unit = "lines"), size = 7),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.1, color = "gray40"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 10))
