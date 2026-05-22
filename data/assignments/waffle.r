library(tidyverse)
library(waffle)
library(MetBrewer)

# raw <- tribble(~Parents, ~Children, ~Share,
#                "Compulsory school", "Compulsory school", 36.2,
#                "Compulsory school", "Apprenticeship/Lower secondary", 42.3,
#                "Compulsory school", "Higher secondary", 12.1,
#                "Compulsory school", "Tertiary education", 9.4,
#                "Apprenticeship/Lower secondary", "Compulsory school", 8.9,
#                "Apprenticeship/Lower secondary", "Apprenticeship/Lower secondary", 54.1, #
#                "Apprenticeship/Lower secondary", "Higher secondary", 18.7,
#                "Apprenticeship/Lower secondary", "Tertiary education", 18.3,
#                "Higher secondary", "Compulsory school", 5.9,
#                "Higher secondary", "Apprenticeship/Lower secondary", 20.8,
#                "Higher secondary", "Higher secondary", 30.8,
#                "Higher secondary", "Tertiary education", 42.5,
#                "Tertiary education", "Compulsory school", 5.8,
#                "Tertiary education", "Apprenticeship/Lower secondary", 11.7,
#                "Tertiary education", "Higher secondary", 21.2,
#                "Tertiary education", "Tertiary education", 61.3
# )

raw <- read.csv("waffle.csv")

plotdata <- raw |> filter(Parents %in% c("Apprenticeship/Lower secondary",
                              "Tertiary education")) |> 
  mutate(Children = factor(Children, levels = c("Tertiary education",
                                                "Higher secondary",
                                                "Apprenticeship/Lower secondary",
                                                "Compulsory school")),
         Parents = ifelse(Parents == "Tertiary education", "Parents with tertiary education", "Parents with apprenticeship/lower secondary"))

plotdata |> 
  ggplot() +
  geom_waffle(aes(fill = Children, values = Share), size = 1.1, n_rows = 5, 
              na.rm=T, color = "white", make_proportional = T) + 
  facet_wrap(~Parents, ncol = 1) +
  scale_fill_manual(values = met.brewer("Lakota"), 
                    name = "Education of descendants (25-44 years)") +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  labs(title = "Educational persistence in Austria",
       caption = "Source: Bildung in Zahlen 2023/24, Statistics Austria. Figure: @matschnetzer") +
  theme_minimal(base_family = "Roboto Condensed") +
  coord_equal() +
  theme_enhance_waffle() +
  theme(strip.text.x=element_text(size = 13, margin=margin(b = 5, t = 5), hjust = 0),
        plot.caption = element_text(margin = margin(t = 4),
                                    size = 7),
        plot.title = element_text(margin = margin(b = 6), size = 16),
        legend.title=element_text(size = 9))

ggsave("waffle.png", width=8, height=4, dpi=320, bg = "white")
