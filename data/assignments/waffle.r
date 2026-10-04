library(tidyverse)
library(waffle)
library(MetBrewer)

# faclevel <- c("Bottom 50%","Next 40%","Top 6-10%","Top 2-5%","Richest 1%")
# df <- tibble(labels=factor(faclevel,levels=rev(faclevel)),
#        'Population share'=c(50,40,5,4,1),
#        'Net wealth share'=c(3,31,11,16,39))
# plotdf <- df |> 
#      pivot_longer(-labels, names_to = "share", values_to = "value")
# write.csv(plotdf, "waffle.csv")


raw <- read.csv("waffle.csv")

plotdat <- raw |> 
  mutate(labels = factor(labels, levels = c("Bottom 50%", "Next 40%", "Top 6-10%", "Top 2-5%", "Richest 1%")),
          share = factor(share, levels = c("Population share", "Net wealth share")))

plotdat |> 
  ggplot() +
  geom_waffle(aes(fill = labels, values = value), size = 1.1, n_rows = 5, 
              na.rm=T, color = "white", make_proportional = T) + 
  facet_wrap(~share, ncol = 1) +
  scale_fill_manual(values = met.brewer("Lakota"), 
                    guide = guide_legend(reverse = T), 
                    name = NULL) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  labs(title = "Net wealth shares in Austria",
       caption = "Source: HFCS, OeNB. Figure: @matschnetzer") +
  theme_minimal(base_family = "Roboto Condensed") +
  coord_equal() +
  theme_enhance_waffle() +
  theme(strip.text.x=element_text(size = 13, margin=margin(b = 5, t = 5), hjust = 0),
        plot.caption = element_text(margin = margin(t = 4),
                                    size = 7),
        plot.title = element_text(margin = margin(b = 6), size = 16),
        legend.title=element_text(size = 9))

ggsave("waffle.png", width=8, height=4, dpi=320, bg = "white")
