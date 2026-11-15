# Five label techniques in ggplot2, using public interest-rate data
# Data: FRED (OECD short-term rates, Fed funds) and ECB Data Portal (DFR, M3)
# install.packages(c("ggplot2", "ggrepel", "scales", "remotes"))
# remotes::install_github("jimjam-slam/ggflags")

library(ggplot2)
library(ggrepel)
library(scales)

# ---- data -----------------------------------------------------------
fred <- function(id) {
  d <- read.csv(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id))
  d <- data.frame(date = as.Date(d[[1]]), value = suppressWarnings(as.numeric(d[[2]])))
  d[!is.na(d$value), ]
}

ecb <- function(key) {
  d <- read.csv(paste0("https://data-api.ecb.europa.eu/service/data/", key, "?format=csvdata"))
  t <- d$TIME_PERIOD
  t <- ifelse(nchar(t) == 7, paste0(t, "-01"), t)   # monthly "YYYY-MM" -> date
  data.frame(date = as.Date(t), value = d$OBS_VALUE)
}

# Country averages of OECD immediate (interbank) rates since 2000
cc <- c("US", "GB", "JP", "CA", "CH", "AU", "NZ", "SE",
        "NO", "DK", "KR", "MX", "PL", "HU", "CZ", "ZA")
rates <- do.call(rbind, lapply(cc, function(iso) {
  d <- fred(paste0("IRSTCI01", iso, "M156N"))
  d <- d[d$date >= as.Date("2000-01-01"), ]
  data.frame(iso2 = iso, mean = mean(d$value), sd = sd(d$value))
}))


# ---- text labels with ggrepel --------------------------------------------
ggplot(rates, aes(mean, sd)) +
  geom_point(size = 1.5) +
  geom_label_repel(aes(label = iso2), size = 3, fill = "grey95",
                   min.segment.length = 0, seed = 1) +
  labs(x = "Average rate since 2000, %", y = "Standard deviation, pp") +
  theme_void() +
  theme(axis.text        = element_text(colour = "grey30", size = 9),
        axis.title.x     = element_text(colour = "grey30", size = 9, margin = margin(t = 6)),
        axis.title.y     = element_text(colour = "grey30", size = 9, angle = 90, margin = margin(r = 6)),
        panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
        plot.margin      = margin(10, 10, 10, 10))


# ---- annotate("label") for structural breaks -----------------------------
dfr <- ecb("FM/D.U2.EUR.4F.KR.DFR.LEV")   # ECB deposit facility rate, daily

brk <- data.frame(
  date  = as.Date(c("2008-10-15", "2014-06-11", "2022-07-27")),
  label = c("Financial crisis", "Negative rates", "Hiking cycle")
)

ggplot(dfr, aes(date, value)) +
  geom_vline(xintercept = brk$date, linetype = "dotted", colour = "grey50") +
  geom_hline(yintercept=0, color="black", linetype="dashed") +
  geom_step() +
  annotate("label", x = brk$date, y = 4.6, label = brk$label,
           hjust = 0, size = 3, fill = "white") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  theme_void() +
  theme(axis.text          = element_text(colour = "grey30", size = 9),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        plot.margin        = margin(10, 10, 10, 10))


# ---- min / max values as labels, no y axis -------------------------------
ff  <- fred("FEDFUNDS")
ff  <- ff[ff$date >= as.Date("2000-01-01"), ]
ext <- ff[c(which.min(ff$date), which.max(ff$value), which.min(ff$value),  which.max(ff$date)), ]

ggplot(ff, aes(date, value)) +
  geom_line(linewidth=.75) +
  geom_point(data = ext) +
  geom_label(data = ext, aes(label = paste0(date, ": \n", sprintf("%.2f%%", value))),
             vjust = c(1.2, -.2, -.4, -.4), hjust=c(.3, .2, .5, .8), size = 3, fill="white") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_void() +
  theme(axis.text.x = element_text(colour = "grey30", size = 9),
        plot.margin = margin(10, 10, 10, 10)) +
  theme(panel.grid.major=element_line(color="grey", linewidth=.3),
        plot.margin        = margin(10, 10, 10, 10))


# ---- Euro-formatted axis labels ------------------------------------------
m3 <- ecb("BSI/M.U2.Y.V.M30.X.1.U2.2300.Z01.E")   # euro area M3, EUR millions

ggplot(m3, aes(date, value)) +
  geom_line(linewidth=0.75) +
  scale_y_continuous(
    labels = label_currency(prefix = "", suffix = " Mrd. €", scale = 1e-3,
                                  big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_void() +
  theme(axis.text          = element_text(colour = "grey30", size = 9),
        axis.text.y        = element_text(hjust = 1, margin = margin(r = 4)),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        plot.margin        = margin(10, 10, 10, 10))
