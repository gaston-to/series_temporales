library(tidyverse)
library(fpp2)
library(GGally)

autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands") +
  theme_bw()

autoplot(melsyd)

autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

##
## SCATTER PLOTS
##
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")


# Suponiendo que elecdemand tiene 17520 filas (365*24*2)
start_time <- as.POSIXct("2014-01-01 00:00:00")
time_index <- start_time + minutes(30) * (seq_len(nrow(elecdemand)) - 1)

df <- as_tibble(elecdemand)
df$Time <- time_index
df <- df %>%
  select(-WorkDay) %>%
  pivot_longer(cols = -Time, names_to = "Variable", values_to = "value")

ggplot(df, aes(x = Time, y = value)) +
  geom_line() +
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +
  facet_grid(Variable ~ ., scales = "free_y") +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")


df %>%
  pivot_wider(names_from = Variable, values_from = value) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  # geom_smooth() +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")

# -----------------------------------------------------------------------------
# Scatter plot matrix
# -----------------------------------------------------------------------------

visnights %>% glimpse()

autoplot(visnights, facets = TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Nightly visitors to Australian capital cities")

GGally::ggpairs(
  visnights[, 1:5] %>% as.data.frame()
)

# ausber es una ts que contiene datos trimestrales de cerveza en Australia
# desde 1956 hasta 1995

?ausbeer
autoplot(ausbeer %>% window(start = 1992))

ausbeer %>% window(start = 1992) %>% glimpse

ggseasonplot(ausbeer)
