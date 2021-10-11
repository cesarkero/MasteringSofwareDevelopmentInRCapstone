## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MasteringSofwareDevelopmentInRCapstone)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyr)

## -----------------------------------------------------------------------------
df <- eq_clean_data(NOAArawData='../data-raw/earthquakes-2021-10-06_09-24-38_+0200.tsv')
head(df)

## -----------------------------------------------------------------------------
dfclean <- eq_location_clean(df)

# store in data
# use_data(dfclean)

## -----------------------------------------------------------------------------
dfs <- dfclean %>%
  filter(Year>2015 & Year<2021 & (COUNTRY =='CHINA' | COUNTRY =='GREECE'))

p1 <- dfs %>%
ggplot() +
geom_timeline(aes(x = Date,
                  y = COUNTRY,
                  size = Mag,
                  colour = Deaths)) +
  theme_minimal()+
  theme(legend.position="bottom")

p1

## -----------------------------------------------------------------------------
p2 <- p1 + geom_eqloc(aes(x = Date, y = COUNTRY, locations = LOCATION_NAME, number = 3))
p2

## -----------------------------------------------------------------------------
p3 <- dfs %>% eq_map(annot_col = "Date")
p3

## -----------------------------------------------------------------------------
dfs %>%
  mutate(popup = eq_create_label(.)) %>%
  eq_map(annot_col = "popup")

