## Data Visualization (QSS17) 17F
## Project 3: Country of Origin of NHL Players
##
## Name: Billy McGrath
## Date: 11/1/17


# Initial Settings
rm(list = ls())

library(tidyverse)
library(gridExtra)


# Function to read in and clean the data
read_and_wrangle <- function(from, to) {
  file.name <- "data/HockeyRelativeAgeData.txt"
  nhl <- read.csv(file.name, stringsAsFactors = FALSE, sep = "\t") %>% 
    select(Country, Year) %>% 
    filter(Year <= to & Year >= from) %>% 
    group_by(Country) %>% 
    summarise(Players = ifelse(length(Country) > 0, 1, 0))
}

nhl.pre1960 <- read_and_wrangle(from = 1800, to = 1960)
nhl.post1960 <- read_and_wrangle(from = 1961, to = 2017)


# Country code data frame
url <- "http://www.qogdata.pol.gu.se/data/qog_bas_cs_jan17.csv"

ccode <- read_csv(url) %>% 
  select(cname, ccodealp) %>% 
  rename(Country = cname,
         id = ccodealp)

#anti_join(nhl.pre1960, ccode)
#anti_join(nhl.post1960, ccode)

#ccode %>% filter(grepl("Netherlands|France|United|Serbia|Korea|South", Country))


# Cleans the data frames to match the country codes
nhl.pre1960 <- nhl.pre1960 %>% 
  mutate(Country = ifelse(Country == "France", "France (1963-)", Country),
         Country = ifelse(Country == "England", "United Kingdom", Country),
         Country = ifelse(Country == "USA", "United States", Country),
         Country = ifelse(Country == "Holland", "Netherlands", Country))

nhl.post1960 <- nhl.post1960 %>% 
  mutate(Country = ifelse(Country == "France", "France (1963-)", Country),
         Country = ifelse(Country == "England", "United Kingdom", Country),
         Country = ifelse(Country == "USA", "United States", Country),
         Country = ifelse(Country == "Serbia-Montenegro|Yugoslavia", "Serbia", Country),
         Country = ifelse(Country == "South-Africa", "South Africa", Country),
         Country = ifelse(Country == "Korea", "South Korea", Country))


# Reads in the world map data set
map.file <- "data/HiResWorldMapWithISO3.csv"
worldmap <- read.csv(map.file)


# Function to merge the map data set with the hockey data
merge_map = function(dataframe) {
  codes <- full_join(dataframe, ccode, by = "Country")
  world_join <- full_join(codes, worldmap, by = "id")
  return(world_join)
}

world_pre1960 <- merge_map(dataframe = nhl.pre1960)
world_post1960 <- merge_map(dataframe = nhl.post1960)


# Function to create the map plot (country is filled if an NHL player was born there)
map_plot = function(dataframe, maptitle) {
  ggplot() +
    geom_polygon(data = dataframe,
                 mapping = aes(x = long,
                               y = lat,
                               group = group,
                               fill = Players),
                 color = "white") +
    scale_fill_gradient(high = "#002f7c", na.value = "#42cef4") +
    coord_equal() + 
    guides(fill = FALSE) +
    theme_gray() +
    labs(x = "", y = "",
         title = maptitle)
}

map_pre1960 <- map_plot(dataframe = world_pre1960, maptitle = "Country of Origin for NHL Players born before 1960")
map_post1960 <- map_plot(dataframe = world_post1960, maptitle = "Country of Origin for NHL Players born after 1960")

g <- arrangeGrob(map_pre1960, map_post1960)
grid.arrange(map_pre1960, map_post1960)
ggsave(filename = "figure/Project3_figure.pdf", plot = g, width = 10, height = 7.5)
ggsave(filename = "figure/Project3_figure.png", plot = g, width = 10, height = 7.5)

