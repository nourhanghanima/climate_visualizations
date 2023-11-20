install.packages("geojsonio")
install.packages("rgdal")
install.packages("highcharter")
install.packages("mapdata")
install.packages("viridis")
install.packages("giscoR")
install.packages("dplyr")
library(rgdal)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(broom)
library(highcharter)
library(maps)
library(mapdata)
library(countrycode)
library(viridis)
library(sf)
library(giscoR)
library(scales)
library(glue)
library(stringr)
#NASA data
#plot showing global temperatures have increased by over one degree centigrade since 1800
nasatempanomaly <- read_csv("/Users/nourhanghanima/Downloads/GLB.Ts+dSST.csv", skip = 1, na = "***") |>
  select(year = Year, t_diff = "J-D") |> drop_na()
View(nasatempanomaly)

annotation <- nasatempanomaly |> 
  slice (1, n()) |>
  mutate(t_diff = 0, 
         x = year + c(-6, 5))
max_t_diff <- format(round(max(nasatempanomaly$t_diff), 1), nsmall=1)

ggplot(nasatempanomaly, aes(x = year, y = t_diff, fill = t_diff)) +
  geom_col(show.legend=FALSE) +
  geom_text(data=annotation, aes(x = x, label = year)) +
  geom_text(x= 1880, y = 1, hjust = 0,
            label = glue("Global temperatures have increased by over {max_t_diff}\u00B0C since {min(nasatempanomaly$year)}")) +
  scale_fill_stepsn(colors = c("#0022FC", "#FCF8DD", "#E20F00"),
                    values = rescale(c(min(nasatempanomaly$t_diff), 0, max(nasatempanomaly$t_diff))),
                    limits = c(min(nasatempanomaly$t_diff), max(nasatempanomaly$t_diff)),
                    n.breaks = 9) +
  theme_void() +
  theme (
    plot.background = element_rect(fill = "white", color=NA)
  ) 

max(nasatempanomaly$t_diff)

ggsave("temperatures_bar_plot.png", width = 7, height = 4)


#natural disaster data 
disasterdata <- read.csv("/Users/nourhanghanima/Downloads/Climate-related_Disasters_Frequency.csv") 
disasterdata <- disasterdata |> mutate(disaster_type = case_when(
  (str_detect(disasterdata$Indicator, "Drought")) ~ "Drought",
  (str_detect(disasterdata$Indicator, "Extreme temperature")) ~ "Extreme Temperature",
  (str_detect(disasterdata$Indicator, "Flood")) ~ "Flood",
  (str_detect(disasterdata$Indicator, "Landslide")) ~ "Landslide",
  (str_detect(disasterdata$Indicator, "Storm")) ~ "Storm",
  (str_detect(disasterdata$Indicator, "Wildfire")) ~ "Wildfire"
))

disasterdata2 <- disasterdata |> pivot_longer(F1980:F2022, names_to = "years", values_to = "disasternumber") |>
  arrange(Country, years)
disasterdata2 <- na.omit(disasterdata2)

#years for you is not coded as numbers because it has F before the year
disasterdata2 <- disasterdata2 |> mutate(year = sub('.', '', disasterdata2$years))

disasterdata2$year <- as.numeric(disasterdata2$year)

View(disasterdata2)

disasterdata2 <- disasterdata2 |> group_by(Country, year) |> mutate(totalcountrydisasters = sum(disasternumber))

disasterdata2 <- disasterdata2 |> group_by(year) |> mutate (yearlydisasters = sum (disasternumber))

disasterdata2 <- disasterdata2 |> group_by(disaster_type, year) |> mutate(globaldisastertype = sum(disasternumber))

disasterdata3 <- disasterdata2  |> select(year, yearlydisasters, globaldisastertype)
disasterdata3 <- disasterdata3|> distinct(year, yearlydisasters, globaldisastertype, .keep_all = TRUE) |> arrange(year)

write.csv(disasterdata3, "/Users/nourhanghanima/Downloads/disasterdata3.csv")

globaldisasters <- disasterdata2  |> select (year, yearlydisasters) |>  group_by(year)
globaldisasters <- globaldisasters |> distinct(year, yearlydisasters, .keep_all = TRUE) |> arrange(year) |> mutate(Entity = "All disasters")

droughts <- disasterdata2 |> filter(disaster_type == "Drought") |> select (year, globaldisastertype) |> 
  rename(Entity = disaster_type) |> distinct(year, globaldisastertype) |> arrange(year)

View(droughts)

ggplot(disasterdata3, aes(x= year, y = yearlydisasters, fill = disaster_type)) +
  scale_x_continuous(name = "year", breaks = c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012,
                                               2016, 2020)) +
  geom_col()

count(disasterdata2$ISO2)


#shows global disasters but can't fill by disaster type because you're using a different dataset
#the problem with the previous attempts was that R was counting duplicates of the years
ggplot(globaldisasters, aes(x = year, y = yearlydisasters)) +
  geom_col(fill = "#0022FC") +
  scale_x_continuous(name = "year", breaks = c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013,
                                               2016, 2019, 2022)) +
  theme_minimal() +
  theme (
    plot.background = element_rect(fill = "white", color=NA),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) +
  labs(y = "Number of global natural disasters", x = "Year")

count(disasterdata2, year) |> print(n = 200)

ggsave("disasterplot2.png")


