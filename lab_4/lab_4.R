################################################################################
############################# Part 1 ###########################################
################################################################################

## Loading libraries
library(tidyverse)
library(readr)


## Loading and cleaning the dataa
df <- read_csv('labs/lab_4/births.csv') %>% 
  rename(age      = Age, 
         n_women  = "Number of women", 
         n_births = "Number of births") %>% 
  filter(!is.na(n_women)) %>% 
  mutate(afr = n_births/n_women)


## Plotting the age specific fertility rate 
ggplot(data = df, aes(x = age, y = afr))+
  geom_line()+
  theme(plot.title        = element_text(size=12),
        plot.subtitle     = element_text(size=9),
        legend.title      = element_text(size=9),
        plot.caption      = element_text(size=6, hjust = 0),
        axis.title        = element_text(size = 11),
        strip.background  = element_blank(),
        strip.placement   = "outside",
        panel.grid.minor  = element_line('grey', size = 0.1, linetype = 'dashed'),
        panel.grid.major  = element_line('grey', size = 0.1, linetype = 'dashed'),
        legend.key        = element_rect(fill = NA),
        text              = element_text(family="Palatino"),
        panel.background  = element_rect(fill = "transparent", colour = NA),
        plot.background   = element_rect(fill = "transparent", colour = NA))+
  xlab("Age") +
  ylab("Age specific fertility rate") +
  labs(title    = "Age specific fertility rates",
       size     = 'Population:',
       subtitle = "The age specific fertility rate gives births per woman for each age 25 and 35",
       caption  = "Source: Introuction to Economic Data, Hans Sievertsen, 2022.")


## Calculating the total fertility rate 
sum(df %>% select(afr)) %>% round(d = 2)
# The total fertility rate is 0.57.










################################################################################
############################# Part 2 ###########################################
################################################################################

## Loading libraries 
library(readxl)


## Loading and cleaning the data 
mpd2020 <- read_excel('~/Dropbox/economic_data/labs/lab_1/mpd2020.xlsx', 
                      sheet = 4, skip = 1) %>% 
  dplyr::select(year, GBR) %>% 
  filter(year  >= 1979) %>% 
  rename(gdppc = GBR) %>% 
  mutate(gdppc_1 = lag(gdppc, n = 1)) %>% 
  mutate(gdppc_g = (gdppc/gdppc_1 - 1)*100) %>% 
  filter(year>=1980) %>%
  mutate(positive = ifelse(gdppc_g>0, 'positive', 'negative')) %>% 
  write_csv(., 'growth_rates.csv')


## Exporting the dataset
# write_csv(mpd2020, 'growth_rates.csv')


## Plotting the growth rates
ggplot(data = mpd2020, aes(x = year, y = gdppc_g, fill = positive))+
  geom_bar(stat='identity') +
  theme(plot.title       = element_text(size=12),
        plot.subtitle    = element_text(size=9),
        legend.title     = element_text(size=9),
        plot.caption     = element_text(size=6, hjust = 0),
        axis.title       = element_text(size = 11),
        strip.background = element_blank(),
        strip.placement  = "outside",
        panel.grid.minor = element_line('grey', size = 0.1, linetype = 'dashed'),
        panel.grid.major = element_line('grey', size = 0.1, linetype = 'dashed'),
        legend.key       = element_rect(fill = NA),
        legend.position  = "none",
        text             = element_text(family="Palatino"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA))+
  xlab("Year") +
  ylab("Growth rate") +
  labs(title    = "The growth rate of GDP per capita for the UK, 1980-2018",
       color    = ' ',
       subtitle = "Real GDP per capita in 2011 dollars.",
       caption  = "Source: Maddison Project Database (MPD) 2020")





