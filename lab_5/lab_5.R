
################################################################################
################################# Part 1 #######################################
################################################################################

## Loading libraries
library(tidyverse)
library(readr)


## Loading and cleaning the data on life satisfaction  
life_sat <- read_csv("labs/lab_5/ilc_pw01__custom_7985898_linear.csv") %>% 
  select(geo, TIME_PERIOD, OBS_VALUE) %>% 
  rename(country  = geo,
         year     = TIME_PERIOD, 
         life_sat = OBS_VALUE)


## Loading and cleaning the data on GDP per capita   
gdppc <- read_csv("labs/lab_5/sdg_08_10__custom_7985512_linear.csv") %>% 
  select(geo, TIME_PERIOD, OBS_VALUE) %>% 
  rename(country = geo,
         year    = TIME_PERIOD, 
         gdppc   = OBS_VALUE)


## Combining the datasets 
df <- life_sat %>% 
  full_join(gdppc, by = c('country', 'year')) %>% 
  filter(!is.na(life_sat), 
         !is.na(gdppc), 
         country!='EU27_2020') 


## Plotting the data 
ggplot(data=df, aes(x = gdppc, y = life_sat, shape = as.factor(year)))+
  geom_point(size = 1.3)+
  scale_shape_manual(values=c(0,3,1))+
  theme(plot.title = element_text(size=12),
        plot.subtitle = element_text(size=9),
        legend.title = element_blank(),
        legend.position="bottom",
        plot.caption = element_text(size=6, hjust = 0),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.grid.minor = element_line('grey', size = 0.1, linetype = 'dashed'),
        panel.grid.major = element_line('grey', size = 0.1, linetype = 'dashed'),
        legend.key = element_rect(fill = NA),
        text=element_text(family="Palatino"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))+
  xlab("Real GDP per capita") +
  ylab("Overall life satisfaction (1=lowest, 10=highest)") +
  labs(title = "Life satisfaction vs. GDP per capita",
       size = 'Population:',
       color = ' ',
       subtitle = "GDP per capita is measured in 2010 prices",
       caption = "Source: Eurostat (2023). Note: Chain linked volumes (2010), euro per capita.")







################################################################################
################################# Part 2 #######################################
################################################################################

countries <- c("United Kingdom", 
               "United States",
               "Austria", 
               "Australia",
               "Belgium",
               "Switzerland",
               "Denmark",
               "Spain",
               "France",
               "Greece",
               "Italy",
               "Japan",
               "Netherlands",
               "Norway",
               "New Zealand",
               "Poland",
               "Portugal",
               "Slovak Republic",
               "Sweden")


## Life expectancy
life_exp <- read_csv("labs/lab_5/wb/API_SP.DYN.LE00.IN_DS2_en_csv_v2_5871609.csv", skip = 4) %>% 
  select(-"Indicator Name", -"Indicator Code") %>% 
  rename(country = "Country Name",
         ccode   = "Country Code") %>% 
  gather(year, life_exp, 3:66) %>% 
  filter(year>=1990, year<=2020) %>% 
  filter(country %in% countries)%>% 
  mutate(life_exp = as.numeric(life_exp))


## GDP per capita ppp in constant prices
gdppc <- read_csv("labs/lab_5/wb/API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_5873868.csv", skip = 4) %>% 
  select(-"Indicator Name", -"Indicator Code") %>% 
  rename(country = "Country Name",
         ccode   = "Country Code") %>% 
  gather(year, gdppc, 3:66) %>% 
  filter(year>=1990, year<=2020) %>% 
  filter(country %in% countries) %>% 
  mutate(gdppc = as.numeric(gdppc))


## Combining the datasets
df <- life_exp %>% 
  full_join(gdppc, by = c('country', 'ccode', 'year')) %>% 
  mutate(lei = (life_exp - 20)/(85-20),
         ii  = (log(gdppc) - log(100))/(log(75000) - log(100)),
         odi = (lei * ii)^0.5) %>% 
  mutate(uk = ifelse(country == 'United Kingdom', 1, 0) %>% as.factor())



## Plotting the data 
ggplot(data = df, aes(x = as.numeric(year), y = odi, color = country))+
  geom_line(aes(alpha = uk))+
  scale_color_manual(values = rep(1, 19))+
  scale_alpha_manual(values = c(0.1, 1))+
  theme(plot.title = element_text(size=12),
        plot.subtitle = element_text(size=9),
        legend.title = element_blank(),
        legend.position="none",
        plot.caption = element_text(size=6, hjust = 0),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.grid.minor = element_line('grey', size = 0.1, linetype = 'dashed'),
        panel.grid.major = element_line('grey', size = 0.1, linetype = 'dashed'),
        legend.key = element_rect(fill = NA),
        text=element_text(family="Palatino"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))+
  xlab("Year") +
  ylab("Our development index (ODI)") +
  labs(title = "Our development index by country, 1990-2020",
       subtitle = "index created from life expectancy at birth and real GDP per capita (PPP)",
       caption = "Source: World Bank. Note: ODI is a weighted average of GDP per capita PPP in 2017 constant prices and life expectancy at birth.")




