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
         ccode = "Country Code") %>% 
  gather(year, life_exp, 3:66) %>% 
  filter(year>=1990, year<=2020) %>% 
  filter(country %in% countries)%>% 
  mutate(life_exp = as.numeric(life_exp))


## Plotting the data 
ggplot(data = df, aes(x = as.numeric(year), y = odi, color = country))+
  geom_line(aes(alpha = uk))+
  scale_color_manual(values = rep(1, 19))+
  scale_alpha_manual(values = c(0.1, 1))+
  theme(legend.position="none")
