ggplot(data = df, aes(x = log(gdppc), y = life_exp, color = continent))+
  geom_point(aes(size = pop), alpha = 0.6, stroke = 0.2)+
  scale_size_continuous(range = c(0.5, 8), 
                        breaks = c(6e+8, 1.3e+9), 
                        labels = c('600M', '1.3B'))+
  theme(plot.title = element_text(size=12),
        plot.subtitle = element_text(size=9),
        legend.title = element_text(size=9),
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
  xlab("GDP per capita (log)") +
  ylab("Life expectancy at birth") +
  labs(title = "Life expectancy vs. GDP per capita, 2015",
       size = 'Population:',
       color = ' ',
       subtitle = "GDP per capita is adjusted for inflation and differences in the cost of living between countries.",
       caption = "Source: UN WPP (2022); Zijdeman et al. (2015); Riley (2005); Maddison Project Database 2020 (Bolt and van Zanden, 2020).\nNote: GDP per capita is expressed in international-dollars at 2011 prices.")+
  scale_x_continuous(n.breaks=8) +
  scale_y_continuous(n.breaks=8) +
  geom_text_repel(aes(label=ifelse(country %in% c('China', 
                                                  'Democratic Republic of Congo', 
                                                  'South Africa', 'Indonesia', 
                                                  'Equatorial Guinea', 
                                                  'Trinidad and Tobago', 
                                                  'Italy', 'Poland', 
                                                  'Brazil', 'Italy', 
                                                  'Poland', 'Angola', 
                                                  'Nigeria', 'United States', 
                                                  'North Korea', 'Ethiopia', 
                                                  'Chad', 'India', 'Pakistan'), country, '')),
                  size = 3, 
                  show.legend  = FALSE) +
  guides(size = guide_legend(override.aes=list(shape = 1)),
         color = guide_legend(override.aes=list(shape = 15)))