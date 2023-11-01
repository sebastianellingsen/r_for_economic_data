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
