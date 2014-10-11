# this is the plain code for HW3

# load the data

gdURL <-"http://tiny.cc/gapminder"
gDat <- read.delim(file = gdURL)

# load the library
library(dplyr)
library(ggplot2)
library(ggthemes)
library(knitr)
library(reshape2)

# change data.frame to tbl_df 
gtbl <- tbl_df(gDat)
glimpse(gtbl)

# Get the maximum and minimum of GDP per capita for all continents.
gdp_int <- gtbl %>%
  group_by(continent) %>%
  summarize(min_gdpPercap = min(gdpPercap), max_gdpPercap = max(gdpPercap))
gdp_int.r = melt(gdp_int)

kable(gdp_int)
ggplot(gdp_int.r, aes(continent,value,fill=variable))+
  geom_bar(position="dodge",stat="identity")+
  theme_bw()

# Look at the spread of GDP per capita within the continents.
gdp_spread <- gtbl %>%
  group_by(continent) %>%
  summarize(spread_gdpPercap = max(gdpPercap)-min(gdpPercap))

kable(gdp_spread)
ggplot(gdp_spread, aes(continent,spread_gdpPercap,fill=spread_gdpPercap))+
  geom_bar(position="dodge",stat="identity")+
  theme_bw()

# Compute 90% trimmed mean of life expectancy for different years.
lifeExp_tmean <- gtbl %>%
  group_by(year) %>%
  summarize(tmean_lifeExp= mean(lifeExp,trim = 0.05))
kable(lifeExp_tmean)
ggplot(lifeExp_tmean, aes(year,tmean_lifeExp))+
  ggtitle("Trimmed average over year")+
  geom_point(color="red")+
  geom_line(color="blue")+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# a weighted mean, weighting by population. 
lifeExp_wmean <- gtbl %>%
  group_by(year) %>%
  summarize(wmean_lifeExp= weighted.mean(lifeExp,pop))

kable(lifeExp_wmean)
ggplot(lifeExp_wmean, aes(year,wmean_lifeExp))+
  ggtitle("Weighted average over year")+
  geom_point(color="red")+
  geom_line(color="blue")+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# How is life expectancy changing over time on different continents?
lifeExp_mean <- gtbl %>%
  group_by(continent, year) %>%
  summarize(wmean_lifeExp= weighted.mean(lifeExp,pop))
kable(lifeExp_mean)
ggplot(lifeExp_mean, aes(year, wmean_lifeExp, colour = continent)) +
  facet_wrap( ~ continent) +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Life Expectancy")+
  ggtitle("Weighted average over year")+
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# Report the absolute and/or relative abundance of countries
# with low life expectancy over time by continent: Compute some 
# measure of worldwide life expectancy – you decide – a mean or
# median or some other quantile or perhaps your current age. 
# The determine how many countries on each continent have a life 
# expectancy less than this benchmark, for each year.
benchmark<-median(gtbl$lifeExp) # use median as benchmark
lifeExp_abu<-gtbl %>%
  group_by(continent, year) %>%
  filter(lifeExp < benchmark) %>%
  summarize(n_countries = n_distinct(country))

kable(lifeExp_abu)

ggplot(lifeExp_abu, aes(year,n_countries,colour = continent))+
  facet_wrap( ~ continent, ncol = 2) +
  geom_point()+
  geom_line()+
  ggtitle("NO. of countries with lifeExp under median")+
  theme_bw()

# Find countries with interesting stories.
gtbl %>%
  filter(continent == "Asia") %>%
  select(year, country, lifeExp) %>%
  arrange(year) %>%
  group_by(year) %>%
  filter(min_rank(desc(lifeExp)) < 2 | min_rank(lifeExp) < 2)

# We see that (min = Afghanistan, max = Japan) is the most frequent result
# Compare Afghanistan, Japan and world average
data_avg<- gtbl %>%
  group_by(year) %>%
  summarize(lifeExp= mean(lifeExp))
data_life<-tbl_df(data.frame(year = data_avg$year, 
                             Jap = filter(gtbl, country == "Japan")$lifeExp,
                             Afg = filter(gtbl, country == "Afghanistan")$lifeExp,
                             Avg = data_avg$lifeExp))

kable(data_life)
ggplot(data=data_life)+
  geom_line(aes(x = year, y=Jap,colour = "Jap"))+
  geom_line(aes(x = year, y=Afg,colour = "Afg"))+
  geom_line(aes(x = year, y=Avg,colour = "Avg"))+
  xlab("Year") + ylab("Life Expectancy")+ggtitle("Japan vs Afghanistan")+
  scale_colour_manual("Life Exp",values = c("Jap" = "red","Afg" = "blue","Avg" = "yellow"))+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# find the country experiencing the sharpest 5-year drop in life expectancy
gtbl %>%
  group_by(continent, country) %>%
  select(country, year, continent, lifeExp) %>%
  mutate(le_delta = lifeExp - lag(lifeExp)) %>%
  summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>%
  filter(min_rank(worst_le_delta) < 2) %>%
  arrange(worst_le_delta)

#
data_drop <- tbl_df(data.frame(year = data_avg$year, 
                               Rwa = filter(gtbl, country == "Rwanda")$lifeExp,
                               Cam = filter(gtbl, country == "Cambodia")$lifeExp,
                               ES  = filter(gtbl, country == "El Salvador")$lifeExp,
                               Mon = filter(gtbl, country == "Montenegro")$lifeExp,
                               Aus = filter(gtbl, country == "Australia")$lifeExp,
                               Avg = data_avg$lifeExp))
kable(data_drop)
ggplot(data=data_drop)+
  geom_line(aes(x = year, y=Rwa,colour = "Rwa"))+
  geom_line(aes(x = year, y=Cam,colour = "Cam"))+
  geom_line(aes(x = year, y=ES,colour = "Es"))+
  geom_line(aes(x = year, y=Mon,colour = "Mon"))+
  geom_line(aes(x = year, y=Aus,colour = "Aus"))+
  geom_line(aes(x = year, y=Avg,colour = "Avg"))+
  xlab("Year") + ylab("Life Expectancy")+ggtitle("Great Drop")+
  scale_colour_manual("Life Exp",values = c("Rwa" = "red","Cam" = "blue",
                                            "Avg" = "yellow","Es" = "green",
                                            "Mon" = "black", "Aus" = "pink"))+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# analysis for Rwanda
data_Rwa = gtbl %>% filter(country == "Rwanda")
kable(data_Rwa)
ggplot(data_Rwa, aes(year, pop)) +
  ggtitle("Rwanda Population")+
  geom_point(color="red")+
  geom_line(color="blue")+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))

ggplot(data_Rwa, aes(year, gdpPercap)) +
  ggtitle("Rwanda gdpPercap")+
  geom_point(color="red")+
  geom_line(color="blue")+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))

