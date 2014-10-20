library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(ggthemes)
library(knitr)
gDat <- read.delim("gapminderDataFiveYear.txt")
str(gDat)

nlevels(gDat$continent)

# Drop Oceania
hDat<- gDat %>%
  filter(continent != "Oceania")

iDat  <- hDat %>% droplevels
nlevels(iDat$continent)

j_coefs <- ddply(gDat, ~ country + continent, function(dat, offset = 1952) {
  the_fit <- lm(lifeExp ~ I(year - offset), dat)
  setNames(coef(the_fit), c("intercept", "slope"))
})

str(j_coefs)
names(j_coefs)
head(j_coefs)


post_arrange <- j_coefs %>% arrange(slope)
post_reorder <- j_coefs %>%
  mutate(country = reorder(country, slope))
post_both <- j_coefs %>%
  mutate(country = reorder(country, slope)) %>%
  arrange(country)

identical(post_arrange, post_reorder)
identical(post_both, post_reorder)
identical(post_arrange, post_both)

ggplot(post_arrange, aes(x = slope, y = country)) + 
  geom_point(size = 3) + theme_gdocs()
ggplot(post_reorder, aes(x = slope, y = country)) + 
  geom_point(size = 3) + theme_gdocs()
ggplot(post_both, aes(x = slope, y = country)) + 
  geom_point(size = 3) + theme_gdocs()


h_countries <- c("Italy", "Germany", "Japan", "United Kingdom","United States", "China")
hDat <- gDat %>%
  filter(country %in% h_countries) %>% 
  droplevels
hDat <- hDat %>%
  mutate(leader =revalue(hDat$country, c("Italy" = "Mussolini", 
            "Germany" = "Hitler", 
            "Japan" = "Hideki Tojo", 
            "United Kingdom" = "Churchill",
            "United States" = "Roosevelt", 
            "China" = "Chiang Kai-shek")))

kable(hDat)
table(hDat$leader)

jDat <- gDat %>%
  mutate(continent = reorder(continent, lifeExp/gdpPercap, max))

p <- ggplot(jDat, aes(x = continent, y = (lifeExp/gdpPercap), group = 1,color = continent))
p + geom_jitter(size = 3, position = position_jitter(width = .1)) +
  stat_summary(fun.y = max, geom = "path")+
  labs(y = "Ratio")+
  theme_gdocs()

p <- ggplot(gDat, aes(x = continent, y = (lifeExp/gdpPercap), group = 1,color = continent))
p + geom_jitter(size = 3, position = position_jitter(width = .1)) +
  labs(y = "Ratio")+
  stat_summary(fun.y = max, geom = "path") +
  theme_gdocs()


write.table(hDat, "hDat.cvs", sep = ",", row.names = FALSE,
            quote = FALSE)
readhDat<-read.table("hDat.cvs",sep = ",", header = TRUE)

readhDat <- readhDat %>%
  mutate(leader =revalue(hDat$country, c("Italy" = "Mussolini", 
                                         "Germany" = "Hitler", 
                                         "Japan" = "Hideki Tojo", 
                                         "United Kingdom" = "Churchill",
                                         "United States" = "Roosevelt", 
                                         "China" = "Chiang Kai-shek")))
identical(hDat, readhDat)

saveRDS(hDat, "hDat.rds")
hDatRDS <- readRDS("hDat.rds")
identical(hDat, hDatRDS)

dput(hDat, "hDat-dput.txt")
hDatPut <- dget("hDat-dput.txt")
identical(hDat, hDatPut)


readhDat.wf<-read.table("hDat.cvs",sep = ",", header = TRUE,stringsAsFactors = FALSE)
str(readhDat.wf)

readhDat.wf$country = factor(readhDat.wf$country)
readhDat.wf$continent = factor(readhDat.wf$continent)
readhDat.wf$leader = factor(readhDat.wf$leader)

hDat.order <- hDat %>%
  mutate(country = reorder(country, lifeExp, max))

levels(readhDat.wf$country)
readhDat.wf$country = factor(readhDat.wf$country, level = levels(hDat.order$country))
levels(readhDat.wf$country)