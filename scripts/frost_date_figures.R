### McFarland Hill weather station frost date figures
### Schoodic Institute at Acadia National Park, 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)



#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

## Read and clean 1982 - 2014 data
t1 <- read.csv("data/ME170100_2551daily.csv") %>% 
  select(Day, Month, Year, julian = JD, tavg = TmeanF, tmax = TmaxF, tmin = TminF) %>% 
  rename_with(., ~tolower(.))

## Read and clean 2014 - 2023 data
t2 <- read.csv("data/USR0000MMCF_daily_2014_2023.csv") %>% 
  mutate(day = day(DATE),
         month = month(DATE),
         year = year(DATE),
         datej = as.Date(DATE, format = "%Y-%m-%d"),
         julian = as.integer(format(datej, "%j"))) %>% 
  rename_with(., ~tolower(.)) %>% 
  select(day, month, year, julian, tavg, tmax, tmin)

fd <- bind_rows(t1, t2) %>% 
  filter(!is.na(tmin)) %>% 
  filter(year > 1982 & year < 2023)



#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

## Last spring frost
spring <- fd %>%
  group_by(year) %>% 
  filter(julian < 180 & tmin <= 32) %>% 
  slice(which.max(julian)) %>% 
  select(year, month, day, last.frost = julian, tmin) %>% 
  mutate(last.frost = as.Date(paste(2000, month, day, sep = "-")))


## First fall frost
fall <- fd %>%
  group_by(year) %>% 
  filter(julian > 180 & tmin <= 32) %>% 
  slice(which.min(julian)) %>% 
  select(year, month, day, first.frost = julian, tmin) %>% 
  mutate(first.frost = as.Date(paste(2000, month, day, sep = "-")))



#------------------------------------------------#
####             Create Figures               ####
#------------------------------------------------#

### Spring figure
## Base plotting
plot(spring$year, spring$last.frost,
     ylab="Julian Date", xlab="Year",
     main="McFarland Hill Last Spring Frost")
abline(120, 0, col="blue")
lines(spring$year, spring$last.frost)
text(2013.5, 118, "April", col="blue")
abline(151, 0, col="dark grey")
text(2013.8, 149, "May", col="dark grey")
text(2013.8, 122, "May", col="dark grey")
text(2013.5, 154, "June", col="orange")
text(1988, 109, "McFarland Hill Weather Station Data", cex=0.5)
text(1988, 107.5, "Created by N. Fisichelli", cex=0.5)

abline(lm(last.frost ~ year, data=spring))

## ggplot recreation
spring %>% 
  ggplot(aes(x = year, y = last.frost)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#578e5b") +
  labs(title = "Acadia National Park last spring frost", 
       subtitle = "Data from the McFarland Hill weather station 1983 - 2022",
       caption = "Figure created by Schoodic Institute",
       x = "Year",
       y = "Month and day of year") +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 13),
        axis.text = element_text(color = "black", size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(0,.5,0,0, unit = "cm")),
        axis.title.x = element_text(margin = margin(.5,0,0,0, unit = "cm")),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        plot.subtitle = element_text(margin = margin(0,0,0.5,0, unit = "cm")))

ggsave("outputs/last_frosts_1983_2022.png", height = 6, width = 8.5, dpi = 350)

lm(last.frost ~ year, data=spring)


### Fall figure
## Base plotting
plot(fall$year, fall$first.frost,
     ylab="Julian Date", xlab="Year",
     main="McFarland Hill First Fall Frost")
abline(274, 0, col="blue")
lines(fall$year, fall$first.frost)
text(2013.5, 118, "April", col="blue")
abline(305, 0, col="dark grey")
text(2013.8, 149, "May", col="dark grey")
text(2013.8, 122, "May", col="dark grey")
text(2013.5, 154, "June", col="orange")
text(1988, 109, "McFarland Hill Weather Station Data", cex=0.5)
text(1988, 107.5, "Created by N. Fisichelli", cex=0.5)

abline(lm(first.frost ~ year, data=fall))

## ggplot recreation
fall %>% 
  ggplot(aes(x = year, y = first.frost)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#C57E21") +
  labs(title = "Acadia National Park first fall frost", 
       subtitle = "Data from the McFarland Hill weather station 1983 - 2022",
       caption = "Figure created by Schoodic Institute",
       x = "Year",
       y = "Month and day of year") +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 13),
        axis.text = element_text(color = "black", size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(0,.5,0,0, unit = "cm")),
        axis.title.x = element_text(margin = margin(.5,0,0,0, unit = "cm")),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        plot.subtitle = element_text(margin = margin(0,0,0.5,0, unit = "cm")))


ggsave("outputs/first_frosts_1983_2022.png", height = 6, width = 8.5, dpi = 350)


lm(first.frost ~ year, data=fall)

