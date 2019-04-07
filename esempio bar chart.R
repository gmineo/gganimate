library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggrepel)

# Package to pull in cansim data, courtesy of Jens von Bergman and Dmitry Shkolnik
library(cansim) 

# Now update to the newest gganimate version
#install.packages("devtools")
#devtools::install_github('thomasp85/gganimate')
library(gganimate)

provlist <- c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec","New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador")

short_provs <- function(df){
  df <- df %>%
    filter(GEO %in% c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec","New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador"))
  df <- df %>%
    mutate(GEO.short = case_when(
      GEO=="British Columbia" ~ "BC",
      GEO=="Alberta" ~ "AB",
      GEO=="Saskatchewan" ~ "SK",
      GEO=="Manitoba" ~ "MB",
      GEO=="Ontario" ~ "ON",
      GEO=="Quebec" ~ "QC",
      GEO=="New Brunswick" ~ "NB",
      GEO=="Prince Edward Island" ~ "PE",
      GEO=="Nova Scotia" ~ "NS",
      GEO=="Newfoundland and Labrador" ~ "NL"
    ))
  df$GEO.short <- factor(df$GEO.short, levels=c("BC","AB","SK","MB","ON","QC","NB","PE","NS","NL"))
}


pop <- get_cansim(17100005) %>% normalize_cansim_values()
df.pop <- pop %>%
  mutate(year=as.integer(REF_DATE)) %>%
  filter(GEO %in% provlist,
         Sex=="Both sexes",
         `Age group`=="All ages") %>%
  dplyr::select(GEO,year,pop=VALUE)

df.GDPpercap <-left_join(df.GDP,df.pop, by=c("year","GEO")) %>%
  mutate(GDPpercap=GDP/pop)

df.GDPpercap$GEO.short<-short_provs(df.GDPpercap)

GDP <- get_cansim(36100222) %>% normalize_cansim_values()
df.GDP <- GDP %>%
  mutate(year=as.integer(REF_DATE)) %>%
  filter(GEO %in% provlist,
         Prices=="Chained (2012) dollars",
         Estimates=="Gross domestic product at market prices") %>%
  dplyr::select(GEO,year,GDP=VALUE)
df.GDP$GEO.short<-short_provs(df.GDP)


plotdata <- df.GDPpercap %>%
  group_by(year) %>%
  mutate(ordering = rank(GDPpercap)) %>%
  ungroup() 

p<-ggplot(plotdata,
          aes(ordering, group = GEO.short,color=GEO.short,fill=GEO.short)) +
  geom_tile(aes(y = GDPpercap/2, 
                height = GDPpercap,
                width = 0.9), alpha = 0.9) +
  # text on top of bars
  geom_text(aes(y = GDPpercap, label = GEO.short), hjust = -0.4) +
  # text in x-axis (requires clip = "off" in coord_cartesian)
  geom_text(aes(y = 0, label = GEO.short), hjust = 1.4) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_color_viridis_d(name="")+
  scale_fill_viridis_d(name="")+
  scale_y_continuous(labels=scales::dollar)+
  theme_tufte(14,"Avenir")+
  guides(color=F,fill=F)+
  labs(title='{frame_time}', x = "",y="GDP per capita ($2012)") +
  theme(plot.title = element_text(hjust = 1, size = 22),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) + 
  transition_time(year)+
  ease_aes('cubic-in-out')

animate(p, nframes = 100, fps = 5, end_pause = 20) #again, use anim_save(filename) to save





  