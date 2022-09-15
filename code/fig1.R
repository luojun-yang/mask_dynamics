library(ggplot2)
library(dplyr)
library(lubridate)
library(cowplot)

date <- "nov11_21" #date of data access

mask_death_fear <- read.csv(paste("../data/mask_death_fear_",date,".csv",sep=""))
mask_death_fear$date <- as_date(mask_death_fear$date)

# code country regions
north_euro <- c("Denmark", 
                "Estonia", 
                "Finland", 
                "Iceland", 
                "Ireland", 
                "Latvia", 
                "Lithuania", 
                "Sweden", 
                "United Kingdom", 
                "Norway")
west_south_euro <- c("Greece",
                     "Italy",
                     "Portugal",
                     "Spain",
                     "Austria",
                     "Belgium",
                     "France",
                     "Germany",
                     "Luxembourg",
                     "Netherlands",
                     "switzerland")
mask_death_fear$region <- mask_death_fear$continent
mask_death_fear$region[mask_death_fear$country %in% north_euro] <- "Northern Europe"
mask_death_fear$region[mask_death_fear$country %in% west_south_euro] <- "Western & Southern Europe"

#### Figure 1A
highlight_list <- c("Singapore","United Kingdom","Sweden")
mask_death_fear %>% 
  dplyr::filter(country %in% highlight_list) -> mask_long_highlight
mask_long_highlight$country <- factor(mask_long_highlight$country,
                                      levels=c("Singapore", "United Kingdom", "Sweden"),
                                      labels=c("Singapore", "United Kingdom", "Sweden"))

p1a <- mask_death_fear %>% 
  filter(country != 'Australia') %>%
  ggplot()+
  geom_line(aes(x=date,y=mask_percent,group=country,color=region),size=0.2, alpha=0.3)+
  geom_line(aes(x=date,y=mask_percent,group=country,color=region), data=mask_long_highlight, size=1.5
            , alpha=0.6)+
  geom_point(aes(x=date,y=mask_percent,group=country,color=region,
                 size=(new_deaths_smoothed_per_million),
                 alpha=(new_deaths_smoothed_per_million)),data=mask_long_highlight)+
  scale_size(range=c(1,8),trans='log10')+
  scale_alpha(range=c(0.2,0.8),trans='log10')+
  scale_color_brewer(palette = "Dark2")+
  scale_x_date(date_labels = "%b %Y", breaks = "3 month")+
  labs(x=NULL, y="% Wearing masks", 
       size=paste("New deaths attributed to\nCOVID-19 per million"), 
       alpha=paste("New deaths attributed to\nCOVID-19 per million"),
       color="Countries by region" )+
  theme_cowplot(font_size=24)+
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))+
  theme(legend.position='top', 
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 10),
        legend.spacing.y = unit(0, "pt"))+
  guides(size=guide_legend(direction='vertical', order =1),
         alpha=guide_legend(direction='vertical', order =1),
         color=guide_legend(direction='vertical', order =2))

p1a
ggsave("../figs/fig1_a.pdf",dpi=300,bg = "transparent")


### Figure 1B
p1b <- mask_long_highlight %>% 
  ggplot(
    aes(x=new_deaths_smoothed_per_million,y=mask_percent,color=region,alpha=date))+
  geom_path(size=1)+
  geom_point(size=0.7,alpha=0.8)+
  facet_wrap(~country,nrow=3)+
  labs(x="New deaths per million\nattributed to COVID-19", 
       y="% Wearing masks")+
  scale_color_manual(values=c('#1B9E77','#7570B3'))+
  scale_alpha_date(range=c(0.4,1),'Date',breaks = as.Date(c("2020-04-1",'2020-10-1','2021-4-1','2021-8-1')))+
  guides(color=FALSE,alpha=FALSE)+
  scale_x_log10()+
  theme_minimal(base_size=24)+
  theme(axis.title.x = element_text(margin = margin(t = 0, unit = "pt")))
p1b
ggsave("../figs/fig1_b.pdf",dpi=300,bg = "transparent")


#### Supplementary figures

ps1 <- mask_death_fear %>%
  dplyr::arrange(continent) %>%
  dplyr::mutate(country=factor(country,levels=unique(country))) %>%
  ggplot(aes(x=date,y=mask_percent,group=country,color=continent))+
  geom_point(size=0.2)+
  geom_line()+
  scale_x_date(date_breaks='2 month', date_labels="%y %b")+
  labs(x="Date", y="% Wearing masks",color="Continent")+
  facet_wrap(~country)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ps1

ps2 <- mask_death_fear %>% 
  dplyr::arrange(continent) %>%
  dplyr::mutate(country=factor(country,levels=unique(country))) %>%
  ggplot(aes(x=new_deaths_smoothed_per_million,y=mask_percent,color=continent,alpha=as.numeric(date)))+
  geom_point(size=0.5)+
  geom_path(size=1)+
  facet_wrap(~country, scales="fixed")+
  labs(x="New deaths per million (smoothed)", y="% Wearing masks", color="Continent")+
  guides(alpha=FALSE)+
  theme_classic()
ps2

ps3 <- mask_death_fear %>% 
  dplyr::arrange(continent) %>%
  dplyr::mutate(country=factor(country,levels=unique(country))) %>%
  ggplot(aes(x=fear_percent,y=mask_percent,color=continent,alpha=as.numeric(date)))+
  geom_path(size=1)+
  geom_point(size=0.5)+
  facet_wrap(~factor(country))+
  annotate(geom='line',x=c(10,90),y=c(10,90), 
           linetype='dashed', color='gray')+
  labs(x="% Afraid of catching coronavirus", 
       y="% Wearing masks",
       color="Continent")+
  guides(alpha=FALSE)+
  theme_classic()
ps3


