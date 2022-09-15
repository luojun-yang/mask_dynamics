library(ggplot2)
library(dplyr)
library(patchwork)
library(RColorBrewer)
library(cowplot)


source('simulations.R')

### Figure 2A
cases <- read.csv("../results/cases.csv")

p1 <- cases %>% 
  ggplot()+
  geom_rect(aes(ymin=0, ymax=1, xmin=100,
                xmax=200, fill='Policy duration'))+
  geom_line(aes(x=time,y=mask,color=case_m,group=case_m),size=1.5,alpha=1)+
  scale_color_manual(values=c('#2c7bb6','#abd9e9','#d7191c','#fdae61'),
                     labels=c('LH: Loose, high stringency', 'LL: Loose, low stringency',
                              'TH: Tight, high stringency', 'TL: Tight, low stringency'))+
  geom_line(data=data.frame(time=times,r=r_vec),aes(x=time,y=r,lty='Incidence'),
            color='black',size=1, alpha=0.5)+
  geom_text(aes(x=label_x,y=label_y,label=case_m,color=case_m), 
            position=position_nudge(0.5), hjust=0, show.legend=FALSE, size=6)+
  scale_linetype_manual(values=c('Incidence'='dashed'))+
  scale_fill_manual(values=c('Policy duration'='gray95'))+
  scale_x_continuous(limits=c(0,300))+
  plot_annotation(tag_levels='A')+
  labs(x='Days since first case of infection', y='% Wearing masks', color=NULL,lty=NULL, fill=NULL)+
  theme_cowplot(font_size=18)+
  theme(legend.spacing.y = unit(-0.1, "cm"),
        legend.key.size =  unit(0.5, "in"))
p1
ggsave("../figs/fig2_a.png",dpi=300,bg = "transparent", width=10, height=4)

### Figure 2B
results <- read.csv("../results/auc.csv")

variable_names <- c(
  Duration = "Duration" ,
  Timing = "Onset"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

p2 <- results %>%
  filter(w>0) %>%
  ggplot(aes(X,s,fill=as.numeric(auc)))+
  geom_raster()+
  labs(x='Days',y='Pro-mask Policy Stringency',fill='',na.value='gray')+
  facet_grid(X_var~label,labeller=labeller(X_var=variable_names))+
  scale_fill_distiller(type='seq',palette='YlGnBu',limits=c(1,400), trans='log2')+
  theme_cowplot(font_size=18)
p2
ggsave("../figs/fig2_b.png",dpi=300,bg = "transparent")

p1 / p2 +
  plot_layout(heights = c(1, 1.5), widths=c(1,0.2)) +
  plot_annotation(tag_levels = 'A') +
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 20, hjust = 0, vjust = 0))

ggsave("../figs/fig2.png",dpi=300,bg = "transparent",width=12,height=10)

