#  --------------------------------------------------------------------- #
#  scatter plot
#  --------------------------------------------------------------------- #
library(showtext)
library(rvest)
library(cowplot)
library(ozmaps)
library(ggmap)
library(tidyverse)
library(sf)
library(ggdist)
library(leaflet)
library(tidyquant)
library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)
library(quantreg)
library(classInt)
library(viridis)
library(sp)
library(ggrepel)
library(forcats)
#remotes::install_github("inkeso/miscfun")
library(miscfun)
library(extrafont)
font_import()
#loadfonts(device = "Comic Sans MS")

read.table('cen_on.txt',
           sep =",", header = TRUE)
# Load data
cen_on <- read.table('cen_on.txt',
                     sep =",", header = TRUE)
all_on <- read.table('all_on.txt',
                     sep =",", header = TRUE)

## color choice
bl = miscfun::GetTolColors( 11, scheme = "sunset")[4]
bl2= miscfun::GetTolColors( 11, scheme = "sunset")[5]
ye = miscfun::GetTolColors( 11, scheme = "sunset")[8]
col_r = miscfun::GetTolColors( 11, scheme = "sunset")[c(5, 4, 8,7)]
mid_c = miscfun::GetTolColors(  9, scheme = "PRGn")[7]

# scatter plot
d_bg = all_on[,-1]

v1= ggplot(data= cen_on, aes(x = (no2)^(1/3), y =(FP)^(1/3), label=loc))+ #, alpha=month))+
  ggtitle('Scatter plot of mean values of \n NO2 and PM 2.5 for Ontario regions')+
  theme(legend.position=("none"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour ="grey95"),
        plot.margin = margin(3, 3, 0.5, 0.5, "cm"),
        axis.text.x = element_text(family="Comic Sans MS",
                                   margin = margin(t = 0.15, unit = "cm")),
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        axis.title.x =  element_text(family="Comic Sans MS", size=5.25,
                                     margin = margin(t = 0.3, unit = "cm")),
        axis.title.y =  element_text(family="Comic Sans MS", size=5.25,
                                     margin = margin(r = 0.3, unit = "cm")),
        
        plot.title = element_text(size=10, hjust=0.65,vjust=2, family="Comic Sans MS")
        #legend.box.background = element_rect(fill = "white", color = NA),
        #panel.background = element_rect(fill = "white",colour = NA),
        #   plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  #geom_point(data = d_bg, aes(x = (no2)^(1/3), y =(FP)^(1/3)), colour = "grey", alpha = .2) +
  stat_density_2d(data = d_bg, geom = "polygon", aes(x =(no2)^(1/3), y =(FP)^(1/3), 
                                                     alpha = (..level..) ^ 2), fill= mid_c, bins = 4)+ #'grey70'
  
  geom_point(aes(x =(no2)^(1/3), y =(FP)^(1/3), color=cate), 
             shape = 1, stroke = 1.1, size = 1.75, alpha=0.9) +
  
  geom_point(data = cen_on[cen_on$loc=='Hamilton',] ,aes(x =(no2)^(1/3), y =(FP)^(1/3)), 
             size = 3.5, alpha=0.6,color=bl2, shape=21)+
  scale_color_manual(values=c(col_r)) +
  geom_text_repel(min.segment.length = 1, box.padding = 0.3, size=2.5, max.overlaps = 15,
                  segment.ncp = 5, color = "black", bg.color = "grey10",bg.r = 0.01,
                  segment.curvature = 0.3, segment.angle = 60, force = 30, family = "Comic Sans MS")+
  labs(x = expr(paste('Density of Air Quality: ', NO2^frac(1,3))), 
       y = expr(paste('Density of Air Quality: ', PM2.5^frac(1,3))), family = "Comic Sans MS")
v1



# Histogram with density plot
m=round(median(cen_on$no),1)
v=as.numeric()
for(i in 1:4){
  d=round(boxplot.stats(cen_on$no)$stats,1)[1:5]
  v=c(v, d[i+1]-d[i])
}

## data and color
ran = data.frame(range = v, medi=c(m),
                 value=c('min', 'low', 'high', 'max'), con = c(rep('no',4)))
col_r = miscfun::GetTolColors( 11, scheme = "sunset")[c(5, 4, 8,7)]

i1= ran%>% mutate(value = factor(value, levels= c('max', 'high', 'low', 'min'))) %>%
  ggplot(aes(fill=value, y=range, x=con)) + 
  ggtitle("Nitrogen Monoxide emission") +
  geom_bar(position="stack", stat="identity", alpha=0.7)+ coord_flip()+
  scale_fill_manual(values = col_r)+
  theme(legend.position=("none"),
        axis.ticks.length.x = unit(0, "cm"),axis.ticks.length.y = unit(0, "cm"),
        axis.text.x = element_text(family="Comic Sans MS",size=8,
                                   margin = margin(t = 0.15, unit = "cm")),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.background = element_rect(colour = "black", fill="white", size=1),
        panel.spacing = unit(c(0,100,0,0), "cm"),
        plot.title = element_text(size=7, family = "Comic Sans MS"),
        #plot.background = element_rect(fill = "transparent", color = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        panel.background = element_rect(fill = "transparent",color ="transparent")) +
  geom_errorbar(aes(ymax=medi, ymin=medi))+xlab(NULL)+ylab(NULL)+
  scale_y_continuous(breaks=c(0,cumsum(ran$range)))
i1


# density plot
col_m = miscfun::GetTolColors( 11, scheme = "sunset")[c(6)]

i2= ggplot(cen_on, aes(x=no)) + 
  geom_density(alpha=.6, color='transparent', fill=col_m)+ 
  #geom_vline(aes(xintercept=m), color="black", linetype="dashed", size=1)+xlab(NULL)+
  theme(legend.position=("none"),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        panel.background = element_rect(fill = "transparent",color = "transparent"))+ 
  xlab(NULL)+ylab(NULL)
i2





# ratio, density plot
ra_on = all_on; ra_on[,5] = c(all_on$FP/all_on$no2); colnames(ra_on)[5] = c('ratio')



## color
bl2 = miscfun::GetTolColors( 11, scheme = "sunset")[5]
ye2 = miscfun::GetTolColors( 11, scheme = "sunset")[7]


i3 = ggplot(data = ra_on, aes(x = factor(gr_ca), y=ratio, fill=factor(gr_ca))) +
  ggdist::stat_halfeye(
    point_size=1, interval_size=0.4, #adjust=0.5, 
    .width=c(0,1))+scale_fill_manual(values=c(ye2, bl2)) +
  coord_flip()+
  ggtitle("Distribution in ratio of NO2 & PM 2.5") +
  theme(legend.position=("none"),
        axis.ticks.length.x = unit(0, "cm"),axis.ticks.length.y = unit(0, "cm"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.background = element_rect(colour = "black", fill="white", size=1),
        panel.spacing = unit(c(0,100,0,0), "cm"),
        axis.text.x = element_text(family="Comic Sans MS",size=8,
                                   margin = margin(t = 0.15, unit = "cm")),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour ="grey90"),
        #plot.margin = margin(1,1,1,1, "mm"),
        plot.title = element_text(size=7.5, family = "Comic Sans MS"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent")) +
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(breaks=seq(1,4,1))
i3


## combined
ggdraw() +
  draw_plot(v1) +
  #draw_plot(i2, x = 0.175, y = 0.733, width = .231, height = .1)+  
  #draw_plot(i1, x = 0.7, y = 0.133, width = .23, height = .13)+
  draw_plot(i1, x = 0.172, y = 0.742, width = .23, height = .115)+
  draw_plot(i3, x = 0.71, y = 0.215, width = .27, height = .2)





# second candidate

d_bg_ham = d_bg[which(d_bg$loc == 'Hamilton'),]

err =data.frame(no2 = mean(d_bg_ham$no2^(1/3)), FP = mean(d_bg_ham$FP^(1/3)), 
                no2_sd = sd(d_bg_ham$no2^(1/3)),  FP_sd = sd(d_bg_ham$FP^(1/3)))
err

md_c = miscfun::GetTolColors( 11, scheme = "sunset")[6]

i4 = ggplot(data= d_bg_ham, aes(x = (no2)^(1/3), y =(FP)^(1/3), label=loc))+ #, alpha=month))+
  ggtitle("Hamilton Air Quality") +
  theme(legend.position=("none"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour ="grey90"),
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        axis.ticks.length.x = unit(0, "cm"),axis.ticks.length.y = unit(0, "cm"),
        plot.title = element_text(size=7.5, family = "Comic Sans MS"),
        plot.background = element_rect(colour = "black", fill="white", size=1),
        axis.text.x = element_text(family="Comic Sans MS",size=8,
                                   margin = margin(t = 0.15, unit = "cm")),
        axis.text.y = element_text(family="Comic Sans MS",size=8,
                                   margin = margin(r = 0.15, unit = "cm"))
  )+
  stat_density_2d(data = d_bg, geom = "polygon", aes(x =(no2)^(1/3), y =(FP)^(1/3), 
                                                     alpha = (..level..) ^ 2), fill= mid_c, bins = 4)+
  
  # geom_linerange(data = err, aes(x = no2, ymin = (FP-FP_sd), ymax = (FP+FP_sd)), 
  #               height = 0, color="gray75")+
  #  geom_errorbarh(data = err, aes(y = FP, xmin = (no2-no2_sd), xmax = (no2+no2_sd)), 
  #                height = 0, color="gray75")+
  
  geom_point(aes(x =(no2)^(1/3), y =(FP)^(1/3)), shape = 1, stroke = 0.6, size = 0.75, alpha=0.9,
             color=bl)+  xlab(NULL)+ylab(NULL)+
  geom_point(aes(x =(no2)^(1/3), y =(FP)^(1/3)), size = 1.5, alpha=0.6,
             color=bl2, shape=21)+
  scale_x_continuous(breaks=seq(1.25,2.25,0.5))+
  scale_y_continuous(breaks=seq(1.65,2.15,0.25))

i4



# Summarise data:
# subDF not found ??
# sub_hm <- plyr::ddply(d_bg_ham,.(year), summarise, 
#                X = mean(no2^(1/3)), Y = mean(FP^(1/3)), 
#                X_sd = sd(no2^(1/3), na.rm = T), Y_sd = sd(FP^(1/3)))
# i5=ggplot(subDF, aes(x = X, y = Y))+
# geom_errorbar(aes(x = X, 
#                   ymin = (Y-Y_sd), 
#                   ymax = (Y+Y_sd)), 
#               width = 1, size = 0.5)
# i4 %+% i5



## final conclusion
fin = ggdraw() +
  draw_plot(v1) +
  draw_plot(i1, x = 0.15, y = 0.735, width = .23, height = .115)+
  draw_plot(i3, x = 0.71, y = 0.15, width = .27, height = .2)+
  draw_plot(i4, x = 0.725, y = 0.73, width = .22, height = .1825)
fin
ggsave("fin1.png", fin, height=6.5, width=10)

