#  --------------------------------------------------------------------- #
#  box plot
#  --------------------------------------------------------------------- #
library(showtext)
library(rvest)
library(cowplot)
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
#options(warn=-1)


setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

setSessionTimeLimit(cpu = Inf, elapsed = Inf)

# hourly organizer
fil5<-function(file){
  d<-read_excel(paste0(file), na = c('9999', '-999'), skip = 13, n_max = 3654)
  d$year <- as.numeric(format(as.Date(d$Date), '%Y'))
  d$month <- as.numeric(format(as.Date(d$Date), '%m')) 
  d$day <- as.numeric(format(as.Date(d$Date), '%d')) 
  
  weekdays1 = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
  d$wday = factor((weekdays(as.Date(d$Date)) %in% weekdays1), 
                  levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
  
  d2<-pivot_longer(d, cols = starts_with('H'), names_to = 'Hour', names_pattern = "(\\d+)", names_transform = c(Hour = as.integer))
  om = which(d2$value %in% c(boxplot.stats(d2$value)$out))
  
  ds<-group_by(d2, Hour, day, month, year) %>% summarise(value = value[-c(om)])
  ds=as.data.frame(na.omit(ds))
  return(ds)
}


# burlington
bu_no_p2<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO2-20/burlington.xlsx")
bu_no_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO-20/burlington.xlsx")
bu_fp_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/FP-20/burlington.xlsx")




# ham_mountain
ham_mountain_no_p2<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO2-20/ham_mountain.xlsx")
ham_mountain_no_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO-20/ham_mountain.xlsx")
ham_mountain_fp_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/FP-20/ham_mountain.xlsx")




# ham
ham_no_p2<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO2-20/ham.xlsx")
ham_no_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO-20/ham.xlsx")
ham_fp_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/FP-20/ham.xlsx")




# milton
milton_no_p2<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO2-20/milton.xlsx")
milton_no_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO-20/milton.xlsx")
milton_fp_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/FP-20/milton.xlsx")




# toro
to_no_p2<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO2-20/toro.xlsx")
to_no_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO-20/toro.xlsx")
to_fp_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/FP-20/toro.xlsx")



# win
wi_no_p2<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO2-20/win.xlsx")
wi_no_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO-20/win.xlsx")
wi_fp_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/FP-20/win.xlsx")




# sarnia
sa_no_p2<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO2-20/sarnia.xlsx")
sa_no_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/NO-20/sarnia.xlsx")
sa_fp_p<-fil5("/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/data/FP-20/sarnia.xlsx")






# central, scatter plot
fil11=function(d1, d2, d3, labl){
  d=merge(d1, d2, by=c('Hour','day','month', 'year'))
  d=merge(d, d3, by=c('Hour','day','month', 'year'))
  d=cbind(d, rep(labl, length(d[,1])))
  colnames(d)=c('Hour','day', 'month','year','no2', 'FP', 'no' , 'loc')
  return(d)
}


c2_win = fil11(wi_no_p2, wi_fp_p, wi_no_p, 'Windsor')
c2_to = fil11(to_no_p2, to_fp_p, to_no_p, 'Toronto ')
c2_ham = fil11(ham_no_p2, ham_fp_p, ham_no_p, 'Hamilton')
c2_bu = fil11(bu_no_p2, bu_fp_p, bu_no_p, 'Burlington')
c2_hamm = fil11(ham_mountain_no_p2, ham_mountain_fp_p, ham_mountain_no_p,'Hamilton \n Mountain')
c2_mil = fil11(milton_no_p2, milton_fp_p, milton_no_p, 'Milton  ')
c2_sa = fil11(sa_no_p2, sa_fp_p, sa_no_p, 'Sarnia  ')


box_on = rbind(c2_win, c2_to, c2_ham, c2_bu, c2_hamm, c2_mil, c2_sa)

## color setting
fp_c = miscfun::GetTolColors( 11, scheme = "sunset")[7:11]
no2_c = miscfun::GetTolColors( 11, scheme = "sunset")[5:1]

# PM 2.5 box plot
bb1=box_on %>%
  group_by(loc) %>%
  mutate(loc = factor(loc, levels= c('Hamilton', 'Windsor', 'Toronto ', 'Milton  ', 
                                     'Hamilton \n Mountain', 'Burlington', 'Sarnia  ')))

top7r = c('Hamilton', 'Windsor', 'Toronto ', 'Milton  ', 
          'Hamilton \n Mountain', 'Burlington', 'Sarnia  ')


## filterting, take off top, bottom 15% of each region
pm_cut=function(lo, data){
  bb2_l=subset(data, data$loc==lo)
  bb2_fil = bb2_l[ quantile(bb2_l$FP, prob=15/100) <= bb2_l$FP &
                     bb2_l$FP <= quantile(bb2_l$FP, prob=1-15/100),]
  return(bb2_fil)
}
pm_cut=data.frame(rbind(pm_cut(top7r[1],bb1), pm_cut(top7r[2],bb1), pm_cut(top7r[3],bb1), pm_cut(top7r[4],bb1),
                        pm_cut(top7r[5],bb1), pm_cut(top7r[6],bb1), pm_cut(top7r[7],bb1)))

bb1_fil=pm_cut %>%
  mutate(loc = factor(loc, levels= c('Hamilton', 'Windsor', 'Toronto ', 'Milton  ', 
                                     'Hamilton \n Mountain', 'Burlington', 'Sarnia  ')))


## plot, PM 2.5
b1 = ggplot(bb1_fil, aes(y = factor(loc, levels= c('Hamilton', 'Windsor', 'Toronto ', 'Milton  ', 
                                           'Hamilton \n Mountain', 'Burlington', 'Sarnia  ')),
                 x = FP, alpha=0.875)) +
  # filtered distribution on top
  ggdist::stat_halfeye(aes(slab_alpha = stat(f), fill = stat(abs(x) > 0), 
           fill_type = "gradient"), point_size=1, interval_size=0.4, #adjust=0.5, 
                       .width=0, show.legend=F)+  
  scale_fill_manual(values = c("gray90", "skyblue"), guide = 'none')+
  scale_alpha_continuous(guide = "none")+
  
  # then interval wise colors
  stat_interval(data = bb1, aes(x=FP, y = loc), .width = c(.10, .15, .25, .5, .7, .8),
                interval_size = 3.2) +
  scale_color_manual(values = c('transparent', fp_c),
                     name = "Top 7 region \n PM 2.5 distribution",
                     labels = c('', "15-85%", "25-75%", "37.5-62.5%", "42.5-57.5%", "45-55%"),
                     guide = guide_legend(direction = "horizontal", nrow = 1))+
  stat_pointinterval(aes(x = FP), .width = c(.001, 0.1), 
      position = position_nudge(y = -0.12), size=0.5) +
  scale_x_continuous(breaks=seq(0,14,2))+
  
  
  theme(legend.position='bottom',legend.margin=margin(-5, 0, 0, 0),
        axis.ticks.length.x = unit(0, "cm"), axis.ticks.length.y = unit(0, "cm"),
        axis.text.x = element_text(family="Comic Sans MS",
                                   margin = margin(t = 0.15, unit = "cm")),
        axis.text.y=element_text(family="Comic Sans MS", 
          margin= margin(t = 0.05, unit = "cm", r = -.55)),
        axis.ticks.y=element_blank(),
        axis.title.x =  element_text(family="Comic Sans MS", size=5.25,
                                     margin = margin(t = 0.15, unit = "cm")),
        axis.title.y =  element_blank(),
        
        panel.spacing = unit(c(0,100,0,0), "cm"),
        panel.grid.major.x = element_line(colour ="grey95"),
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        
        panel.background = element_rect(fill = NA),
        legend.title = element_text(family ="Comic Sans MS", size=6),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text = element_text(family = "Comic Sans MS", size=5, color='gray55'),
        legend.key.height= unit(0.2, 'cm'), legend.key.width= unit(0.7, 'cm'),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
  )+labs(x = expr(paste('Distribution of Air Quality: ', NO2)), family = "Comic Sans MS")
b1





# NO2 box plot
bb2=box_on %>%
  group_by(loc) %>%
  mutate(loc = factor(loc, levels= c('Hamilton', 'Windsor', 'Toronto ', 'Milton  ', 
                                     'Hamilton \n Mountain', 'Burlington', 'Sarnia  ')))
top7r = c('Hamilton', 'Windsor', 'Toronto ', 'Milton  ', 
  'Hamilton \n Mountain', 'Burlington', 'Sarnia  ')
## fitlering, take off top and bottom 15%

bl_cut=function(lo, data){
  bb2_l=subset(data, data$loc==lo)
  bb2_fil = bb2_l[ quantile(bb2_l$no2, prob=15/100) <= bb2_l$no2 &
                   bb2_l$no2 <= quantile(bb2_l$no2, prob=1-15/100),]
  return(bb2_fil)
}
bl_cut=data.frame(rbind(bl_cut(top7r[1],bb2), bl_cut(top7r[2],bb2), bl_cut(top7r[3],bb2), bl_cut(top7r[4],bb2),
                        bl_cut(top7r[5],bb2), bl_cut(top7r[6],bb2), bl_cut(top7r[7],bb2)))

bb2_fil=bl_cut %>%
  mutate(loc = factor(loc, levels= c('Hamilton', 'Windsor', 'Toronto ', 'Milton  ', 
              'Hamilton \n Mountain', 'Burlington', 'Sarnia  ')))



## plots
b2 = ggplot(bb2_fil, aes(x = no2, y = factor(loc, levels= c('Hamilton', 'Windsor', 'Toronto ', 'Milton  ', 
      'Hamilton \n Mountain', 'Burlington', 'Sarnia  ')), alpha=0.875)) +
  # filtered distribution on top
  ggdist::stat_halfeye(aes(slab_alpha = stat(f), fill = stat(abs(x) > 0), 
 fill_type = "gradient"), point_size=1, interval_size=0.4, #adjust=0.5, 
                       .width=0, show.legend=F)+  
  scale_fill_manual(values = c("gray90", "skyblue"), guide = 'none')+
  stat_interval(data = bb2, aes(x=no2, y = loc), .width = c(.10, .15, .25, .5, .7, .8),
                interval_size = 3.2) +
  scale_alpha_continuous(guide = "none")+
  
  # color
  scale_color_manual(values = c('transparent', no2_c),
                     name = "Top 7 region \n NO2 distribution",
                     labels = c('', "15-85%", "25-75%", "37.5-62.5%", "42.5-57.5%", "45-55%"),
                     guide = guide_legend(direction = "horizontal", nrow = 1))+
  stat_pointinterval(aes(x = no2), .width = c(.001, 0.1), 
                     position = position_nudge(y = -0.12), size=0.5) +
  
  scale_x_reverse(breaks=seq(0,18,2)) +
  theme(legend.position='bottom',legend.margin=margin(-5, 0, 0, 0),
        axis.ticks.length.x = unit(0, "cm"), axis.ticks.length.y = unit(0, "cm"),
        axis.text.x = element_text(family="Comic Sans MS",
                                   margin = margin(t = 0.15, unit = "cm")),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.title.x =  element_text(family="Comic Sans MS", size=5.25,
                                     margin = margin(t = 0.15, unit = "cm")),
        axis.title.y =  element_blank(),
        
        panel.spacing = unit(c(0,100,0,0), "cm"),
        panel.grid.major.x = element_line(colour ="grey95"),
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        
        panel.background = element_rect(fill = NA),
        legend.title = element_text(family ="Comic Sans MS", size=6),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text = element_text(family = "Comic Sans MS", size=5, color='gray55'),
        legend.key.height= unit(0.2, 'cm'), legend.key.width= unit(0.7, 'cm'),
        legend.box.background = element_rect(fill = "transparent", color = "transparent")
  )+labs(x = expr(paste('Distribution of Air Quality: ', NO2)), family = "Comic Sans MS")
b2



# filtered distribution on top
ggplot()+
ggdist::stat_halfeye(data = bb2_fil, aes(x=no2, slab_alpha = stat(f), fill = stat(abs(x) > 0), 
                                  fill_type = "gradient"), point_size=1, interval_size=0.4, #adjust=0.5, 
                     .width=c(0,1))+  
  scale_fill_manual(values = c("gray90", "skyblue"), guide = 'none')+
  scale_slab_alpha_continuous(guide = "none")



# side plot, variance
### NO2
## top 70% distribution

### !!!!!!!!!!!! run scatter plot for all_on variable
no2_d = all_on[ quantile(all_on$no2, prob=15/100) < all_on$no2 &
                  all_on$no2 < quantile(all_on$no2, prob=1-15/100),]
fp_d = all_on[ quantile(all_on$FP, prob=15/100) < all_on$FP &
                 all_on$FP < quantile(all_on$FP, prob=1-15/100),]

max(no2_d$no2); min(no2_d$no2)
max(fp_d$FP); min(fp_d$FP)

## color
bl2 = miscfun::GetTolColors( 11, scheme = "sunset")[5]
ye2 = miscfun::GetTolColors( 11, scheme = "sunset")[7]


s1 = 
  ggplot(data = no2_d, aes(y=no2)) +
  ggdist::stat_halfeye(aes(slab_alpha = stat(f), fill = stat(abs(x) > 0), 
    fill_type = "gradient"), point_size=1, interval_size=0.4, #adjust=0.5, 
                       .width=c(0,1))+  
  scale_fill_manual(values = c("gray90", "skyblue"), guide = 'none')+
  scale_slab_alpha_continuous(guide = "none")+
  stat_interval(data = all_on, aes(y=no2),
                .width = c(.10, .15, .25, .5, .7, .8),
                interval_size = 2) +
  scale_color_manual(values = c('transparent', no2_c), name = "Top 7 region \n NO2 distribution",
            labels = c('', "70%", "50%", "25%", "15%", "10%"),
            guide = guide_legend(direction = "horizontal"))+
  stat_pointinterval(aes(y = no2), .width = c(.001, 0.1), 
                     position = position_nudge(x = -0.07), size=0.5)+
  coord_flip()+
  ggtitle("Distribution of NO2 in Ontario") +
  theme(legend.position='none',
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
        plot.title = element_text(size=7.5, family = "Comic Sans MS",hjust = 0.5)
        ) + xlab(NULL)+ylab(NULL)+
  scale_y_reverse(breaks=seq(0,8,1))
s1

  

### FP PM2.5
## top 70% distribution

s2 = 
  ggplot(data = fp_d, aes(y=FP)) +
  ggdist::stat_halfeye(aes(slab_alpha = stat(f), fill = stat(abs(x) > 0), 
                           fill_type = "gradient"), point_size=1, interval_size=0.4, #adjust=0.5, 
                       .width=c(0,1))+  
  scale_fill_manual(values = c("gray90", ye2), guide = 'none')+
  scale_slab_alpha_continuous(guide = "none")+
  stat_interval(data = all_on, aes(y=FP),
                .width = c(.10, .15, .25, .5, .7, .8),
                interval_size = 2) +
  scale_color_manual(values = c('transparent', fp_c), name = "Top 7 region \n PM 2.5 distribution",
                     labels = c('', "70%", "50%", "25%", "15%", "10%"),
                     guide = guide_legend(direction = "horizontal"))+
  stat_pointinterval(aes(y = FP), .width = c(.001, 0.1), 
                     position = position_nudge(x = -0.07), size=0.5)+
  coord_flip()+
  ggtitle("Distribution of PM 2.5 in Ontario") +
  theme(legend.position='none',
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
        plot.title = element_text(size=7.5, family = "Comic Sans MS",hjust = 0.5)
  ) + xlab(NULL)+ylab(NULL)+
  scale_y_continuous(breaks=seq(0,8,0.5))
s2
  



# geospatial matching
p_map <- (read_sf('/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/Ministry_of_Health_Public_Health_Unit_Boundary/MOH_PHU_BOUNDARY.shp')
          %>% st_simplify(dTolerance = 100))

top7 = c('Toronto Downtown', 'Hamilton Downtown', 'Hamilton Mountain', 
         'Milton', 'Windsor Downtown', 'Burlington', 'Sarnia')



# location matching
d=read.csv('/Users/kyusoinlims/Dropbox/Mac/Desktop/untitled folder/location.csv')
str_locate(d, 'Address')

# data formation place of where location
i=1; k=1; da=data.frame(nrow=3, ncol=50)
while(i<=length(d[,1])){
  if (d[i,1] == 'Address'){
    da[k,1] = d[i-2,1]
    da[k,2:3] = c(as.numeric(d[i+2, 2]), as.numeric(d[i+1, 2])); k=k+1; i=i+1
  }
  else{k=k+1; i=i+1}
}

## data conversion
dat=na.omit(da)
colnames(dat)=c('loc','lat', 'long')
datt=dat[,2:3]
colnames(datt)=c('latitude', 'longitude')

# conversion
(sites <- st_as_sf(datt, coords = c("latitude", "longitude"), 
                   crs = 4326, agr = "constant"))
length(datt[,2])
si=cbind(dat[,1],sites)






# idea: impact = sum(FP+NO2) * population/500000

## math magnitude combination
top7_no2m = group_by(box_on, year, loc) %>% summarise(value = mean(no2, na.rm=T))
top7_fpm = group_by(box_on, year, loc) %>% summarise(value = mean(no2, na.rm=T))
#test = sub('\n', '', top7_fpm$loc)

top7_m = merge(top7_no2m, top7_fpm, by=c('loc'))
top7_m[,6] = c(top7_m$value.x + top7_m$value.y)

## mathematical magnitude
pop_d = data.frame(dat...1.=top7, pop=c(to_pop/500000, ha_pop/500000, ha_pop/500000, 
                  mi_pop/500000, wi_pop/500000, bu_pop/500000, sa_pop/500000),
          ratio = c(rep('typeA', 3), rep('typeB', 4)))
pop_d[,4] = c(top7_m[6,6]*(to_pop/500000), top7_m[2,6]*(ha_pop/500000),
              top7_m[3,6]*(ha_pop/500000), top7_m[4,6]*(mi_pop/500000),
              top7_m[7,6]*(wi_pop/500000), top7_m[1,6]*(bu_pop/500000),
              top7_m[5,6]*(sa_pop/500000))
colnames(pop_d)[4] = c('impact')

## (alternative) new subset
new_m = subset(dat, dat$loc %in% top7)
new_m = merge(new_m, pop_d, by=c('loc'))


## partial map
si_new = subset(si, si$dat...1. %in% top7)
si_ci = merge(si_new, pop_d, by=c('dat...1.'))

## differentiation into scale, discrete impact scale
si_ci$groups <- cut(as.numeric(si_ci$impact), breaks = c(1, 10, 90, 100))



crop_factor <- st_bbox(c(xmin = -84.25, xmax = -78.5, 
                         ymax = 42, ymin = 44), crs = st_crs(p_map))
oz_cropped <- st_crop(p_map, crop_factor)

## color
mid_c = miscfun::GetTolColors(9, scheme = "PRGn")[3:9]



# base
df <- data.frame(x = 1, y = 1)
base <- ggplot(df, aes(x, y)) + geom_point()+
  theme(legend.position='bottom',
        axis.ticks.length.x = unit(0, "cm"),axis.ticks.length.y = unit(0, "cm"),
        axis.text.x = element_text(margin = margin(t = 0.15, unit = "cm"), family="Comic Sans MS", 
                                   size=4.75),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        
        panel.spacing = unit(c(0,100,0,0), "cm"),
        panel.grid.major.x = element_line(colour ="grey95"),
        plot.margin = margin(12, 50, 50, 50, "cm"),
        panel.background = element_rect(fill = NA),
        
        legend.title = element_text(family ="Comic Sans MS", size=6),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text = element_text(family = "Comic Sans MS", size=5, color='gray55'),
        legend.key.height= unit(0.2, 'cm'), legend.key.width= unit(0.7, 'cm'),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        axis.line.x = element_blank(), axis.line.y = element_blank()
  )+xlab(NULL)+ylab(NULL)





## combine
fin2 = ggdraw(base) +
draw_plot(b2, x = 0, y = 0.2975, width = 0.5, height =0.555)+# width = 1.125, height =1.167)+
draw_plot(b1, x = 0.45, y = 0.2975, width = 0.5, height =0.555)+
draw_plot(s1, x = 0.025, y = 0.83, width = 0.3, height =0.165)+
draw_plot(s2, x = 0.6625, y = 0.83, width = 0.3, height =0.165)+
draw_plot_label(c('2020 year NO2 & PM2.5 in \n   top 7 regions of Ontario'), 
                family = "Comic Sans MS", 0.3685, 0.915, size = 6.75, color='gray40')
fin2
ggsave("fin2.png", fin2, width=11, height=6.5)


  