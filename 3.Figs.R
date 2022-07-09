setwd("/Users/megancattau/Dropbox/0_EarthLab/US_Pyromes/Pyromes/pyromes_code/Data")

library(rgdal)
library(sp)
library(sf)
library(dplyr)
library(raster)
library(tidyr)
library(rgeos)
library(ggplot2)
library(plyr)
library(cowplot)                                        

library(ggthemes)
library(ggmap)
library(RColorBrewer)
library(ggpubr)
library(rlist)

# Figure S1
data_crs<- " +proj=utm +zone=13 +datum=WGS84 +units=m +no_defs +ellps=WGS84 "
Ecoregion_clip <- st_read(dsn = 'Ecoregion', layer = "ecoregion_proj_fix_clip", quiet = TRUE) %>%
  st_transform(., data_crs)
Ecoregion_fix<-st_make_valid(Ecoregion_clip)
Ecoregion_simple<-st_simplify(Ecoregion_fix)
Ecoregion2<-as(Ecoregion_simple, 'Spatial')
Ecoregion<-as(Ecoregion_fix, 'Spatial')

overlay <- fortify(Ecoregion2, region="NA_L1NAME")
samples_df<-read.csv("Results/samples_df.csv")
samples_df<-samples_df[,-1]	
samples_spatial<-samples_df
coordinates(samples_spatial)<-~x+y
proj4string(samples_spatial)<-CRS("+init=epsg:32613")
# all the mean values plus FID and ecoregion
# samples_df_mean<-samples_df[,c(1:15, 376, 379)]
samples_spatial_mean<-samples_spatial[,c(1:15, 435, 436)]

names_vector<-c("Num fires (MODIS)", "Num fires (MTBS)", "Num fires (FPA FOD)", "Mean Intensity (MODIS)", "Maximum Intensity (MODIS)", "Mean Fire Size (MTBS)", "Max Fire Size (MTBS)",  "Mean Fire Size (FPA FOD)", "Max Fire Size (FPA FOD)",  "Burned Area (MTBS)",  "Burned Area (FPA FOD)", "Season Length (MODIS)",  "Season Length (MTBS)",  "Season Length (FPA FOD)", "Prop human ign (FPA FOD)")

units_simple<-c("n fires", "n fires", "n fires", "MW", "MW", "ha", "ha", "ha","ha", "ha", "ha", "days","days", "days", "proportion")


names_no_units<-c("Fire Frequency", "Fire Frequency", "Fire Frequency", "Average Intensity", "Extreme Intensity", "Average Fire Size", "Extreme Fire Size", "Average Fire Size", "Extreme Fire Size", "Burned area", "Burned area", "Season Length","Season Length", "Season Length", "Human Ignitions")

units_simple<-c("n fires", "n fires", "n fires", "MW", "MW", "ha", "ha", "ha","ha", "ha", "ha", "days","days", "days", "proportion")
    
plot_char <- function (var){
  ggplot(as.data.frame(samples_spatial_mean), aes(x=x, y=y, colour=samples_spatial_mean@data[,var]))+
    coord_equal() +
    geom_point(size=1)+
    scale_color_gradient(low="white", high="darkred")+
    ggtitle(paste0("(", letters[var], ") ", names_vector[var])) +
    theme(plot.title = element_text(hjust = 0.5, size=10))+
    labs(colour=units_simple[var])+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.ticks=element_blank(),	
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_blank())+
    geom_polygon(data=overlay, aes(x=long, y=lat, group=group), fill=NA, colour="black")
}

pdf(file = "/Users/megancattau/Dropbox/0_EarthLab/US_Pyromes/Pyromes/pyromes_code/Data/Figures/FigS1.pdf", height=10, width=8)
ggarrange(plot_char(1), plot_char(2),  plot_char(3),  
          plot_char(4),  plot_char(5),  plot_char(6),  
          plot_char(7),  plot_char(8),  plot_char(9),   
          plot_char(10), plot_char(11),  plot_char(12),  
          plot_char(13),  plot_char(14),  plot_char(15),   
          ncol=3, nrow=5, legend=c("right"))    
dev.off()


samples_df_k<-read.csv("Results/samples_df_k.csv")
samples_df_k<-samples_df_k[,-1]  
 
 

local_max_dunn<-c( 2,  5,  8, 14, 19, 24, 28, 30, 32, 35, 37, 39)

make_map_kmeans<-function(kmeans_number, let){
	ggplot(samples_df_k, aes(x, y)) + 
	geom_point(aes(color = factor(kmeans_number)), stroke=.6) +  
	coord_equal() +
	ggtitle(paste0("k = ", local_max_dunn[let])) +
    theme(plot.title = element_text(hjust = 0.5, size=10))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.ticks=element_blank(),	
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_blank())+
	geom_polygon(data=overlay, aes(x=long, y=lat, group=group), fill=NA, colour="black")+
	labs(colour="Pyromes")
}


pdf(file = "/Users/megancattau/Dropbox/0_EarthLab/US_Pyromes/Pyromes/pyromes_code/Data/Figures/FigS7.pdf", height=10, width=8)
k2<-make_map_kmeans(samples_df_k$kmeans2, 1)
k5<-make_map_kmeans(samples_df_k$kmeans5, 2)
k8<-make_map_kmeans(samples_df_k$kmeans8, 3)
k14<-make_map_kmeans(samples_df_k$kmeans14, 4)
k19<-make_map_kmeans(samples_df_k$kmeans19, 5)
k24<-make_map_kmeans(samples_df_k$kmeans24, 6)
k28<-make_map_kmeans(samples_df_k$kmeans28, 7)
k30<-make_map_kmeans(samples_df_k$kmeans30, 8)
k32<-make_map_kmeans(samples_df_k$kmeans32, 9)
k35<-make_map_kmeans(samples_df_k$kmeans35, 10)
k37<-make_map_kmeans(samples_df_k$kmeans37, 11)
k39<-make_map_kmeans(samples_df_k$kmeans39, 12)
ggarrange(k2, k5, k8, k14, k19, k24, k28, k30, k32, k35, k37, k39,
ncol=3, nrow=4, legend=c("none"))
dev.off()


# scale_color_manual(name="Pyromes", labels=c("1- Highest and increasing human impact, moderate characteristics", "2- Increasing human ignitions, least extreme characteristics", "3- Moderate human impact, moderate characteristics", "4- Smallest human impact, most extreme characteristics, and increasing area and intensity", "5- Moderate human impact, increasingly extreme intensity"), values=c(cbPalette))+

make_map_disagg<-function(pyrome_number){
	ggplot(samples_df_k[samples_df_k$kmeans8==pyrome_number,], aes(x, y)) + 
	geom_point(aes(color= factor(kmeans8))) +  
	coord_equal() +
	scale_color_manual(values=c(colorblind_pal()(8)[pyrome_number]))+
	ggtitle(paste0("k=", pyrome_number)) +
	theme_nothing(legend = TRUE) +
	geom_polygon(data=overlay, aes(x=long, y=lat, group=group), fill=NA, colour="black")+
	labs(colour="Pyrome")
}	
	
k8_1<-make_map_disagg(1)
k8_2<-make_map_disagg(2)
k8_3<-make_map_disagg(3)
k8_4<-make_map_disagg(4)
k8_5<-make_map_disagg(5)
k8_6<-make_map_disagg(6)
k8_7<-make_map_disagg(7)
k8_8<-make_map_disagg(8)

pdf(file = "/Users/megancattau/Dropbox/0_EarthLab/US_Pyromes/Pyromes/pyromes_code/Data/Figures/FigS8.pdf", height=6, width=7)	
ggarrange(k8_1, k8_2, k8_3, k8_4, k8_5, k8_6, k8_7, k8_8, 
ncol=2, nrow=4, legend=c("none"))
dev.off()



### Figure 1	
k8<- ggplot() +
  geom_tile(data = samples_df_k, aes(x=x, y=y, fill = as.factor(kmeans8))) +
  geom_sf(data=Ecoregion_simple, fill="transparent", color = "black",lwd=0.5)+
  scale_fill_manual(name="Pyrome",values=c(colorblind_pal()(8)))+
  theme_void()

MODIS_FRP_dens <- 
  ggplot(samples_df_k[samples_df_k$kmeans8!="7",], aes(x=Mean_FRP_MODIS_mean, color = as.factor(kmeans8))) +
  geom_density(key_glyph = "path") +
  theme_bw() +
  xlab("Average Intensity (MW)\n Transformation: Square Root") +
  scale_x_continuous(trans = "sqrt",
                     breaks = c(10,50,100,200, 300, 400),
                     labels = scales::label_number_si())+
  ylab("Density     ") +
  scale_color_manual(values=c(colorblind_pal()(8)[c(1:6,8)]))+
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
     
 cuberoot_trans<-scales::trans_new(name="cuberoot",
                transform = function(x)x^(1/4), 
                inverse = function(x)x^4)
                       
FOD_mean_area_dens <-
  ggplot(samples_df_k[samples_df_k$kmeans8!="7",], aes(x=Mean_area_FOD_mean, color = as.factor(kmeans8))) +
  geom_density() +
  theme_bw() +
  scale_x_continuous(trans = cuberoot_trans,
                     breaks = c(10,100,1000,3000, 5000, 10000, 20000),
                     labels = scales::label_number_si())+
  xlab("Average Fire Size (ha)\n Transformation: Cube Root") +
  ylab("Density     ")+
  scale_color_manual(values=c(colorblind_pal()(8)[c(1:6,8)]))+
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
        
FOD_mean_freq_dens <-   
ggplot(samples_df_k[samples_df_k$kmeans8!="7",], aes(x=Number_fires_FOD_mean, color = as.factor(kmeans8))) +
  geom_density() +
  theme_bw() +
  scale_x_continuous(trans = "log1p",
                     breaks=c(10,30,100,200,400),
                     labels = scales::label_number_si())+
  xlab("Fire Frequency (n fires)\n Transformation: Log + 1") +
  ylab("Density     ")+
  scale_color_manual(values=c(colorblind_pal()(8)[c(1:6,8)]))+
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
        
FOD_mean_sl_dens<-          
ggplot(samples_df_k[samples_df_k$kmeans8!="7",], aes(x=Std_JD_FOD_mean, color = as.factor(kmeans8))) +
  geom_density() +
  ylab("Density     ")+
  xlab("Season Length (days)\n Transformation: Identity")+
  theme_bw() +
  # scale_color_colorblind() +
  scale_color_manual(values=c(colorblind_pal()(8)[c(1:6,8)]))+
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

FOD_sum_area_dens<- 
 ggplot(samples_df_k[samples_df_k$kmeans8!="7",], aes(x=Sum_area_FOD_mean, color = as.factor(kmeans8))) +
  geom_density() +
  theme_bw() +
  xlab("Area Burned (ha)\n Transformation: Cube Root") +
  scale_x_continuous(trans=cuberoot_trans,
                     breaks= c(10, 100,1000, 5000,10000,15000),
                     labels = scales::label_number_si())+
  ylab("Density     ") +
  scale_color_manual(values=c(colorblind_pal()(8)[c(1:6,8)]))+
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


grouped_list<-list.load("Results/grouped_list.rds")

grouped_list_no7<-list(length=15)
for (i in 1:15){
	grouped_list_no7[[i]]<-grouped_list[[i]][which(grouped_list[[i]][1]!="7"),]
}

make_gg_time8<-function(char){
	ggplot(data = grouped_list_no7[[char]], aes(x = year, y = value_by_group)) +
	geom_point(aes(color = factor(kmeans8))) +
	# ylim(ylim1, ylim2)+
	geom_smooth(method="lm", aes(group = kmeans8, color = factor(kmeans8)))+
	scale_color_manual(values=c(colorblind_pal()(8)[c(1:6,8)]))+
	labs(x="Year", y=units_simple[char], colour="Pyrome")+
	theme_bw()+
	theme(legend.position = "none")
}		

make_gg_time8_e<-function(char){
	ggplot(data = grouped_list_no7[[char]], aes(x = year, y = value_by_group)) +
	geom_point(aes(color = factor(kmeans8))) +
	# ylim(ylim1, ylim2)+
	geom_smooth(method="lm", aes(group = kmeans8, color = factor(kmeans8)))+
	scale_color_manual(values=c(colorblind_pal()(8)[c(1:6,8)]))+
	  scale_y_continuous(trans = cuberoot_trans,
                     breaks = c(10,100,1000,3000, 5000, 10000, 20000),
                     labels = scales::label_number_si())+
	labs(x="Year", y=paste0(units_simple[char], "\n (Cube Root)"), colour="Pyrome")+
	theme_bw()+
	theme(legend.position = "none")
}

make_gg_time8_k<-function(char){
	ggplot(data = grouped_list_no7[[char]], aes(x = year, y = value_by_group)) +
	geom_point(aes(color = factor(kmeans8))) +
	# ylim(ylim1, ylim2)+
	geom_smooth(method="lm", aes(group = kmeans8, color = factor(kmeans8)))+
	scale_color_manual(values=c(colorblind_pal()(8)[c(1:6,8)]))+
	  scale_y_continuous(trans = cuberoot_trans,
                     breaks= c(10, 100,1000, 5000,10000,15000),
                     labels = scales::label_number_si())+
	labs(x="Year", y=paste0(units_simple[char], "\n (Cube Root)"), colour="Pyrome")+
	theme_bw()+
	theme(legend.position = "none")
}

# Figure 1

dens <- ggarrange(MODIS_FRP_dens, FOD_mean_area_dens,  FOD_mean_freq_dens, FOD_mean_sl_dens, FOD_sum_area_dens, 
ncol =1, labels = c("(b)","(d)","(f)","(h)","(j)"),label.x =0, hjust = 0)

trends <- ggarrange(
make_gg_time8(4), 
make_gg_time8_e(8), 
make_gg_time8(3), 
make_gg_time8(14), 
make_gg_time8_k(11), ncol =1, labels = c("(c)","(e)","(g)","(i)","(k)"),label.x =0, hjust = 0)

fig1 <- ggdraw(xlim = c(0,7.5), ylim = c(0,10)) +
  draw_plot(k8, x=0, y=7, height = 3, width = 7.5) +
  draw_label("(a)", x=1.4, y=9.6, fontface = "bold")+
  draw_plot(dens, x=0.1, y=0, height = 7, width = 3) +
  draw_plot(trends, x=3.4,y=0, height = 7, width=3)

ggsave(plot=fig1,"Figures/Fig1.pdf", height = 10, width = 7.5)
ggsave(plot=fig1,"Figures/Fig1.png", height = 10, width = 7.5)



#### Fig S9	
k5<-ggplot() +
  geom_tile(data = samples_df_k, aes(x=x, y=y, fill = as.factor(kmeans5))) +
  geom_sf(data=Ecoregion_simple, fill="transparent", color = "black",lwd=0.5)+
  scale_fill_manual(name="Pyrome",values=c(colorblind_pal()(5)))+
  theme_void()


MODIS_FRP_dens5 <- 
  ggplot(samples_df_k, aes(x=Mean_FRP_MODIS_mean, color = as.factor(kmeans5))) +
  geom_density(key_glyph = "path") +
  theme_bw() +
  xlab("Average Intensity (MW)\n Transformation: Square Root") +
  scale_x_continuous(trans = "sqrt",
                     breaks = c(10,50,100,200, 300, 400),
                     labels = scales::label_number_si())+
  ylab("Density") +
  scale_color_colorblind() +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
     
 cuberoot_trans<-scales::trans_new(name="cuberoot",
                transform = function(x)x^(1/4), 
                inverse = function(x)x^4)
                       
FOD_mean_area_dens5 <-
  ggplot(samples_df_k, aes(x=Mean_area_FOD_mean, color = as.factor(kmeans5))) +
  geom_density() +
  theme_bw() +
  scale_x_continuous(trans = cuberoot_trans,
                     breaks = c(10,100,1000,3000, 5000, 10000, 20000),
                     labels = scales::label_number_si())+
  xlab("Average Fire Size (ha)\n Transformation: Cube Root") +
  ylab("Density")+
  scale_color_colorblind() +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
        
FOD_mean_freq_dens5 <-   
ggplot(samples_df_k, aes(x=Number_fires_FOD_mean, color = as.factor(kmeans5))) +
  geom_density() +
  theme_bw() +
  scale_x_continuous(trans = "log1p",
                     breaks=c(10,30,100,200,400),
                     labels = scales::label_number_si())+
  xlab("Fire Frequency (n fires)\n Transformation: Log + 1") +
  ylab("Density")+
  scale_color_colorblind() +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank())
        
FOD_mean_sl_dens5<-          
ggplot(samples_df_k, aes(x=Std_JD_FOD_mean, color = as.factor(kmeans5))) +
  geom_density() +
  ylab("Density")+
  xlab("Season Length (days)\n Transformation: Identity")+
  theme_bw() +
  scale_color_colorblind() +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

FOD_sum_area_dens5<- 
 ggplot(samples_df_k, aes(x=Sum_area_FOD_mean, color = as.factor(kmeans5))) +
  geom_density() +
  theme_bw() +
  xlab("Area Burned (ha)\n Transformation: Cube Root") +
  scale_x_continuous(trans=cuberoot_trans,
                     breaks= c(10, 100,1000, 5000,10000,15000),
                     labels = scales::label_number_si())+
  ylab("Density") +
  scale_color_colorblind() +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

grouped_list5<-list.load("Results/grouped_list5.rds")

make_gg_time5<-function(char){
	ggplot(data = grouped_list5[[char]], aes(x = year, y = value_by_group5)) +
	geom_point(aes(color = factor(kmeans5))) +
	geom_smooth(method="lm", aes(group = kmeans5, color = factor(kmeans5)))+
	scale_color_manual(values=c(colorblind_pal()(5)))+
	labs(x="Year", y=units_simple[char], colour="Pyrome")+
	theme_bw()+
	theme(legend.position = "none")
}	


dens5 <- ggarrange(MODIS_FRP_dens5, FOD_mean_area_dens5,  FOD_mean_freq_dens5, FOD_mean_sl_dens5, FOD_sum_area_dens5, 
ncol =1, labels = c("(b)","(d)","(f)","(h)","(j)"),label.x =0, hjust = 0)

trends5 <- ggarrange(
make_gg_time5(4), 
make_gg_time5(8), 
make_gg_time5(3), 
make_gg_time5(14), 
make_gg_time5(11), ncol =1, labels = c("(c)","(e)","(g)","(i)","(k)"),label.x =0, hjust = 0)

figS9 <- ggdraw(xlim = c(0,7.5), ylim = c(0,10)) +
  draw_plot(k5, x=0, y=7, height = 3, width = 7.5) +
  draw_label("(a)", x=1.4, y=9.6, fontface = "bold")+
  draw_plot(dens5, x=0.1, y=0, height = 7, width = 3) +
  draw_plot(trends5, x=3.4,y=0, height = 7, width=3)

ggsave(plot=figS9,"Figures/FigS9.pdf", height = 10, width = 7.5)
ggsave(plot=figS9,"Figures/FigS9.png", height = 10, width = 7.5)


# Fig 3 - Niche Trade-offs


#just include values >0 for variable 1
unicode_minus = function(x) sub('^-', '\U2212', format(x))
make_nich_fig<-function(data, variable1, variable2, SIlabel){
	gg1 <- merge(data, aggregate(cbind(mean.x=log(data[ ,variable1]), mean.y=log(data[ , variable2]))~kmeans8, data[ , ], mean, na.action=na.omit), by="kmeans8")
	gg <- merge(gg1, aggregate(cbind(se.x=log(data[ ,variable1]), se.y=log(data[ ,variable2]))~kmeans8, data[ , ], sd, na.action=na.omit), by="kmeans8")
	ggplot(gg, aes(log(gg[,variable1+1]), log(gg[,variable2+1]),color=factor(kmeans8)))+
	geom_point(alpha=.4, size=1)+
	labs(x=paste0("log ", units_simple[variable1]), y=paste0("log ", units_simple[variable2]), color="Pyrome")+ 
	geom_point(data=gg, alpha = .2, size=1.5, aes(x=mean.x, y=mean.y), inherit.aes = FALSE)+
	ggtitle(paste0("(", letters[SIlabel], ") ", names_no_units[variable1], " and ", names_no_units[variable2]))+
	geom_errorbar(data=gg, width=0, alpha = .2, aes(x=mean.x, ymin=mean.y-se.y,ymax=mean.y+se.y, color=factor(kmeans8)), inherit.aes = FALSE)+
	scale_color_manual(values=c(colorblind_pal()(8)))+
	geom_errorbarh(data=gg, height=0, alpha = .2, aes(y=mean.y, xmin=mean.x-se.x,xmax=mean.x+se.x, color=factor(kmeans8)),  inherit.aes = FALSE)+
	theme_bw()+
	scale_x_continuous(labels = unicode_minus) + 
	scale_y_continuous(labels = unicode_minus)

}


make_gg_time<-function(char){
	ggplot(data = grouped_list[[char]], aes(x = year, y = value_by_group)) +
	geom_point(aes(color = factor(kmeans8))) +
	geom_smooth(method="lm", aes(group = kmeans8, color = factor(kmeans8)))+
	scale_color_manual(values=c(colorblind_pal()(8)))+
	labs(x="Year", y=names(grouped_list[char]), colour="Pyrome")+
	theme_bw()
}		
	
	
# scale_colour_manual(values=(cbPalette[c(2,1)]), labels=c("Primarily Lightning (>75%)", "Primarily Anthropogenic (>75%)")) +

samples_df_k_MODIS<-samples_df_k[samples_df_k$Number_fires_MODIS_mean>0,]
samples_df_k_MTBS<-samples_df_k[samples_df_k$Number_fires_MTBS_mean>0,]
samples_df_k_FOD<-samples_df_k[samples_df_k$Number_fires_FOD_mean>0,]

# Figure 3
ggarrange(
make_nich_fig(samples_df_k_MODIS, 1, 4, 1), # mean FRP
make_nich_fig(samples_df_k_FOD, 3, 8, 2), # mean size - FOD
ncol=2, nrow=1, legend="none")
ggsave("Figures/Fig3.png", height = 5, width=10)


### Figure S12
ggarrange(
# Number fires and intensity
make_nich_fig(samples_df_k_MODIS, 1, 4, 1), # mean FRP
make_nich_fig(samples_df_k_MODIS, 1, 5, 2), # max FRP
# Season length and intensity
make_nich_fig(samples_df_k_MODIS, 13, 4, 3), # mean and mean
make_nich_fig(samples_df_k_MODIS, 13, 5, 4), # max and max
# Number fires and fire event size
make_nich_fig(samples_df_k_MTBS, 2, 6, 5), # mean size - mtbs
make_nich_fig(samples_df_k_MTBS, 2, 7, 6), # max event size - mtbs
make_nich_fig(samples_df_k_FOD, 3, 8, 7), # mean size - FOD
make_nich_fig(samples_df_k_FOD, 3, 9, 8), # max size - FOD
ncol=2, nrow=4, legend="none")
ggsave("Figures/FigS12_1.png", height = 10, width=8)

ggarrange(
# Season length and Fire event size
make_nich_fig(samples_df_k_MTBS, 12, 6, 9), # mean size and SL - mtbs
make_nich_fig(samples_df_k_MTBS, 12, 7, 10), # max size and SL - mtbs
make_nich_fig(samples_df_k_FOD, 14, 8, 11), # mean size and SL - FOD
make_nich_fig(samples_df_k_FOD, 14, 9, 12), # max size and SL - FOD
# Number fires and burned area
make_nich_fig(samples_df_k_MTBS, 2, 10, 13), # mtbs
make_nich_fig(samples_df_k_FOD, 3, 11, 14), # FOD
# Season length and Burned area
make_nich_fig(samples_df_k_MTBS, 12, 10, 15), # mtbs
make_nich_fig(samples_df_k_FOD, 14, 11, 16), # FOD
ncol=2, nrow=4, legend="none")
ggsave("Figures/FigS12_2.png", height = 10, width=8)

ggarrange(
# Intensity and fire event size
make_nich_fig(samples_df_k_MTBS, 4, 6, 17), # mean FRP and mean size MTBS
make_nich_fig(samples_df_k_MTBS, 5, 7, 18), # max FRP and max size MTBS
make_nich_fig(samples_df_k_FOD, 4, 8, 19), # mean FRP and mean size FOD
make_nich_fig(samples_df_k_FOD, 5, 9, 20), # max and max size FOD
# Intensity and burned area
make_nich_fig(samples_df_k_MTBS, 4, 10, 21), # mean FRP and BA MTBS
make_nich_fig(samples_df_k_MTBS, 5, 10, 22), # max FRP and BA MTBS
make_nich_fig(samples_df_k_FOD, 4, 11, 23), # mean FRP and BA FOD
make_nich_fig(samples_df_k_FOD, 5, 11, 24), # max FRP and BA FOD
ncol=2, nrow=4, legend="none")
ggsave("Figures/FigS12_3.png", height = 10, width=8)

ggarrange(
# Number fires and season length
make_nich_fig(samples_df_k_MTBS, 2, 12, 25), # MTBS
make_nich_fig(samples_df_k_MODIS, 1, 13, 26), #MODIS
make_nich_fig(samples_df_k_FOD, 3, 14, 1),# FOD
ncol=2, nrow=2, legend="none")
ggsave("Figures/FigS12_4.png", height = 5, width=8)



############ Scratch ###############

# below is if Fig 1b, d, f, h, j are maps
make_char<-function(char){
	ggplot(py_dat %>%
    filter(variable == unique(py_dat$variable)[char])) +
    geom_tile(aes(x=x, y=y, fill = pyrome_mean)) +
    geom_sf(data=Ecoregion_simple, fill="transparent", color = "black",lwd=0.5)+
    scale_fill_gradient(low="lightyellow", high="darkred",name=us[i])+
    annotate("text",label = unique(py_dat$variable)[i], x=xpos,y=ypos,
             size=4)+
    theme_void()
}

char_plots <- ggarrange(make_char(1),make_char(2), make_char(3), make_char(4), make_char(5),
                       nrow=5, labels = c("b", "d", "f", "h", "j"))
 char_plots
 dev.off()
 
 
 ## Map of characteristic value for each
mean_char8_df<-read.table("Results/mean_char8_df.txt")
mean_char_df_t8<-data.frame(t(mean_char8_df))
names(mean_char_df_t8)<-paste0("mean_", names(samples_df_k)[1:15])
mean_char_df_t8$kmeans8<-1:8

# merge with cluster num, x, and y
new_df8<-vector('list', 15)
for (i in 1:15){
	new_df8[[i]]<-merge(samples_df_k[,c(436,437, 441)], mean_char_df_t8[,c(i,16)], by="kmeans8")
}


#########
# r_template <- raster(ncol=ncol(Fishnet), nrow=nrow(Fishnet))
# extent(r_template)<-extent(Fishnet)
# projection(r_template)<-projection(Fishnet)

# k5_raster<-rasterize(samples_df_k_spatial, r_template, field=samples_df_k_spatial$kmeans5)
			
# ...sample that raster at samples_df
# samples_df_spatial<-samples_df
# coordinates(samples_df_spatial)<-~x + y														# make it spatial
# plot(k5_raster)
# plot(samples_df_spatial, add=TRUE)

# samples_df_k2<-raster::extract(k5_raster, samples_df_spatial)			# extract values at sample points
# samples_df_k2<-replace(samples_df_k2, samples_df_k2==0, NA)
# samples_df$kmeans5<-samples_df_k2


