##DAY3CODE##
library(baseballr)
library(ggplot2)
library(dbplyr)
library(grDevices)

library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)

#DEFINE YESTERDAY
yesterday<- Sys.Date()-1

#GET YESTERDAY'S GAMES FROM STATCAST ("YYYY-MM-DD")
rdata<-scrape_statcast_savant(start_date=yesterday-30, end_date=yesterday,playerid = "543037", player_type = 'pitcher')

#Create the Zone
zone_x_n <- -10/12
zone_x_p <- 10/12


rdata$zone_x_n <- -10/12
rdata$zone_x_p <- 10/12
rdata$midpoint <- ((rdata$sz_top+rdata$sz_bot)*(.5))

#Create the heart
rdata$heart_x_n <- (rdata$zone_x_n*(2/3))
rdata$heart_x_p <- (rdata$zone_x_p*(2/3))

rdata$heart_y_b <- (rdata$midpoint-((rdata$sz_top-rdata$midpoint)*(2/3)))
rdata$heart_y_t <- (rdata$midpoint+((rdata$sz_top-rdata$midpoint)*(2/3)))

#Create Shadow
rdata$shadow_x_n <- (rdata$zone_x_n)*(1+(1/3))
rdata$shadow_x_p <- (rdata$zone_x_p)*(1+(1/3))

rdata$shadow_y_b <- (rdata$midpoint-((rdata$sz_top-rdata$midpoint)*(1+(1/3))))
rdata$shadow_y_t <- (rdata$midpoint+((rdata$sz_top-rdata$midpoint)*(1+(1/3))))

#Create chase
rdata$chase_x_n <- (rdata$zone_x_n)*(2)
rdata$chase_x_p <- (rdata$zone_x_p)*(2)

rdata$chase_y_b <- (rdata$midpoint-((rdata$sz_top-rdata$midpoint)*2))
rdata$chase_y_t <- (rdata$midpoint+((rdata$sz_top-rdata$midpoint)*2))

#PLOT AVERAGE SIZES OF ZONES
shadowin<-mean(rdata$shadow_x_n, trim=0, na.rm=TRUE)
shadowout<-mean(rdata$shadow_x_p, trim=0, na.rm=TRUE)
shadowbot<-mean(rdata$shadow_y_b, trim=0, na.rm=TRUE)
shadowtop<-mean(rdata$shadow_y_t, trim=0, na.rm=TRUE)

chasein<-mean(rdata$chase_x_n, trim=0, na.rm=TRUE)
chaseout<-mean(rdata$chase_x_p, trim=0, na.rm=TRUE)
chasebot<-mean(rdata$chase_y_b, trim=0, na.rm=TRUE)
chasetop<-mean(rdata$chase_y_t, trim=0, na.rm=TRUE)

zonetop<-mean(rdata$sz_top, trim=0, na.rm=TRUE)
zonebot<-mean(rdata$sz_bot, trim=0, na.rm=TRUE)

heartin<-mean(rdata$heart_x_n, trim=0, na.rm=TRUE)
heartout<-mean(rdata$heart_x_p, trim=0, na.rm=TRUE)
heartbot<-mean(rdata$heart_y_b, trim=0, na.rm=TRUE)
hearttop<-mean(rdata$heart_y_t, trim=0, na.rm=TRUE)

#Determine Swing Decision Location
rdata$isheart[(rdata$plate_x>rdata$heart_x_n)&(rdata$plate_x<rdata$heart_x_p)&(rdata$plate_z>rdata$heart_y_b)&(rdata$plate_z<rdata$heart_y_t)]<-"heart"

rdata$isshadow[(rdata$plate_x>rdata$shadow_x_n)&(rdata$plate_x<rdata$shadow_x_p)&(rdata$plate_z>rdata$shadow_y_b)&(rdata$plate_z<rdata$shadow_y_t)&is.na(rdata$isheart)]<-"shadow"

rdata$ischase[(rdata$plate_x>rdata$chase_x_n)&(rdata$plate_x<rdata$chase_x_p)&(rdata$plate_z>rdata$chase_y_b)&(rdata$plate_z<rdata$chase_y_t)&is.na(rdata$isheart)&is.na(rdata$isshadow)]<-"chase"

rdata$iswaste[is.na(rdata$isheart)&is.na(rdata$isshadow)&is.na(rdata$ischase)]<-"waste"

rdata$isheart[is.na(rdata$isheart)] <- ""
rdata$isshadow[is.na(rdata$isshadow)] <- ""
rdata$ischase[is.na(rdata$ischase)] <- ""
rdata$iswaste[is.na(rdata$iswaste)] <- ""

#Limit data to only:
sdata<- subset(rdata, rdata$pitcher=="543037")

#Create Column that combines the different SDL locations into one
sdata$sdl <- paste(sdata$isheart, sdata$isshadow, sdata$ischase, sdata$iswaste)
zonethick <- 1.5


chasezone <- geom_rect(data=NULL, mapping=aes(xmin=chasein, xmax=chaseout, ymin=chasebot, ymax=chasetop),
                       size=zonethick, alpha=0, color="#FFE617")


shadowzone <-geom_rect(data=NULL, mapping=aes(xmin=shadowin, xmax=shadowout, ymin=shadowbot, ymax=shadowtop),
                       size=zonethick, alpha=0, color="#EB8B75")

strikezone <- geom_rect(data=NULL, mapping=aes(xmin=sdata$zone_x_n, xmax=zone_x_p, ymin=zonebot, ymax=zonetop), 
                        size=zonethick-.7, alpha=0, color="green")

heartzone<-geom_rect(data=NULL, mapping=aes(xmin=heartin, xmax=heartout, ymin=heartbot, ymax=hearttop),
                              size=zonethick, alpha=0, color=c("#B970B9"), fill="#DFB0DB")

plotPitchesS = ggplot(sdata, aes(sdata$plate_x, sdata$plate_z, color=factor(sdata$pitch_type)))+ggtitle("Gerrit Cole Pitch Locations")+
  geom_point(size=1.5)+scale_color_manual(values=c("#fff37f", "#f8acac","#f4b8fc", "#83A883", "#84EC90"), name="Pitch Type")+
  theme(legend.title = element_text(colour="red", size=9, face="bold"))+
  guides(colour=guide_legend(override.aes = list(size=4)))+
  xlim(c(-3, 3))+ylim(c(-0, 7))+theme_bw()+
  shadowzone+strikezone+heartzone+chasezone

plotPitchesS

plotPitchesR = ggplot(sdata, aes(sdata$release_pos_x, sdata$release_pos_z, color=factor(sdata$pitch_type)))+ggtitle("Gerrit Cole Release Points")+
  geom_point(size=1.5)+scale_color_manual(values=c("#fff37f", "#f8acac","#f4b8fc", "#83A883", "#84EC90"), name="Pitch Type")+
  theme(legend.title = element_text(colour="red", size=9, face="bold"))+
  guides(colour=guide_legend(override.aes = list(size=4)))+
  xlim(c(-3, 3))+ylim(c(-0, 7))+theme_bw()

plotPitchesR

figure<-ggarrange(plotPitchesR, plotPitchesS, common.legend = TRUE)
figure
annotate_figure(figure,
                bottom = text_grob("Hitter's POV, last 30 days", color = "#FF7400", face = "bold", size = 14))



