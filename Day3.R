##DAY3CODE##

#DEFINE YESTERDAY
yesterday<- Sys.Date()-1

#GET YESTERDAY'S GAMES FROM STATCAST ("YYYY-MM-DD")
rdata<-scrape_statcast_savant(start_date=yesterday-5, end_date=yesterday)

#Create the Zone
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

#Create Column that combines the different SDL locations into one
rdata$sdl <- paste(rdata$isheart, rdata$isshadow, rdata$ischase, rdata$iswaste)



ggplot()+geom_rect(data=NULL, mapping=aes(xmin=heartin, xmax=heartout, ymin=heartbot, ymax=hearttop),
                   size=1.5, alpha=0, color="red")

chasezone <- geom_rect(data=NULL, mapping=aes(xmin=chasein, xmax=chaseout, ymin=chasebot, ymax=chasetop),
                       size=1.5, alpha=0, color="dodgerblue4")


shadowzone <-geom_rect(data=NULL, mapping=aes(xmin=shadowin, xmax=shadowout, ymin=shadowbot, ymax=shadowtop),
                       size=1.5, alpha=0, color="firebrick4")

strikezone <- geom_rect(data=NULL, mapping=aes(xmin=rdata$zone_x_n, xmax=rdata$zone_x_p, ymin=zonebot, ymax=zonetop), 
                        size=1.5, alpha=0, color="black")

heartzone<-geom_rect(data=NULL, mapping=aes(xmin=heartin, xmax=heartout, ymin=heartbot, ymax=hearttop),
                     size=1.5, alpha=0, color="forestgreen")

plotPITCHESz = ggplot(rdata, aes(rdata$plate_x, rdata$plate_z, color=factor(rdata$sdl)), size=2)+
  scale_color_manual(values=c("dodgerblue", "firebrick1", "yellow", "green"))+guides(colour = guide_legend(override.aes = list(size=3)))+
  geom_point(size=1)+xlim(c(-4, 4))+ylim(c(-2, 6))+shadowzone+strikezone+heartzone+chasezone



plotPITCHESz
plotPITCHESz








