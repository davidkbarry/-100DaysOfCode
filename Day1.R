
#DAY1CODE

#DEFINE YESTERDAY
yesterday<- Sys.Date()-1

#GET YESTERDAY'S GAMES FROM STATCAST
yesterdaygames<-scrape_statcast_savant(start_date = yesterday, end_date = yesterday)

#PLOT CALLED STRIKES
yesterdaystrikes<- subset(yesterdaygames, description=='called_strike')
plotPITCHstrike = ggplot(yesterdaystrikes, aes(plate_x, plate_z))+
  geom_pointdensity() +
  xlim(c(-3, 3))+ylim(c(0,6))+  
  scale_color_viridis(option="A")+labs(title="Called Strikes from Yesterday", subtitle=yesterday)+
  geom_rect(data=NULL, mapping=aes(xmin=-((17/2)/12), xmax=((17/2)/12), ymin=1.5, ymax=3.5),size=2, color="tomato", alpha=0.005)
plotPITCHstrike

#PLOT CALLED BALLS
yesterdayballs<- subset(yesterdaygames, description=='ball')
plotPITCHballs = ggplot(yesterdayballs, aes(plate_x, plate_z))+
  geom_pointdensity() +
  xlim(c(-3, 3))+ylim(c(0,6))+  
  scale_color_viridis(option="D") +labs(title="Called Balls from Yesterday", subtitle=yesterday)+
  geom_rect(data=NULL, mapping=aes(xmin=-((17/2)/12), xmax=((17/2)/12), ymin=1.5, ymax=3.5),size=2, color="tomato", alpha=0.005)

plotPITCHballs


