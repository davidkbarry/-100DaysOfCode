#Day 4: Get xwOBA from Yesterday

#1: Set Yesterday
yesterday=Sys.Date()-1
remove(low)

ydascrape<- scrape_statcast_savant(start_date=yesterday-5, end_date=yesterday)

#Delete non-balls in play

ydawobacon<- subset(ydascrape, ydascrape$woba_denom==1)

#Plot: x<- Exit speed, y<- launch angle, colored by: xwOBA/wOBAcon


###wOBA value (actual outcome)
wobavalplot = ggplot(ydawobacon, aes(launch_speed,launch_angle, color=woba_value))+
  geom_point()+scale_color_gradient2(low="navyblue", high="red", space="Lab", mid="white", midpoint=1)

wobavalplot


###xwOBA (estimated wOBA using speed/angle)
wobaconplot = ggplot(ydawobacon, aes(launch_speed, launch_angle, color=ydawobacon$estimated_woba_using_speedangle))+
  geom_point()+scale_color_gradient2(low="blue", high="red", space="Lab", mid="white", midpoint=1, guide="colourbar", name="xwOBA")

wobaconplot

mindate = ydascrape[order(ydascrape$game_date),]
mindate
