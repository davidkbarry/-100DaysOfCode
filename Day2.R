##DAY2CODE##

#DEFINE YESTERDAY
yesterday<- Sys.Date()-1

#GET YESTERDAY'S GAMES FROM STATCAST ("YYYY-MM-DD")
yesterdaygames<-scrape_statcast_savant(start_date=(yesterday-7), end_date=yesterday)


#Determine which pitches are truly in/out of respective hitter's strike zones
yesterdaygames$iozone<-"outzone"
yesterdaygames$iozone[yesterdaygames$plate_x>((-17/2)/12) & yesterdaygames$plate_x<((17/2)/12) & yesterdaygames$plate_z>yesterdaygames$sz_bot & yesterdaygames$plate_z<yesterdaygames$sz_top]<-"inzone"


#find the misses (description=='called_strike' or 'ball')
yesterdaygames$correctcall[yesterdaygames$description=='called_strike']<-"call"
yesterdaygames$correctcall[yesterdaygames$description=='ball']<- "call"
ydagames<- subset(yesterdaygames, yesterdaygames$correctcall=="call")
ydagames$correctcall[ydagames$description=='ball' & ydagames$iozone=='inzone']<- 'incorrect'
ydagames$correctcall[ydagames$description=='called_strike' & ydagames$iozone=='outzone']<- 'incorrect'
ydagames$correctcall[ydagames$description=='called_strike' & ydagames$iozone=='inzone']<- 'correct'
ydagames$correctcall[ydagames$description=='ball' & ydagames$iozone=='outzone']<- 'correct'


#Plot the graph
plotPITCHES = ggplot(ydagames, aes(plate_x, plate_z, color=factor(correctcall)), size=16)+scale_color_manual(values=c("dodgerblue", "red"))+
  geom_point(size=1)+xlim(c(-4, 4))+ylim(c(-2, 6))+
  labs(title="Missed Calls by Inning", subtitle=("09/12/2019 to 09/19/2019"))+guides(colour = guide_legend(override.aes = list(size=3)))+
  geom_rect(data=NULL, mapping=aes(xmin=-((17/2)/12), xmax=((17/2)/12), ymin=1.6087, ymax=3.37),size=.5, color="black", alpha=0.00001)

plotPITCHES            

#Plot the graph of all home teams
plotPITCHES+facet_wrap(~ydagames$inning, nrow=3)+theme(legend.title = element_blank())


#Calculate Avg. Height of K Zone
avg_sz_bot<- mean(ydagames$sz_bot)
[1] 1.608758
avg_sz_top<- mean(ydagames$sz_top)
[1] 3.373386
##

library(ggplot2)
library(grid)
library(gridExtra)
