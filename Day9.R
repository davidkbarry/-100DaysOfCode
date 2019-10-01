#Day 9

#Libraries Loaded:
library(baseballr)
library(ggplot2)
library(dbplyr)
library(grDevices)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)

library(RColorBrewer)
library(dplyr)

library(data.table)

playoffs<- rbind(scrape_statcast_savant(start_date= "2017-10-03",end_date= "2017-12-01"),
                 scrape_statcast_savant(start_date= "2016-10-04",end_date= "2016-12-01"),
                 scrape_statcast_savant(start_date= "2015-10-06",end_date= "2015-12-02"),
                 scrape_statcast_savant(start_date= "2014-09-30",end_date= "2014-12-02"),
                 scrape_statcast_savant(start_date= "2013-10-01",end_date= "2013-12-02"),
                 scrape_statcast_savant(start_date= "2012-10-05",end_date= "2012-12-02"),
                 scrape_statcast_savant(start_date= "2011-09-30",end_date= "2011-12-03"),
                 scrape_statcast_savant(start_date= "2010-10-06",end_date= "2010-12-03")
)

playoffs13<- playoffs12

##PLAYOFFS SINCE 2013


playoffsFF <- subset(playoffs13, playoffs13$pitch_type=="FF")


playoffgroup<- playoffsFF %>%
  group_by(pitcher, game_date) %>%
  summarize(num=n(),
    avg_vel=mean(release_speed, na.rm = TRUE))

playoffsFF$outing <- paste(playoffsFF$pitcher, playoffsFF$game_date, " ")

playoffsFF %>%
  group_by(outing) %>%
  mutate(Count=row_number())


library(data.table)

DT <- data.table(playoffsFF)
DT[, id := seq_len(.N), by = outing]
DT[, id := rowid(outing)]

playoffsFF<-DT


ggplot()+geom_line(DT, mapping=aes(DT$id, DT$release_speed, color=factor(DT$outing)))+
  theme(legend.position = "none")+
  labs(x="Pitch Count", y="Fastball Velo")+
  labs(title = "Postseason Fastball Velocity",
       subtitle = "Since 2010",
       caption = "Data source: Statcast and Pitch F/X")








