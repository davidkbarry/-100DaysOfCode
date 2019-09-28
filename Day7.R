##DAY7CODE##
library(baseballr)
library(ggplot2)
library(dbplyr)
library(grDevices)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)




ggspraychart(rizzo, fill_value = "bb_type", 
             fill_palette = c("#A2C8EC", "#006BA4", "#FF940E","#595959", "#C85200"))+
  ggtitle("Anthony Rizzo") +
  labs(subtitle = "spray chart this season")


 


rizzo <- scrape_statcast_savant(
  start_date = "2019-03-31",
  end_date= "2019-09-25",
  player_type = "batter",
  playerid = "519203"
)



warnings()

rizzo %>%
  select(events, hc_x, hc_y)%>%
  head()


select(rizzo_bip, events, hc_x, hc_y)


  
rizzo_bip <- rizzo %>%
  filter(type=="X")
rizzo_bip %>%
  select(pitch_type, hc_x, hc_y)%>%
  head()

spray_chart <- function(...) {
  ggplot(...)+
    geom_curve(x=33, xend=233, y=-100, yend=-100, curvature=-.65)+
    geom_segment(x=128, xend-33, y=-208, yen2=-100)+
    geom_segment(x=128, xend=223, y=-208, yend=-100)+
    geom_curve(x=83, xend=173, y=-155, yend=-156, curvature = -.65, linetype="dotted")+
    coord_fixed()+
    scale_x_continuous(NULL, limits=c(25, 225))+scale_y_continuous(NULL, limits=c(25, 225))
  
}

spray_chart(rizzo_bip, aes(x=hc_x, y=-hc_y, color="red"))

test1<- function(){
  print("hi there")
}

test1



b <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
b +
  geom_curve(x = 4, y = 14, xend = 10, yend = 30, colour = "red")+
  geom_segment(x = 10, y = 14, xend = 1, yend = 23, colour = "blue")
b + geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2), data = df, curvature = -0.2)b + geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2), data = df, curvature = 1)b + geom_curve(
  aes(x = x1, y = y1, xend = x2, yend = y2),
  data = df,
  arrow = arrow(length = unit(0.03, "npc"))
)
ggplot(seals, aes(long, lat)) +
  geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat),
               arrow = arrow(length = unit(0.1,"cm"))) +
  borders("state")
# Use lineend and linejoin to change the style of the segments
df2 <- expand.grid(
  lineend = c('round', 'butt', 'square'),
  linejoin = c('round', 'mitre', 'bevel'),
  stringsAsFactors = FALSE
)
df2 <- data.frame(df2, y = 1:9)
ggplot(df2, aes(x = 1, y = y, xend = 2, yend = y, label = paste(lineend, linejoin))) +
  geom_segment(
    lineend = df2$lineend, linejoin = df2$linejoin,
    size = 3, arrow = arrow(length = unit(0.3, "inches"))
  ) +
  geom_text(hjust = 'outside', nudge_x = -0.2) +
  xlim(0.5, 2)
# You can also use geom_segment to recreate plot(type = "h") :
counts <- as.data.frame(table(x = rpois(100,5)))
counts$x <- as.numeric(as.character(counts$x))
with(counts, plot(x, Freq, type = "h", lwd = 10))
ggplot(counts, aes(x, Freq)) +
  geom_segment(aes(xend = x, yend = 0), size = 10, lineend = "butt")











