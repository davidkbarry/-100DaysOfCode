##DAY8CODE##

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


#Pull the data
yesterday<- Sys.Date()-1

felix<-scrape_statcast_savant(start_date="2019-03-20", end_date=yesterday,playerid = "433587", player_type = 'pitcher')


#Create dataframe with calculated average release locations
avglocfelix<- felix %>%
  group_by(pitch_type) %>%
  summarize(N=n(),
    avg_rpx=mean(release_pos_x, na.rm = TRUE),
    avg_rpz=mean(release_pos_z, na.rm=TRUE))

avglocfelix$color<- left_join(colorsb)


  
#Make pitch types factor() [is this necessary?]
felix$fpitch_type <- factor(felix$pitch_type)
avglocfelix$fpitch_type<- factor(avglocfelix$pitch_type)


#plot them both!
ggplot()+geom_point(data=felix, aes(felix$release_pos_x, felix$release_pos_z, color=felix$fpitch_type), size=2)+scale_color_manual(values=mycol4)+
  new_scale_color()+theme_bw()+ggtitle("King Felix's Release Points by Pitch Type\
  2019 Regular Season")+theme(legend.title=element_blank())+
  geom_point(data=avglocfelix, aes(avglocfelix$avg_rpx, avglocfelix$avg_rpz, color=avglocfelix$fpitch_type), size=6, name="Pitch Type")+
  xlim(c(-3, 0))+ylim(c(4, 7))+scale_color_manual(values=mycol3)

#plot them both!
ggplot()+geom_line(data=felix, aes(felix$game_date, felix$release_pos_z), size=1)+scale_color_manual(values=mycol4)+
theme_bw()+ggtitle("King Felix's Avg Release Point\
  2019 Regular Season")+theme(legend.title=element_blank())+ylim(0,7)


ggplot(data=felix, aes(felix$release_pos_x, felix$release_pos_z, color=felix$fpitch_type), size=1)



#Some test data
dat <- data.frame(x=runif(10),y=runif(10),
                  grp = rep(LETTERS[1:5],each = 2),stringsAsFactors = TRUE)

#default pitch list
plist <-c("FF","FT","SI","FC","CH","FS","SC","FO","SL","GY","CU","KC","KN")



#Create a custom color scale


mycol4<- c("FF"="#e896a4",
           "FT"="#eeb481",
           "SI"="#fece7f",
           "FC"="#c99f95",
           "CH"="#8ede9c",
           "FS"="#9dd5d5",
           "SC"="#afed99",
           "FO"="#aae5d5",
           "SL"="#f6f38a",
           "GY"="#c9d7e9",
           "CU"="#7fe8f6",
           "KC"="#b09ae6",
           "KN"="#9da1e6")

install.packages("tinycolor")


mycol2<- c("FF"="#71af1e","FT"="#71af1e","SI"="#f5c3fd","FC"="#f5c3fd","CH"="#92db80","FS"="#92db80","SC"="#5d0148","FO"="#5d0148","SL"="#c305df","GY"="#c305df","CU"="#b152c4","KC"="#b152c4","KN"="#f755f9")



#code below from github (eliocamp) to create new scale
{


new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}

#' Convenient functions
new_scale_fill <- function() {
  new_scale("fill")
}

new_scale_color <- function() {
  new_scale("colour")
}

new_scale_colour <- function() {
  new_scale("colour")
}

#' Special behaviour of the "+" for adding a `new_aes` object
#' It changes the name of the aesthethic for the previous layers, appending
#' "_new" to them. 
ggplot_add.new_aes <- function(object, plot, object_name) {
  plot$layers <- lapply(plot$layers, bump_aes, new_aes = object)
  plot$scales$scales <- lapply(plot$scales$scales, bump_aes, new_aes = object)
  plot$labels <- bump_aes(plot$labels, new_aes = object)
  plot
}


bump_aes <- function(layer, new_aes) {
  UseMethod("bump_aes")
}

bump_aes.Scale <- function(layer, new_aes) {
  old_aes <- layer$aesthetics[remove_new(layer$aesthetics) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  layer$aesthetics[layer$aesthetics %in% old_aes] <- new_aes
  
  if (is.character(layer$guide)) {
    layer$guide <- match.fun(paste("guide_", layer$guide, sep = ""))()
  }
  layer$guide$available_aes[layer$guide$available_aes %in% old_aes] <- new_aes
  layer
}

bump_aes.Layer <- function(layer, new_aes) {
  original_aes <- new_aes
  
  old_aes <- names(layer$mapping)[remove_new(names(layer$mapping)) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  old_geom <- layer$geom
  
  old_setup <- old_geom$handle_na
  new_setup <- function(self, data, params) {
    colnames(data)[colnames(data) %in% new_aes] <- original_aes
    old_setup(data, params)
  }
  
  new_geom <- ggplot2::ggproto(paste0("New", class(old_geom)[1]), old_geom,
                               handle_na = new_setup)
  
  new_geom$default_aes <- change_name(new_geom$default_aes, old_aes, new_aes)
  new_geom$non_missing_aes <- change_name(new_geom$non_missing_aes, old_aes, new_aes)
  new_geom$required_aes <- change_name(new_geom$required_aes, old_aes, new_aes)
  new_geom$optional_aes <- change_name(new_geom$optional_aes, old_aes, new_aes)
  
  layer$geom <- new_geom
  
  old_stat <- layer$stat
  
  old_setup2 <- old_stat$handle_na
  new_setup <- function(self, data, params) {
    colnames(data)[colnames(data) %in% new_aes] <- original_aes
    old_setup2(data, params)
  }
  
  new_stat <- ggplot2::ggproto(paste0("New", class(old_stat)[1]), old_stat,
                               handle_na = new_setup)
  
  new_stat$default_aes <- change_name(new_stat$default_aes, old_aes, new_aes)
  new_stat$non_missing_aes <- change_name(new_stat$non_missing_aes, old_aes, new_aes)
  new_stat$required_aes <- change_name(new_stat$required_aes, old_aes, new_aes)
  new_stat$optional_aes <- change_name(new_stat$optional_aes, old_aes, new_aes)
  
  layer$stat <- new_stat
  
  layer$mapping <- change_name(layer$mapping, old_aes, new_aes)
  layer
}

bump_aes.list <- function(layer, new_aes) {
  old_aes <-  names(layer)[remove_new(names(layer)) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  names(layer)[names(layer) %in% old_aes] <- new_aes
  layer
}

change_name <- function(list, old, new) {
  UseMethod("change_name")
}

change_name.character <- function(list, old, new) {
  list[list %in% old] <- new
  list
}

change_name.default <- function(list, old, new) {
  nam <- names(list)
  nam[nam %in% old] <- new
  names(list) <- nam
  list
}

change_name.NULL <- function(list, old, new) {
  NULL
}

remove_new <- function(aes) {
  stringi::stri_replace_all(aes, "", regex = "(_new)*")
}
}
