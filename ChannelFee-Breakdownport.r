ChannelFeeTypeBreakdown <- function(course,facility = " ",display_data,direc,year){

##FOR SCRIPT TESTING PURPOSES 
course="1757 Golf Club";facility = " ";display_data=1;year="2019"
## FOR TESTING ^^^^^^^^
  
## not needed? ## setwd("W:/Analytics/Packages/Channel Fee Type Breakdown")  
## For when it's complete-BELOW## 
#setwd("/srv/shiny-server/Analytics/Packages/Channel Fee Type Breakdown")
setwd("/srv/shiny-server/BetaGerhard/")

###Testing script
setwd("w:/BetaGerhard/")  
## Testing ^^^^
  
library(ggplot2)
library(ggiraph)
library(tidyr)
library(DT)
library(scales)
library(grid)
library(dplyr)
library(xlsx)
library(htmlwidgets)

#### FILTER DATA BY YEAR ###### no need to break the years up - jk only select by each year
SQLData <- read.csv(paste0("BreakDownAllCourses",year,".csv"),stringsAsFactors = FALSE)

#### Must break Years into 2 Files and the combine into 1 Dataframe
#Part 1 is from 1/1 to 3/31
#SQLData1 <- read.csv(paste0("BreakDownAllCourses",year,"Part1.csv"),stringsAsFactors = FALSE)
#Part 2 is from 4/1 to 6/31
#SQLData2 <- read.csv(paste0("BreakDownAllCourses",year,"Part2.csv"),stringsAsFactors = FALSE)
#Part 3 is from 7/1 to 9/31
#SQLData3 <- read.csv(paste0("BreakDownAllCourses",year,"Part3.csv"),stringsAsFactors = FALSE)
#Part 4 is from 10/1 to 12/31
#SQLData4 <- read.csv(paste0("BreakDownAllCourses",year,"Part4.csv"),stringsAsFactors = FALSE)

#SQLData <- rbind(SQLData1, SQLData2, SQLData3, SQLData4)

colnames(SQLData)[1] <- "Course"

##SQLData <- read.csv('BreakDownAllCourses2018.csv')

## Remove the quotation marks from course & facility name #
SQLData$Course <- gsub("\"", "", SQLData$Course)
SQLData$Facility <- gsub("\"", "", SQLData$Facility)

#### FILTER BY COURSE ####

SQLData = SQLData[which(SQLData$Course %in% course),]


##would have to change SQLData$Course to SQLData$Facility to get everything to work
if(facility != " "){
  SQLData = SQLData[which(SQLData$Facility == facility),]
}
#else SQLData$Facility = NA

#### IF SQLData SELECTED COURSE HAS ZERO rows - then put up warning message ####
if(nrow(SQLData) < 1){
    nullplot = ggplot(SQLData) + geom_blank()
    return(list(nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot, nullplot, nullplot, nullplot,"NOT ENOUGH DATA"))
  }


##Make Rounds column into factor
#SQLData$Rounds = as.factor(SQLData$Rounds)
##Make TeeTimeHM into time
SQLData$TeeTimeHM = as.POSIXct(SQLData$TeeTimeHM,format="%H:%M:%S")
##Make Total.Fees numeric
SQLData$Total.Fees <- as.numeric(SQLData$Total.Fees)
##Make HoursBookedBeforeTT integer
SQLData$HoursBookedBeforeTT <- as.numeric(SQLData$HoursBookedBeforeTT, na.rm=TRUE)

## OR SQLData <- read.csv('TestChannelFeeDateTime.csv', stringsAsFactors = FALSE)
#### Total Sort by Booking Channel ####
shop.rounds.vec <- (SQLData[SQLData$Channel=="Shop",])
res.center.rounds.vec <- (SQLData[SQLData$Channel=="Res Center",])
TPY.rounds.vec <- (SQLData[SQLData$Channel=="TPY",])
web.rounds.vec <- (SQLData[SQLData$Channel=="Website",])

#### Weekday Sort by Booking Channel #####
weekday.rounds.vec <- (SQLData[SQLData$DayType=="Weekday",])
weekday.shop.rounds.vec <- (weekday.rounds.vec[weekday.rounds.vec$Channel=="Shop",])
weekday.res.center.rounds.vec <- (weekday.rounds.vec[weekday.rounds.vec$Channel=="Res Center",])
weekday.TPY.rounds.vec <- (weekday.rounds.vec[weekday.rounds.vec$Channel=="TPY",])
weekday.web.rounds.vec <- (weekday.rounds.vec[weekday.rounds.vec$Channel=="Website",])

#### Weekend Sort by Booking Channel ########
weekend.rounds.vec <- (SQLData[SQLData$DayType=="Weekend",])
weekend.shop.rounds.vec <- (weekend.rounds.vec[weekend.rounds.vec$Channel=="Shop",])
weekend.res.center.rounds.vec <- (weekend.rounds.vec[weekend.rounds.vec$Channel=="Res Center",])
weekend.TPY.rounds.vec <- (weekend.rounds.vec[weekend.rounds.vec$Channel=="TPY",])
weekend.web.rounds.vec <- (weekend.rounds.vec[weekend.rounds.vec$Channel=="Website",])

##### IF NOT ENOUGH DATA - STOP #####
# if(nrow(c(SQLData, shop.rounds.vec, res.center.rounds.vec, TPY.rounds.vec, website.rounds.vec)) == 0){
#   nullplot = plotly_empty()
#   return(list(nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot, nullplot, nullplot, nullplot,"NOT ENOUGH DATA"))
# }
# 
# vectorcheck <- c(shop.rounds.vec, res.center.rounds.vec, TPY.rounds.vec, website.rounds.vec)
# 
# for(i in vectorcheck){
#   if(nrow(c(SQLData, shop.rounds.vec, res.center.rounds.vec, TPY.rounds.vec, website.rounds.vec)) == 0){
#     nullplot = plotly_empty()
#     return(list(nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot, nullplot, nullplot, nullplot,"NOT ENOUGH DATA"))
#   }
# }
#### Empty nrow Vec fill in for each Channel ####

EmptyDFs<- data.frame(Course = rep(as.character(course),1),
                     Channel = rep(as.character("Shop"),1),
                     Facility = rep(as.character(facility),1),
                     FeeType = rep(as.character("Comp"),1),
                     Year = rep(as.integer(year),1),
                     Month = rep(as.integer(1),1),
                     MonthName = rep(as.character("January"),1),
                     DateTeeTime = rep(NA,1),
                     TeeTimeHM = rep(NA,1),
                     TTHour = rep(as.integer(12),1),
                     HoursBookedBeforeTT = rep(as.integer(0),1),
                     DayType = rep(as.character("Weekday"),1),
                     Rounds = rep(as.numeric(0),1),
                     Total.Fees = rep(as.numeric(0),1),
                     stringsAsFactors = FALSE
                     )

EmptyDFr<- data.frame(Course = rep(as.character(course),1),
                     Channel = rep(as.character("Res Center"),1),
                     Facility = rep(as.character(facility),1),
                     FeeType = rep(as.character("Comp"),1),
                     Year = rep(as.integer(year),1),
                     Month = rep(as.integer(1),1),
                     MonthName = rep(as.character("January"),1),
                     DateTeeTime = rep(NA,1),
                     TeeTimeHM = rep(NA,1),
                     TTHour = rep(as.integer(12),1),
                     HoursBookedBeforeTT = rep(as.integer(0),1),
                     DayType = rep(as.character("Weekday"),1),
                     Rounds = rep(as.numeric(0),1),
                     Total.Fees = rep(as.numeric(0),1),
                     stringsAsFactors = FALSE
                     )

EmptyDFt<- data.frame(Course = rep(as.character(course),1),
                      Channel = rep(as.character("TPY"),1),
                      Facility = rep(as.character(facility),1),
                      FeeType = rep(as.character("Comp"),1),
                      Year = rep(as.integer(year),1),
                      Month = rep(as.integer(1),1),
                      MonthName = rep(as.character("January"),1),
                      DateTeeTime = rep(NA,1),
                      TeeTimeHM = rep(NA,1),
                      TTHour = rep(as.integer(12),1),
                      HoursBookedBeforeTT = rep(as.integer(0),1),
                      DayType = rep(as.character("Weekday"),1),
                      Rounds = rep(as.numeric(0),1),
                      Total.Fees = rep(as.numeric(0),1),
                      stringsAsFactors = FALSE
                      )

EmptyDFw<- data.frame(Course = rep(as.character(course),1),
                      Channel = rep(as.character("Website"),1),
                      Facility = rep(as.character(facility),1),
                      FeeType = rep(as.character("Comp"),1),
                      Year = rep(as.integer(year),1),
                      Month = rep(as.integer(1),1),
                      MonthName = rep(as.character("January"),1),
                      DateTeeTime = rep(NA,1),
                      TeeTimeHM = rep(NA,1),
                      TTHour = rep(as.integer(12),1),
                      HoursBookedBeforeTT = rep(as.integer(0),1),
                      DayType = rep(as.character("Weekday"),1),
                      Rounds = rep(as.numeric(0),1),
                      Total.Fees = rep(as.numeric(0),1),
                      stringsAsFactors = FALSE
                      )
  
## Need to include Facility as part of file download name, but only when necessary 


############Aggregate by Rounds & Revenue (Sum) TOTAL ################

if(nrow(shop.rounds.vec) > 0){
  shop.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,shop.rounds.vec,FUN=sum)
}else{
  shop.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFs,FUN=sum)
}


if(nrow(TPY.rounds.vec) > 0){
  TPY.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,TPY.rounds.vec,FUN=sum)
}else{
  TPY.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFt,FUN=sum)
}


if(nrow(web.rounds.vec) > 0){
  web.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,web.rounds.vec,FUN=sum)
}else{
  web.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFw,FUN=sum)
}



if(nrow(res.center.rounds.vec) > 0){
  res.center.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,res.center.rounds.vec,FUN=sum)
}else{
  res.center.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFr,FUN=sum)
}


##### Aggregate by Rounds & Revenue (Sum) WEEKDAY ######    Aggregate by Rounds & Revenue (Sum) WEEKDAY ######
if(nrow(weekday.shop.rounds.vec) > 0){
  weekday.shop.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,weekday.shop.rounds.vec,FUN=sum)
}else{
  weekday.shop.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFs,FUN=sum)
}


if(nrow(weekday.TPY.rounds.vec) > 0){
  weekday.TPY.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,weekday.TPY.rounds.vec,FUN=sum)
}else{
  weekday.TPY.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFt,FUN=sum)
}


if(nrow(weekday.web.rounds.vec) > 0){
  weekday.web.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,weekday.web.rounds.vec,FUN=sum)
}else{
  weekday.web.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFw,FUN=sum)
}


if(nrow(weekday.res.center.rounds.vec) > 0){
  weekday.res.center.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,weekday.res.center.rounds.vec,FUN=sum)
}else{
  weekday.res.center.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFr,FUN=sum)
}


##### Aggregate by Rounds & Revenue (Sum) WEEKEND ######    Aggregate by Rounds & Revenue (Sum) WEEKEND ######
if(nrow(weekend.shop.rounds.vec) > 0){
  weekend.shop.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,weekend.shop.rounds.vec,FUN=sum)
}else{
  weekend.shop.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFs,FUN=sum)
}


if(nrow(weekend.TPY.rounds.vec) > 0){
  weekend.TPY.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,weekend.TPY.rounds.vec,FUN=sum)
}else{
  weekend.TPY.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFt,FUN=sum)
}


if(nrow(weekend.web.rounds.vec) > 0){
  weekend.web.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,weekend.web.rounds.vec,FUN=sum)
}else{
  weekend.web.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFw,FUN=sum)
}


if(nrow(weekend.res.center.rounds.vec) > 0){
  weekend.res.center.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,weekend.res.center.rounds.vec,FUN=sum)
}else{
  weekend.res.center.rounds.FeeTypeSum.vec = aggregate(cbind(Rounds, Total.Fees)~Course+Channel+Year+MonthName+FeeType,EmptyDFr,FUN=sum)
}




##### TOTAL graph Dataframe #############
totaldf <- rbind.data.frame(shop.rounds.FeeTypeSum.vec, 
                            res.center.rounds.FeeTypeSum.vec, 
                            TPY.rounds.FeeTypeSum.vec,
                            web.rounds.FeeTypeSum.vec)

#### WEEKDAY graph Dataframe ########## - graphs not included 
weekdaydf <- rbind.data.frame(weekday.shop.rounds.FeeTypeSum.vec, 
                              weekday.res.center.rounds.FeeTypeSum.vec, 
                              weekday.TPY.rounds.FeeTypeSum.vec,
                              weekday.web.rounds.FeeTypeSum.vec)

##### WEEKEND graph dataframe ####### - graphs not included
weekenddf <- rbind.data.frame(weekend.shop.rounds.FeeTypeSum.vec, 
                              weekend.res.center.rounds.FeeTypeSum.vec, 
                              weekend.TPY.rounds.FeeTypeSum.vec,
                              weekend.web.rounds.FeeTypeSum.vec)


################ ######################################
###### INTERACTIVE graphs below ###########

#Create tooltip css for mouse hover info (ggiraph)
tooltip_css <- "background-color:gray;color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;font-family:Trebuchet MS"
#create the tooltip #
totaldf$tooltiprounds = paste0("Units Sold: ", formatC(totaldf$Rounds, format="d", big.mark=","), "<br/>",
                               "Fee Type: ", totaldf$FeeType)

#orders the months chronologically
totaldf$MonthName <- factor(totaldf$MonthName, levels = month.name)

##### Logo Entered As Background Image - must add to ggplot before the geom_lines #####
# require("pacman")
# library(pacman)
# require("grid")
# library(grid)
# require("png")
# library(png)
# library(magick)

image3 <- jpeg::readJPEG("numbers2.jpg")

imagex <- rasterGrob(image3, width = unit(1, "npc"), height = unit(1, "npc"))

###### START OF BLOCK GGIRAPH PLOTS ######
####### INTERACTIVE TOTAL Rounds graph ########

jrototal2 <- ggplot(totaldf, aes(x=MonthName, y=Rounds, group=FeeType, color=FeeType)) +
  annotation_custom(imagex) +
  geom_line() +
  geom_point_interactive(aes(tooltip=tooltiprounds, data_id=Rounds)) +
  facet_wrap(~Channel)+
  scale_y_continuous(labels=scales::comma, limits = c(0,NA))+
  #ylim(0,NA) +
  theme(text=element_text(family="Trebuchet MS", face="bold", size=10))


krototal2 <- jrototal2 + theme(legend.position="bottom",legend.title=element_blank(),
                               plot.title = element_text(hjust = 0.5, size=12), axis.text.x = element_text(angle=90, vjust=.5, size=10), axis.text.y=element_text(size=10)) +
  ylab("Units Sold") + xlab("Month") + ggtitle(paste("Client:", totaldf$Year, "Units Sold by Channel/Fee Type"))

#ggiraph(code = print(krototal2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.total.rounds.graph <- ggiraph(code = print(krototal2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:2pt;")

htmlwidgets::saveWidget(CFTD.total.rounds.graph, "cftdunits_lineplot2.html", selfcontained = TRUE, libdir = NULL)

##### INTERACTIVE TOTAL REVENUE graph #########

#library(scales)


#create the tooltip # #tooltips need to have formatC in order to include commas
totaldf$tooltiprevenue = paste0("Revenue: $", formatC(totaldf$Total.Fees, format="d", big.mark=","), "<br/>",
                                "Fee Type: ", totaldf$FeeType)

totaldf$MonthName <- factor(totaldf$MonthName, levels = month.name)

jrevtotal2 <- ggplot(totaldf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType)) +
  annotation_custom(imagex) +
  geom_line() +
  geom_point_interactive(aes(tooltip=tooltiprevenue, data_id=Rounds)) +
  facet_wrap(~Channel) + scale_y_continuous(labels=scales::dollar, limits = c(0,NA)) + theme(text=element_text(family="Trebuchet MS", face="bold",size=10))  #theme_classic() ##+ scale_y_continuous(label=dollar_format())


krevtotal2 <- jrevtotal2 + theme(legend.position="bottom",legend.title=element_blank(),
                                 plot.title = element_text(hjust = 0.5, size=12), axis.text.x = element_text(angle=90, vjust=.5, size=10), axis.text.y=element_text(size=10)) +
  ylab("Revenue") + xlab("Month") + ggtitle(paste("Client:", totaldf$Year, " Revenue by Channel/Fee Type"))

#ggiraph(code = print(krevtotal2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.total.revenue.graph <- ggiraph(code = print(krevtotal2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:2pt;")

htmlwidgets::saveWidget(CFTD.total.revenue.graph, "cftdrevenue_lineplot2.html", selfcontained = TRUE, libdir = NULL)


####### INTERACTIVE WEEKDAY ROUNDS graph ########

#create the tooltip #
weekdaydf$tooltiprounds = paste0("Rounds: ", formatC(weekdaydf$Rounds, format="d", big.mark=","), "<br/>",
                                 "Fee Type: ", weekdaydf$FeeType)

# ##arrange months chronologically #
weekdaydf$MonthName <- factor(weekdaydf$MonthName, levels = month.name)

jroweekday2 <- ggplot(weekdaydf, aes(x=MonthName, y=Rounds, group=FeeType, color=FeeType)) +
  annotation_custom(imagex) +
  geom_line() +
  geom_point_interactive(aes(tooltip=tooltiprounds, data_id=Rounds)) +
  facet_wrap(~Channel)+
  scale_y_continuous(labels=scales::comma, limits = c(0,NA))+
  #ylim(0,NA) + theme_classic()
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10))



kroweekday2 <- jroweekday2 + theme(legend.position="bottom",legend.title=element_blank(),
                                   plot.title = element_text(hjust = 0.5, size=12), axis.text.x = element_text(angle=90, vjust=.5, size=10), axis.text.y=element_text(size=10)) +
  ylab("Rounds") + xlab("Month") + ggtitle(paste(weekdaydf$Course, ":", weekdaydf$Year, " Weekday Rounds by Channel/Fee Type"))

#ggiraph(code = print(kroweekday2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.weekday.rounds.graph <- ggiraph(code = print(kroweekday2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:1pt;")

##### INTERACTIVE WEEKDAY REVENUE graph ##########

#create the tooltip #
weekdaydf$tooltiprevenue = paste0("Revenue: $", formatC(weekdaydf$Total.Fees, format="d", big.mark=","), "<br/>",
                                  "Fee Type: ", weekdaydf$FeeType)
##arrange months chronologically #
weekdaydf$MonthName <- factor(weekdaydf$MonthName, levels = month.name)

jrevweekday2 <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType)) +
  annotation_custom(imagex) +
  geom_line() +
  geom_point_interactive(aes(tooltip=tooltiprevenue, data_id=Total.Fees)) +
  facet_wrap(~Channel)+
  scale_y_continuous(labels=scales::dollar, limits = c(0,NA))+
  #ylim(0,NA) + theme_classic()
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10))



krevweekday2 <- jrevweekday2 + theme(legend.position="bottom",legend.title=element_blank(),
                                     plot.title = element_text(hjust = 0.5, size=12), axis.text.x = element_text(angle=90, vjust=.5, size=10), axis.text.y=element_text(size=10)) +
  ylab("Revenue") + xlab("Month") + ggtitle(paste(weekdaydf$Course, ":", weekdaydf$Year, "Weekday Revenue by Channel/Fee Type"))

#ggiraph(code = print(krevweekday2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.weekday.revenue.graph <- ggiraph(code = print(krevweekday2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:1pt;")



####### INTERACTIVE WEEKEND ROUNDS graph ########

#create the tooltip #
weekenddf$tooltiprounds = paste0("Rounds: ", formatC(weekenddf$Rounds, format="d", big.mark=","), "<br/>",
                                 "Fee Type: ", weekenddf$FeeType)

##arrange months chronologically #
weekenddf$MonthName <- factor(weekenddf$MonthName, levels = month.name)

jroweekend2 <- ggplot(weekenddf, aes(x=MonthName, y=Rounds, group=FeeType, color=FeeType)) +
  annotation_custom(imagex) +
  geom_line() +
  geom_point_interactive(aes(tooltip=tooltiprounds, data_id=Rounds)) +
  facet_wrap(~Channel)+
  scale_y_continuous(labels=scales::comma, limits = c(0,NA))+
  #ylim(0,NA) + theme_classic()
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10))



kroweekend2 <- jroweekend2 + theme(legend.position="bottom",legend.title=element_blank(),
                                   plot.title = element_text(hjust = 0.5, size=12), axis.text.x = element_text(angle=90, vjust=.5, size=10), axis.text.y=element_text(size=10)) +
  ylab("Rounds") + xlab("Month") + ggtitle(paste(weekenddf$Course, ":", weekenddf$Year, "Weekend Rounds by Channel/Fee Type"))

#ggiraph(code = print(kroweekend2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.weekend.rounds.graph <- ggiraph(code = print(kroweekend2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:1pt;")


#### INTERACTIVE WEEKEND REVENUE graph ####

#create the tooltip #
weekenddf$tooltiprevenue = paste0("Revenue: $", formatC(weekenddf$Total.Fees, format="d", big.mark=","), "<br/>",
                                  "Fee Type: ", weekenddf$FeeType)

##arrange months chronologically #
weekenddf$MonthName <- factor(weekenddf$MonthName, levels = month.name)

jrevweekend2 <- ggplot(weekenddf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType)) +
  annotation_custom(imagex) +
  geom_line() +
  geom_point_interactive(aes(tooltip=tooltiprevenue, data_id=Total.Fees)) +
  facet_wrap(~Channel)+
  scale_y_continuous(labels=scales::dollar, limits = c(0,NA))+
  #ylim(0,NA) + theme_classic()
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10))



krevweekend2 <- jrevweekend2 + theme(legend.position="bottom",legend.title=element_blank(),
                                     plot.title = element_text(hjust = 0.5, size=12), axis.text.x = element_text(angle=90, vjust=.5, size=10), axis.text.y=element_text(size=10)) +
  ylab("Revenue") + xlab("Month") + ggtitle(paste(weekenddf$Course, ":", weekenddf$Year, "Weekend Revenue by Channel/Fee Type"))

#ggiraph(code = print(krevweekend2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.weekend.revenue.graph <- ggiraph(code = print(krevweekend2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:1pt;")

##### END OF STOP GGIRAPH GRAPHS ^^^ #######

##### ADD TABLE DISPLAY for 'testdf' object ############
##### COLORS for Channel Column in TABLES####
#Channel Colors
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# n = length(unique(totrndtable$Channel))
# colschann = gg_color_hue(n)
# 
# names(colschann) = unique(totrndtable$Channel)
# 
# #FeeType Colors
# gg_color_hue <- function(n) {
#   hues = seq(10, 575, length = n + 1)
#   hcl(h = hues, l = 95, c = 100)[1:n]
# }
# n = length(unique(totrndtable$FeeType))
# colsfee = gg_color_hue(n)
# 
# names(colsfee) = unique(totrndtable$FeeType)


##### DATA TABLES BEGIN #####
##### TOTAL TABLES ####
totrndtable = spread(totaldf[,-which(colnames(totaldf) %in% c("Course", "Year", "Total.Fees", "tooltiprounds", "tooltiprevenue"))], MonthName, Rounds)
totrevtable = spread(totaldf[,-which(colnames(totaldf) %in% c("Course","Year", "Rounds", "tooltiprounds", "tooltiprevenue"))], MonthName, Total.Fees)

## fill in blanks with 0
totrndtable[which(is.na(totrndtable),arr.ind = TRUE)] <- 0
totrevtable[which(is.na(totrevtable),arr.ind = TRUE)] <- 0

#### Add TOTAL column and row for Total Rnd & Rev ####
totrndtable <- totrndtable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
totrndtable <- cbind(totrndtable, Total = rowSums(totrndtable[,3:ncol(totrndtable)]))
#totrndtable <- totrndtable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))

totrevtable <- totrevtable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
totrevtable <- cbind(totrevtable, Total = rowSums(totrevtable[,3:ncol(totrevtable)]))
#totrndtable <- totrndtable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))


##### COLORS for Channel Column in TABLES####
#Channel Colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = length(unique(totrndtable$Channel))
colschann = gg_color_hue(n)

names(colschann) = unique(totrndtable$Channel)

#FeeType Colors
gg_color_hue <- function(n) {
  hues = seq(10, 575, length = n + 1)
  hcl(h = hues, l = 95, c = 100)[1:n]
}
n = length(unique(totrndtable$FeeType))
colsfee = gg_color_hue(n)

names(colsfee) = unique(totrndtable$FeeType)

# Create DT Table
totrndtableDT <- datatable(totrndtable, caption = htmltools::tags$caption(paste('Course:',totaldf$Course, totaldf$Year,'-','Facility:',SQLData$Facility,'Table: Total Rounds')), filter = 'top',
                           extensions = 'Buttons', options = list(
                             dom = 'Bfrtip', 
                             buttons = list('copy',list(extend='excel',filename=paste(course,year,'Table: Total Rounds',Sys.Date()))), pageLength=nrow(totrndtable)
                           )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(totrndtable$Channel), colschann)
              ,fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(totrndtable$FeeType), colsfee)
              ,fontWeight = 'bold') 


totrevtableDT <- datatable(totrevtable, caption = htmltools::tags$caption(paste(totaldf$Course, totaldf$Year,'-', 'Facility:', SQLData$Facility, 'Table: Total Revenue')), filter = 'top',
                           extensions = 'Buttons', options = list(
                             dom = 'Bfrtip',
                             buttons = list('copy', list(extend='excel', filename=paste(course,year,'Table: Total Revenue',Sys.Date()))), pageLength=nrow(totrndtable)
                           )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(totrevtable$Channel), colschann)
              ,fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(totrevtable$FeeType), colsfee)
              ,fontWeight = 'bold') %>%
  formatCurrency(3:15)

#### create html widgets for tables ####
htmlwidgets::saveWidget(totrndtableDT, "cftdunits_table2.html", selfcontained = TRUE, libdir = NULL)
htmlwidgets::saveWidget(totrevtableDT, "cftdrevenue_table2.html", selfcontained = TRUE, libdir = NULL)



##### WEEKDAY TABLES #### - not displayed on website
wkdyrndtable = spread(weekdaydf[,-which(colnames(weekdaydf) %in% c("Course", "Year", "Total.Fees", "tooltiprounds", "tooltiprevenue"))], MonthName, Rounds)
wkdyrevtable = spread(weekdaydf[,-which(colnames(weekdaydf) %in% c("Course","Year", "Rounds", "tooltiprounds", "tooltiprevenue"))], MonthName, Total.Fees)

wkdyrndtable[which(is.na(wkdyrndtable),arr.ind = TRUE)] <- 0
wkdyrevtable[which(is.na(wkdyrevtable),arr.ind = TRUE)] <- 0

#### Add TOTAL column and row for WEEKDAY Rnd & Rev ####
wkdyrndtable <- wkdyrndtable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
wkdyrndtable <- cbind(wkdyrndtable, Total = rowSums(wkdyrndtable[,3:ncol(wkdyrndtable)]))
#wkdyrndtable <- wkdyrndtable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))

wkdyrevtable <- wkdyrevtable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
wkdyrevtable <- cbind(wkdyrevtable, Total = rowSums(wkdyrevtable[,3:ncol(wkdyrevtable)]))
#totrndtable <- totrndtable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))


##### COLORS for Channel Column in TABLES####
#Channel Colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = length(unique(wkdyrndtable$Channel))
colschann = gg_color_hue(n)

names(colschann) = unique(wkdyrndtable$Channel)

#FeeType Colors
gg_color_hue <- function(n) {
  hues = seq(5, 375, length = n + 1)
  hcl(h = hues, l = 95, c = 100)[1:n]
}
n = length(unique(wkdyrndtable$FeeType))
colsfee = gg_color_hue(n)

names(colsfee) = unique(wkdyrndtable$FeeType)

# Create DT Table for Weekday Rounds
wkdyrndtableDT <- datatable(wkdyrndtable, caption = htmltools::tags$caption(paste(totaldf$Course, totaldf$Year,'-', 'Facility:', SQLData$Facility, 'Table: Weekday Rounds')), filter = 'top',
                            extensions = 'Buttons', options = list(
                              dom = 'Bfrtip',
                              buttons = list('copy',list(extend='excel',filename=paste(course,year,'Table: Weekday Rounds',Sys.Date()))), pageLength=nrow(wkdyrndtable)
                            )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(wkdyrndtable$Channel), 
                                                      colschann),fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(wkdyrndtable$FeeType), 
                                                      colsfee),fontWeight = 'bold')

# Create DT Table for Weekday Revenue
wkdyrevtableDT <- datatable(wkdyrevtable, caption = htmltools::tags$caption(paste(totaldf$Course, totaldf$Year,'-', 'Facility:', SQLData$Facility, 'Table: Weekday Revenue')), filter = 'top',
                            extensions = 'Buttons', options = list(
                              dom = 'Bfrtip',
                              buttons = list('copy',list(extend='excel',filename=paste(course,year,'Table: Weekday Revenue',Sys.Date()))), pageLength=nrow(wkdyrevtable)
                            )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(wkdyrevtable$Channel), colschann)
              ,fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(wkdyrevtable$FeeType), colsfee)
              ,fontWeight = 'bold') %>%
  formatCurrency(3:15)

#### WEEKEND TABLES BEGIN #####
##### WEEKEND TABLES ####
wkndrndtable = spread(weekenddf[,-which(colnames(weekenddf) %in% c("Course", "Year", "Total.Fees", "tooltiprounds", "tooltiprevenue"))], MonthName, Rounds)
wkndrevtable = spread(weekenddf[,-which(colnames(weekenddf) %in% c("Course","Year", "Rounds", "tooltiprounds", "tooltiprevenue"))], MonthName, Total.Fees)

wkndrndtable[which(is.na(wkndrndtable),arr.ind = TRUE)] <- 0
wkndrevtable[which(is.na(wkndrevtable),arr.ind = TRUE)] <- 0

#### Add TOTAL column and row for WEEKEND Rnd & Rev ####
wkndrndtable <- wkndrndtable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
wkndrndtable <- cbind(wkndrndtable, Total = rowSums(wkndrndtable[,3:ncol(wkndrndtable)]))
#wkdyrndtable <- wkdyrndtable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))

wkndrevtable <- wkndrevtable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
wkndrevtable <- cbind(wkndrevtable, Total = rowSums(wkndrevtable[,3:ncol(wkndrevtable)]))
#totrndtable <- totrndtable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))



##### COLORS for Channel Column in TABLES WEEKEND####
#Channel Colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = length(unique(wkndrndtable$Channel))
colschann = gg_color_hue(n)

names(colschann) = unique(wkndrndtable$Channel)

#FeeType Colors
gg_color_hue <- function(n) {
  hues = seq(5, 375, length = n + 1)
  hcl(h = hues, l = 95, c = 100)[1:n]
}
n = length(unique(wkndrndtable$FeeType))
colsfee = gg_color_hue(n)

names(colsfee) = unique(wkndrndtable$FeeType)


# Create DT Table for Weekend Rounds
wkndrndtableDT <- datatable(wkndrndtable, caption = htmltools::tags$caption(paste(totaldf$Course, totaldf$Year,'-', 'Facility:', SQLData$Facility, 'Table: Weekend Rounds')), filter = 'top',
                            extensions = 'Buttons', options = list(
                              dom = 'Bfrtip',
                              buttons = list('copy',list(extend='excel',filename=paste(course,year,'Table: Weekend Rounds',Sys.Date()))), pageLength=nrow(wkndrndtable)
                            )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(wkndrndtable$Channel), 
                                                      colschann),fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(wkndrndtable$FeeType), 
                                                      colsfee),fontWeight = 'bold')

# Create DT Table for Weekend Revenue
wkndrevtableDT <- datatable(wkndrevtable, caption = htmltools::tags$caption(paste(totaldf$Course, totaldf$Year,'-', 'Facility:', SQLData$Facility, 'Table: Weekend Revenue')), filter = 'top',
                            extensions = 'Buttons', options = list(
                              dom = 'Bfrtip',
                              buttons = list('copy',list(extend='excel',filename=paste(course,year,'Table: Weekend Revenue',Sys.Date()))), pageLength=nrow(wkndrevtable)
                            )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(wkndrevtable$Channel), colschann)
              ,fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(wkndrevtable$FeeType), colsfee)
              ,fontWeight = 'bold') %>%
  formatCurrency(3:15)


#### ENTER VALUES FOR GGIRAPH GRAPHS ####
# CFTD.total.rounds.graph <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))
# CFTD.total.revenue.graph <-ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))
# CFTD.weekday.rounds.graph <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))
# CFTD.weekday.revenue.graph <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))
# CFTD.weekend.rounds.graph <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))
# CFTD.weekend.revenue.graph <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))

################################################# vv BLOCK OUT Below UNTIL FINISHED vv #################################################
#### FULL WORKBOOK DOWNLOAD ####
## change working directory to save image files ##
setwd("/srv/wd/Comp Assessment/Downloads/")
#setwd("w:/Comp Assessment/Downloads/")

####CREATE WB ####
#BkAdvWB <- XLConnect::loadWorkbook(paste0("/srv/wd/Comp Assessment/Downloads/",direc,"BookedAdvance.xlsx"), create=TRUE)
CFTBWB <- createWorkbook(type="xlsx")

#### Create png image immediatly before dev.off ####

png(filename = "totalcftb.rnd.png", width=900, height=600)
print(krototal2) #total hours before tee time count graph
dev.off()

png(filename = "totalcftb.rev.png", width=900, height=600)
print(krevtotal2) #total hours before tee time density graph
dev.off()

png(filename = "wkdycftb.rnd.png", width=900, height=600)
print(kroweekday2) #weekday hours before tee time count graph
dev.off()

png(filename = "wkdycftb.rev.png", width=900, height=600)
print(krevweekday2) #weekday hours before tee time density graph
dev.off()

png(filename = "wkndcftb.rnd.png", width=900, height=600)
print(kroweekend2) #weekend hours before tee time count graph
dev.off()

png(filename = "wkndcftb.rev.png", width=900, height=600)
print(krevweekend2) #weekend hours before tee time density graph
dev.off()

#create a CellStyle that goes into CFTBWB
cstylecols <- CellStyle(CFTBWB) + Font(CFTBWB, isBold=TRUE) + Border(color="black") #+ Alignment(wrapText = TRUE)

TotalRndChnFee <- paste0(year," Total: Rounds - By Channel and Fee")
WkdyRndChnFee <- paste0(year," Weekday: Rounds - By Channel and Fee")
WkndRndChnFee <- paste0(year," Weekend: Rounds - By Channel and Fee")

TotalRevChnFee <- paste0(year," Total: Revenue - By Channel and Fee")
WkdyRevChnFee <- paste0(year," Weekday: Revenue - By Channel and Fee")
WkndRevChnFee <- paste0(year," Weekend: Revenue - By Channel and Fee")

#### SHEET 1 TOTAL - Channel-Fee Breakdown
####Add graphs and tables to 'Hours Booked in Advance-Channel' Sheet ####
Sheetname1 = "TOTAL - Channel-Fee Breakdown"

#### Create sheet in workbook #### SHEET 1
sheet1 <- createSheet(CFTBWB, sheetName = Sheetname1)

#### Creating/Defining the Cells for xlsx text input to WB ####
## xlsx package - getRows from sheet, getCells from rows###
## in xlsx you have to define the cells (rows & cols) for each sheet
rows1 <- createRow(sheet1, 1:80) #creates 120 rows
cells1 <- createCell(rows1, colIndex = 1:60) #creates 43 columns & adds them to rows

# 'year' Total: Rounds - By Channel and Fee
cell <- cells1[[31,1]]
setCellValue(cell, TotalRndChnFee)
setCellStyle(cell, cstylecols)

# 'year' Total: Revenue - By Channel and Fee
cell <- cells1[[31,20]]
setCellValue(cell, TotalRevChnFee)
setCellStyle(cell, cstylecols)

#### ADD TOTAL - Channel/Fee CHARTS ####
addPicture(file=paste0("totalcftb.rnd.png"), sheet1, startRow=1, startColumn=1)
addPicture(file=paste0("totalcftb.rev.png"), sheet1, startRow=1, startColumn=20)

#### ADD Round & Revenue DT Tables -Channel:Fee ####
addDataFrame(totrndtable, sheet1, startRow=32, startColumn=1, row.names=FALSE, colnamesStyle=cstylecols)
addDataFrame(totrevtable, sheet1, startRow=32, startColumn=20, row.names=FALSE, colnamesStyle=cstylecols)

#### SHEET 2 WEEKDAY - Channel-Fee Breakdown
####Add graphs and tables to 'Hours Booked in Advance-Channel' Sheet ####
Sheetname2 = "WEEKDAY - Channel-Fee Breakdown"

#### Create sheet in workbook #### SHEET 2
sheet2 <- createSheet(CFTBWB, sheetName = Sheetname2)

#### Creating/Defining the Cells for xlsx text input to WB ####
## xlsx package - getRows from sheet, getCells from rows###
## in xlsx you have to define the cells (rows & cols) for each sheet
rows1 <- createRow(sheet2, 1:80) #creates 120 rows
cells1 <- createCell(rows1, colIndex = 1:60) #creates 43 columns & adds them to rows

# 'year' Total: Rounds - By Channel and Fee
cell <- cells1[[31,1]]
setCellValue(cell, WkdyRndChnFee)
setCellStyle(cell, cstylecols)

# 'year' Total: Revenue - By Channel and Fee
cell <- cells1[[31,20]]
setCellValue(cell, WkdyRevChnFee)
setCellStyle(cell, cstylecols)

#### ADD TOTAL - Channel/Fee CHARTS ####
addPicture(file=paste0("wkdycftb.rnd.png"), sheet2, startRow=1, startColumn=1)
addPicture(file=paste0("wkdycftb.rev.png"), sheet2, startRow=1, startColumn=20)

#### ADD Round & Revenue DT Tables -Channel:Fee ####
addDataFrame(wkdyrndtable, sheet2, startRow=32, startColumn=1, row.names=FALSE, colnamesStyle=cstylecols)
addDataFrame(wkdyrevtable, sheet2, startRow=32, startColumn=20, row.names=FALSE, colnamesStyle=cstylecols)

#### SHEET 3 WEEKEND - Channel-Fee Breakdown
####Add graphs and tables to 'Hours Booked in Advance-Channel' Sheet ####
Sheetname3 = "WEEKEND - Channel-Fee Breakdown"

#### Create sheet in workbook #### SHEET 2
sheet3 <- createSheet(CFTBWB, sheetName = Sheetname3)

#### Creating/Defining the Cells for xlsx text input to WB ####
## xlsx package - getRows from sheet, getCells from rows###
## in xlsx you have to define the cells (rows & cols) for each sheet
rows1 <- createRow(sheet3, 1:80) #creates 120 rows
cells1 <- createCell(rows1, colIndex = 1:60) #creates 43 columns & adds them to rows

# 'year' Total: Rounds - By Channel and Fee
cell <- cells1[[31,1]]
setCellValue(cell, WkndRndChnFee)
setCellStyle(cell, cstylecols)

# 'year' Total: Revenue - By Channel and Fee
cell <- cells1[[31,20]]
setCellValue(cell, WkndRevChnFee)
setCellStyle(cell, cstylecols)

#### ADD TOTAL - Channel/Fee CHARTS ####
addPicture(file=paste0("wkndcftb.rnd.png"), sheet3, startRow=1, startColumn=1)
addPicture(file=paste0("wkndcftb.rev.png"), sheet3, startRow=1, startColumn=20)

#### ADD Round & Revenue DT Tables -Channel:Fee ####
addDataFrame(wkndrndtable, sheet3, startRow=32, startColumn=1, row.names=FALSE, colnamesStyle=cstylecols)
addDataFrame(wkndrevtable, sheet3, startRow=32, startColumn=20, row.names=FALSE, colnamesStyle=cstylecols)



#### SAVE CFTBWB WORKBOOK ####

#w:/BetaGerhard/
#saveWorkbook(CFTBWB,paste0("w:/BetaGerhard/",course," ", Sys.Date(), " Channel Fee Breakdown.xlsx")) #***TEST SAVE***
## doesnt work --- saveWorkbook(CFTBWB,paste0("/srv/wd/BetaGerhard/",course," ", Sys.Date(), " Channel Fee Breakdown.xlsx"))
saveWorkbook(CFTBWB,paste0("/srv/wd/Comp Assessment/Downloads/",direc,"Channel Fee Breakdown.xlsx"))


################################################ ^^ BLOCK OUT ABOVE UNTIL FINISHED ^^ ##################################################


### END Tables&Graphs
return(list("CFTDserv" = CFTD.total.rounds.graph, CFTD.total.revenue.graph, 
            CFTD.weekday.rounds.graph, CFTD.weekday.revenue.graph, 
            CFTD.weekend.rounds.graph, CFTD.weekend.revenue.graph, 
            totrndtableDT, totrevtableDT, wkdyrndtableDT, wkdyrevtableDT,
            wkndrndtableDT, wkndrevtableDT, ""))
}
