ChannelFee <- function(client, account = " ",display_data,direc,year){

##FOR SCRIPT TESTING PURPOSES 
client="Client Name";account = " ";display_data=1;year="2019"
## FOR TESTING ^^^^^^^^
  
setwd("Y:/ChannelFeeProject/")  

  
library(ggplot2)
library(ggiraph)
library(tidyr)
library(DT)
library(scales)
library(grid)
library(dplyr)
library(xlsx)
library(htmlwidgets)

#### FILTER DATA BY YEAR ###### 
SQLData <- read.csv(paste0("BreakDownAllclients",year,".csv"),stringsAsFactors = FALSE)


## ensure 1st column has correct name
colnames(SQLData)[1] <- "client"


## Remove the quotation marks from client & account name #
SQLData$client <- gsub("\"", "", SQLData$client)
SQLData$account <- gsub("\"", "", SQLData$account)

#### FILTER BY client ####

SQLData = SQLData[which(SQLData$client %in% client),]


##would have to change SQLData$client to SQLData$account to get everything to work
if(account != " "){
  SQLData = SQLData[which(SQLData$account == account),]
}
#else SQLData$account = NA

#### IF SQLData SELECTED client HAS ZERO rows - then put up warning message ####
if(nrow(SQLData) < 1){
    nullplot = ggplot(SQLData) + geom_blank()
    return(list(nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot, nullplot, nullplot, nullplot,"NOT ENOUGH DATA"))
  }


##Make units column into factor
#SQLData$units = as.factor(SQLData$units)
##Make TeeTimeHM into time
SQLData$TeeTimeHM = as.POSIXct(SQLData$TeeTimeHM,format="%H:%M:%S")
##Make Total.Fees numeric
SQLData$Total.Fees <- as.numeric(SQLData$Total.Fees)
##Make HoursBookedBeforeTT integer
SQLData$HoursBookedBeforeTT <- as.numeric(SQLData$HoursBookedBeforeTT, na.rm=TRUE)

## OR SQLData <- read.csv('TestChannelFeeDateTime.csv', stringsAsFactors = FALSE)
#### Total Sort by Booking Channel ####
shop.units.vec <- (SQLData[SQLData$Channel=="Shop",])
res.center.units.vec <- (SQLData[SQLData$Channel=="Res Center",])
TPY.units.vec <- (SQLData[SQLData$Channel=="TPY",])
web.units.vec <- (SQLData[SQLData$Channel=="Website",])

#### Weekday Sort by Booking Channel #####
weekday.units.vec <- (SQLData[SQLData$DayType=="Weekday",])
weekday.shop.units.vec <- (weekday.units.vec[weekday.units.vec$Channel=="Shop",])
weekday.res.center.units.vec <- (weekday.units.vec[weekday.units.vec$Channel=="Res Center",])
weekday.TPY.units.vec <- (weekday.units.vec[weekday.units.vec$Channel=="TPY",])
weekday.web.units.vec <- (weekday.units.vec[weekday.units.vec$Channel=="Website",])

#### Weekend Sort by Booking Channel ########
weekend.units.vec <- (SQLData[SQLData$DayType=="Weekend",])
weekend.shop.units.vec <- (weekend.units.vec[weekend.units.vec$Channel=="Shop",])
weekend.res.center.units.vec <- (weekend.units.vec[weekend.units.vec$Channel=="Res Center",])
weekend.TPY.units.vec <- (weekend.units.vec[weekend.units.vec$Channel=="TPY",])
weekend.web.units.vec <- (weekend.units.vec[weekend.units.vec$Channel=="Website",])

##### IF NOT ENOUGH DATA - STOP #####
# if(nrow(c(SQLData, shop.units.vec, res.center.units.vec, TPY.units.vec, website.units.vec)) == 0){
#   nullplot = plotly_empty()
#   return(list(nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot, nullplot, nullplot, nullplot,"NOT ENOUGH DATA"))
# }
# 
# vectorcheck <- c(shop.units.vec, res.center.units.vec, TPY.units.vec, website.units.vec)
# 
# for(i in vectorcheck){
#   if(nrow(c(SQLData, shop.units.vec, res.center.units.vec, TPY.units.vec, website.units.vec)) == 0){
#     nullplot = plotly_empty()
#     return(list(nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot,nullplot, nullplot, nullplot, nullplot,"NOT ENOUGH DATA"))
#   }
# }
#### Empty nrow Vec fill in for each Channel ####

EmptyDFs<- data.frame(client = rep(as.character(client),1),
                     Channel = rep(as.character("Shop"),1),
                     account = rep(as.character(account),1),
                     FeeType = rep(as.character("Comp"),1),
                     Year = rep(as.integer(year),1),
                     Month = rep(as.integer(1),1),
                     MonthName = rep(as.character("January"),1),
                     DateTeeTime = rep(NA,1),
                     TeeTimeHM = rep(NA,1),
                     TTHour = rep(as.integer(12),1),
                     HoursBookedBeforeTT = rep(as.integer(0),1),
                     DayType = rep(as.character("Weekday"),1),
                     units = rep(as.numeric(0),1),
                     Total.Fees = rep(as.numeric(0),1),
                     stringsAsFactors = FALSE
                     )

EmptyDFr<- data.frame(client = rep(as.character(client),1),
                     Channel = rep(as.character("Res Center"),1),
                     account = rep(as.character(account),1),
                     FeeType = rep(as.character("Comp"),1),
                     Year = rep(as.integer(year),1),
                     Month = rep(as.integer(1),1),
                     MonthName = rep(as.character("January"),1),
                     DateTeeTime = rep(NA,1),
                     TeeTimeHM = rep(NA,1),
                     TTHour = rep(as.integer(12),1),
                     HoursBookedBeforeTT = rep(as.integer(0),1),
                     DayType = rep(as.character("Weekday"),1),
                     units = rep(as.numeric(0),1),
                     Total.Fees = rep(as.numeric(0),1),
                     stringsAsFactors = FALSE
                     )

EmptyDFt<- data.frame(client = rep(as.character(client),1),
                      Channel = rep(as.character("TPY"),1),
                      account = rep(as.character(account),1),
                      FeeType = rep(as.character("Comp"),1),
                      Year = rep(as.integer(year),1),
                      Month = rep(as.integer(1),1),
                      MonthName = rep(as.character("January"),1),
                      DateTeeTime = rep(NA,1),
                      TeeTimeHM = rep(NA,1),
                      TTHour = rep(as.integer(12),1),
                      HoursBookedBeforeTT = rep(as.integer(0),1),
                      DayType = rep(as.character("Weekday"),1),
                      units = rep(as.numeric(0),1),
                      Total.Fees = rep(as.numeric(0),1),
                      stringsAsFactors = FALSE
                      )

EmptyDFw<- data.frame(client = rep(as.character(client),1),
                      Channel = rep(as.character("Website"),1),
                      account = rep(as.character(account),1),
                      FeeType = rep(as.character("Comp"),1),
                      Year = rep(as.integer(year),1),
                      Month = rep(as.integer(1),1),
                      MonthName = rep(as.character("January"),1),
                      DateTeeTime = rep(NA,1),
                      TeeTimeHM = rep(NA,1),
                      TTHour = rep(as.integer(12),1),
                      HoursBookedBeforeTT = rep(as.integer(0),1),
                      DayType = rep(as.character("Weekday"),1),
                      units = rep(as.numeric(0),1),
                      Total.Fees = rep(as.numeric(0),1),
                      stringsAsFactors = FALSE
                      )
  
## Need to include account as part of file download name, but only when necessary 


############Aggregate by units & Revenue (Sum) TOTAL ################

if(nrow(shop.units.vec) > 0){
  shop.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,shop.units.vec,FUN=sum)
}else{
  shop.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFs,FUN=sum)
}


if(nrow(TPY.units.vec) > 0){
  TPY.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,TPY.units.vec,FUN=sum)
}else{
  TPY.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFt,FUN=sum)
}


if(nrow(web.units.vec) > 0){
  web.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,web.units.vec,FUN=sum)
}else{
  web.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFw,FUN=sum)
}



if(nrow(res.center.units.vec) > 0){
  res.center.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,res.center.units.vec,FUN=sum)
}else{
  res.center.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFr,FUN=sum)
}


##### Aggregate by units & Revenue (Sum) WEEKDAY ######    Aggregate by units & Revenue (Sum) WEEKDAY ######
if(nrow(weekday.shop.units.vec) > 0){
  weekday.shop.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,weekday.shop.units.vec,FUN=sum)
}else{
  weekday.shop.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFs,FUN=sum)
}


if(nrow(weekday.TPY.units.vec) > 0){
  weekday.TPY.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,weekday.TPY.units.vec,FUN=sum)
}else{
  weekday.TPY.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFt,FUN=sum)
}


if(nrow(weekday.web.units.vec) > 0){
  weekday.web.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,weekday.web.units.vec,FUN=sum)
}else{
  weekday.web.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFw,FUN=sum)
}


if(nrow(weekday.res.center.units.vec) > 0){
  weekday.res.center.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,weekday.res.center.units.vec,FUN=sum)
}else{
  weekday.res.center.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFr,FUN=sum)
}


##### Aggregate by units & Revenue (Sum) WEEKEND ######    Aggregate by units & Revenue (Sum) WEEKEND ######
if(nrow(weekend.shop.units.vec) > 0){
  weekend.shop.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,weekend.shop.units.vec,FUN=sum)
}else{
  weekend.shop.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFs,FUN=sum)
}


if(nrow(weekend.TPY.units.vec) > 0){
  weekend.TPY.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,weekend.TPY.units.vec,FUN=sum)
}else{
  weekend.TPY.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFt,FUN=sum)
}


if(nrow(weekend.web.units.vec) > 0){
  weekend.web.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,weekend.web.units.vec,FUN=sum)
}else{
  weekend.web.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFw,FUN=sum)
}


if(nrow(weekend.res.center.units.vec) > 0){
  weekend.res.center.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,weekend.res.center.units.vec,FUN=sum)
}else{
  weekend.res.center.units.FeeTypeSum.vec = aggregate(cbind(units, Total.Fees)~client+Channel+Year+MonthName+FeeType,EmptyDFr,FUN=sum)
}




##### TOTAL graph Dataframe #############
totaldf <- rbind.data.frame(shop.units.FeeTypeSum.vec, 
                            res.center.units.FeeTypeSum.vec, 
                            TPY.units.FeeTypeSum.vec,
                            web.units.FeeTypeSum.vec)

#### WEEKDAY graph Dataframe ########## - graphs not included 
weekdaydf <- rbind.data.frame(weekday.shop.units.FeeTypeSum.vec, 
                              weekday.res.center.units.FeeTypeSum.vec, 
                              weekday.TPY.units.FeeTypeSum.vec,
                              weekday.web.units.FeeTypeSum.vec)

##### WEEKEND graph dataframe ####### - graphs not included
weekenddf <- rbind.data.frame(weekend.shop.units.FeeTypeSum.vec, 
                              weekend.res.center.units.FeeTypeSum.vec, 
                              weekend.TPY.units.FeeTypeSum.vec,
                              weekend.web.units.FeeTypeSum.vec)


################ ######################################
###### INTERACTIVE graphs below ###########

#Create tooltip css for mouse hover info (ggiraph)
tooltip_css <- "background-color:gray;color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;font-family:Trebuchet MS"
#create the tooltip #
totaldf$tooltipunits = paste0("Units Sold: ", formatC(totaldf$units, format="d", big.mark=","), "<br/>",
                               "Fee Type: ", totaldf$FeeType)

#orders the months chronologically
totaldf$MonthName <- factor(totaldf$MonthName, levels = month.name)

##### Pic Entered As Background Image - must add to ggplot before the geom_lines #####

image3 <- jpeg::readJPEG("numbers2.jpg") ## img must be located in working directory/folder

imagex <- rasterGrob(image3, width = unit(1, "npc"), height = unit(1, "npc"))

###### START OF BLOCK GGIRAPH PLOTS ######
####### INTERACTIVE TOTAL units graph ########

jrototal2 <- ggplot(totaldf, aes(x=MonthName, y=units, group=FeeType, color=FeeType)) +
  annotation_custom(imagex) +
  geom_line() +
  geom_point_interactive(aes(tooltip=tooltipunits, data_id=units)) +
  facet_wrap(~Channel)+
  scale_y_continuous(labels=scales::comma, limits = c(0,NA))+
  #ylim(0,NA) +
  theme(text=element_text(family="Trebuchet MS", face="bold", size=10))


krototal2 <- jrototal2 + theme(legend.position="bottom",legend.title=element_blank(),
                               plot.title = element_text(hjust = 0.5, size=12), axis.text.x = element_text(angle=90, vjust=.5, size=10), axis.text.y=element_text(size=10)) +
  ylab("Units Sold") + xlab("Month") + ggtitle(paste("Client:", totaldf$Year, "Units Sold by Channel/Fee Type"))

#ggiraph(code = print(krototal2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.total.units.graph <- ggiraph(code = print(krototal2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:2pt;")

htmlwidgets::saveWidget(CFTD.total.units.graph, "cftdunits_lineplot2.html", selfcontained = TRUE, libdir = NULL)

##### INTERACTIVE TOTAL REVENUE graph #########


#create the tooltip # #tooltips need to have formatC in order to include commas
totaldf$tooltiprevenue = paste0("Revenue: $", formatC(totaldf$Total.Fees, format="d", big.mark=","), "<br/>",
                                "Fee Type: ", totaldf$FeeType)

totaldf$MonthName <- factor(totaldf$MonthName, levels = month.name)

jrevtotal2 <- ggplot(totaldf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType)) +
  annotation_custom(imagex) +
  geom_line() +
  geom_point_interactive(aes(tooltip=tooltiprevenue, data_id=units)) +
  facet_wrap(~Channel) + scale_y_continuous(labels=scales::dollar, limits = c(0,NA)) + theme(text=element_text(family="Trebuchet MS", face="bold",size=10))  #theme_classic() ##+ scale_y_continuous(label=dollar_format())


krevtotal2 <- jrevtotal2 + theme(legend.position="bottom",legend.title=element_blank(),
                                 plot.title = element_text(hjust = 0.5, size=12), axis.text.x = element_text(angle=90, vjust=.5, size=10), axis.text.y=element_text(size=10)) +
  ylab("Revenue") + xlab("Month") + ggtitle(paste("Client:", totaldf$Year, " Revenue by Channel/Fee Type"))

#ggiraph(code = print(krevtotal2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.total.revenue.graph <- ggiraph(code = print(krevtotal2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:2pt;")

htmlwidgets::saveWidget(CFTD.total.revenue.graph, "cftdrevenue_lineplot2.html", selfcontained = TRUE, libdir = NULL)


####### INTERACTIVE WEEKDAY units graph ########

#create the tooltip #
weekdaydf$tooltipunits = paste0("units: ", formatC(weekdaydf$units, format="d", big.mark=","), "<br/>",
                                 "Fee Type: ", weekdaydf$FeeType)

# ##arrange months chronologically #
weekdaydf$MonthName <- factor(weekdaydf$MonthName, levels = month.name)

jroweekday2 <- ggplot(weekdaydf, aes(x=MonthName, y=units, group=FeeType, color=FeeType)) +
  annotation_custom(imagex) +
  geom_line() +
  geom_point_interactive(aes(tooltip=tooltipunits, data_id=units)) +
  facet_wrap(~Channel)+
  scale_y_continuous(labels=scales::comma, limits = c(0,NA))+
  #ylim(0,NA) + theme_classic()
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10))



kroweekday2 <- jroweekday2 + theme(legend.position="bottom",legend.title=element_blank(),
                                   plot.title = element_text(hjust = 0.5, size=12), axis.text.x = element_text(angle=90, vjust=.5, size=10), axis.text.y=element_text(size=10)) +
  ylab("units") + xlab("Month") + ggtitle(paste(weekdaydf$client, ":", weekdaydf$Year, " Weekday units by Channel/Fee Type"))

#ggiraph(code = print(kroweekday2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.weekday.units.graph <- ggiraph(code = print(kroweekday2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:1pt;")

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
  ylab("Revenue") + xlab("Month") + ggtitle(paste(weekdaydf$client, ":", weekdaydf$Year, "Weekday Revenue by Channel/Fee Type"))

#ggiraph(code = print(krevweekday2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.weekday.revenue.graph <- ggiraph(code = print(krevweekday2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:1pt;")



####### INTERACTIVE WEEKEND units graph ########

#create the tooltip #
weekenddf$tooltipunits = paste0("units: ", formatC(weekenddf$units, format="d", big.mark=","), "<br/>",
                                 "Fee Type: ", weekenddf$FeeType)

##arrange months chronologically #
weekenddf$MonthName <- factor(weekenddf$MonthName, levels = month.name)

jroweekend2 <- ggplot(weekenddf, aes(x=MonthName, y=units, group=FeeType, color=FeeType)) +
  annotation_custom(imagex) +
  geom_line() +
  geom_point_interactive(aes(tooltip=tooltipunits, data_id=units)) +
  facet_wrap(~Channel)+
  scale_y_continuous(labels=scales::comma, limits = c(0,NA))+
  #ylim(0,NA) + theme_classic()
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10))



kroweekend2 <- jroweekend2 + theme(legend.position="bottom",legend.title=element_blank(),
                                   plot.title = element_text(hjust = 0.5, size=12), axis.text.x = element_text(angle=90, vjust=.5, size=10), axis.text.y=element_text(size=10)) +
  ylab("units") + xlab("Month") + ggtitle(paste(weekenddf$client, ":", weekenddf$Year, "Weekend units by Channel/Fee Type"))

#ggiraph(code = print(kroweekend2), hover_css = "cursor:pointer;fill:red;r:8pt;")

CFTD.weekend.units.graph <- ggiraph(code = print(kroweekend2), selection_type ="none", tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:1pt;")


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
  ylab("Revenue") + xlab("Month") + ggtitle(paste(weekenddf$client, ":", weekenddf$Year, "Weekend Revenue by Channel/Fee Type"))

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
# n = length(unique(totunittable$Channel))
# colschann = gg_color_hue(n)
# 
# names(colschann) = unique(totunittable$Channel)
# 
# #FeeType Colors
# gg_color_hue <- function(n) {
#   hues = seq(10, 575, length = n + 1)
#   hcl(h = hues, l = 95, c = 100)[1:n]
# }
# n = length(unique(totunittable$FeeType))
# colsfee = gg_color_hue(n)
# 
# names(colsfee) = unique(totunittable$FeeType)


##### DATA TABLES BEGIN #####
##### TOTAL TABLES ####
totunittable = spread(totaldf[,-which(colnames(totaldf) %in% c("client", "Year", "Total.Fees", "tooltipunits", "tooltiprevenue"))], MonthName, units)
totrevtable = spread(totaldf[,-which(colnames(totaldf) %in% c("client","Year", "units", "tooltipunits", "tooltiprevenue"))], MonthName, Total.Fees)

## fill in blanks with 0
totunittable[which(is.na(totunittable),arr.ind = TRUE)] <- 0
totrevtable[which(is.na(totrevtable),arr.ind = TRUE)] <- 0

#### Add TOTAL column and row for Total unit & Rev ####
totunittable <- totunittable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
totunittable <- cbind(totunittable, Total = rowSums(totunittable[,3:ncol(totunittable)]))
#totunittable <- totunittable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))

totrevtable <- totrevtable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
totrevtable <- cbind(totrevtable, Total = rowSums(totrevtable[,3:ncol(totrevtable)]))
#totunittable <- totunittable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))


##### COLORS for Channel Column in TABLES####
#Channel Colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = length(unique(totunittable$Channel))
colschann = gg_color_hue(n)

names(colschann) = unique(totunittable$Channel)

#FeeType Colors
gg_color_hue <- function(n) {
  hues = seq(10, 575, length = n + 1)
  hcl(h = hues, l = 95, c = 100)[1:n]
}
n = length(unique(totunittable$FeeType))
colsfee = gg_color_hue(n)

names(colsfee) = unique(totunittable$FeeType)

# Create DT Table
totunittableDT <- datatable(totunittable, caption = htmltools::tags$caption(paste('client:',totaldf$client, totaldf$Year,'-','account:',SQLData$account,'Table: Total units')), filter = 'top',
                           extensions = 'Buttons', options = list(
                             dom = 'Bfrtip', 
                             buttons = list('copy',list(extend='excel',filename=paste(client,year,'Table: Total units',Sys.Date()))), pageLength=nrow(totunittable)
                           )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(totunittable$Channel), colschann)
              ,fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(totunittable$FeeType), colsfee)
              ,fontWeight = 'bold') 


totrevtableDT <- datatable(totrevtable, caption = htmltools::tags$caption(paste(totaldf$client, totaldf$Year,'-', 'account:', SQLData$account, 'Table: Total Revenue')), filter = 'top',
                           extensions = 'Buttons', options = list(
                             dom = 'Bfrtip',
                             buttons = list('copy', list(extend='excel', filename=paste(client,year,'Table: Total Revenue',Sys.Date()))), pageLength=nrow(totunittable)
                           )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(totrevtable$Channel), colschann)
              ,fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(totrevtable$FeeType), colsfee)
              ,fontWeight = 'bold') %>%
  formatCurrency(3:15)

#### create html widgets for tables ####
htmlwidgets::saveWidget(totunittableDT, "cftdunits_table2.html", selfcontained = TRUE, libdir = NULL)
htmlwidgets::saveWidget(totrevtableDT, "cftdrevenue_table2.html", selfcontained = TRUE, libdir = NULL)



##### WEEKDAY TABLES #### - not displayed on website
wkdyunittable = spread(weekdaydf[,-which(colnames(weekdaydf) %in% c("client", "Year", "Total.Fees", "tooltipunits", "tooltiprevenue"))], MonthName, units)
wkdyrevtable = spread(weekdaydf[,-which(colnames(weekdaydf) %in% c("client","Year", "units", "tooltipunits", "tooltiprevenue"))], MonthName, Total.Fees)

wkdyunittable[which(is.na(wkdyunittable),arr.ind = TRUE)] <- 0
wkdyrevtable[which(is.na(wkdyrevtable),arr.ind = TRUE)] <- 0

#### Add TOTAL column and row for WEEKDAY unit & Rev ####
wkdyunittable <- wkdyunittable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
wkdyunittable <- cbind(wkdyunittable, Total = rowSums(wkdyunittable[,3:ncol(wkdyunittable)]))
#wkdyunittable <- wkdyunittable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))

wkdyrevtable <- wkdyrevtable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
wkdyrevtable <- cbind(wkdyrevtable, Total = rowSums(wkdyrevtable[,3:ncol(wkdyrevtable)]))
#totunittable <- totunittable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))


##### COLORS for Channel Column in TABLES####
#Channel Colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = length(unique(wkdyunittable$Channel))
colschann = gg_color_hue(n)

names(colschann) = unique(wkdyunittable$Channel)

#FeeType Colors
gg_color_hue <- function(n) {
  hues = seq(5, 375, length = n + 1)
  hcl(h = hues, l = 95, c = 100)[1:n]
}
n = length(unique(wkdyunittable$FeeType))
colsfee = gg_color_hue(n)

names(colsfee) = unique(wkdyunittable$FeeType)

# Create DT Table for Weekday units
wkdyunittableDT <- datatable(wkdyunittable, caption = htmltools::tags$caption(paste(totaldf$client, totaldf$Year,'-', 'account:', SQLData$account, 'Table: Weekday units')), filter = 'top',
                            extensions = 'Buttons', options = list(
                              dom = 'Bfrtip',
                              buttons = list('copy',list(extend='excel',filename=paste(client,year,'Table: Weekday units',Sys.Date()))), pageLength=nrow(wkdyunittable)
                            )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(wkdyunittable$Channel), 
                                                      colschann),fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(wkdyunittable$FeeType), 
                                                      colsfee),fontWeight = 'bold')

# Create DT Table for Weekday Revenue
wkdyrevtableDT <- datatable(wkdyrevtable, caption = htmltools::tags$caption(paste(totaldf$client, totaldf$Year,'-', 'account:', SQLData$account, 'Table: Weekday Revenue')), filter = 'top',
                            extensions = 'Buttons', options = list(
                              dom = 'Bfrtip',
                              buttons = list('copy',list(extend='excel',filename=paste(client,year,'Table: Weekday Revenue',Sys.Date()))), pageLength=nrow(wkdyrevtable)
                            )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(wkdyrevtable$Channel), colschann)
              ,fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(wkdyrevtable$FeeType), colsfee)
              ,fontWeight = 'bold') %>%
  formatCurrency(3:15)

#### WEEKEND TABLES BEGIN #####
##### WEEKEND TABLES ####
wkndunittable = spread(weekenddf[,-which(colnames(weekenddf) %in% c("client", "Year", "Total.Fees", "tooltipunits", "tooltiprevenue"))], MonthName, units)
wkndrevtable = spread(weekenddf[,-which(colnames(weekenddf) %in% c("client","Year", "units", "tooltipunits", "tooltiprevenue"))], MonthName, Total.Fees)

wkndunittable[which(is.na(wkndunittable),arr.ind = TRUE)] <- 0
wkndrevtable[which(is.na(wkndrevtable),arr.ind = TRUE)] <- 0

#### Add TOTAL column and row for WEEKEND unit & Rev ####
wkndunittable <- wkndunittable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
wkndunittable <- cbind(wkndunittable, Total = rowSums(wkndunittable[,3:ncol(wkndunittable)]))
#wkdyunittable <- wkdyunittable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))

wkndrevtable <- wkndrevtable %>% bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
wkndrevtable <- cbind(wkndrevtable, Total = rowSums(wkndrevtable[,3:ncol(wkndrevtable)]))
#totunittable <- totunittable %>% bind_cols(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))



##### COLORS for Channel Column in TABLES WEEKEND####
#Channel Colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = length(unique(wkndunittable$Channel))
colschann = gg_color_hue(n)

names(colschann) = unique(wkndunittable$Channel)

#FeeType Colors
gg_color_hue <- function(n) {
  hues = seq(5, 375, length = n + 1)
  hcl(h = hues, l = 95, c = 100)[1:n]
}
n = length(unique(wkndunittable$FeeType))
colsfee = gg_color_hue(n)

names(colsfee) = unique(wkndunittable$FeeType)


# Create DT Table for Weekend units
wkndunittableDT <- datatable(wkndunittable, caption = htmltools::tags$caption(paste(totaldf$client, totaldf$Year,'-', 'account:', SQLData$account, 'Table: Weekend units')), filter = 'top',
                            extensions = 'Buttons', options = list(
                              dom = 'Bfrtip',
                              buttons = list('copy',list(extend='excel',filename=paste(client,year,'Table: Weekend units',Sys.Date()))), pageLength=nrow(wkndunittable)
                            )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(wkndunittable$Channel), 
                                                      colschann),fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(wkndunittable$FeeType), 
                                                      colsfee),fontWeight = 'bold')

# Create DT Table for Weekend Revenue
wkndrevtableDT <- datatable(wkndrevtable, caption = htmltools::tags$caption(paste(totaldf$client, totaldf$Year,'-', 'account:', SQLData$account, 'Table: Weekend Revenue')), filter = 'top',
                            extensions = 'Buttons', options = list(
                              dom = 'Bfrtip',
                              buttons = list('copy',list(extend='excel',filename=paste(client,year,'Table: Weekend Revenue',Sys.Date()))), pageLength=nrow(wkndrevtable)
                            )) %>%
  formatStyle('Channel', backgroundColor = styleEqual(unique(wkndrevtable$Channel), colschann)
              ,fontWeight = 'bold') %>%
  formatStyle('FeeType', backgroundColor = styleEqual(unique(wkndrevtable$FeeType), colsfee)
              ,fontWeight = 'bold') %>%
  formatCurrency(3:15)


#### ENTER VALUES FOR GGIRAPH GRAPHS ####
# CFTD.total.units.graph <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))
# CFTD.total.revenue.graph <-ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))
# CFTD.weekday.units.graph <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))
# CFTD.weekday.revenue.graph <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))
# CFTD.weekend.units.graph <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))
# CFTD.weekend.revenue.graph <- ggplot(weekdaydf, aes(x=MonthName, y=Total.Fees, group=FeeType, color=FeeType))

################################################# vv BLOCK OUT Below UNTIL FINISHED vv #################################################
#### FULL WORKBOOK DOWNLOAD ####
## change working directory to save image files ##
setwd("/folder/projectimg/")

####CREATE WB ####
CFTBWB <- createWorkbook(type="xlsx")

#### Create png image immediatly before dev.off ####

png(filename = "totalcftb.unit.png", width=900, height=600)
print(krototal2) #total hours before tee time count graph
dev.off()

png(filename = "totalcftb.rev.png", width=900, height=600)
print(krevtotal2) #total hours before tee time density graph
dev.off()

png(filename = "wkdycftb.unit.png", width=900, height=600)
print(kroweekday2) #weekday hours before tee time count graph
dev.off()

png(filename = "wkdycftb.rev.png", width=900, height=600)
print(krevweekday2) #weekday hours before tee time density graph
dev.off()

png(filename = "wkndcftb.unit.png", width=900, height=600)
print(kroweekend2) #weekend hours before tee time count graph
dev.off()

png(filename = "wkndcftb.rev.png", width=900, height=600)
print(krevweekend2) #weekend hours before tee time density graph
dev.off()

#create a CellStyle that goes into CFTBWB
cstylecols <- CellStyle(CFTBWB) + Font(CFTBWB, isBold=TRUE) + Border(color="black") #+ Alignment(wrapText = TRUE)

TotalunitChnFee <- paste0(year," Total: units - By Channel and Fee")
WkdyunitChnFee <- paste0(year," Weekday: units - By Channel and Fee")
WkndunitChnFee <- paste0(year," Weekend: units - By Channel and Fee")

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

# 'year' Total: units - By Channel and Fee
cell <- cells1[[31,1]]
setCellValue(cell, TotalunitChnFee)
setCellStyle(cell, cstylecols)

# 'year' Total: Revenue - By Channel and Fee
cell <- cells1[[31,20]]
setCellValue(cell, TotalRevChnFee)
setCellStyle(cell, cstylecols)

#### ADD TOTAL - Channel/Fee CHARTS ####
addPicture(file=paste0("totalcftb.unit.png"), sheet1, startRow=1, startColumn=1)
addPicture(file=paste0("totalcftb.rev.png"), sheet1, startRow=1, startColumn=20)

#### ADD Round & Revenue DT Tables -Channel:Fee ####
addDataFrame(totunittable, sheet1, startRow=32, startColumn=1, row.names=FALSE, colnamesStyle=cstylecols)
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

# 'year' Total: units - By Channel and Fee
cell <- cells1[[31,1]]
setCellValue(cell, WkdyunitChnFee)
setCellStyle(cell, cstylecols)

# 'year' Total: Revenue - By Channel and Fee
cell <- cells1[[31,20]]
setCellValue(cell, WkdyRevChnFee)
setCellStyle(cell, cstylecols)

#### ADD TOTAL - Channel/Fee CHARTS ####
addPicture(file=paste0("wkdycftb.unit.png"), sheet2, startRow=1, startColumn=1)
addPicture(file=paste0("wkdycftb.rev.png"), sheet2, startRow=1, startColumn=20)

#### ADD Round & Revenue DT Tables -Channel:Fee ####
addDataFrame(wkdyunittable, sheet2, startRow=32, startColumn=1, row.names=FALSE, colnamesStyle=cstylecols)
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

# 'year' Total: units - By Channel and Fee
cell <- cells1[[31,1]]
setCellValue(cell, WkndunitChnFee)
setCellStyle(cell, cstylecols)

# 'year' Total: Revenue - By Channel and Fee
cell <- cells1[[31,20]]
setCellValue(cell, WkndRevChnFee)
setCellStyle(cell, cstylecols)

#### ADD TOTAL - Channel/Fee CHARTS ####
addPicture(file=paste0("wkndcftb.unit.png"), sheet3, startRow=1, startColumn=1)
addPicture(file=paste0("wkndcftb.rev.png"), sheet3, startRow=1, startColumn=20)

#### ADD Round & Revenue DT Tables -Channel:Fee ####
addDataFrame(wkndunittable, sheet3, startRow=32, startColumn=1, row.names=FALSE, colnamesStyle=cstylecols)
addDataFrame(wkndrevtable, sheet3, startRow=32, startColumn=20, row.names=FALSE, colnamesStyle=cstylecols)



#### SAVE CFTBWB WORKBOOK ####

saveWorkbook(CFTBWB)

} ## ends function that could be called in R Shiny app
