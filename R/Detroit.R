rm(list=ls())
getwd()

####LIBRARIES####
library(openair)
library(xlsx)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)






####MAPPING: ####

##start of loop
#creating max table 

setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\Mapping")
FILES <- list.files( pattern = ".txt")


MaxTable<-c()

for (i in 1:length(FILES)) {
  
setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\Mapping")
  

CurrentData=read.table(file=FILES[i], skip = 7, header = TRUE, sep = "", fill = TRUE, 
                  stringsAsFactors = FALSE, na.strings = "NaN", col.names = c( "Date", "Time", "CO2", "CH4", "H2S", "C2H2", "M3DSU", "M3DSV", "M3DSW", 
                                                                               "M3DS2DSpeed", "M3DS3DSpeed", "M3DSAzimuth", "M3DSElevation", 
                                                                               "M3DSSoS", "M3DSSonicTemp", "M3DSErrorCode", "GPSTrack", 
                                                                               "GPSGroundSpeed", "GPSTime", "GPSLatitude", "GPSLongitude", 
                                                                               "GPSDoP", "BEN", "TOL", "ETB", "XYO", "XYM", "XYP", "SO2", 
                                                                               "STY", "FOR", "NH3", "NO", "NO2", "O3", "CellPressure", 
                                                                               "CellTemperature", "SUMMAPressure", "VOC", "AirMar.Wind.Speed..m.s.", 
                                                                               "AirMar.Wind.Direction..TRUE.", "AirMarAirTemp[oC]", "AirMarRelHumidity[%]", 
                                                                               "AirMarBarometer[mBar]", "AirMarHeading", "AirMarGroundSpeed[MPH]", 
                                                                               "AirMar.Latitude", "AirMar.Longitude"))
  
####CurrentData - substituting file name with generic call####

##enter script to automatically name dataset; adding "Mapping" name to name extracted from files#
x<-FILES[i] 
x<-substr(x,1,11)
NewName<-paste("DetSCOUT",x,sep="")

#data already imported, format date 
CurrentData_short<- CurrentData[, c("Date", "Time","CH4", "H2S", "BEN", "TOL", "XYP", "AirMar.Latitude", "AirMar.Longitude",
                                            "AirMar.Wind.Speed..m.s.", "AirMar.Wind.Direction..TRUE.")]



#format and concatenate DateTime variable
CurrentData_short$DateTime<- as.POSIXct(paste(CurrentData_short$Date, CurrentData_short$Time), format = "%m/%d/%y %H:%M:%S", tz = "GMT")

#Round to 2 decimal places
CurrentData_short$H2S<-round(CurrentData_short$H2S, digits = 2) 
CurrentData_short$CH4<-round(CurrentData_short$CH4, digits = 2) 
CurrentData_short$BEN<-round(CurrentData_short$BEN, digits = 2)
CurrentData_short$TOL<-round(CurrentData_short$TOL, digits = 2)
CurrentData_short$XYP<-round(CurrentData_short$XYP, digits = 2)



#format time variable
CurrentData_short$Time <-as.POSIXct((CurrentData_short$Time), format="%m/%d/%Y %H:%M:%S", tz = "GMT")

#drop date and time
CurrentData_short<- CurrentData_short[, c("DateTime", "CH4", "H2S", "BEN", "TOL", "XYP", "AirMar.Latitude", "AirMar.Longitude",
                                                  "AirMar.Wind.Speed..m.s.", "AirMar.Wind.Direction..TRUE.")]

#flag variables (MDL)
CurrentData_short$H2Sflag<-" " #add a flag column; 
CurrentData_short$H2Sflag[CurrentData_short$H2S < -7.86] <-"MD"    ##< MDL  
CurrentData_short$H2Sflag[CurrentData_short$H2S >= -7.86 & CurrentData_short$H2S <= 7.86 ] <-"ND" ## |MDL|
CurrentData_short$H2Sflag[CurrentData_short$H2S >= 7.87 & CurrentData_short$H2S < 23.58 ] <-"PQ" ##Practical quantitation limit; 3 x MDL
CurrentData_short$H2Sflag[CurrentData_short$H2S >= 50690.0 ] <-"EH" ##Estimated; exceeds upper range

CurrentData_short$CH4flag<-" " #add a flag column; 
CurrentData_short$CH4flag[CurrentData_short$CH4 < -0.00157] <-"MD"    ##< MDL  
CurrentData_short$CH4flag[CurrentData_short$CH4 >= -0.00157 & CurrentData_short$CH4 <= 0.00157 ] <-"ND" ## |MDL|
CurrentData_short$CH4flag[CurrentData_short$CH4 >= 0.00158 & CurrentData_short$CH4 < 0.00471 ] <-"PQ" ##Practical quantitation limit; 3 x MDL
CurrentData_short$CH4flag[CurrentData_short$CH4 >= 200 ] <-"EH" ##Estimated; exceeds upper range

CurrentData_short$BENflag<-" " #add a flag column; 
CurrentData_short$BENflag[CurrentData_short$BEN < -4.80] <-"MD"    ##< MDL  
CurrentData_short$BENflag[CurrentData_short$BEN>= -4.80 & CurrentData_short$BEN <= 4.80 ] <-"ND" ## |MDL|
CurrentData_short$BENflag[CurrentData_short$BEN >= 4.81 & CurrentData_short$BEN < 24 ] <-"PQ" ##Practical quantitation limit; 3 x MDL
CurrentData_short$BENflag[CurrentData_short$BEN >= 106 ] <-"EH" ##Estimated; exceeds upper range

CurrentData_short$TOLflag<-" " #add a flag column; 
CurrentData_short$TOLflag[CurrentData_short$TOL < -3.69] <-"MD"    ##< MDL  
CurrentData_short$TOLflag[CurrentData_short$TOL >= -3.69 & CurrentData_short$TOL <= 3.69 ] <-"ND" ## |MDL|
CurrentData_short$TOLflag[CurrentData_short$TOL >= 3.70 & CurrentData_short$TOL < 18.45 ] <-"PQ" ##Practical quantitation limit; 3 x MDL
CurrentData_short$TOLflag[CurrentData_short$TOL >= 98 ] <-"EH" ##Estimated; exceeds upper range

CurrentData_short$XYPflag<-" " #add a flag column; 
CurrentData_short$XYPflag[CurrentData_short$XYP < -4.05] <-"MD"    ##< MDL  
CurrentData_short$XYPflag[CurrentData_short$XYP>= -4.05 & CurrentData_short$XYP <= 4.05 ] <-"ND" ## |MDL|
CurrentData_short$XYPflag[CurrentData_short$XYP >= 4.06 & CurrentData_short$XYP < 20.25 ] <-"PQ" ##Practical quantitation limit; 3 x MDL
CurrentData_short$XYPflag[CurrentData_short$XYP >= 101.9 ] <-"EH" ##Estimated; exceeds upper range



#rename
names(CurrentData_short)[names(CurrentData_short) == 'H2S'] <- 'H2S(ppb)'
names(CurrentData_short)[names(CurrentData_short) == 'CH4'] <- 'CH4(ppm)'
names(CurrentData_short)[names(CurrentData_short) == 'BEN'] <- 'BEN(ppb)'
names(CurrentData_short)[names(CurrentData_short) == 'TOL'] <- 'TOL(ppb)'
names(CurrentData_short)[names(CurrentData_short) == 'XYP'] <- 'XYP(ppb)'


names(CurrentData_short)[names(CurrentData_short) == 'AirMar.Latitude'] <- 'Latitude'
names(CurrentData_short)[names(CurrentData_short) == 'AirMar.Longitude'] <- 'Longitude'
names(CurrentData_short)[names(CurrentData_short) == 'AirMar.Wind.Speed..m.s.'] <- 'ws'
names(CurrentData_short)[names(CurrentData_short) == 'AirMar.Wind.Direction..TRUE.'] <- 'wd'

##reorder and export flagged file 
CurrentData_short<-CurrentData_short[, c("DateTime", "H2S(ppb)", "H2Sflag", "CH4(ppm)", "CH4flag", "BEN(ppb)", "BENflag", 
                                         "TOL(ppb)", "TOLflag", "XYP(ppb)", "XYPflag",  "Latitude", "Longitude", "ws", "wd")]


##export flagged datafile
setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\flagged")
write.xlsx(as.data.frame(CurrentData_short), file = paste(NewName, ".xlsx" ,sep=""), col.names=TRUE, row.names = FALSE)

##Create MAX table from flagged dataset

#Make data long
CurrentData_MAXlong<- CurrentData_short[, c("DateTime", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")]

CurrentData_maxlong.data<- melt(CurrentData_MAXlong, id.vars = "DateTime")

CurrentData_MAX<- tapply(CurrentData_maxlong.data$value, CurrentData_maxlong.data$variable, max, na.rm=TRUE)
#View(CurrentData_MAX)

##export MAX table## 

setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\analysis")
write.xlsx(as.data.frame(CurrentData_MAX), file = paste(NewName, "MAX.xlsx" , sep="") , col.names=TRUE, row.names = TRUE)


##remove MD for graphing
#subset data removing MD flag for graphing - 
CurrentData_short_graph <- CurrentData_short
#CurrentData_short_graph<-CurrentData_short_graph[!(CurrentData_short_graph$H2Sflag == 'MD'), ]
#CurrentData_short_graph<-CurrentData_short_graph[!(CurrentData_short_graph$Ch4flag == 'MD'), ]

##export graphs
setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\plots")

##chiffon chart time series H2S, CH4, BEN
CurrentData_LC<- CurrentData_short_graph[, c("DateTime", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")]
CurrentData_LClong.data<- melt(CurrentData_LC, id.vars = "DateTime")

png(paste(NewName, ".png" ,sep=""), width=1200, height=618)
CurrentData_LC<-ggplot(CurrentData_LClong.data, aes(x=DateTime, y=value, color=factor(variable)))+geom_point()+ 
  facet_grid(variable ~ ., scales="free_y")+ylab("")+
  xlab("Time")+ggtitle(NewName)+ theme(strip.background = element_rect(fill='lemonchiffon1'))
print(CurrentData_LC)
dev.off()


##H2S only with line
CurrentData_H2SLC<- CurrentData_short_graph[, c("DateTime", "H2S(ppb)")]
CurrentData_H2Slong.data<- melt(CurrentData_H2SLC, id.vars = "DateTime")

png(paste(NewName, "_H2S.png" ,sep=""), width=1200, height=618)
CurrentData_H2S<-ggplot(CurrentData_H2Slong.data, aes(x=DateTime, y=value, color=factor(variable)))+geom_point()+ 
  facet_grid(variable ~ ., scales="free_y")+ylab("")+
  geom_hline(yintercept=7.86, color="blue", size=1, linetype="dashed")+ annotate("text", min(CurrentData_H2Slong.data$DateTime), 5, label="MDL")+
  geom_hline(yintercept=-7.76, color="blue", size=1, linetype="dashed")+ 
  xlab("Time")+ggtitle(paste (NewName, " H2S (ppb)", sep=""))+ theme(strip.background = element_rect(fill='lemonchiffon1'))
print(CurrentData_H2S)
dev.off()


##CH4 only with line
CurrentData_CH4LC<- CurrentData_short_graph[, c("DateTime", "CH4(ppm)")]
CurrentData_CH4long.data<- melt(CurrentData_CH4LC, id.vars = "DateTime")

png(paste(NewName, "_CH4.png" ,sep=""), width=1200, height=618)
CurrentData_CH4<-ggplot(CurrentData_CH4long.data, aes(x=DateTime, y=value, color=factor(variable)))+geom_point()+ 
  facet_grid(variable ~ ., scales="free_y")+ylab("")+
  geom_hline(yintercept=0.00157, color="blue", size=1, linetype="dashed")+ annotate("text", min(CurrentData_CH4long.data$DateTime), .05, label="MDL")+
  geom_hline(yintercept=-0.00157, color="blue", size=1, linetype="dashed")+ 
  xlab("Time")+ggtitle(paste (NewName, " CH4 (ppm)", sep="")) + theme(strip.background = element_rect(fill='lemonchiffon1'))
print(CurrentData_CH4)
dev.off()

##BEN only with line
CurrentData_BENLC<- CurrentData_short_graph[, c("DateTime", "BEN(ppb)")]
CurrentData_BENlong.data<- melt(CurrentData_BENLC, id.vars = "DateTime")

png(paste(NewName, "_BEN.png" ,sep=""), width=1200, height=618)
CurrentData_BEN<-ggplot(CurrentData_BENlong.data, aes(x=DateTime, y=value, color=factor(variable)))+geom_point()+ 
  facet_grid(variable ~ ., scales="free_y")+ylab("")+
  geom_hline(yintercept=4.80, color="blue", size=1, linetype="dashed")+ annotate("text", min(CurrentData_BENlong.data$DateTime), .05, label="MDL")+
  geom_hline(yintercept=-4.80, color="blue", size=1, linetype="dashed")+ 
  xlab("Time")+ggtitle(paste (NewName, " BEN (ppb)", sep=""))+ theme(strip.background = element_rect(fill='lemonchiffon1'))
print(CurrentData_BEN)
dev.off()


MaxTable<-rbind(MaxTable, CurrentData_MAX)

}

##export 
setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\analysis")
write.xlsx(as.data.frame(MaxTable), file = "MaxTable.xlsx", col.names=TRUE, row.names = FALSE)









####STATIONARY: ####

##start of loop
#creating max table 

setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\Stationary")
FILES <- list.files( pattern = ".txt")

MaxTableST<-c()

for (i in 1:length(FILES)) {
  
  setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\Stationary")
  
  CurrentDataST=read.table(file=FILES[i], skip = 34, header = TRUE, sep = "", fill = TRUE, 
                           stringsAsFactors = FALSE, na.strings = "NaN", col.names = c( "Date", "Time", "CO2", "CH4", "H2S", "C2H2", "M3DSU", "M3DSV", "M3DSW", 
                                                                                        "M3DS2DSpeed", "M3DS3DSpeed", "M3DSAzimuth", "M3DSElevation", 
                                                                                        "M3DSSoS", "M3DSSonicTemp", "M3DSErrorCode", "GPSTrack", 
                                                                                        "GPSGroundSpeed", "GPSTime", "GPSLatitude", "GPSLongitude", 
                                                                                        "GPSDoP", "BEN", "TOL", "ETB", "XYO", "XYM", "XYP", "SO2", 
                                                                                        "STY", "FOR", "NH3", "NO", "NO2", "O3", "CellPressure", 
                                                                                        "CellTemperature", "SUMMAPressure", "VOC", "AirMar.Wind.Speed..m.s.", 
                                                                                        "AirMar.Wind.Direction..TRUE.", "AirMarAirTemp[oC]", "AirMarRelHumidity[%]", 
                                                                                        "AirMarBarometer[mBar]", "AirMarHeading", "AirMarGroundSpeed[MPH]", 
                                                                                        "AirMar.Latitude", "AirMar.Longitude"))
  
  ####CurrentDataST - substituting file name with generic stationary call####
  
  ##enter script to automatically name dataset; adding "Stationary" name to name extracted from files#
  x<-FILES[i] 
  x<-substr(x,1,11)
  NewName<-paste("StationaryStellantis",x,sep="")
  
  #data already imported, format date 
  CurrentDataST_short<- CurrentDataST[, c("Date", "Time","CH4", "H2S", "BEN", "TOL", "ETB", "XYO", "XYM", 
                                          "XYP", "STY", "AirMar.Latitude", "AirMar.Longitude",
                                          "AirMar.Wind.Speed..m.s.", "AirMar.Wind.Direction..TRUE.")]
  
  #data already imported, format and concatenate DateTime variable
  CurrentDataST_short$DateTime<- as.POSIXct(paste(CurrentDataST_short$Date, CurrentDataST_short$Time), format = "%m/%d/%y %H:%M:%S", tz = "GMT")
  
  #Round to 2 decimal places
  CurrentDataST_short$H2S<-round(CurrentDataST_short$H2S, digits = 2) 
  CurrentDataST_short$CH4<-round(CurrentDataST_short$CH4, digits = 2) 
  CurrentDataST_short$BEN<-round(CurrentDataST_short$BEN, digits = 2)
  CurrentDataST_short$TOL<-round(CurrentDataST_short$TOL, digits = 2)
  CurrentDataST_short$XYP<-round(CurrentDataST_short$XYP, digits = 2)
  
  
  #drop date and time
  
  CurrentDataST_short<- CurrentDataST_short[, c("DateTime", "CH4", "H2S", "BEN", "TOL", "XYP", "AirMar.Latitude", "AirMar.Longitude",
                                                "AirMar.Wind.Speed..m.s.", "AirMar.Wind.Direction..TRUE.")]
  
  #View(CurrentDataST_short)
  #flag variables (MDL)
  #flag variables (MDL)
  CurrentDataST_short$H2Sflag<-" " #add a flag column; 
  CurrentDataST_short$H2Sflag[CurrentDataST_short$H2S < -7.86] <-"MD"    ##< MDL  
  CurrentDataST_short$H2Sflag[CurrentDataST_short$H2S >= -7.86 & CurrentDataST_short$H2S <= 7.86 ] <-"ND" ## |MDL|
  CurrentDataST_short$H2Sflag[CurrentDataST_short$H2S >= 7.87 & CurrentDataST_short$H2S < 23.58 ] <-"PQ" ##Practical quantitation limit; 3 x MDL
  CurrentDataST_short$H2Sflag[CurrentDataST_short$H2S >= 50690.0 ] <-"EH" ##Estimated; exceeds upper range
  
  CurrentDataST_short$CH4flag<-" " #add a flag column; 
  CurrentDataST_short$CH4flag[CurrentDataST_short$CH4 < -0.00157] <-"MD"    ##< MDL  
  CurrentDataST_short$CH4flag[CurrentDataST_short$CH4 >= -0.00157 & CurrentDataST_short$CH4 <= 0.00157 ] <-"ND" ## |MDL|
  CurrentDataST_short$CH4flag[CurrentDataST_short$CH4 >= 0.00158 & CurrentDataST_short$CH4 < 0.00471 ] <-"PQ" ##Practical quantitation limit; 3 x MDL
  CurrentDataST_short$CH4flag[CurrentDataST_short$CH4 >= 200 ] <-"EH" ##Estimated; exceeds upper range
  
  CurrentDataST_short$BENflag<-" " #add a flag column; 
  CurrentDataST_short$BENflag[CurrentDataST_short$BEN < -4.80] <-"MD"    ##< MDL  
  CurrentDataST_short$BENflag[CurrentDataST_short$BEN>= -4.80 & CurrentDataST_short$BEN <= 4.80 ] <-"ND" ## |MDL|
  CurrentDataST_short$BENflag[CurrentDataST_short$BEN >= 4.81 & CurrentDataST_short$BEN < 24 ] <-"PQ" ##Practical quantitation limit; 3 x MDL
  CurrentDataST_short$BENflag[CurrentDataST_short$BEN >= 106 ] <-"EH" ##Estimated; exceeds upper range
  
  CurrentDataST_short$TOLflag<-" " #add a flag column; 
  CurrentDataST_short$TOLflag[CurrentDataST_short$TOL < -3.69] <-"MD"    ##< MDL  
  CurrentDataST_short$TOLflag[CurrentDataST_short$TOL >= -3.69 & CurrentDataST_short$TOL <= 3.69 ] <-"ND" ## |MDL|
  CurrentDataST_short$TOLflag[CurrentDataST_short$TOL >= 3.70 & CurrentDataST_short$TOL < 18.45 ] <-"PQ" ##Practical quantitation limit; 3 x MDL
  CurrentDataST_short$TOLflag[CurrentDataST_short$TOL >= 98 ] <-"EH" ##Estimated; exceeds upper range
  
  CurrentDataST_short$XYPflag<-" " #add a flag column; 
  CurrentDataST_short$XYPflag[CurrentDataST_short$XYP < -4.05] <-"MD"    ##< MDL  
  CurrentDataST_short$XYPflag[CurrentDataST_short$XYP>= -4.05 & CurrentDataST_short$XYP <= 4.05 ] <-"ND" ## |MDL|
  CurrentDataST_short$XYPflag[CurrentDataST_short$XYP >= 4.06 & CurrentDataST_short$XYP < 20.25 ] <-"PQ" ##Practical quantitation limit; 3 x MDL
  CurrentDataST_short$XYPflag[CurrentDataST_short$XYP >= 101.9 ] <-"EH" ##Estimated; exceeds upper range
  
  
  
  
  #rename
  names(CurrentDataST_short)[names(CurrentDataST_short) == 'H2S'] <- 'H2S(ppb)'
  names(CurrentDataST_short)[names(CurrentDataST_short) == 'CH4'] <- 'CH4(ppm)'
  names(CurrentDataST_short)[names(CurrentDataST_short) == 'BEN'] <- 'BEN(ppb)'
  names(CurrentDataST_short)[names(CurrentDataST_short) == 'TOL'] <- 'TOL(ppb)'
  names(CurrentDataST_short)[names(CurrentDataST_short) == 'XYP'] <- 'XYP(ppb)'
  
  
  
  names(CurrentDataST_short)[names(CurrentDataST_short) == 'AirMar.Latitude'] <- 'Latitude'
  names(CurrentDataST_short)[names(CurrentDataST_short) == 'AirMar.Longitude'] <- 'Longitude'
  names(CurrentDataST_short)[names(CurrentDataST_short) == 'AirMar.Wind.Speed..m.s.'] <- 'ws'
  names(CurrentDataST_short)[names(CurrentDataST_short) == 'AirMar.Wind.Direction..TRUE.'] <- 'wd'
  #View(CurrentDataST_short)
  
  
  ##reorder and export flagged file 
  CurrentDataST_short<-CurrentDataST_short[, c("DateTime", "H2S(ppb)", "H2Sflag", "CH4(ppm)", "CH4flag", "BEN(ppb)", "BENflag", 
                                               "TOL(ppb)", "TOLflag", "XYP(ppb)", "XYPflag",  "Latitude", "Longitude", "ws", "wd")]
  
  
  ##export flagged datafile
  setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\flagged")
  write.xlsx(as.data.frame(CurrentDataST_short), file = paste(NewName,".xlsx", sep=""), col.names=TRUE, row.names = FALSE)
  
  ##Create MAX table from flagged dataset
  #Make data long
  CurrentDataST_MAXlong<- CurrentDataST_short[, c("DateTime", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")]
  
  CurrentDataST_maxlong.data<- melt(CurrentDataST_MAXlong, id.vars = "DateTime")
  #View(CurrentDataST_maxlong.data)
  CurrentDataST_MAX<- tapply(CurrentDataST_maxlong.data$value, CurrentDataST_maxlong.data$variable, max, na.rm=TRUE)
  #View(CurrentDataST_MAX)
  
  
  ##remove MD for graphing
  #subset data removing MD flag for graphing - 
  CurrentDataST_short_graph <- CurrentDataST_short
  #View(CurrentDataST_short_graph)
  #CurrentDataST_short_graph<-CurrentDataST_short_graph[!(CurrentDataST_short_graph$H2Sflag == 'MD'), ]
  #CurrentDataST_short_graph<-CurrentDataST_short_graph[!(CurrentDataST_short_graph$Ch4flag == 'MD'), ]
  
  
  ##export graphs
  setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\plots")
  
  #library(openair)
  
  ##chiffon chart time series H2S, CH4, BEN
  CurrentDataST_LC<- CurrentDataST_short_graph[, c("DateTime", "H2S(ppb)", "CH4(ppm)", "BEN(ppb)", "TOL(ppb)", "XYP(ppb)")]
  #View(CurrentDataST_LC)
  CurrentDataST_LClong.data<- melt(CurrentDataST_LC, id.vars = "DateTime")
  #View(CurrentDataST_LClong.data)
  #str(CurrentDataST_LClong.data)
  
  png(paste(NewName, ".png" ,sep=""), width=1200, height=618)
  
  CurrentDataST_LC<-ggplot(CurrentDataST_LClong.data, aes(x=DateTime, y=value, color=factor(variable)))+geom_point()+ 
    facet_grid(variable ~ ., scales="free_y")+ylab("")+
    # geom_hline(yintercept=3.41, color="blue", size=1, linetype="dashed")+ annotate("text", min(CurrentDataST_LClong.data$Time), 5, label="MDL")+
    #  geom_hline(yintercept=-3.41, color="blue", size=1, linetype="dashed")+
    xlab("Time")+ggtitle(NewName)+ theme_set(theme_grey(base_size = 20))
  
  print(CurrentDataST_LC)
  dev.off()
  
  
  ##H2S only with line
  CurrentDataST_H2SLC<- CurrentDataST_short_graph[, c("DateTime", "H2S(ppb)")]
  #View(CurrentDataST_short_graph)
  #View(CurrentDataST_H2SLC)
  CurrentDataST_H2Slong.data<- melt(CurrentDataST_H2SLC, id.vars = "DateTime")
  #View(CurrentDataST_H2Slong.data)
  
  png(paste(NewName, "_H2S.png" ,sep=""), width=1200, height=618)
  CurrentDataST_H2S<-ggplot(CurrentDataST_H2Slong.data, aes(x=DateTime, y=value, color=factor(variable)))+geom_point()+ 
    facet_grid(variable ~ ., scales="free_y")+ylab("")+
    geom_hline(yintercept=7.86, color="blue", size=1, linetype="dashed")+ annotate("text", min(CurrentDataST_H2Slong.data$DateTime), 5, label="MDL")+
    geom_hline(yintercept=-7.86, color="blue", size=1, linetype="dashed")+ 
    xlab("Time")+ggtitle(paste(NewName, "H2S (ppb)", sep=""))+ theme_set(theme_grey(base_size = 20))
  print(CurrentDataST_H2S)
  dev.off()
  
  
  ##CH4 only with line
  CurrentDataST_CH4LC<- CurrentDataST_short_graph[, c("DateTime", "CH4(ppm)")]
  CurrentDataST_CH4long.data<- melt(CurrentDataST_CH4LC, id.vars = "DateTime")
  
  png(paste(NewName, "_CH4.png" ,sep=""), width=1200, height=618)
  CurrentDataST_CH4<-ggplot(CurrentDataST_CH4long.data, aes(x=DateTime, y=value, color=factor(variable)))+geom_point()+ 
    facet_grid(variable ~ ., scales="free_y")+ylab("")+
    geom_hline(yintercept=0.00157, color="blue", size=1, linetype="dashed")+ annotate("text", min(CurrentDataST_CH4long.data$DateTime), .05, label="MDL")+
    geom_hline(yintercept=-0.00157, color="blue", size=1, linetype="dashed")+ 
    xlab("Time")+ggtitle(paste(NewName, "CH4 (ppm)", sep=""))+ theme_set(theme_grey(base_size = 20))
  print(CurrentDataST_CH4)
  dev.off()
  
  ##BEN only with line
  CurrentDataST_BENLC<- CurrentDataST_short_graph[, c("DateTime", "BEN(ppb)")]
  CurrentDataST_BENlong.data<- melt(CurrentDataST_BENLC, id.vars = "DateTime")
  
  png(paste(NewName, "_BEN.png" ,sep=""), width=1200, height=618)
  CurrentDataST_BEN<-ggplot(CurrentDataST_BENlong.data, aes(x=DateTime, y=value, color=factor(variable)))+geom_point()+ 
    facet_grid(variable ~ ., scales="free_y")+ylab("")+
    geom_hline(yintercept=4.80, color="blue", size=1, linetype="dashed")+ annotate("text", min(CurrentDataST_BENlong.data$DateTime), .05, label="MDL")+
    geom_hline(yintercept=-4.80, color="blue", size=1, linetype="dashed")+ 
    xlab("Time")+ggtitle(paste(NewName, "BEN (ppb)", sep=""))+ theme_set(theme_grey(base_size = 20))
  print(CurrentDataST_BEN)
  dev.off()
  
  setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\plots")
  
  
  #rename variables for openair format
  #View(CurrentDataST_short_graph)
  names(CurrentDataST_short_graph)[names(CurrentDataST_short_graph) == 'DateTime'] <- 'time'
  names(CurrentDataST_short_graph)[names(CurrentDataST_short_graph) == 'ws(m/s)'] <- 'ws'
  names(CurrentDataST_short_graph)[names(CurrentDataST_short_graph) == 'wd'] <- 'wd'
  
  
  #H2S polarplot##
  png(paste(NewName, "_H2SPPMEAN.png", sep=""), width=800, height=800)
  CurrentDataST_H2SPP<-polarPlot(CurrentDataST_short_graph, pollutant = "H2S(ppb)", min.bin = 1, statistic = "mean", 
                                 main = paste(NewName, "- H2S(ppb)", sep=""))
  print(CurrentDataST_H2SPP)
  dev.off()
  
  #H2S polarplot##
  png(paste(NewName, "_H2SPPMAX.png", sep=""), width=800, height=800)
  CurrentDataST_H2SPP<-polarPlot(CurrentDataST_short_graph, pollutant = "H2S(ppb)", min.bin = 1, statistic = "max", 
                                 main = paste(NewName, "- H2S(ppb)", sep=""))
  print(CurrentDataST_H2SPP)
  dev.off()
  
  #H2S pollution rose
  png(paste(NewName, "_H2SPR.png", sep=""), width=800, height=800)
  CurrentDataST_H2SPR<-pollutionRose(CurrentDataST_short_graph, pollutant = "H2S(ppb)",  
                                     main = paste(NewName, "H2S Rose (ppb)", sep=""))
  print(CurrentDataST_H2SPR)
  dev.off()
  
  
  #CH4 polarplot##
  png(paste(NewName, "_CH4PPMAX.png", sep=""), width=800, height=800)
  CurrentDataST_CH4PP<-polarPlot(CurrentDataST_short_graph, pollutant = "CH4(ppm)", min.bin = 1, statistic = "max", 
                                 main = paste(NewName, "- CH4(ppm) MAX", sep=""))
  print(CurrentDataST_CH4PP)
  dev.off()
  
  #CH4 pollution rose
  png(paste(NewName, "_CH4PR.png", sep=""), width=800, height=800)
  CurrentDataST_CH4PR<-pollutionRose(CurrentDataST_short_graph, pollutant = "CH4(ppm)",  
                                     main = paste(NewName, "CH4 Rose (ppm)", sep=""))
  print(CurrentDataST_CH4PR)
  dev.off()
  
  #BEN polarplot##
  png(paste(NewName, "_BENPPMAX.png", sep=""), width=800, height=800)
  CurrentDataST_CH4PP<-polarPlot(CurrentDataST_short_graph, pollutant = "BEN(ppb)", min.bin = 1, statistic = "max", 
                                 main = paste(NewName, "- BEN(ppb) MAX", sep=""))
  print(CurrentDataST_CH4PP)
  dev.off()
  
  #BEN pollution rose
  png(paste(NewName, "_BENPR.png", sep=""), width=800, height=800)
  CurrentDataST_BENPR<-pollutionRose(CurrentDataST_short_graph, pollutant = "BEN(ppb)",  
                                     main = paste(NewName, "BEN Rose (ppb)", sep=""))
  print(CurrentDataST_CH4PR)
  dev.off()
  
  
  MaxTableST<-rbind(MaxTableST, CurrentDataST_MAX)
  
  
}

##export flagged datafile
setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP season\\Detroit\\analysis")
write.xlsx(as.data.frame(MaxTableST), file = "MaxTableST.xlsx", col.names=TRUE, row.names = FALSE)
















































































####QA####


setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP SEASON\\Detroit\\Calibration")
FILES <- list.files( pattern = ".txt")

for (i in 1:length(FILES)) {
  
setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP SEASON\\Detroit\\Calibration")
FILES <- list.files( pattern = ".txt")  
  

CurrentDataCal=read.table(file=FILES[i], skip = 26, header = TRUE, sep = "", fill = TRUE, 
                  stringsAsFactors = FALSE, na.strings = "NaN", col.names = c( "Date", "Time", "CO2", "CH4", "H2S", "C2H2", "M3DSU", "M3DSV", "M3DSW", 
                                                                               "M3DS2DSpeed", "M3DS3DSpeed", "M3DSAzimuth", "M3DSElevation", 
                                                                               "M3DSSoS", "M3DSSonicTemp", "M3DSErrorCode", "GPSTrack", 
                                                                               "GPSGroundSpeed", "GPSTime", "GPSLatitude", "GPSLongitude", 
                                                                               "GPSDoP", "BEN", "TOL", "ETB", "XYO", "XYM", "XYP", "SO2", 
                                                                               "STY", "FOR", "NH3", "NO", "NO2", "O3", "CellPressure", 
                                                                               "CellTemperature", "SUMMAPressure", "VOC", "AirMar.Wind.Speed..m.s.", 
                                                                               "AirMar.Wind.Direction..TRUE.", "AirMarAirTemp[oC]", "AirMarRelHumidity[%]", 
                                                                               "AirMarBarometer[mBar]", "AirMarHeading", "AirMarGroundSpeed[MPH]", 
                                                                               "AirMar.Latitude", "AirMar.Longitude"))

##enter script to automatically name dataset; adding "Calibration" name to name extracted from files#
x<-FILES[i] 
x<-substr(x,1,11)
NewName<-paste("CalibrationDetroit",x,sep="")



#RFD030821_CA02 <- read_csv("Calibration210308_CA02.csv", 
#                           col_types = cols(Date = col_date(format = "%m/%d/%y"), 
#                                            Time = col_time(format = "%H:%M:%S"), 
#                                            X1 = col_skip()))
#View(RFD030821_CA02)


#format and concatenate DateTime variable

#MI191022_CA03$DateTime<- as.POSIXct(paste(MI191022_CA03$Date,MI191022_CA03$Time), format = "%m/%d/%y %H:%M:%S", tz = "GMT")



CurrentDataCal_short<- CurrentDataCal[, c("Date","Time","CH4", "H2S", "BEN", "TOL", "XYP")]

#flag variables (MDL)

CurrentDataCal_short$H2Sflag<-"AZ" #add a flag column; QA
CurrentDataCal_short$CH4flag<-"AZ" #add a flag column; QA
CurrentDataCal_short$BENflag<-"AZ" #add a flag column; QA
CurrentDataCal_short$TOLflag<-"AZ" #add a flag column; QA
CurrentDataCal_short$XYPflag<-"AZ" #add a flag column; QA


#Round to 2 decimal places
CurrentDataCal_short$H2S<-round(CurrentDataCal_short$H2S, digits = 2) 
CurrentDataCal_short$CH4<-round(CurrentDataCal_short$CH4, digits = 2)
CurrentDataCal_short$BEN<-round(CurrentDataCal_short$BEN, digits = 2)
CurrentDataCal_short$TOL<-round(CurrentDataCal_short$TOL, digits = 2)
CurrentDataCal_short$XYP<-round(CurrentDataCal_short$XYP, digits = 2)

#rename
names(CurrentDataCal_short)[names(CurrentDataCal_short) == 'H2S'] <- 'H2S(ppb)'
names(CurrentDataCal_short)[names(CurrentDataCal_short) == 'CH4'] <- 'CH4(ppm)'
names(CurrentDataCal_short)[names(CurrentDataCal_short) == 'BEN'] <- 'BEN(ppb)'
names(CurrentDataCal_short)[names(CurrentDataCal_short) == 'TOL'] <- 'TOL(ppb)'
names(CurrentDataCal_short)[names(CurrentDataCal_short) == 'XYP'] <- 'XYP(ppb)'


##reorder and export flagged file 
CurrentDataCal_short<-CurrentDataCal_short[, c("Date", "Time", "H2S(ppb)", "H2Sflag", "CH4(ppm)", "CH4flag", "BEN(ppb)", "BENflag",
                                           "TOL(ppb)", "TOLflag", "XYP(ppb)", "XYPflag")]

setwd("C:\\Users\\Mfuoco\\OneDrive - Environmental Protection Agency (EPA)\\2021 GMAP SEASON\\Detroit\\Calibration")
write.xlsx(as.data.frame(CurrentDataCal_short), file = paste(NewName, ".xlsx", sep=""), col.names=TRUE, row.names = FALSE)

}

