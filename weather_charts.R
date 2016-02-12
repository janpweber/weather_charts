##test commit
checkDataAvailabilityForDateRange("TXL", "1997-01-01", "2015-12-19",
                                  station_type = "airportCode")
#### DOWNLOAD WEATHER DATA ####
require(weatherData)

weatherTXL2014 <- getWeatherForYear("TXL", "2014")
weatherTXL2013 <- getWeatherForYear("TXL", "2013")
weatherTXL2012 <- getWeatherForYear("TXL", "2012")
weatherTXL2011 <- getWeatherForYear("TXL", "2011")
weatherTXL2010 <- getWeatherForYear("TXL", "2010")
TXL <- getWeatherForDate("TXL", 
                         start_date = "2013-01-01",
                         end_date = "2015-12-19",
                         opt_custom_columns = T,
                         custom_columns = c(1:23))
#
#nam <- paste("A", 5, sep = "")
#assign(nam, TXL)

#1997:2013

### MAKE ONE DATA FRAME ###
weatherTXL <- rbind(weatherTXL2014,
                    weatherTXL2013,
                    weatherTXL2012,
                    weatherTXL2011,
                    weatherTXL2010)

### SAVE DATA FRAME weatherTXL ###
save(weatherTXL, file = "weatherTXLfile.RData")
load("weatherTXLfile.RData")

### CLEAN UP WEATHER DATA ###
require(lubridate)
weatherTXL$DayOfYear    <- yday(weatherTXL$Date)
weatherTXL$Year         <- year(weatherTXL$Date)
weatherTXL$Month        <- month(weatherTXL$Date, label = T)

weatherTXL$Date <- as.POSIXct(weatherTXL$Date) # conversion needed, dplyr doesn't like
                                                # POSIXlt, which is the previous format

### PLOT COLORFUL SUNS ###
require(ggplot2)
p <- ggplot(weatherTXL, aes(x=DayOfYear, y= Max_TemperatureC, fill=Month, color=Month))

p <- p+ geom_bar(stat = "identity")+
        scale_y_continuous(limits = c(-50, 40))+
        #        scale_x_date(brakes = date_breaks(width = "1 month"))+
        coord_polar()+
        facet_wrap(~Year, ncol = 3)+
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks  = element_blank())+
        labs(x = "", y = "")

print(p)

### 
require(dplyr)
require(tidyr)
difference <- weatherTXL %>%
        filter(Month %in% c("Jan", "Feb", "Mar"))%>%
        filter(Year %in% c("2010", "2014"))%>%
        select(DayOfYear, Year, Max_TemperatureC)%>%
        spread(Year, Max_TemperatureC)%>%
        mutate(Diff = `2014`-`2010`)%>%
        select(Diff)

pdata <- weatherTXL %>%
        filter(Month %in% c("Jan", "Feb", "Mar"))%>%
        filter(Year %in% c("2010", "2014"))%>%
        cbind(difference) %>%
        filter(Year == 2010)


p <- ggplot(pdata, aes(x=DayOfYear, y= Diff))

p <- p+ geom_line()+
        theme_classic()
print(p)

### PLOT per Month

weatherTXL2014$Date <- as.POSIXct(weatherTXL2014$Date)
weatherTXL2014$Month        <- month(weatherTXL2014$Date,
                                 label = T,
                                 abbr = F)
weatherTXL2014$DayOfMonth <- mday(weatherTXL2014$Date)
pdata<- weatherTXL2014
#pdata$minfive[pdata$Max_TemperatureC <= -5 ] <- -5 

#pdata$Cm5to5[pdata$Max_TemperatureC <= 0 & pdata$Max_TemperatureC >= -5] <- pdata$Max_TemperatureC[pdata$Max_TemperatureC <= 0 & pdata$Max_TemperatureC >= -5]
#pdata$C0to5[pdata$Max_TemperatureC <=5 & pdata$Max_TemperatureC >=0] <- pdata$Max_TemperatureC[pdata$Max_TemperatureC <=5 & pdata$Max_TemperatureC >=0]
#pdata$C5to10[pdata$Max_TemperatureC <=10 & pdata$Max_TemperatureC >=5] <- pdata$Max_TemperatureC[pdata$Max_TemperatureC <=10 & pdata$Max_TemperatureC >=5] 
pdata$Cm5[pdata$Max_TemperatureC<  -5 ] <- -5 
pdata$C5[pdata$Max_TemperatureC  >   5 ] <-  5 
pdata$C10[pdata$Max_TemperatureC >  10 ] <- 10
pdata$C15[pdata$Max_TemperatureC >  15 ] <- 15
pdata$C20[pdata$Max_TemperatureC >  20 ] <- 20
pdata$C25[pdata$Max_TemperatureC >  25 ] <- 25
pdata$C30[pdata$Max_TemperatureC >  30 ] <- 30
# p <- ggplot(pdata, 
#             aes(x = Date, y = Max_TemperatureC), color = "black") +
#         geom_bar(stat = "identity")+
#         geom_bar(stat = "identity", aes(y = twenty), fill="#FF0000")+
#         geom_bar(stat = "identity", aes(y = ften), fill="#FFFF00")+
#         geom_bar(stat = "identity", aes(y = ten), fill="#00FFFF")+
#         geom_bar(stat = "identity", aes(y = ftt), fill ="#00FF00")+
#         geom_bar(stat = "identity", aes(y = ztf), fill ="#0000FF")+
#         geom_bar(stat = "identity", aes(y = minfive), fill ="#9933FF")+
#         facet_wrap(~Month,
#                    scales = "free_x",
#                    ncol = 4)
# p
# 
# pdata <- filter(pdata, Month == "February")
# p <- ggplot(pdata, 
#             aes(x = Date, y = Max_TemperatureC), colour = "#FF0000", fill = "white") +
#         geom_bar(stat = "identity")+
#         geom_bar(stat = "identity", aes(y = twenty))+
#         geom_bar(stat = "identity", aes(y = ften))+
#         geom_bar(stat = "identity", aes(y = ten))+
#         geom_bar(stat = "identity", aes(y = ftt))+
#         geom_bar(stat = "identity", aes(y = ztf))+
#         geom_bar(stat = "identity", aes(y = minfive))#+
#         #facet_wrap(~Month,
#                   # scales = "free_x",
#                   # ncol = 4)
# p
pdata <- filter(pdata, Month == "January")
thin <- 0.5
p <- ggplot(pdata, aes(x = DayOfMonth))+
        geom_bar(stat = "identity", aes(y=Max_TemperatureC), fill = NA, size = 1, color = "black", width = 0.8)+
        geom_bar(stat = "identity", aes(y=Cm5), fill = NA, size = thin, color = "black", width = 0.8)+
        geom_bar(stat = "identity", aes(y= C5) , fill = NA, size = thin, color = "black", width = 0.8)+
        geom_bar(stat = "identity", aes(y=C10) , fill = NA, size = thin, color = "black", width = 0.8)+
        geom_text(aes(y=0.3, label= DayOfMonth))+
        scale_y_continuous(minor_breaks = -10:15)+
        theme(panel.background = element_blank(),
              #panel.grid.major.x = element_blank(),
              #panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())
p
