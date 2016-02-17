##test commit
checkDataAvailabilityForDateRange("TXL", "1997-01-01", "2015-12-19",
                                  station_type = "airportCode")
#### DOWNLOAD WEATHER DATA ####
require(weatherData)
weatherTXL2016 <- getWeatherForYear("TXL", "2016")
weatherTXL2015 <- getWeatherForYear("TXL", "2015")
weatherTXL2014 <- getWeatherForYear("TXL", "2014")
weatherTXL2013 <- getWeatherForYear("TXL", "2013")
weatherTXL2012 <- getWeatherForYear("TXL", "2012")
weatherTXL2011 <- getWeatherForYear("TXL", "2011")
weatherTXL2010 <- getWeatherForYear("TXL", "2010")
TXL <- getWeatherForDate("TXL", 
                         start_date = "1997-01-01",
                         end_date = "2015-12-19",
                         opt_custom_columns = T,
                         custom_columns = c(1:23)) %>% 
        select(-2)

### COMPLETE DATA FOR LOCATION TXL FROM 1997 ###

for(i in 1:20){
if(last_date != today()){
last_date <- as.Date(max(TXL$Date)) #check latest date available

new_weather_data <- getWeatherForDate("TXL", #get all new data up to today
                                      start_date = last_date+1,
                                      end_date = today(),
                                      opt_custom_columns = T,
                                      custom_columns = c(1:23))
new_weather_data<- new_weather_data %>% select(-2)
new_weatherTXL <- rbind(TXL, new_weather_data) # combine
TXL <- new_weatherTXL
last_date <- as.Date(max(TXL$Date))
}}
min(TXL$Date)
max(TXL$Date)
object.size(TXL)



names(new_weather_data)
names(TXL)
identical(names(new_weather_data), names(TXL))
comp <- cbind(names(new_weather_data), names(TXL))
View(comp)
### MAKE ONE DATA FRAME ###
weatherTXL <- rbind(weatherTXL2016,
                    weatherTXL2015,
                    weatherTXL2014,
                    weatherTXL2013,
                    weatherTXL2012,
                    weatherTXL2011,
                    weatherTXL2010)

### SAVE DATA FRAME weatherTXL ###
save(weatherTXL, file = "weatherTXLfile.RData")
load("weatherTXLfile.RData")

### UPDATE DATA FRAME

last_date <- as.Date(max(weatherTXL$Date)) #check latest date available

##!! there is a limit on how many days can be retrieved, so this needs to be repeated
new_weather_data <- getWeatherForDate("TXL", #get all new data up to today
                                      start_date = last_date+1,
                                      end_date = today())
new_weatherTXL <- rbind(weatherTXL, new_weather_data) # combine
weatherTXL <- new_weatherTXL


save(weatherTXL, file = "weatherTXLfile.RData") #save the new data object

### CLEAN UP WEATHER DATA ###
require(lubridate)
names(weatherTXL) <- c("Date", "C_max", "C_mean", "C_min")
weatherTXL$DayOfYear    <- yday(weatherTXL$Date)
weatherTXL$Year         <- year(weatherTXL$Date)
weatherTXL$Month        <- month(weatherTXL$Date, label = T)
weatherTXL$DayOfMonth <- mday(weatherTXL$Date)
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

### NO IDEA WHAT THIS WAS FOR!
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

pdata$Cm5[pdata$C_max <  -5 ] <- -5 
pdata$C5[pdata$C_max  >   5 ] <-  5 
pdata$C10[pdata$C_max >  10 ] <- 10
pdata$C15[pdata$C_max >  15 ] <- 15
pdata$C20[pdata$C_max >  20 ] <- 20
pdata$C25[pdata$C_max >  25 ] <- 25
pdata$C30[pdata$C_max >  30 ] <- 30
# p <- ggplot(pdata, 
#             aes(x = Date, y = C_max), color = "black") +
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
#             aes(x = Date, y = C_max), colour = "#FF0000", fill = "white") +
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
        geom_bar(stat = "identity", aes(y=C_max), fill = NA, size = 1, color = "black", width = 0.8)+
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

## TUFTE BY MONTH
require(ggthemes)
averages <- weatherTXL %>% 
        filter(Year >= 2011, Year < 2016) %>% 
        group_by(Month, DayOfMonth) %>% 
        summarise(C_5y_average = mean(C_max),
               C_5y_max = min(C_max),
               C_5y_min = max(C_max)) %>%
        ungroup()

weatherTXL %>% 
        filter(Year == 2016) %>% 
        ggplot(aes(x = DayOfMonth, y = C_max))+
        geom_bar(stat = "identity", width = 0.5, fill = "grey40")+
        #geom_hline(yintercept = seq(-10,25,5), color= "white")+ # does not work as I wanted, because grid is overplotted
        geom_linerange(data = averages, aes(ymin=C_5y_min, ymax=C_5y_max))+
        geom_point(data = averages, aes(y = C_5y_average))+
        #scale_x_continuous(breaks = 1:max(weatherTXL$DayOfMonth))+
        geom_hline(yintercept = 0)+
        theme_bw()+
        facet_wrap(~Month, ncol=4, scales = "free")
