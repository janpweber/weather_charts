
checkDataAvailabilityForDateRange("TXL", "1997-01-01", "2015-12-19",
                                  station_type = "airportCode")
#### DOWNLOAD WEATHER DATA ####
require(weatherData)


### COMPLETE DATA FOR LOCATION TXL FROM 1997 ###
if(file.exists("TXLfile.RData"))load("TXLfile.RData")
ifelse(exists("TXL"), last_date <- as.Date(max(TXL$Date)), last_date <- "1997-01-01")

if(file.exists("TXLfile.RData")){
        load("TXLfile.RData")
        last_date <- max(TXL$Date) %>% as.Date()
} else(last_date <- "1997-01-01" %>% invisible()) # the invisible does not work here!


## get all new data weather data up to today
## there is a limit on how many days can be retrieved, so this needs to be repeated
while(last_date != today()){
        new_weather_data <- getWeatherForDate("TXL", 
                                              start_date = last_date+1,
                                              end_date = today(),
                                              opt_custom_columns = T,
                                              custom_columns = c(1:23))
        new_weather_data<- new_weather_data %>% select(-2) # second column had incosistent name and was not necessary
        new_weatherTXL <- rbind(TXL, new_weather_data) # combine
        TXL <- new_weatherTXL
        last_date <- as.Date(max(TXL$Date))
}

# object.size(TXL)
save(TXL, file = "TXLfile.RData")


### CLEAN UP WEATHER DATA ###
require(lubridate)

clean_weather_data <- function(weatherdf){
        names(weatherdf)[1:4] <- c("Date", "C_max", "C_mean", "C_min")
        weatherdf$DayOfYear    <- yday(weatherdf$Date)
        weatherdf$Year         <- year(weatherdf$Date)
        weatherdf$Month        <- month(weatherdf$Date, label = T)
        weatherdf$DayOfMonth   <- mday(weatherdf$Date)
        weatherdf$Date <- as.POSIXct(weatherdf$Date)# conversion needed, dplyr doesn't like POSIXlt, which is the previous format
        return(weatherdf)
}

### PLOT COLORFUL SUNS ###
require(ggplot2)
p <- ggplot(weatherTXL, aes(x=DayOfYear, y= C_max, fill=Month, color=Month))

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
        geom_linerange(data = averages, inherit.aes = F, aes(x= DayOfMonth, ymin=C_5y_min, ymax=C_5y_max))+
        geom_point(data = averages, inherit.aes = F, aes(x = DayOfMonth, y = C_5y_average))+
        geom_hline(yintercept = 0)+
        theme_bw()+
        facet_wrap(~Month, ncol=4, scales = "free")


###SCRATCH###
weatherTXL <- TXL %>% clean_weather_data()
weatherTXL %>% group_by(Year) %>% summarise(count = n()) %>% filter(count < 356) #years 2000, 2004, 2016 are incomplete

weatherTXL %>% group_by(Month, DayOfMonth) %>% mutate(C_max = mean(C_max)) %>%
        ungroup() %>% filter(Year == 1997) %>% 
        ggplot(aes(x = DayOfMonth, y = C_max))+
        geom_bar(stat = "identity", width = 0.5, fill = "grey40")+
        facet_wrap(~Month, ncol=4, scales = "free")

# select for each day the max temperature:
maxC_df<- weatherTXL %>% group_by(Month, DayOfMonth) %>% summarise(C_max = max(C_max)) %>% ungroup()
# data frame to be matched
maxC_Year_df <- weatherTXL %>% select(Month, DayOfMonth, C_max, Year, DayOfYear)
# resulting data frame, joining the max temperature with the corresponding year(s)
test <- left_join(maxC_df, maxC_Year_df)

test %>% group_by(Month, DayOfMonth) %>% summarise(count =n()) %>% filter(count>1) %>% ungroup() # which days max were present more than once

#plot how many hottest days each year had
test %>% ggplot(aes(x = Year))+
        geom_bar()+ facet_wrap(~Month)
#let's look at the values:
test_df <- test %>% select(Year) %>% table() %>% as.data.frame() 
###############
#which is the hottest year on average?
weatherTXL %>% group_by(Year) %>% summarise(average = mean(C_max)) %>% filter(average >10) %>%  qplot(Year, average, data = .)

#######

p <- ggplot(weatherTXL, aes(x=DayOfYear, y= C_max, fill=Month, color=Month))

p <- p+ geom_bar(stat = "identity")+
        scale_y_continuous(limits = c(-50, 40))+
        #        scale_x_date(brakes = date_breaks(width = "1 month"))+
        coord_polar()+
        facet_wrap(~Year, ncol = 7)+
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks  = element_blank())+
        labs(x = "", y = "")+
        geom_bar(data = test, stat = "identity", fill = "red", width = 5)

print(p)
