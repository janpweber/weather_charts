require(weatherData)
require(lubridate)
require(dplyr)
# checkDataAvailabilityForDateRange("TXL", "1997-01-01", today(),
#                                   station_type = "airportCode")
#### FUNCTIONS ####

clean_weather_data <- function(weatherdf){
        names(weatherdf)[1:4] <- c("Date", "C_max", "C_mean", "C_min")
        weatherdf$DayOfYear    <- yday(weatherdf$Date)
        weatherdf$Year         <- year(weatherdf$Date)
        weatherdf$Month        <- month(weatherdf$Date, label = T)
        weatherdf$DayOfMonth   <- mday(weatherdf$Date)
        # date needs conversion, as dplyr doesn't like POSIXlt, 
        # which is the date format before we convert it to POSIXct       
        weatherdf$Date <- as.POSIXct(weatherdf$Date)
        return(weatherdf)
}

#### DOWNLOAD WEATHER DATA ####

### COMPLETE DATA FOR LOCATION TXL FROM 1997 ###

## potentially load (previously built) local data frame to reduce download volume 
if(file.exists("TXLfile.RData"))load("TXLfile.RData")
ifelse(exists("TXL"), 
       last_date <- max(TXL$Date) %>% as.Date(), 
       last_date <- "1997-01-01")

## get weather data up to today for data frame TXL
## there is a limit to how many days can be retrieved, 
## so this needs to be repeated
while(last_date != today()){
        new_weather_data <- getWeatherForDate("TXL", 
                                              start_date = last_date+1,
                                              end_date = today(),
                                              opt_custom_columns = T,
                                              custom_columns = c(1:23))
        
        new_weather_data<- new_weather_data %>% 
                select(-2) # second column had incosistent name and was not necessary
        ## if no data are yet available for the current date, stop the loop:
        if(as.Date(new_weather_data[1,1]) == 0)break()
        ## otherwise proceed by combining the old with new data
        new_weatherTXL <- rbind(TXL, new_weather_data)
        TXL <- new_weatherTXL
        last_date <- as.Date(max(TXL$Date))
        
}

# object.size(TXL)
## saves the data frame TXL to reduce future download volume
save(TXL, file = "TXLfile.RData") 


#### CLEAN UP WEATHER DATA ####

weather_df <- clean_weather_data(TXL)

#### INSPECT DATA ####

weather_df %>% 
        group_by(Year) %>% 
        summarise(count = n()) %>% 
        filter(count < 356) #years 2000, 2004, 2016 are incomplete

#### PLOT COLORFUL SUNS ####
require(ggplot2)

## these "sun" plots may take some time to compute
## but you could filter for years (as shown in the commented-out section)
weather_df %>% 
        # filter(Year == 2015) %>% 
        ggplot(aes(x=DayOfYear, y= C_max, fill=Month, color=Month))+
        geom_bar(stat = "identity")+
        scale_y_continuous(limits = c(-50, 40))+
        coord_polar()+
        facet_wrap(~Year, ncol = 7)+ # use as many columns as deemed necessary
        theme_classic()+
        theme(legend.position = "none",
              axis.text.x = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks  = element_blank())+
        labs(x = "", y = "")


#### PLOT TUFTE-STYLE PLOT BY MONTH ####

## the aim of this section is to plot this year's (or last year's or whatever)
## daily maximum temperature as a bar and put it into perspective with previous
## data, here from the last 5 years (2011-2015)
## (mean indicated by dot, a line indicating the spread (min to max))
require(ggthemes)

max_plot_day <- weather_df %>% 
        filter(Year == 2016) %>% 
        summarise(max(DayOfYear)) %>% 
        as.numeric()

## preparing averages and ranges:
averages <- weather_df %>% 
        filter(Year >= 2011, Year <= 2015) %>% # filter the years for averaging
        filter(DayOfYear <= max_plot_day) %>%  
        group_by(Month, DayOfMonth) %>% 
        summarise(C_5y_average = mean(C_max),
                  C_5y_max = min(C_max),
                  C_5y_min = max(C_max)) %>%
        ungroup()
        
## plotting:
weather_df %>% 
        filter(Year == 2016) %>% 
        ggplot(aes(x = DayOfMonth, y = C_max))+
        geom_bar(stat = "identity", width = 0.5, fill = "grey40")+
        geom_linerange(data = averages, 
                       inherit.aes = F, 
                       aes(x= DayOfMonth, ymin=C_5y_min, ymax=C_5y_max))+
        geom_point(data = averages, 
                   inherit.aes = F, 
                   aes(x = DayOfMonth, y = C_5y_average))+
        geom_hline(yintercept = 0)+
        theme_bw()+
        facet_wrap(~Month, ncol=4)

#### GETTING AN IDEA FOR THE TEMPERATURE DEVELOPMENT ####
### PLOT AVERAGE DAILY TEMPERATURES ###
##!! needs some work, e.g. what does the moving average mean? shouldn't it be one-sided?
##!! using moving average of moving average without understanding it... also not good
##!! play around with different moving averages?

weather_df %>% 
        group_by(Month, DayOfMonth) %>% 
        mutate(C_max = mean(C_max)) %>%
        ungroup() %>% 
        filter(Year == 1997) %>% # just need one year's data
        ggplot(aes(x = DayOfMonth, y = C_max))+
        geom_bar(stat = "identity", width = 0.5, fill = "grey40")+
        facet_wrap(~Month, ncol=4)


granularity <- 7 #odd number for calculating moving average (7 = average of number +/- 3 surrounding days) 
avg_filter <- rep(1/granularity, granularity)

difday <- weather_df %>% 
        group_by(Month, DayOfMonth) %>% 
        summarise(C_ave_max = mean(C_max, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(diff = C_ave_max - lag(C_ave_max),
               movaverage = stats::filter(diff, avg_filter, sides = 2, circular = T),
               movaverage2 = stats::filter(movaverage, avg_filter, sides = 2, circular = T))
        
difday %>% ggplot(aes(x = DayOfMonth, y = movaverage))+
        #geom_line()+
        geom_ribbon(aes(ymin = 0, ymax = movaverage2), fill = "red")+
        # geom_bar(stat = "identity", aes(y = movaverage2), fill = "black")+
        # geom_bar(stat = "identity", fill = "grey40")+
        facet_wrap(~Month, ncol=4)+
        theme_bw()


        


#### THE QUEST FOR HOTTEST TEMPERATURES ####
##!! needs work, e.g. restructuring
# select for each day the max temperature:
maxC_df<- weatherTXL %>% group_by(Month, DayOfMonth) %>% summarise(C_max = max(C_max)) %>% ungroup()
# data frame to be matched
maxC_Year_df <- weatherTXL %>% select(Month, DayOfMonth, C_max, Year, DayOfYear)
# resulting data frame, joining the max temperature with the corresponding year(s)
test <- left_join(maxC_df, maxC_Year_df)

test %>% group_by(Month, DayOfMonth) %>% summarise(count =n()) %>% filter(count>1) %>% ungroup() # which days max were present more than once

## COLOURFUL SUNS WITH HOTTEST DAYS INDICATED by thick red bars
weatherTXL %>% 
        filter(Year == 2015) %>% 
        ggplot(aes(x=DayOfYear, y= C_max, fill=Month, color=Month))+
        geom_bar(stat = "identity")+
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


#plot how many hottest days each year had
test %>% ggplot(aes(x = Year))+
        geom_bar()+ facet_wrap(~Month)
#let's look at the values:
test_df <- test %>% select(Year) %>% table() %>% as.data.frame() 
###############
#which is the hottest year on average?
weatherTXL %>% group_by(Year) %>% summarise(average = mean(C_max)) %>% filter(average >10) %>%  qplot(Year, average, data = .)

#######

#### SCRATCH ####
##!! now i remember: plot average and current Cmax and their difference!

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







