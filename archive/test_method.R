library(tidyverse)
library(readxl)
df1 <- read_excel("data/Copy of COVID-19-geographic-disbtribution-worldwide.xlsx")
df1$dateRep <- as.Date(df1$dateRep)
unique(df1$countriesAndTerritories)

#choose drop down list country
#question:  how to determine length of series for control limits?  Arbitrary 20?   Use Lynda F trick of adding 0.1 to zero after first reported value?

df1_US <- df1 %>% filter(countriesAndTerritories == "United_States_of_America") %>% arrange(dateRep)

#determine when to start the series for U.S. March 1

df1_US_deaths <- df1_US %>% filter(dateRep >= as.Date("2020-03-01"))

# p0 <- ggplot(data=df1_US_deaths,aes(x=dateRep,y=deaths))+
#   theme_bw()+
#   geom_point()+
#   geom_line()+
#   labs(title="U.S.deaths", caption="Source: https://ourworldindata.org/coronavirus-source-data, 27 Mar 2020")
# 
# p0

#LF trick
nudge_zero <- function(x){
  if(identical(x,0)){
    x <- 0.1
  }
  return(x)
}

#NA conversion
zero_NA <- function(x){
  if(identical(x,0)){
    x <- NA
  }
  return(x)
}

df1_US_deaths$deaths_nudge <- unlist(lapply(df1_US_deaths$deaths,zero_NA))

df1_US_deaths$log_count_deaths <- log10(df1_US_deaths$deaths_nudge)
df1_US_deaths$serial_day <- c(1:nrow(df1_US_deaths))

data_use <- df1_US_deaths
buffer <- 7

p0 <- ggplot(data=data_use,aes(x=dateRep,y=deaths))+
  theme_bw()+
  geom_point(size=rel(2.0),colour="blue")+
  geom_line()+
  labs(title="U.S.Daily New Deaths", caption="Source: https://ourworldindata.org/coronavirus-source-data, 27 Mar 2020")+
  xlab("Date")+
  xlim(min(data_use$dateRep),max(data_use$dateRep)+buffer)

p0

#now compute the linear regression for the log10 counts
lm_out <- lm(data=data_use,data_use$log_count_deaths ~ data_use$serial_day)

#check plot
p1<- ggplot(data=data_use,aes(x=serial_day,y=log_count_deaths))+
  theme_bw()+
  geom_point()+
  geom_line()
  
p2 <- p1+ geom_smooth(method=lm)

#should handle the break in the series more elegantly

cchart_df <- data.frame(data_use[!is.na(data_use$log_count_deaths),c("dateRep","serial_day")],
                        lm_out$residuals,c(NA,diff(lm_out$residuals)),lm_out$fitted.values)



AvgMR <- mean(abs(cchart_df$lm_out.residuals))
cchart_df$UCL <- lm_out$fitted.values+2.66*mean(AvgMR,na.rm=TRUE)
cchart_df$LCL <- lm_out$fitted.values-2.66*mean(AvgMR,na.rm=TRUE)

#buffer with buffer days beyond max date
buffer_serial_day <- seq(from=max(cchart_df$serial_day)+1,to=max(cchart_df$serial_day)+buffer,by=1)
predicted_value <- lm_out$coefficients[1]+ lm_out$coefficients[2]*buffer_serial_day
buffer_dates <- seq.Date(from=max(cchart_df$dateRep)+1,to=max(cchart_df$dateRep)+buffer,by="day")
buffer_df <- cbind.data.frame(buffer_dates,
                              buffer_serial_day,
                              rep(NA,buffer),
                              rep(NA,buffer),
                              predicted_value,
                              predicted_value +2.66*mean(AvgMR,na.rm=TRUE),
                              predicted_value - 2.66*mean(AvgMR,na.rm=TRUE))

names(buffer_df) <- names(cchart_df)


df_out <- rbind(cchart_df,buffer_df)

df_out$predict <- 10^df_out$lm_out.fitted.values
df_out$UCL_anti_log <- 10^df_out$UCL
df_out$LCL_anti_log <- 10^df_out$LCL

p3 <- p0 + geom_line(data=df_out,aes(x=dateRep,y=predict),linetype="solid",colour="red")+
        geom_line(data=df_out,aes(x=dateRep,y=UCL_anti_log),linetype="dotted")+
        geom_line(data=df_out,aes(x=dateRep,y=LCL_anti_log),linetype="dotted")

p3
