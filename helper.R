#helper files

#NA functions

#LF trick  function
nudge_zero <- function(x){
  if(identical(x,0)){
    x <- 0.1
  }
  return(x)
}

#function to do NA conversion
zero_NA <- function(x){
  if(x==0){
    x <- NA
  }
  return(x)
}


# A function factory for getting integer y-axis values.
# from: https://joshuacook.netlify.com/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


#function to find index that marks first sequence of length_use values.  Default length = 8 per Lloyd Provost 30 March 2020 
index_test <- function(x,
                       index,
                       length_use=8){
  x_check <- x[index:(index+length_use - 1)]
  if(all(x_check>0)){
    use_seq <- TRUE
    index_use <- index
  } else {
    use_seq <- FALSE
    index_use <- index + 1
  }
  return(list(use_seq,index_use))
}

 
#function to take data frame, e.g. states df or countries df, a name for the country or state and returns a data 
#frame with three columns (1 row):  the "Date_of_first_death","date_of_c-chart_signal","shift_rule_signal"
#Shift rule signal is TRUE or FALSE.  It uses the Provost rule: look for at least 8 deaths before applying the 
#c-chart rules.
find_start_date_Provost <- function(data,
                                    location_name,
                                    start_date=start_date){
 
 
  df1_X <- data %>% filter(countriesAndTerritories == location_name) %>% arrange(dateRep)
  
  Rule_shift <- NA  
  
  #bound the length of the calculations, no more than cc_length records used to compute center line and upper limit
  #note that this parameter is NOT the same as the baseline parameter chosen by the user
  cc_length <- 20

  if(any(df1_X$deaths >0,na.rm=TRUE)) {
    
    dates_of_deaths <- df1_X$dateRep[which(df1_X$deaths>0)]
    
    #dates_of_deaths <- df1_X$dateRep[which(df1_X$deaths>0)]
    
    start_date_deaths <- dates_of_deaths[1]
    
    #allow the user to start the analysis after the date of first death
    if(length(start_date)==0) {
    
        start_date0 <- start_date_deaths
    
      } else start_date0 <- as.Date(max(start_date,start_date_deaths, na.rm=TRUE), origin="1970-01-01")
    
    df1_X_deaths <- df1_X %>% filter(dateRep >= start_date0)
    
    #Provost rule:   look for records that comprise the first 8 deaths. This is a parameter that could be changed so needs definition.
    #replace_na is from tidyr  https://stackoverflow.com/questions/25576358/calculate-cumsum-while-ignoring-na-values
    #because if upload a file with NA for deaths, the logic will fail,   Reliance on cumsum function to pull out the accumulated
    #runs above the central line is also problematic--if NAs are in the death column, cumsum function will not work correctly.
    Provost_start_count <- 8
    Rule_shift_length <- 8
    
    
    if(any(cumsum(replace_na(df1_X_deaths$deaths,0)) >= Provost_start_count)) {
      index_Provost <- min(which(cumsum(replace_na(df1_X_deaths$deaths,0)) >= Provost_start_count),na.rm=TRUE)
      
      #Australia_nudge named in honor of Australia which @4-12-2020 had an initial series length 25 and then c-chart signal at the next record
      Australia_nudge <- 5
      
      i <- index_Provost-1
      
      cc_length_adjusted <- max(cc_length,index_Provost+Australia_nudge, na.rm=TRUE)
      
      stop <- FALSE
      
      while(!stop) {
        test_series0 <- df1_X_deaths %>% filter(dateRep >= start_date0 & dateRep <= start_date0+i) %>% pull(deaths)
        
        #fix the limits at cc_length_adjusted if series has that many records
        
        if(length(test_series0) <= cc_length_adjusted){
          CL <- mean(test_series0, na.rm=TRUE)
          
          C_UCL <- CL + 3*sqrt(CL)
          
          x1 <- test_series0 > CL
          
          #https://stackoverflow.com/questions/48551492/count-consecutive-true-values-within-each-block-separately
          # the next calculation will fail if there are NA values for deaths--the logic test in the 
          #previous line will return NAs in the logical vector x1 corresponding to NAs in test_series0
          #
          x2 <- ave(x1, cumsum(!x1), FUN = cumsum)
          
        } else {
          #here we are implicitly restricting the c chart calculations to be no more than cc_length_adjusted records after the first death
          #if the series is shorter than cc_length_adjusted, just remove the NA values at the bottom of the test_series0 vector
          CL <-mean(test_series0[1:cc_length_adjusted],na.rm=TRUE)
          
          C_UCL <- CL + 3*sqrt(CL)
          
          x1 <- test_series0 > CL
          
          x2 <- ave(x1, cumsum(!x1), FUN = cumsum)
        }
        
        Rule_1 <- any(which(test_series0 > C_UCL))
        
        Rule_shift = any(x2 >= Rule_shift_length)
        
        if(Rule_1){
          index_start <- which.max(test_series0> C_UCL)
          
          start_date1 <- df1_X_deaths$dateRep[index_start]
          
          stop <- TRUE
          
        } else if(Rule_shift){
          index_start <- which.max(x2)
          
          start_date1 <- df1_X_deaths$dateRep[index_start]
          
          stop <- TRUE
        } else if(nrow(df1_X_deaths) > i) {
          #j <- j+1 
          i <- i+1
        } else {
          start_date1 <- NA
          CL <- CL
          C_UCL <- C_UCL
          stop <- TRUE
        }
      }
      #END OF THE LOOP CHECKING THAT THERE ARE RECORDS WITH DEATHS
    } else {
      start_date1 <- NA
      CL <- NA
      C_UCL <- NA
    }
  } else {
    start_date0 <- NA
    start_date1 <- NA
    CL <- NA
    C_UCL <- NA
  }
  
  #pass the key dates and C-chart information.  Note that any or all of these objects may be NA
  list(
    first_death = start_date0,
    c_chart_signal = start_date1,
    CL_out = CL,
    C_UCL_out = C_UCL)
  
}


#new function to label stages, MDEC created 4-7-2020 and modified to accept Provost starting rule
create_stages_Provost <- function(data1,date_cutoffs, baseline){
  data_stages <- list()
 
  # if date_cutoffs$first_death is NA (no deaths), stage1 is the whole data.frame 
  first_death_date <- date_cutoffs$first_death
  
  # if (is.na(first_death_date)) stage1 <- data1
  # else stage1 <- data1 %>% filter(dateRep < first_death_date)
  # 
  # stage1$stage <- 'Pre-deaths'
  # data_stages$stage1 <- stage1
  
  if(!is.na(first_death_date)) {
      if(first_death_date > min(data1$dateRep,na.rm=TRUE)) {
          stage1 <- data1 %>% filter(dateRep < first_death_date)
          stage1$stage <- 'Pre-deaths'
          data_stages$stage1 <- stage1
      }
  } else {
    stage1 <- data1
    
    if (nrow(stage1) > 0) {
      stage1$stage <- 'Pre-deaths'
      data_stages$stage1 <- stage1
    }
  }
  
  # If there has been a death, stage 2 starts on the day of the first death,
  
  if (!is.na(date_cutoffs$first_death)) {
    stage2 <- data1 %>% filter(dateRep >= date_cutoffs$first_death)
    
    
    
    # If c_chart_signal is observed, cut off stage2 before that date.
    if (!is.na(date_cutoffs$c_chart_signal)) {
      stage2 <- stage2 %>% filter(dateRep < date_cutoffs$c_chart_signal)
    }
    
    stage2$stage <- 'Deaths observed before c-chart signal'
    
    data_stages$stage2 <- stage2
    
  }
  
 
  # If there has been a c-chart signal observed, stage 3 begins with that date and is at 
  # least 5 subsequent days with deaths > 0, up to 20 (determined by value of baseline argument).
  min_length_chart <- 5
  
  if(!is.na(date_cutoffs$c_chart_signal)) {
    stage3 <- data1 %>% filter(dateRep >= date_cutoffs$c_chart_signal) 
    
    stage3_check <- stage3 %>% filter(deaths > 0)
    
    #stage3_short indicates whether or not there are sufficient records to fit the exponential after the c-chart signal
    stage3_short <- FALSE
    
    if(nrow(stage3_check) >= min_length_chart) {
      
        stage3 <- head(stage3, baseline)
        
        stage3$stage <- 'Exponential growth and fit'
        
        data_stages$stage3 <- stage3
      
      # If there has been a c-chart signal observed, and there's enough data after the c-chart signal
      # start plus baseline period, make that stage 4  NEED TO ACCOUNT FOR ZEROS IN STAGE 3
      # because these embedded zeros will be set to NA in the subsequent calculation of linear model
      # when we take the log10 of deaths.
      
        #count the number of records that have 0 in the death series for stage 3  
        #count_zeros <- length(stage3$deaths[stage3$deaths==0])
        
        #stage4 <- data1 %>% filter(dateRep >= date_cutoffs$c_chart_signal + baseline + count_zeros)
        stage4 <- data1 %>% filter(dateRep > max(stage3$dateRep))
        
        if(nrow(stage4)> 0) {
          
            stage4$stage <- 'Observations after exponential limits established'
          
            data_stages$stage4 <- stage4
        }
      } else {
        stage3_short <- TRUE
        
        stage3$stage <- 'Not enough data to fit exponential'
        
        data_stages$stage3 <- stage3
    }
      
  } 
  

  data_out <- dplyr::bind_rows(data_stages)
}


#function to subset master data set by location_name, start_date and pad by buffer_days
#creates a list of objects:  
# (1) df1_X: data frame will all raw data starting with record of first day reported death(s), records identified by stages
# (2) df_exp_fit:  data frame with fitted values from lm of log10(deaths), including a buffer number of records past last data day
# (3) lm_out:  the linear model fitted to the log10(deaths)
# (4) date_cutoffs:  a list of the date of first death(s), the date of c chart signal marking start of exp fit series, the center line
#                         of the c-chart (may be NULL) and the UCL of the c-chart (may be NULL)
make_location_data <- function(data,
                               location_name,
                               buffer_days,
                               baseline,
                               start_date){
 
  #create an object that will have data frames, dates of stages and the linear model fit
  #I can create data frames with 0 rows and dates of stages with NA values.   What about a 'null' linear model?
  data_results_list <- list()
 
  df1_X <- data %>% filter(countriesAndTerritories == location_name) %>% arrange(dateRep)
  #dates_of_deaths <- df1_X$dateRep[which(df1_X$deaths>0)]

  #initialize two list entries that are conditionally calculated by the rest of the function  Create empty df, not null object!
  #the first df will have 0 rows and the list will have length 0.
  data_results_list$df_exp_fit <- data.frame()
  data_results_list$lm_out <- list()
  
  date_cutoffs <- find_start_date_Provost(data = df1_X, location_name = location_name, start_date = start_date)
  
  data_results_list$date_cutoffs <- date_cutoffs
   
  df1_X <- create_stages_Provost(data=df1_X,date_cutoffs=date_cutoffs,
                         baseline = baseline)

  df1_X$deaths_nudge <- df1_X %>% pull(deaths)
  
  #filter the data to just the deaths series. Assumes that if there is a name, there is at least one record in the data table.
  #Need to allow for a series that has only 0 deaths.
   if(any(df1_X$deaths > 0, na.rm=TRUE)) {  
      df1_X <- df1_X %>% filter(stage != "Pre-deaths")
      
      #may not need to rename stage to stage_data--that is legacy of bug fixing on 4-10, stage is a function name.
      names(df1_X)[names(df1_X)=="stage"] <- "stage_data"
      
      data_results_list$df1_X <- df1_X
      
      #if there are data in stage 2, then we will plot those data in run chart if <= 8 deaths and no control chart signal
      #if there are data in stage 3, we need to calculate the information
      #check if any embedded zeros in stage 3:  we will convert zero to NA on supposition that a zero in this phase represents missing data
      #create a new variable deaths_nudge to represent the adjusted death series
      ################# Allow over-ride of the calculated start_date by the user chosen start_date.#################
      #exp_fit_min_length is the shortest number of records to use for the log of deaths and linear fit
      exp_fit_min_length <- 5
      
      
      #Build the linear model if there are sufficient records and calculate the anti logs of prediction and limits
       if(!is.na(date_cutoffs$c_chart_signal) )  { 
          df1_X_exp_fit <- df1_X %>% filter(stage_data=='Exponential growth and fit')
          
          #check for sufficient stage 3 values to calculate the exponential growth control limits
          
          if(nrow(df1_X_exp_fit)>= exp_fit_min_length) {
                #replace any deaths_nudge value = 0 with NA
                df1_X_exp_fit$deaths_nudge <- unlist(lapply(df1_X_exp_fit$deaths_nudge,zero_NA))
                
                df1_X_exp_fit$log_10_deaths <- log10(df1_X_exp_fit$deaths_nudge)
                
                df1_X_exp_fit$serial_day <- c(1:nrow(df1_X_exp_fit))
                
                #allow the use of a different baseline, user defined input
                df1_X_exp_fit <- df1_X_exp_fit %>% filter(serial_day <= baseline)
                
                lm_out <- lm(data=df1_X_exp_fit,df1_X_exp_fit$log_10_deaths ~ df1_X_exp_fit$serial_day)
                
                data_results_list$lm_out <- lm_out
                
                #update the df1_X component of the output list?  Leave any 0 value in the df?
                
                #should make a prediction for the NA values-- find the value and insert
                cchart_df <- data.frame(df1_X_exp_fit[!is.na(df1_X_exp_fit$log_10_deaths),c("dateRep","serial_day","deaths","log_10_deaths")],
                                        lm_out$residuals,c(NA,diff(lm_out$residuals)),lm_out$fitted.values)
                
                names(cchart_df)[5] <- "differences"
                
                #AvgMR <- mean(abs(cchart_df$differences),na.rm=TRUE)
                #use median moving range as interim fix for removing large ranges from initial fit and repeating calcs
                MedMR <- median(abs(cchart_df$differences),na.rm=TRUE)
                
                cchart_df$UCL <- lm_out$fitted.values + 3.14*MedMR
                
                cchart_df$LCL <- lm_out$fitted.values - 3.14*MedMR
                
                cchart_df$stage_data <- df1_X_exp_fit[!is.na(df1_X_exp_fit$log_10_deaths),] %>% pull(stage_data)
                
                df_exp_fit <- cchart_df
                
              #check for any values in stage 4; compute them
              
              if(any(df1_X$stage_data=='Observations after exponential limits established')) {
                 df1_X_post_fit <- df1_X %>% filter(stage_data=='Observations after exponential limits established')
                  #check the serial day values what is the max?  Error in df output, jump in serial day from 27 to 101??
                 nrows_post_fit <- nrow(df1_X_post_fit)  
                 
                 start_index <- max(df1_X_exp_fit$serial_day)+1
                 
                 df1_X_post_fit$serial_day <- seq(from=start_index, length.out=nrows_post_fit,by=1)
                 
                 #I should check for reported 0 deaths in this epoch just like in stage 3
                 df1_X_post_fit$deaths_nudge <- unlist(lapply(df1_X_post_fit$deaths_nudge,zero_NA))
                 
                 df1_X_post_fit$log_10_deaths <- log10(df1_X_post_fit$deaths_nudge)
                 
                 check_predicted_value <- lm_out$coefficients[1]+ lm_out$coefficients[2]*df1_X_post_fit$serial_day
                 
                 stage_data <- df1_X_post_fit %>% pull(stage_data)
                 
                 df_post_fit_out <- cbind.data.frame(df1_X_post_fit[,c("dateRep","serial_day","deaths","log_10_deaths")],
                                                  rep(NA,nrows_post_fit),
                                                  rep(NA,nrows_post_fit),
                                                  check_predicted_value,
                                                  check_predicted_value + 3.14*MedMR,
                                                  check_predicted_value - 3.14*MedMR,
                                                  stage_data,stringsAsFactors=FALSE)
                 
                 names(df_post_fit_out) <- names(df_exp_fit)
                 
                 df_exp_fit <- rbind.data.frame(df_exp_fit,df_post_fit_out)
                }
            
                 #now add the buffer
                #buffer with buffer days beyond max date
                serial_day_buffer_start <- max(df_exp_fit$serial_day)+1
                
                buffer_dates <- seq.Date(from=max(df_exp_fit$dateRep)+1,to=max(df_exp_fit$dateRep)+buffer_days,by="day")
                
                buffer_serial_day <- seq(from=serial_day_buffer_start,to=serial_day_buffer_start+buffer_days-1,by=1)
                
                predicted_value <- lm_out$coefficients[1]+ lm_out$coefficients[2]*buffer_serial_day
                
                buffer_df <- cbind.data.frame(buffer_dates,
                                              buffer_serial_day,
                                              rep(NA,buffer_days),
                                              rep(NA,buffer_days),
                                              rep(NA,buffer_days),
                                              rep(NA,buffer_days),
                                              predicted_value,
                                              predicted_value + 3.14*MedMR,
                                              predicted_value - 3.14*MedMR,
                                              rep(NA,buffer_days))
                
                names(buffer_df) <- names(df_exp_fit)
                
                df_exp_fit <- rbind.data.frame(df_exp_fit,buffer_df)
                
                df_exp_fit$predict <- 10^df_exp_fit$lm_out.fitted.values
                
                df_exp_fit$UCL_anti_log <- 10^df_exp_fit$UCL
                
                df_exp_fit$LCL_anti_log <- 10^df_exp_fit$LCL
                
                data_results_list$df_exp_fit <- df_exp_fit
          }
          
       }  
   }   
      
   #make conditional:   output is df1_X, date_cutoffs, AND lm_out could be NULL and df_exp_fit could be NULL
  return(data_results_list)
}

#this function makes the exponential chart, the log10 chart, the c-chart and death chart, along
#with descriptive message
#requires location name, buffer days, a list of data objects that is output from function make_location_dat
#title for charts and caption for main display
make_charts <- function(location_use,
                        buffer,
                        make_data,
                        title1,
                        caption_use,
                        constrain_y_axis){
  
  df_no_fit <- make_data$df1_X
  df_fit <- make_data$df_exp_fit
  lm_fit <- make_data$lm_out
  first_death_date <- make_data$date_cutoffs$first_death
  exp_growth_date <- make_data$date_cutoffs$c_chart_signal
  c_chart_CL <- make_data$date_cutoffs$CL_out
  c_chart_UCL <- make_data$date_cutoffs$C_UCL_out

 
  #Here is the outline of the conditional logic that follows:
  #    if(no deaths)
  #         {create empty graph lists}
  #    else if (no c-chart signal)
  #          if(too few points for c-chart)
  #             {make simple chart}
  #          else
  #             {make c-chart}
  #    else #there is a c-chart signal
  #           if(too few points for expo fit)
  #             {add expo phase points to c-chart}
  #           else if(95% CI for slope contains 0 or any negative values)
  #             {add expo phase points to c-chart and label:  no sign of expo growth after c-chart special cause}
  #           else #we can fit the exponential and overlay c-chart
  #             {make exponential charts}
  
  if(is.na(first_death_date)) {
    p_out1 <- list()
    
    p_out2 <- list()
    
    message_out <- "No reported deaths"
    
  } else if(is.na(exp_growth_date)) { #if there is no exponential growth, define plots we can make
    
    index_Provost <- min(which(cumsum(replace_na(df_no_fit$deaths,0)) >=8),na.rm=TRUE)
  
        if(nrow(df_no_fit) < index_Provost) {
          #p_out1 is simple plot of points in time order, p_out2 is empty list
          p_out1 <- ggplot(data=df_no_fit,
                           aes(x=dateRep,y=deaths))+
            theme_bw()+
            geom_point(size=rel(2.5))+
            labs(title = title1)+
            theme(plot.title=element_text(size=rel(1.5)))+
            xlab("")+
            ylab("")+
            xlim(as.Date(NA),max(df_no_fit$dateRep)+buffer)+
            #theme(axis.title.y=element_text(size=rel(1),angle=0,vjust=0.5))+
            theme(axis.text.y=element_text(size=rel(1.5)))+
            theme(axis.text.x=element_text(size=rel(1.5)))+
            scale_y_continuous(breaks = integer_breaks(),limits=c(0,2*max(df_no_fit$deaths,na.rm=TRUE)))
            
            
          
          p_out2 <- list()
          
          message_out <- "Series too short to create a c-chart"
      
        } else  {p_out1 <- ggplot(data=df_no_fit,
                                  aes(x=dateRep,y=deaths))+
                            theme_bw()+
                            geom_point(size=rel(3.0))+
                            geom_line()+
                            labs(title = title1,
                                 subtitle = "c-chart center line (solid) and upper limit (dashed)",
                                 caption = caption_use)+
                            theme(plot.title=element_text(size=rel(1.5)))+
                            xlab("")+
                            ylab("")+
                            xlim(as.Date(NA),max(df_no_fit$dateRep)+buffer)+
                            #theme(axis.title.y=element_text(size=rel(1),angle=0,vjust=0.5))+
                            theme(axis.text.y=element_text(size=rel(1.5)))+
                            theme(axis.text.x=element_text(size=rel(1.5)))+
                            geom_hline(yintercept=c_chart_CL)+
                            geom_hline(yintercept=c_chart_UCL,linetype="dashed")+
                            scale_y_continuous(breaks = integer_breaks(),limits=c(0,2*max(df_no_fit$deaths,na.rm=TRUE)))
                            
        
                p_out2 <- list()
                
                message_out <- "c-chart only"
        }
  }  else if(nrow(df_fit)==0) {
    #now plot c chart with extra points df_fit needs a minimum of five records with non-zero deaths
          p_out1 <- ggplot(data=df_no_fit,
                           aes(x=dateRep,y=deaths,shape=stage_data))+
            theme_bw()+
            geom_point(size=rel(3.0))+
            geom_line()+
            labs(title = title1,
                 subtitle = "c-chart center line (solid) and upper limit (dashed)",
                 caption = caption_use,
                 shape = "Data stage")+
            theme(plot.title=element_text(size=rel(1.5)))+
            xlab("")+
            ylab("")+
            xlim(as.Date(NA),max(df_no_fit$dateRep)+buffer)+
            geom_hline(yintercept=c_chart_CL)+
            geom_hline(yintercept=c_chart_UCL,linetype="dashed")+
            #theme(axis.title.y=element_text(size=rel(1),angle=0,vjust=0.5))+
            theme(axis.text.y=element_text(size=rel(1.5)))+
            theme(axis.text.x=element_text(size=rel(1.5)))+
            scale_y_continuous(breaks = integer_breaks(),limits=c(0,2*max(df_no_fit$deaths,na.rm=TRUE)))+
            theme(legend.position = c(0.05, 0.95),
                  legend.justification = c("left", "top"))
          
          p_out2 <- list()
          
          message_out <- "c-chart plus values after initial signal"
          
  } else if(confint(lm_fit,'df1_X_exp_fit$serial_day',level= 0.95)[1] < 0) {
      #lm_fit$coefficients[2]
      #points after special cause signal lead to negative slope estimate. 
      #alternatively, compute 95% confidence interval and require CI for slope to have lower point > 0
      # conf_int_slope <- confint(lm_fit,'df1_X_exp_fit$serial_day',level= 0.95)[1]
      p_out1 <- ggplot(data=df_no_fit,
                         aes(x=dateRep,y=deaths))+
                theme_bw()+
                geom_point(size=rel(3.0))+
                geom_line()+
                labs(title = title1,
                     subtitle = "c-chart center line (solid) and upper limit (dashed);daily deaths after first special cause signal do not show exponential growth",
                     caption = caption_use)+
                theme(plot.title=element_text(size=rel(1.5)))+
                #theme(axis.title.y=element_text(size=rel(1),angle=0,vjust=0.5))+
                xlab("")+
                ylab("")+
                xlim(as.Date(NA),max(df_no_fit$dateRep)+buffer)+
                theme(axis.text.y=element_text(size=rel(1.5)))+
                theme(axis.text.x=element_text(size=rel(1.5)))+
                geom_hline(yintercept=c_chart_CL)+
                geom_hline(yintercept=c_chart_UCL,linetype="dashed")+
                scale_y_continuous(breaks = integer_breaks(),limits=c(0,2*max(df_no_fit$deaths,na.rm=TRUE)))
                
          
          p_out2 <- list()
          
          message_out <- "c-chart plus values after initial signal, no sign of exponential growth"
          
  } else {
        #exponential plots
        p0 <- ggplot(data=df_fit,aes(x=dateRep,y=deaths,shape=stage_data))+
          theme_bw()+
          geom_point(size=rel(3.0),colour="blue")+
          geom_line()+
          labs(title=title1, 
               caption = caption_use,
               shape = "Data stage") +
          xlab("")+
          ylab("")+
          #ylab("Deaths per day")+
          # xlim(min(df_fit$dateRep),max(df_fit$dateRep)+buffer)+
          theme(axis.text.x=element_text(size=rel(1.5)))+
          theme(axis.text.y=element_text(size=rel(1.5)))+
          theme(axis.title.x=element_text(size=rel(1)))+
          #theme(axis.title.y=element_text(size=rel(1),angle=0,vjust=0.5))+
          theme(title=element_text(size=rel(1.5))) +
          theme(plot.caption = element_text(hjust = 0))
        
        #overlay the exponential fit and the limits
        # p_out <- p0 + geom_line(data=df_fit,aes(x=dateRep,y=predict),linetype="solid",colour="red")+
        #   geom_line(data=df_fit,aes(x=dateRep,y=UCL_anti_log),linetype="dotted")+
        #   geom_line(data=df_fit,aes(x=dateRep,y=LCL_anti_log),linetype="dotted")
        
        p_out <- p0 + geom_line(aes(x=dateRep,y=predict),linetype="solid",colour="red")+
          geom_line(aes(x=dateRep,y=UCL_anti_log),linetype="dotted")+
          geom_line(aes(x=dateRep,y=LCL_anti_log),linetype="dotted")
        
        #overlay the portion of the c-chart up to the point of the signal
        start_date <- min(df_no_fit$dateRep)
        
        end_date <- exp_growth_date - 1
        
        p_out1 <- p_out + geom_point(data=df_no_fit[df_no_fit$dateRep < exp_growth_date,],
                                     aes(x=dateRep,y=deaths,shape=stage_data))+
          geom_segment(aes(x=start_date, xend=end_date, y=c_chart_CL, yend=c_chart_CL))+
          geom_segment(aes(x=start_date, xend=end_date, y=c_chart_UCL, yend=c_chart_UCL),linetype="dashed")+
          xlim(min(df_no_fit$dateRep),max(df_fit$dateRep))+
          theme(legend.position = c(0.05, 0.95),
                legend.justification = c("left", "top")) #,
                #legend.margin = margin())
        
        
        if (constrain_y_axis) {
          p_out1 <- p_out1 + scale_y_continuous(
            limits = c(0, 2*max(df_fit$deaths, na.rm = TRUE))
          )
        }   
        
        #retrict to the values used in the linear fit to plot the log chart
        #df_cchart1 <- df_cchart %>% filter(serial_day <= baseline1)
        
        p_out2 <- ggplot(data=df_fit,aes(x=dateRep,y=log_10_deaths,shape=stage_data))+
          theme_bw()+
          
          geom_line(data=df_fit,aes(x=dateRep,y=lm_out.fitted.values))+
          geom_line(data=df_fit,aes(x=dateRep,y=UCL),linetype="dotted")+
          geom_line(data=df_fit,aes(x=dateRep,y=LCL),linetype="dotted") +
        
          geom_point(size=rel(2.5),colour="blue")+
          geom_line()+
          labs(title = paste0(location_use," log10 Daily Reported Deaths"),
               subtitle = "Limits based on Individuals Shewhart chart calculations using regression residuals",
               shape = "Data stage")+
          ylab("")+
          xlab("")+
          theme(axis.text.x=element_text(size=rel(1.5)))+
          theme(axis.text.y=element_text(size=rel(1.5)))+
          theme(title=element_text(size=rel(1.5))) +
          #theme(axis.title.y=element_text(angle=0,vjust=0.5))+
          scale_shape_discrete(na.translate=FALSE)+
          theme(legend.position = c(0.05, 0.95),
                legend.justification = c("left", "top"))
        
        message_out <- "c-chart and exponential fit"
      }
  
  return(list(message_out=message_out,p_out1=p_out1,p_out2=p_out2))
  
}

#create the computation table
make_computation_table <- function(nobs_raw,
                                   nobs_fit,
                                   first_death_date,
                                   c_chart_signal,
                                   lm_fit,
                                   baseline_fit)
  # buffer,
  #start_date_analysis,
  #chart_message) 
{
  if(length(lm_fit)==0){
    intercept <- NA
    slope <- NA
    lower_conf_value <- NA
    doubling_daily_deaths <- NA
  } else {
    intercept <- lm_fit$coefficient[1]
    slope <- lm_fit$coefficient[2]
    if(lm_fit$coefficient[2] > 0) {
          doubling_daily_deaths <- log10(2)/lm_fit$coefficient[2]
    } else doubling_daily_deaths <- NA
    lower_conf_value <- confint(lm_fit,'df1_X_exp_fit$serial_day')[1]
  }
  #use the caption argument to title the table
  #caption = 'Table 1: This is a simple caption for the table.'
  

  parameter_names <- c("Observations since first reported death",
                       "Date of first reported death",
                       "Date of c-chart signal",
                       "Number of records used to fit the regression",
                       "Slope of log10 deaths vs day",
                       "Intercept of log10 deaths vs day",
                       "Days to double daily reported deaths, exponential phase",
                       "Lower limit of 95% CI for slope"
  )
  
  parameter_values <- c(as.character(nobs_raw),
                        as.character(first_death_date),
                        as.character(c_chart_signal),
                        as.character(baseline_fit),
                        as.character(round(slope,3)),
                        as.character(round(intercept,3)),
                        as.character(round(doubling_daily_deaths,1)),
                        as.character(round(lower_conf_value,3)))
  
  df_out <- cbind.data.frame(parameter_names, parameter_values)
  names(df_out) <- c("Parameter","Value")
  return(df_out)
  
}
