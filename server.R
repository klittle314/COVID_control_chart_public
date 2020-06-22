source("global.R")
shinyServer(function(input, output, session) {
    
    print('Sys.info():')
    print(Sys.info())
    print('.Platform():')
    print(.Platform)
    print('R.version.string:')
    print(R.version.string)
    print('sessionInfo():')
    print(sessionInfo())
    
    output$data_warning <- renderUI({
      
      msg <- NULL
      
      if (!('Country-level ECDC data' %in% data_choices)) {
        msg <- paste0(msg, 'Country-level data not downloaded successfully. ')
      }
      
      if (!('US state-level NY Times data' %in% data_choices)) {
        msg <- paste0(msg, 'State-level data not downloaded successfully. ')
      }
      
      if (!is.null(msg)) {
        msg <- paste0(msg, 'This is likely a temporary issue with data sources - please try again later.')
        h5(msg, style='color: red;')
      }
    })
    
    df_upload <- reactiveVal(value = NULL)

    upload_data <- reactive({
        req(input$upload_data)
        
        try({
            read.csv(input$upload_data$datapath,
                     header = TRUE,
                     stringsAsFactors = FALSE)
        })
    })
    
    upload_message <- reactive({
        if ('try-error' %in% class(upload_data())) {
            
            msg <- 'There was a problem reading your file. Please confirm that it is in CSV format, and that you selected the correct file.'
          
            upload_confirm <- NULL
            
        } else if (!all(c('date', 'location') %in% colnames(upload_data()))) {
            
            missing_cols <- setdiff(c('date', 'location'), colnames(upload_data()))
            msg <- paste0('Columns missing from CSV file: ', paste0(missing_cols, collapse = ', '))
            
            upload_confirm <- NULL
            
        } else if (any(is.na(as.Date(upload_data()$date, format='%m/%d/%Y')))) {
            
            msg <- 'Please confirm date format is MM/DD/YYYY'
          
            upload_confirm <- NULL
          
        } else if (!any(sapply(upload_data(), class) %in% c('integer', 'numeric'))) {
          
            msg <- 'No numeric columns detected. Please add numeric columns.'
            
            upload_confirm <- NULL
            
        } else {
            
            upload_confirm <- renderUI({
                
                list(
                    tags$br(),
                    
                    h4('Preview'),
                    
                    DT::renderDataTable(
                        datatable(upload_data(),
                                  rownames = FALSE)),
                    
                    actionButton(
                        inputId = 'upload_confirm',
                        label   = 'Confirm'))
            })
            
            msg <- 'Data successfully uploaded and parsed. Scroll to bottom of table to click Confirm to complete the data entry.'
        }
      
        output$upload_confirm <- upload_confirm
      
        msg
    })
    
    output$upload_message <- renderUI({
        req(upload_message())
        
        h5(upload_message())
    })
    
    observeEvent(input$upload_confirm, {
        req(upload_data())
        
        numeric_cols <- colnames(upload_data())[sapply(upload_data(), class) %in% c('numeric', 'integer')]
          
        data_add <- upload_data()[c('date', 'location', numeric_cols)]
        colnames(data_add) <- c('dateRep', 'countriesAndTerritories', numeric_cols)
        
        data_add$dateRep <- as.Date(data_add$dateRep, format = '%m/%d/%Y')
        
        data_add <- dplyr::bind_rows(isolate(df_upload()), data_add)
        
        data_add <- unique(data_add)
        
        df_upload(data_add)
        
        updateSelectInput(
          session = session,
          inputId = 'data_source',
          choices = c(data_choices,
                      'User-uploaded data'),
          selected = 'User-uploaded data')
        
        updateSelectInput

        output$upload_confirm <- renderUI({
            list(
                tags$br(),
                
                h4('Data successfully added. Switch to Display tab to view.'))
          
        }) 
    })
    
    observeEvent(input$reset, {
      updateDateInput(session, "start_date",
                        value = defStartdate)
      
      updateNumericInput(session, "buffer",
                        value = defBuffer)
      
      updateNumericInput(session, "baseline_n",
                        value = defBaseline)
    })
    
    display_data <- reactive({
        req(input$data_source)
     
        if (input$data_source == 'Country-level ECDC data')           df_country
        else if (input$data_source == 'US state-level NY Times data') df_state
        else if (input$data_source == 'User-uploaded data')           df_upload()
    })
    
    event_name_choices <- reactive({
      req(display_data())
      
      setdiff(colnames(display_data()), c('dateRep', 'countriesAndTerritories','cum_cases', 'cum_deaths'))
    })
    
    observe({
      req(event_name_choices())
      
      updateSelectInput(
        session = session,
        inputId = 'event_name',
        choices = event_name_choices(),
        selected = event_name_choices()[1])
    })
    
    locations <- reactive({
      req(display_data())
      
      sort(unique(display_data()$countriesAndTerritories))
    })
    
    observe({
        selected <- isolate(input$choose_location)
        choices  <- locations()
        
        if (!(selected %in% choices)) selected <- choices[1]
        
        updateSelectInput(
            session = session,
            inputId = 'choose_location',
            choices = choices,
            selected = selected)
    })
    
    control_chart_caption <- reactive({
        req(input$data_source)
        
        if (input$data_source == 'Country-level ECDC data')           data_source <- 'https://opendata.ecdc.europa.eu/covid19/casedistribution/csv'
        else if (input$data_source == 'US state-level NY Times data') data_source <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
        else if (input$data_source == 'User-uploaded data')           data_source <- 'User-uploaded data file'
        
        sprintf('%s\n\nSource: %s, %s',
                input$chart_caption,
                data_source,
                as.character(Sys.Date()))
       
    })
    
    #make a list that has a frame of the original data, a frame to construct the limit chart, and the linear model
    make_data <- reactive({
        req(display_data(),
            locations(),
            input$choose_location %in% locations(),
            input$event_name)
        
        location_use <- input$choose_location
        data1 <- display_data()
        buffer <- input$buffer
        baseline1 <- input$baseline_n
        start_date_user <- input$start_date
       
        list_use <- make_location_data(data=data1,
                                       event_name = input$event_name,
                                       location_name=location_use,
                                       buffer_days=buffer,
                                       baseline=baseline1,
                                       start_date=start_date_user)
       
        return(list_use)
    })
    
    
    #Note do not use req(make_data()) as all of the values could be NULL or NA.
    #Revision of the construction of the charts
    control_chartNEW <- reactive({
      location_use <- input$choose_location
      buffer <- input$buffer
      baseline1 <- input$baseline_n
      title1 <- paste0(location_use, " Daily Reported ", input$event_name)
      caption_use <- control_chart_caption()
      constrain_y_axis <- input$constrain_y_axis
      
      make_data <- make_data()
      # df_no_fit <- make_data()$df1_X
      # df_fit <- make_data()$df_exp_fit
      # lm_fit <- make_data()$lm_out
      # first_death_date <- make_data()$date_cutoffs$first_death
      # exp_growth_date <- make_data()$date_cutoffs$c_chart_signal
      # c_chart_CL <- make_data()$date_cutoffs$CL
      # c_chart_UCL <- make_data()$date_cutoffs$UCL
    
      
      chart_list <- make_charts(location_use=location_use,buffer=buffer,
                                make_data=make_data,
                                event_name = input$event_name,
                                title1=title1,caption_use=caption_use,
                                constrain_y_axis = constrain_y_axis)
      
      #contents of chart_list:  message_out, p_out1, p_out2 
})
   
    
   
    output$control_chart <- renderPlot({
        
        req(control_chartNEW())
        
        if(control_chartNEW()$message_out != paste0("No reported ", input$event_name)) {
              print(control_chartNEW()$p_out1)
        }
    })
    
    output$log_control_chart <- renderPlot({
        
        req(control_chartNEW())
      
        if(control_chartNEW()$message_out == "c-chart and exponential fit") {
              print(control_chartNEW()$p_out2)
        }
    })
    
    output$download_chart <- downloadHandler(
      filename = function() {
            sprintf('%s_%s_days.png', input$choose_location, input$baseline_n)
        },
        content = function(file) {
            
            png(file, width = 1000, height = 600)
                print(control_chartNEW()$p_out1)
            dev.off(which=dev.cur())
        }
    )
    
    data_for_table <- reactive({
    
      event_name <- input$event_name
     
      #make the stuff that I want to use goes here
      message_out <- control_chartNEW()$message_out
      if(message_out %in% use_raw_table_messages) {
        df_out <- make_data()$df1_X[,c("dateRep","cases","deaths")]
        
        names(df_out) <- c("Date Reported", "Cases","Deaths")
        
        index_pred <- which(df_out$stage_data)
        
      } else if(message_out %in% use_new_expo_table_messages) {
        df_out <- make_data()$df_exp_fit[,c("dateRep","serial_day", input$event_name,
                                            "predict","LCL_anti_log","UCL_anti_log", "stage_data")]
          index_exp_fit <- which.min(df_out$stage_data == "Exponential growth and fit")
          #allow for input$buffer records to have exponential fit and limits to show in the table, else NA appear
          #prevents huge numbers from appearing in the table for serial day 'far' from end of expo fit period.
          index_check <- df_out$stage_data != "Exponential growth and fit" & df_out$serial_day >= index_exp_fit + input$buffer
          
          df_out$predict[index_check] <- NA
          
          df_out$LCL_anti_log[index_check] <- NA
          
          df_out$UCL_anti_log[index_check] <- NA
        
        
        names(df_out) <- c("Date Reported","Serial Day", event_name, paste0('Predicted ', event_name),'Lower Limit','Upper Limit', 'Stage')
        
        df_out[[paste0('Predicted ', event_name)]] <- round(df_out[[paste0('Predicted ', event_name)]],0)
        
        df_out$'Lower Limit' <- round(df_out$'Lower Limit',0)
        
        df_out$'Upper Limit' <- round(df_out$'Upper Limit',0)
        
        
      } else {
        
        df_out <- NULL
      }
      
      return(df_out)
    })
    
    output$message <- renderUI({
         h4(control_chartNEW()$message_out)
    })
       
   output$message2 <- renderUI({
        h4(control_chartNEW()$message_out)
   })
     
    output$data_table <- DT::renderDataTable({
        req(data_for_table())
       
      DT::datatable(data_for_table(),
                    rownames=FALSE)
    })
    
   
    parameters_for_table <- reactive({
      req(make_data())
      # df_no_fit <- make_data()$df1_X
      # df_fit <- make_data()$df_exp_fit
      # lm_fit <- make_data()$lm_out
      # first_death_date <- make_data()$date_cutoffs$first_death
      # exp_growth_date <- make_data()$date_cutoffs$c_chart_signal
      # c_chart_CL <- make_data()$date_cutoffs$CL
      # c_chart_UCL <- make_data()$date_cutoffs$UCL
      count_rows_fit <- nrow(make_data()$df1_X %>% filter(stage_data == "Exponential growth and fit"))
      
      df_out <- make_computation_table(nobs_raw=nrow(make_data()$df1_X),
                                       nobs_fit=nrow(make_data()$df_exp_fit),
                                       first_event_date=make_data()$date_cutoffs$first_event,
                                       c_chart_signal=make_data()$date_cutoffs$c_chart_signal,
                                       lm_fit=make_data()$lm_out,
                                       baseline_fit=min(input$baseline_n,count_rows_fit,na.rm=TRUE))
      
      
    })
    
    output$parameter_table <- DT::renderDataTable({
      req(parameters_for_table())
      
      DT::datatable(parameters_for_table(),
                    rownames=FALSE)
    })
    
    #add parameters to the calculations page
    output$parameters <- renderPrint({
        #req(make_data())
        #require conditional check: if lm object NULL then print message no linear model fitted
        #if lm_object used, then summarize the number of records used, the intercept and slope
        #possibly can show the linear plot on the log scale
        print("values from fitting a straight line by least squares to log10(events)")
        intercept <- make_data()$lm_out$coefficients[1]
        print(intercept)
        slope <- make_data()$lm_out$coefficients[2]
        print(slope)
        print("more stuff goes here")
        print(control_chartNEW()$message_out)
        
        
    })
    
    #download log chart
    output$download_logchart <- downloadHandler(
      filename = function() {
        sprintf('%s_%s_days_log10plot.png', input$choose_location, input$baseline_n)
      },
      content = function(file) {
        
        png(file, width = 1000, height = 600)
        print(control_chartNEW()$p_out2)
        dev.off(which=dev.cur())
      }
    )
    
    output$log_chart_tab <- renderUI({
      req(control_chartNEW()$message_out)
      
      if (control_chartNEW()$message_out == 'c-chart and exponential fit') {
        list(
          plotOutput("log_control_chart",height="500px", width="750px"),
          
          downloadButton(outputId = 'download_logchart',
                         label = 'Download Chart')
        )
      } else {
        h5('Not enough data to display log chart.')
      }
      
    })
    
 })

