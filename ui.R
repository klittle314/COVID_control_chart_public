#references for conditional view of update action button are 
#http://shiny.rstudio.com/articles/dynamic-ui.html and http://shiny.rstudio.com/articles/dynamic-ui.html
library(DT)
library(shinyBS)


shinyUI(navbarPage("COVID-19 Control Chart Application",
                   
                   tabPanel("Overview",
                            h3("Hybrid Shewhart Chart for COVID-19 data"),
                            wellPanel(
                                tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'),
                                
                                tags$blockquote("This project implements a method based on control charts to view phases in daily reported events from COVID-19.
                                The method was developed by Lloyd Provost, Shannon Provost, Rocco Perla, Gareth Parry, and Kevin Little. The code is R and deploys a user interface using Shiny technology.
                                The R code transforms a time series of daily reported events into charts that distinguish phases of COVID-19 infection for a reporting location like a country, state or city"),
                                tags$hr(),
                                
                                tags$p(tags$img(src='example_annotated.jpg', align = "top")),
                                
                                br(),
                                br(),
                                h5('Click',
                                   tags$a('here', 
                                          href = 'http://www.ihi.org/Topics/COVID-19/Documents/IHI-COVID-19-Data-Dashboard-Introduction-and-Methodology.pdf',
                                          target = '_blank'),
                                   'to read about the method details on the',
                                   tags$b('Insitute for Healthcare Improvement'),
                                   'website.'),
                                
                                h5('Click',
                                   tags$a('here', 
                                          href = 'https://www.usnews.com/news/healthiest-communities/articles/2020-03-26/coronavirus-pandemic-reaching-critical-tipping-point-in-america-analysis-shows',
                                          target = '_blank'),
                                   'for an introduction to the method in an article from',
                                   tags$b('U.S. News and World Report'),
                                   '26 March 2020.'),
                                
                                
                                
                                #NYTimes attribution language
                                h5('U.S. data from from ',
                                    tags$b('The New York Times'), 
                                    'based on reports from state and local health agencies. Click',
                                    tags$a('here',
                                      href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html",
                                      target = '_blank'),
                                  'to learn more.'),
                               
                                
                               h6('Note that the New York Times changed the definition of deaths to include probable deaths starting in early May,
                                  as explained ',
                                    tags$a('here',
                                      href="https://github.com/nytimes/covid-19-data/blob/master/PROBABLE-CASES-NOTE.md",
                                      target = 'blank'),
                                      '.'),
                               
                                
                                #helpText("Questions? Contact Kevin Little, Ph.D."),
                                br(),
                                
                                #connection to GitHub repository here
                                h5('Code available on',
                                   tags$b('GitHub.'), 
                                   'Click ',
                                   tags$a('here',
                                          href="https://github.com/klittle314/COVID_control_chart_public",
                                          target = '_blank'),
                                   'to view and download.'),
                                
                                
                                # author info
                                shiny::hr(),
                                em(
                                    span("Created by "),
                                    a("Kevin Little", href = "mailto:klittle@iecodesign.com"),



                                    span("updated 22 June 2020 8:15am U.S. CDT"),


                                    br(), br()
                                    
                                
                                
                                )
                                
                               
                            )
                   ),
                   
                   tabPanel('Upload Data',
                     
                     h4('To upload your own data series, please create a CSV file with the following column names (case sensitive):'),
                     
                     tags$ul(
                       tags$li('date (MM/DD/YYYY format)'),
                       tags$li('location')),
                     
                     h4('Then add as many numeric columns as you wish. Include column names in the first row, and use underscores (_) to separate words in names (e.g. "icu_patients").'),
                     
                     br(),
                     helpText('You may include multiple locations in a single file; the locations will appear in the location drop-down box'),

                     
                     helpText('The file should contain new daily reported events (not cumulative).'),
                     
                     helpText('The current code will not yet handle NA values in the death series.  Zero values are fine.'),
                    

                     h5('Click',
                        tags$a('here', 
                               href = 'https://support.office.com/en-us/article/Import-or-export-text-txt-or-csv-files-5250ac4c-663c-47ce-937b-339e391393ba',
                               target = '_blank'),
                        'for help creating a CSV file in Excel.'),
                     
                     h5('Click',
                        tags$a('here',
                               href = 'https://github.com/klittle314/COVID_control_chart_public/blob/master/test_data/France_test1_resort_dates.csv',
                               target = 'blank'),
                        'for a sample CSV file.'),
                     
                     tags$br(),
                     
                     fileInput(
                       inputId = 'upload_data',
                       label   = 'Select data:',
                       accept  = c('text/csv', '.csv')),
                     
                     uiOutput('upload_message'),
                     
                     uiOutput('upload_confirm')
                  
                   ),
                   
                   tabPanel("Display",
                            
                            sidebarLayout(
                                sidebarPanel( 
                                    width=3,
                                    
                                    h4("Build a control chart by choosing location and adjusting options"),
                                    
                                    uiOutput('data_warning'),
                                    
                                    selectInput(
                                      inputId = 'data_source',
                                      label   = h5('Choose data source'),
                                      choices = data_choices,
                                      selected = data_selected),
                                    
                                    selectInput(
                                      inputId = 'event_name',
                                      label   = h5('Choose event column'),
                                      choices = NULL,
                                      selected = NULL),
                                    
                                    #drop down to select the Site Type
                                    # htmlOutput("selectSiteType"),
                                    # br(),
                                  
                                    #drop down to select the location
                                    selectInput(
                                        inputId  = 'choose_location',
                                        label    = h5("Choose location"),
                                        choices  = location_choices,
                                        selected = location_selected,
                                        width    = "100%"),
                                    
                                    textAreaInput(
                                      inputId = 'chart_caption',
                                      label   = h5('Add caption to chart to comment on the data quality or implications'),
                                      value   = '',
                                      width   = '100%'),
                                    
                                    helpText(h6("Caption will be included in the downloaded image of the chart.")),
                                    
                                    checkboxInput(
                                      inputId = 'show_advanced_controls',
                                      label   = h5('Show additional options')),
                                    
                                    conditionalPanel('input.show_advanced_controls',
                                      #Numeric input for buffer
                                      # 
                                      numericInput("buffer", label = h5("Days beyond end of data series: extend curve and limits"), value = defBuffer, min=1),
                                      
                                      #br(),
                                      #Numeric input for baseline series length used to compute control limits
                                      #The default value should be chosen by code:  requires at least 8 days no more than 20
                                      numericInput("baseline_n", label = h5("Maximum days used to compute exponential growth line and limits"), value = defBaseline, min = 8),
                                      helpText(h6("If there are fewer days in the data series than the maximum, app calculates using all the data.")),
                                     #br(),
                                      
                                      # Checkbox that if checked, constrains control chart y-axis to the range of observed event counts, instead of the 
                                      # range of the projections. Helps view data series for countries with enough data that projections dominate
                                      # the observed series.
                                      checkboxInput(
                                        inputId = 'constrain_y_axis',
                                        label   = h5('Constrain y-axis limits to observed data (instead of projections)'),
                                        value   = TRUE),
                                      
                                      #Input date that marks the start of the limit calculations
                                      dateInput("start_date",label=h5("Custom start date for calculations instead of date of first event"),value=defStartdate),
                                      helpText(h6("Leave blank to allow the start date to be determined as date of first reported event")),
                                      #helpText(h6("The starting date 2019-12-31 tells the app to use all the available data.")),
                                      helpText(h6("You can choose a date after start of the series to focus the graph and calculations on a shorter date range.")),
                                     
                                     actionButton("reset", "Reset Defaults")
                                    )
                                ),
                                mainPanel(
                                  tabsetPanel(id = 'display-tab',type='tabs',  
                                    tabPanel("Basic Chart",
                                              
                                           uiOutput("message"),  
                                            
                                            plotOutput("control_chart",height="500px",width="750px"),
                                                     
                                            downloadButton(outputId = 'download_chart',
                                                           label = 'Download Chart'),
                                            
                                            tags$hr(),
                                            
                                           
                                            DT::dataTableOutput('data_table')
                                    ),
                                  
                                    tabPanel("Log chart",
                                             
                                             uiOutput('log_chart_tab')
                                             
                                    ),
                                    
                                    tabPanel("Calculation Details",
                                             
                                             uiOutput("message2"),
                                             
                                             h6("Parameter values: format work pending"),
                                           
                                          
                                           
                                           DT::dataTableOutput('parameter_table')
                                           
                                           
                                    )
                                           
                                  )
                                )
                              )
                            )
                   
            )
)