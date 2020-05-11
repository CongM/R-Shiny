######################################
## COVID-19 App
## Cong Mu
######################################

#### Set up
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(knitr)
library(MASS)
library(kableExtra)
library(highcharter)


options(knitr.kable.NA = '')

load("allsaved_small.rda")
load("allsaved2_small.rda")

yhats <- list()
yhatslb <- list()
yhatsub <- list()

yhats_2 <- list()
yhatslb_2 <- list()
yhatsub_2 <- list()


#### UI

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = 'COVID-19'),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem('Front Page', tabName = 'frontpage', icon = icon('list')),
                        # menuItem('Read Me', tabName = 'readme', icon = icon('bookmark')),
                        # menuItem('Layout', tabName = 'layout', icon = icon('th'),
                        #          menuItem('Input', tabName = 'input', icon = icon('sign-in')),
                        #          menuItem('Output', tabName = 'output', icon = icon('sign-out'))),
                        # menuItem('Table', tabName = 'table', icon = icon('table'),
                        #          menuItem('knitr', tabName = 'kable'),
                        #          menuItem('DT', tabName = 'datatable')),
                        menuItem('Demographics', tabName = 'plot1', icon = icon('picture-o'),
                                 menuItem('PopulationDensity (Country)', tabName = 'd1'),
                                 menuItem('BalanceImEx', tabName = 'd2'),
                                 menuItem('GeneralCPI', tabName = 'd3'),
                                 menuItem('PhysiciansDensity', tabName = 'd4'),
                                 menuItem('EmploymentServicesPCT', tabName = 'd5'),
                                 menuItem('GDPPercapita', tabName = 'd6'),
                                 menuItem('TertiaryEducationStudents', tabName = 'd7'),
                                 menuItem('InternationalMigrantPCT', tabName = 'd8'),
                                 menuItem('UnemploymentRate', tabName = 'd9'),
                                 menuItem('Visitor', tabName = 'd10'),
                                 menuItem('HealthExpenditurePCT', tabName = 'd11'),
                                 menuItem('SeniorPCT', tabName = 'd12'),
                                 menuItem('ForeignBornPCT', tabName = 'd13'),
                                 menuItem('BSPCT', tabName = 'd14'),
                                 menuItem('NoHealthInsurancePCT', tabName = 'd15'),
                                 menuItem('TotalHCandSARevenue', tabName = 'd16'),
                                 menuItem('MeanTravelTimeToWork', tabName = 'd17'),
                                 menuItem('PercapitaIncome', tabName = 'd18'),
                                 menuItem('PovertyPCT', tabName = 'd19'),
                                 menuItem('TotalEmployment', tabName = 'd20'),
                                 menuItem('PopulationDensity (State)', tabName = 'd21')),
                        menuItem('COVID-19', tabName = 'plot2', icon = icon('picture-o'),
                                 menuItem('Country Level', tabName = 'confirmed'),
                                 # menuItem('Recovered', tabName = 'recovered'),
                                 menuItem('State Level', tabName = 'confirmed2')),
                        menuItem('References', tabName = 'references', icon = icon('book'),
                                 badgeLabel = 'Badge', badgeColor = 'yellow'))
                      ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'frontpage',
                                br(),
                                br(),
                                br(),
                                br(),
                                h1('Analysis on COVID-19 via Nonparametric Bayesian Approach', align = 'center'),
                                br(),
                                h3('Cong Mu', align = 'center'),
                                h4('May 11, 2020', align = 'center')
                                ),
                        
                        # tabItem(tabName = 'readme',
                        #         br(),
                        #         br(),
                        #         br(),
                        #         br(),
                        #         tags$div(
                        #           tags$ul(
                        #             tags$li(h3('We use the following packages in this demo')),
                        #             tags$ul(tags$li(h4('Set-up: shiny, shinydashboard, RColorBrewer')), 
                        #                     tags$li(h4('Data Preparation: jsonlite, dplyr, stringr, purrr, quantmod')), 
                        #                     tags$li(h4('Table: knitr, kableExtra, DT')), 
                        #                     tags$li(h4('Plot: highcharter, dygraphs, igraph, visNetwork, networkD3'))))),
                        #         br(),
                        #         br(),
                        #         tags$div(
                        #           tags$ul(
                        #             tags$li(h3('Some notes')),
                        #             tags$ul(tags$li(h4('The demo introduces some libraries that could be useful in visualization. In particular')), 
                        #                     tags$ul(tags$li(h5('highcharter: used for multiple purposes, such as scatter, histogram, graph, map, etc.')),
                        #                             tags$li(h5('dygraphs: used for time series.')),
                        #                             tags$li(h5('igraph, visNetwork, networkD3: used for networks.'))), 
                        #                     tags$li(h4('Most examples in this demo are from documents/tutorials of each library')), 
                        #                     tags$li(h4('In general, this demo could be used as a template. After modification (layouts, data, etc) based on requirement, re-run the source file shiny_demo.R (Run App in RStudio).')), 
                        #                     tags$li(h4('For more inforamtion, please check References.')))))
                        #         ),
                        
                        # tabItem(tabName = 'input',
                        #         fluidRow(
                        #           box(
                        #             title = 'Slider Input', width = 6, status = 'primary',
                        #             sliderInput('slider', 'Slider:', min=0, max=1000, value=500)),
                        #           box(
                        #             title = 'Numberic Input', width = 6, status = 'warning',
                        #             numericInput('numeric', 'Numeric:', 10))),
                        #         fluidRow(
                        #           box(
                        #             title = 'Radio Button', width = 4, solidHeader = TRUE, status = 'success',
                        #             radioButtons('radio', 'Please select:', list('Choice1' = 'choice1', 'Choice2' = 'choice2'))),
                        #           box(
                        #             title = 'Select Input', width = 4, solidHeader = TRUE, status = 'info',
                        #             selectInput('select', 'Please select:', choices = c('choice1', 'choice2', 'choice3'))),
                        #           box(
                        #             title = 'Text Input', width = 4, solidHeader = TRUE, background = 'maroon',
                        #             textInput('text', 'Please enter：', value = 'enter...')))
                        #         ),
                        
                        # tabItem(tabName = 'output',
                        #         fluidRow(
                        #           box(
                        #             title = 'Verbatim Text Output', width = 6, solidHeader = TRUE, status = 'primary',
                        #             verbatimTextOutput('text_iris')),
                        #           box(
                        #             title = 'Plot Output', width = 6, solidHeader = TRUE, status = 'success',
                        #             plotOutput('plot_iris', height = 250))),
                        #         box(
                        #           title = 'Table Output', width = 12, solidHeader = TRUE, status = 'warning',
                        #           tableOutput('table_iris'))
                        #         ),
                        
                        # tabItem(tabName = 'kable',
                        #         box(
                        #           title = tagList(icon('sign-in'), 'Input'), width = 2, background = 'black',
                        #           radioButtons('input_kable_1', 'Please select:', list('Show row names' = 'TRUE', 'Hide row names' = 'FALSE')),
                        #           submitButton('Update')),
                        #         box(
                        #           title = 'knitr::kable', width = 10, solidHeader = TRUE, status = 'primary',
                        #           tableOutput('example_kable')),
                        #         br(),
                        #         br(),
                        #         h1('---------------------------------------------------------------------------------------------------------')
                        #         ),
                        
                        # tabItem(tabName = 'datatable',
                        #         box(
                        #           title = tagList(icon('sign-in'), 'Input'), width = 2, background = 'black',
                        #           radioButtons('input_datatable_1', 'Please select:', list('Show filter' = 'top', 'Hide filter' = 'none')),
                        #           submitButton('Update')),
                        #         box(
                        #           title = 'DT::datatable', width = 10, solidHeader = TRUE, status = 'primary',
                        #           dataTableOutput('example_datatable'))
                        # ),
                        
                        tabItem(tabName = 'd1',
                                box(
                                  title = 'Population density', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter1', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd2',
                                box(
                                  title = 'Balance imports/exports (millions of US dollars)', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter2', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd3',
                                box(
                                  title = 'Consumer price index: General', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter3', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd4',
                                box(
                                  title = 'Health personnel: Physicians (per 1000 population)', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter4', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd5',
                                box(
                                  title = 'Employment by industry: Services (%) Male and Female', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter5', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd6',
                                box(
                                  title = 'GDP per capita (US dollars)', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter6', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd7',
                                box(
                                  title = 'Students enrolled in tertiary education (thousands)', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter7', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd8',
                                box(
                                  title = 'International migrant stock: Both sexes (% total population)', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter8', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd9',
                                box(
                                  title = 'Unemployment rate - Total', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter9', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd10',
                                box(
                                  title = 'Tourist/visitor arrivals (thousands)', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter10', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd11',
                                box(
                                  title = 'Domestic general government health expenditure (% of total government expenditure)', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter11', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd12',
                                box(
                                  title = 'Persons 65 years and over, percent', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter12', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd13',
                                box(
                                  title = 'Foreign born persons, percent, 2014-2018', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter13', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd14',
                                box(
                                  title = "Bachelor's degree or higher, percent of persons age 25 years+, 2014-2018", width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter14', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd15',
                                box(
                                  title = 'Persons  without health insurance, under age 65 years, percent', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter15', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd16',
                                box(
                                  title = 'Total health care and social assistance receipts/revenue, 2012 ($1,000)', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter16', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd17',
                                box(
                                  title = 'Mean travel time to work (minutes), workers age 16 years+, 2014-2018', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter17', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd18',
                                box(
                                  title = 'Per capita income in past 12 months (in 2018 dollars), 2014-2018', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter18', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd19',
                                box(
                                  title = 'Persons in poverty, percent', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter19', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd20',
                                box(
                                  title = 'Total employment, 2017', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter20', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'd21',
                                box(
                                  title = 'Population per square mile, 2010', width = 12, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter21', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'confirmed',
                                box(
                                  title = tagList(icon('sign-in'), 'Input'), width = 3, background = 'black',
                                  selectInput('confirmed_country', 'Please select the country:', choices = cs),
                                  sliderInput('confirmed_len', 'Length of trajectory:', min=0, max=100, value=50),
                                  submitButton('Update')),
                                box(
                                  title = 'Number of confirmed cases at country level', width = 9, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter_confirmed', height = '600px'))
                        ),
                        
                        # tabItem(tabName = 'recovered',
                        #         box(
                        #           title = tagList(icon('sign-in'), 'Input'), width = 3, background = 'black',
                        #           selectInput('recovered_country', 'Please select the country:', choices = cs),
                        #           sliderInput('recovered_len', 'Length of trajectory:', min=0, max=100, value=50),
                        #           submitButton('Update')),
                        #         box(
                        #           title = 'Number of recovered cases', width = 9, solidHeader = TRUE, status = 'primary',
                        #           highchartOutput('highcharter_recovered', height = '600px'))
                        # ),
                        
                        tabItem(tabName = 'confirmed2',
                                box(
                                  title = tagList(icon('sign-in'), 'Input'), width = 3, background = 'black',
                                  selectInput('confirmed2_state', 'Please select the state:', choices = ss),
                                  sliderInput('confirmed2_len', 'Length of trajectory:', min=0, max=100, value=50),
                                  submitButton('Update')),
                                box(
                                  title = 'Number of confirmed at state level', width = 9, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('highcharter_confirmed2', height = '600px'))
                        ),
                        
                        
                        # tabItem(tabName = 'dygraphs',
                        #         box(
                        #           title = 'dygraphs', width = 12, solidHeader = TRUE, status = 'primary',
                        #           dygraphOutput('example_dygraphs', height = '600px'))
                        # ),
                        
                        # tabItem(tabName = 'visNetwork',
                        #         box(
                        #           title = tagList(icon('sign-in'), 'Input'), width = 3, background = 'black',
                        #           selectInput('input_visNetwork_1', 'Please select the type of plot:', choices = c('Image', 'Icon', 'Custom')),
                        #           submitButton('Update')),
                        #         box(
                        #           title = 'visNetwork', width = 9, solidHeader = TRUE, status = 'primary',
                        #           visNetworkOutput('example_visNetwork', height = '600px'))
                        # ),
                        
                        # tabItem(tabName = 'networkD3',
                        #         box(
                        #           title = 'networkD3', width = 12, solidHeader = TRUE, status = 'primary',
                        #           sankeyNetworkOutput('example_networkD3', height = '600px'))
                        # ),
                        
                        tabItem(tabName = 'references',
                                br(),
                                br(),
                                br(),
                                br(),
                                tags$div(
                                  tags$ul(
                                    tags$li(
                                      h3("Data Source")
                                    ),
                                    tags$ul(
                                      tags$li(a(h4('JHU CSSE'), href = 'https://github.com/CSSEGISandData/COVID-19', target = 'black')),
                                      tags$li(a(h4('UNdata'), href = 'http://data.un.org/', target = 'black')),
                                      tags$li(a(h4('Quickfacts'), href = 'https://www.census.gov/quickfacts/fact/table/US/PST045219', target = 'black')),
                                      tags$li(a(h4('NYTimes'), href = 'https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html', target = 'black')),
                                      tags$li(a(h4('CNN'), href = 'https://www.cnn.com/2020/04/20/us/states-that-require-masks-trnd/index.html', target = 'black'))
                                    )
                                  )
                                )
                                # tags$div(
                                #   tags$ul(
                                #     tags$li(h3('For more information, please check the following resources')),
                                #     tags$ul(tags$li(a(h4('R shiny'), href = 'http://shiny.rstudio.com/tutorial/', target = 'black')), 
                                #             tags$li(a(h4('shinydashboard'), href = 'http://rstudio.github.io/shinydashboard/index.html', target = 'black')),
                                #             tags$li(h4('Table')),
                                #             tags$ul(tags$li(a(h5('knitr'), href = 'https://github.com/yihui/knitr', target = 'black')),
                                #                     tags$li(a(h5('DT'), href = 'https://rstudio.github.io/DT/', target = 'black'))), 
                                #             tags$li(h4('Plot')),
                                #             tags$ul(tags$li(a(h5('highcharter'), href = 'http://jkunst.com/highcharter/', target = 'black')),
                                #                     tags$li(a(h5('dygraphs'), href = 'https://rstudio.github.io/dygraphs/', target = 'black')),
                                #                     tags$li(a(h5('visNetwork'), href = 'https://datastorm-open.github.io/visNetwork/', target = 'black')),
                                #                     tags$li(a(h5('networkD3'), href = 'https://christophergandrud.github.io/networkD3/', target = 'black'))))))
                                )
                        )
                    )
)








#### Server
server <- function(input, output) {
  
  # output$text_iris <- renderPrint({
  #   summary(iris)})
  
  # output$table_iris <- renderTable({
  #   head(iris)})
  
  # output$plot_iris <- renderPlot({
  #   plot(iris)})
  
  # output$example_kable <- function() {
  #   kable(iris, row.names = input$input_kable_1, 'html') %>%
  #     kable_styling('striped', full_width = F)
  # }
  
  # output$example_datatable <- renderDataTable({
  #   datatable(iris, filter = input$input_datatable_1)
  # })
  
  output$highcharter1 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, PopulationDensity) %>%
      arrange(desc(PopulationDensity))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$PopulationDensity, name = "Population density")
  })
  
  output$highcharter2 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, BalanceImEx) %>%
      arrange(desc(BalanceImEx))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$BalanceImEx, name = "Balance imports/exports (millions of US dollars)")
  })
  
  output$highcharter3 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, GeneralCPI) %>%
      arrange(desc(GeneralCPI))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$GeneralCPI, name = "Consumer price index: General")
  })
  
  output$highcharter4 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, PhysiciansDensity) %>%
      arrange(desc(PhysiciansDensity))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$PhysiciansDensity, name = "Health personnel: Physicians (per 1000 population)")
  })
  
  output$highcharter5 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, EmploymentServicesPCT) %>%
      arrange(desc(EmploymentServicesPCT))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$EmploymentServicesPCT, name = "Employment by industry: Services (%) Male and Female")
  })
  
  output$highcharter6 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, GDPpercapita) %>%
      arrange(desc(GDPpercapita))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$GDPpercapita, name = "GDP per capita (US dollars)")
  })
  
  output$highcharter7 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, TertiaryEducationStudents) %>%
      arrange(desc(TertiaryEducationStudents))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$TertiaryEducationStudents, name = "Students enrolled in tertiary education (thousands)")
  })
  
  output$highcharter8 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, InternationalMigrantPCT) %>%
      arrange(desc(InternationalMigrantPCT))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$InternationalMigrantPCT, name = "International migrant stock: Both sexes (% total population)")
  })
  
  output$highcharter9 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, UnemploymentRate) %>%
      arrange(desc(UnemploymentRate))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$UnemploymentRate, name = "Unemployment rate - Total")
  })
  
  output$highcharter10 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, Visitor) %>%
      arrange(desc(Visitor))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$Visitor, name = "Tourist/visitor arrivals (thousands)")
  })
  
  output$highcharter11 <- renderHighchart({
    dat <- demographics %>%
      dplyr::select(Country, HealthExpenditurePCT) %>%
      arrange(desc(HealthExpenditurePCT))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$Country) %>% 
      hc_add_series(dat$HealthExpenditurePCT, name = "Domestic general government health expenditure (% of total government expenditure)")
  })
  
  output$highcharter12 <- renderHighchart({
    dat <- demographics2 %>%
      dplyr::select(State, SeniorPCT) %>%
      arrange(desc(SeniorPCT))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$State) %>% 
      hc_add_series(dat$SeniorPCT, name = "Persons 65 years and over, percent")
  })
  
  output$highcharter13 <- renderHighchart({
    dat <- demographics2 %>%
      dplyr::select(State, ForeignBornPCT) %>%
      arrange(desc(ForeignBornPCT))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$State) %>% 
      hc_add_series(dat$ForeignBornPCT, name = "Foreign born persons, percent, 2014-2018")
  })
  
  output$highcharter14 <- renderHighchart({
    dat <- demographics2 %>%
      dplyr::select(State, BSPCT) %>%
      arrange(desc(BSPCT))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$State) %>% 
      hc_add_series(dat$BSPCT, name = "Bachelor's degree or higher, percent of persons age 25 years+, 2014-2018")
  })
  
  output$highcharter15 <- renderHighchart({
    dat <- demographics2 %>%
      dplyr::select(State, NoHealthInsurancePCT) %>%
      arrange(desc(NoHealthInsurancePCT))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$State) %>% 
      hc_add_series(dat$NoHealthInsurancePCT, name = "Persons without health insurance, under age 65 years, percent")
  })
  
  output$highcharter16 <- renderHighchart({
    dat <- demographics2 %>%
      dplyr::select(State, TotalHCandSARevenue) %>%
      arrange(desc(TotalHCandSARevenue))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$State) %>% 
      hc_add_series(dat$TotalHCandSARevenue, name = "Total health care and social assistance receipts/revenue, 2012 ($1,000)")
  })
  
  output$highcharter17 <- renderHighchart({
    dat <- demographics2 %>%
      dplyr::select(State, MeanTravelTimeToWork) %>%
      arrange(desc(MeanTravelTimeToWork))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$State) %>% 
      hc_add_series(dat$MeanTravelTimeToWork, name = "Mean travel time to work (minutes), workers age 16 years+, 2014-2018")
  })
  
  output$highcharter18 <- renderHighchart({
    dat <- demographics2 %>%
      dplyr::select(State, PercapitaIncome) %>%
      arrange(desc(PercapitaIncome))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$State) %>% 
      hc_add_series(dat$PercapitaIncome, name = "Per capita income in past 12 months (in 2018 dollars), 2014-2018")
  })
  
  output$highcharter19 <- renderHighchart({
    dat <- demographics2 %>%
      dplyr::select(State, PovertyPCT) %>%
      arrange(desc(PovertyPCT))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$State) %>% 
      hc_add_series(dat$PovertyPCT, name = "Persons in poverty, percent")
  })
  
  output$highcharter20 <- renderHighchart({
    dat <- demographics2 %>%
      dplyr::select(State, TotalEmployment) %>%
      arrange(desc(TotalEmployment))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$State) %>% 
      hc_add_series(dat$TotalEmployment, name = "Total employment, 2017")
  })
  
  output$highcharter21 <- renderHighchart({
    dat <- demographics2 %>%
      dplyr::select(State, PopulationDensity) %>%
      arrange(desc(PopulationDensity))
    highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = dat$State) %>% 
      hc_add_series(dat$PopulationDensity, name = "Population per square mile, 2010")
  })
  
  output$highcharter_confirmed <- renderHighchart({
    t <- as.integer(input$confirmed_len)
    dat <- covid19 %>%
      filter(Confirmed != 0 & Days < t) %>%
      filter(Country == input$confirmed_country)
    tempX <- filter(scaledemographics, Country == input$confirmed_country)
    Xi <- cbind(Intercept=1, tempX[,-1]) %>% mefa:::rep.data.frame(t) %>% as.matrix()
    Zi <- cbind(rep(1,t), 0:(t-1))
    i <- which(cs==input$confirmed_country)
    wis <- mvrnorm(n = 10000, mu = rep(0,t), Sigma = diag(rep(sigma2what[i],t)))
    wihats <- apply(wis, 2, mean)
    wihatslb <- apply(wis, 2, quantile, 0.025)
    wihatsub <- apply(wis, 2, quantile, 0.975)
    bihats <- rbind(b1shat[i], b2shat[i])
    bihatslb <- rbind(b1slb[i], b2slb[i])
    bihatsub <- rbind(b1sub[i], b2sub[i])
    yhats[[i]] <- Xi%*%betashat + Zi%*%bihats + wihats
    yhatslb[[i]] <- Xi%*%betaslb + Zi%*%bihatslb + wihatslb
    yhatsub[[i]] <- Xi%*%betasub + Zi%*%bihatsub + wihatsub
    pred <- data.frame(Date = seq(dat$First[1], dat$First[1]+t-1, 1), 
                       yhat = exp(yhats[[i]]),
                       yhatslb = exp(yhatslb[[i]]),
                       yhatsub = exp(yhatsub[[i]]))
    highchart() %>%
      hc_add_series(dat, type = "spline", hcaes(x=Date,y=Confirmed), name = "Confirmed") %>% 
      hc_add_series(pred, type = "spline", hcaes(x=Date,y=yhat), name = "Prediction") %>% 
      hc_add_series(pred, type = "spline", hcaes(x=Date,y=yhatslb), name = "95% CI 1", dashStyle = "shortdash") %>%
      hc_add_series(pred, type = "spline", hcaes(x=Date,y=yhatsub), name = "95% CI 2", dashStyle = "shortdash") %>%
      hc_xAxis(type = "datetime") %>%
      hc_tooltip(shared = TRUE) 
  })

  output$highcharter_confirmed2 <- renderHighchart({
    t <- as.integer(input$confirmed2_len)
    dat <- covid19_2 %>%
      filter(Confirmed != 0 & Days < t) %>%
      filter(State == input$confirmed2_state)
    if (nrow(dat) < t) {
      Zi <- cbind(rep(1,t), c(dat$SAHDays2, (dat$SAHDays2[nrow(dat)]+1):(dat$SAHDays2[nrow(dat)]+t-nrow(dat))))
    } else {
      Zi <- cbind(rep(1,t), dat$SAHDays2) 
    }
    tempX <- filter(scaledemographics2, State == input$confirmed2_state)
    Xi <- cbind(Intercept=1, tempX[,-1]) %>% mefa:::rep.data.frame(t) %>% as.matrix()
    if (input$confirmed2_state %in% mask$State) {
      maskdate <- mask$Mask[mask$State==input$confirmed2_state]
      dat <- mutate(dat, MaskDate = maskdate) %>%
        mutate(MaskDays = as.integer(Date-MaskDate)) %>%
        mutate(Mask = ifelse(MaskDays < 0, 0, 1))
      if (nrow(dat) < t) {
        Xi <- cbind(Xi, Mask = c(dat$Mask, rep(dat$Mask[nrow(dat)], t-nrow(dat))))
      } else {
        Xi <- cbind(Xi, Mask = dat$Mask) 
      }
    } else {
      Xi <- cbind(Xi, Mask = rep(0, t))
    }
    i <- which(ss==input$confirmed2_state)
    wis <- mvrnorm(n = 10000, mu = rep(0,t), Sigma = diag(rep(sigma2what_2[i],t)))
    wihats <- apply(wis, 2, mean)
    wihatslb <- apply(wis, 2, quantile, 0.025)
    wihatsub <- apply(wis, 2, quantile, 0.975)
    bihats <- rbind(b1shat_2[i], b2shat_2[i])
    bihatslb <- rbind(b1slb_2[i], b2slb_2[i])
    bihatsub <- rbind(b1sub_2[i], b2sub_2[i])
    yhats_2[[i]] <- Xi%*%betashat_2 + Zi%*%bihats + wihats
    yhatslb_2[[i]] <- Xi%*%betaslb_2 + Zi%*%bihatslb + wihatslb
    yhatsub_2[[i]] <- Xi%*%betasub_2 + Zi%*%bihatsub + wihatsub
    pred <- data.frame(Date = seq(dat$First[1], dat$First[1]+t-1, 1), 
                       yhat = exp(yhats_2[[i]]),
                       yhatslb = exp(yhatslb_2[[i]]),
                       yhatsub = exp(yhatsub_2[[i]]))
    highchart() %>%
      hc_add_series(dat, type = "spline", hcaes(x=Date,y=Confirmed), name = "Confirmed") %>% 
      hc_add_series(pred, type = "spline", hcaes(x=Date,y=yhat), name = "Prediction") %>% 
      hc_add_series(pred, type = "spline", hcaes(x=Date,y=yhatslb), name = "95% CI 1", dashStyle = "shortdash") %>%
      hc_add_series(pred, type = "spline", hcaes(x=Date,y=yhatsub), name = "95% CI 2", dashStyle = "shortdash") %>%
      hc_xAxis(type = "datetime") %>%
      hc_tooltip(shared = TRUE) 
  })
  
  # output$example_highcharter <- renderHighchart({
  #   if(input$input_highcharter_1 == 'Scatter') {
  #     hchart(mpg, 'scatter', hcaes(x = displ, y = hwy, group = class))
  #   } else if(input$input_highcharter_1 == 'Line') {
  #     data(citytemp)
  #     highchart() %>% 
  #       hc_xAxis(categories = citytemp$month) %>% 
  #       hc_add_series(name = 'Tokyo', data = citytemp$tokyo) %>% 
  #       hc_add_series(name = 'London', data = citytemp$london) %>% 
  #       hc_add_series(name = 'Other city', data = (citytemp$tokyo + citytemp$london)/2)
  #   } else if(input$input_highcharter_1 == 'Histogram') {
  #     hchart(diamonds$price, name = 'You can zoom in') 
  #   } else if(input$input_highcharter_1 == 'Density') {
  #     hchart(density(diamonds$price), type = 'area', color = '#B71C1C', name = 'Price')
  #   } else if(input$input_highcharter_1 == 'Graph') {
  #     N <- 40
  #     net <- sample_gnp(N, p = 2/N)
  #     wc <- cluster_walktrap(net)
  #     V(net)$label <- seq(N)
  #     V(net)$name <- paste('Node ', seq(N))
  #     V(net)$page_rank <- round(page.rank(net)$vector, 2)
  #     V(net)$betweenness <- round(betweenness(net), 2)
  #     V(net)$degree <- degree(net)
  #     V(net)$size <- V(net)$degree
  #     V(net)$comm <- membership(wc)
  #     V(net)$color <- colorize(membership(wc))
  #     hchart(net, layout = layout_with_fr)
  #   } else if(input$input_highcharter_1 == 'Stock') {
  #     SPY <- getSymbols('SPY', from = Sys.Date() - lubridate::years(1), auto.assign = FALSE)
  #     SPY <- adjustOHLC(SPY)
  #     SPY.SMA.10 <- SMA(Cl(SPY), n = 5)
  #     SPY.SMA.200 <- SMA(Cl(SPY), n = 100)
  #     SPY.RSI.14 <- RSI(Cl(SPY))
  #     SPY.RSI.SellLevel <- xts(rep(70, NROW(SPY)), index(SPY))
  #     SPY.RSI.BuyLevel <- xts(rep(30, NROW(SPY)), index(SPY))
  #     
  #     highchart(type = 'stock') %>% 
  #       # create axis
  #       hc_yAxis_multiples(create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)) %>% 
  #       # series
  #       hc_add_series(SPY, yAxis = 0, name = 'SPY') %>% 
  #       hc_add_series(SPY.SMA.10, yAxis = 0, name = 'Fast MA') %>% 
  #       hc_add_series(SPY.SMA.200, yAxis = 0, name = 'Slow MA') %>% 
  #       hc_add_series(SPY$SPY.Volume, color = 'gray', yAxis = 1, type = 'column', name = 'Volume') %>% 
  #       hc_add_series(SPY.RSI.14, yAxis = 2, color = hex_to_rgba('green', 0.7), name = 'Osciallator') %>%
  #       hc_add_series(SPY.RSI.SellLevel, color = hex_to_rgba('red', 0.7), yAxis = 2, name = 'Sell level') %>% 
  #       hc_add_series(SPY.RSI.BuyLevel, color = hex_to_rgba('blue', 0.7), yAxis = 2, name = 'Buy level') 
  #   } else if(input$input_highcharter_1 == 'Map') {
  #     mapdata <- get_data_from_map(download_map_data('countries/us/us-all'))
  #     set.seed(1234)
  #     data_fake <- mapdata %>% 
  #       select(code = `hc-a2`) %>% 
  #       mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))
  #     
  #     hcmap('countries/us/us-all', data = data_fake, value = 'value',
  #           joinBy = c('hc-a2', 'code'), name = 'Fake data',
  #           dataLabels = list(enabled = TRUE, format = '{point.name}'),
  #           borderColor = '#FAFAFA', borderWidth = 0.1,
  #           tooltip = list(valueDecimals = 2, valuePrefix = '$', valueSuffix = ' USD')) 
  #   } else {
  #     colors <- c('#FB1108', '#FD150B', '#FA7806', '#FBE426', '#FCFB8F', '#F3F5E7', '#C7E4EA','#ABD6E6','#9AD2E1')
  #     stars$color <- colorize(log(stars$temp), colors)
  #     x <- c('Luminosity', 'Temperature', 'Distance')
  #     y <- sprintf('{point.%s:.2f}', c('lum', 'temp', 'distance'))
  #     tltip <- tooltip_table(x, y)
  #     
  #     hchart(stars, 'scatter', hcaes(temp, lum, size = radiussun, color = color)) %>% 
  #       hc_chart(backgroundColor = 'black') %>% 
  #       hc_xAxis(type = 'logarithmic', reversed = TRUE) %>% 
  #       hc_yAxis(type = 'logarithmic', gridLineWidth = 0) %>% 
  #       hc_title(text = 'Our nearest Stars') %>% 
  #       hc_subtitle(text = 'In a Hertzsprung-Russell diagram') %>% 
  #       hc_tooltip(useHTML = TRUE, headerFormat = '', pointFormat = tltip) %>% 
  #       hc_size(height = 600)
  #   }
  # })
  
  # output$example_dygraphs <- renderDygraph({
  #   lungDeaths <- cbind(mdeaths, fdeaths)
  #   dygraph(lungDeaths) %>%
  #     dySeries('mdeaths', label = 'Male') %>%
  #     dySeries('fdeaths', label = 'Female') %>%
  #     dyOptions(stackedGraph = TRUE) %>%
  #     dyRangeSelector(height = 20)
  # })
  
  # output$example_visNetwork <- renderVisNetwork({
  #   if(input$input_visNetwork_1 == 'Image') {
  #     path_to_images <- 'https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/img/indonesia/'
  #     nodes <- data.frame(id = 1:4, shape = c('image', 'circularImage'), image = paste0(path_to_images, 1:4, '.png'), label = 'I am an image')
  #     edges <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))
  #     
  #     visNetwork(nodes, edges, width = '100%') %>% 
  #       visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  #       visLayout(randomSeed = 2)
  #   } else if(input$input_visNetwork_1 == 'Icon') {
  #     nb <- 10
  #     nodes <- data.frame(id = 1:nb, label = paste('Label', 1:nb),
  #                         group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
  #                         title = paste0('<p>', 1:nb,'<br>Tooltip !</p>'), stringsAsFactors = FALSE)
  #     edges <- data.frame(from = c(8,2,7,6,1,8,9,4,6,2),
  #                         to = c(3,7,2,7,9,1,5,3,2,9),
  #                         value = rnorm(nb, 10), label = paste('Edge', 1:nb),
  #                         title = paste0('<p>', 1:nb,'<br>Edge Tooltip !</p>'))
  #     
  #     visNetwork(nodes, edges, height = '500px', width = '100%') %>% 
  #       visGroups(groupname = 'A', shape = 'icon', icon = list(code = 'f0c0', size = 75)) %>%
  #       visGroups(groupname = 'B', shape = 'icon', icon = list(code = 'f007', color = 'red')) %>%
  #       visGroups(groupname = 'C', shape = 'icon', icon = list(code = 'f1b9', color = 'black')) %>%
  #       visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = T)) %>%
  #       addFontAwesome() %>%
  #       visLayout(randomSeed = 123)
  #   } else {
  #     nodes <- fromJSON('https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/nodes_miserables.json')
  #     edges <- fromJSON('https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/edges_miserables.json')
  #     
  #     visNetwork(nodes, edges, height = '700px', width = '100%') %>%
  #       visOptions(selectedBy = 'group', highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  #       visPhysics(stabilization = FALSE)
  #   }
  # })
  
  # output$example_networkD3 <- renderSankeyNetwork({
  #   URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/', 'master/JSONdata/energy.json')
  #   Energy <- fromJSON(URL)
  # 
  #   sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = 'source',
  #                 Target = 'target', Value = 'value', NodeID = 'name',
  #                 units = 'TWh', fontSize = 12, nodeWidth = 30)
  # })
  
}




#### Run App
shinyApp(ui, server)



