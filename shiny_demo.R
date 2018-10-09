######################################
## shiny_demo.R
## A demo for shiny in R
######################################

#### Set up
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(quantmod)
library(knitr)
library(kableExtra)
library(DT)
library(highcharter)
library(dygraphs)
library(igraph)
library(visNetwork)
library(networkD3)


options(knitr.kable.NA = '')


#### UI

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = 'Demo'),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem('Front Page', tabName = 'frontpage', icon = icon('list')),
                        menuItem('Read Me', tabName = 'readme', icon = icon('bookmark')),
                        menuItem('Layout', tabName = 'layout', icon = icon('th'),
                                 menuItem('Input', tabName = 'input', icon = icon('sign-in')),
                                 menuItem('Output', tabName = 'output', icon = icon('sign-out'))),
                        menuItem('Table', tabName = 'table', icon = icon('table'),
                                 menuItem('knitr', tabName = 'kable'),
                                 menuItem('DT', tabName = 'datatable')),
                        menuItem('Plot', tabName = 'plot', icon = icon('picture-o'),
                                 menuItem('highcharter', tabName = 'highcharter'),
                                 menuItem('dygraphs', tabName = 'dygraphs'),
                                 menuItem('visNetwork', tabName = 'visNetwork'),
                                 menuItem('networkD3', tabName = 'networkD3')),
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
                                h1('Title', align = 'center'),
                                br(),
                                h3('Author', align = 'center'),
                                h4('Date', align = 'center')
                                ),
                        
                        tabItem(tabName = 'readme',
                                br(),
                                br(),
                                br(),
                                br(),
                                tags$div(
                                  tags$ul(
                                    tags$li(h3('We use the following packages in this demo')),
                                    tags$ul(tags$li(h4('Set-up: shiny, shinydashboard, RColorBrewer')), 
                                            tags$li(h4('Data Preparation: jsonlite, dplyr, stringr, purrr, quantmod')), 
                                            tags$li(h4('Table: knitr, kableExtra, DT')), 
                                            tags$li(h4('Plot: highcharter, dygraphs, igraph, visNetwork, networkD3'))))),
                                br(),
                                br(),
                                tags$div(
                                  tags$ul(
                                    tags$li(h3('Some notes')),
                                    tags$ul(tags$li(h4('The demo introduces some libraries that could be useful in visualization. In particular')), 
                                            tags$ul(tags$li(h5('highcharter: used for multiple purposes, such as scatter, histogram, graph, map, etc.')),
                                                    tags$li(h5('dygraphs: used for time series.')),
                                                    tags$li(h5('igraph, visNetwork, networkD3: used for networks.'))), 
                                            tags$li(h4('Most examples in this demo are from documents/tutorials of each library')), 
                                            tags$li(h4('In general, this demo could be used as a template. After modification (layouts, data, etc) based on requirement, re-run the source file shiny_demo.R (Run App in RStudio).')), 
                                            tags$li(h4('For more inforamtion, please check References.')))))
                                ),
                        
                        tabItem(tabName = 'input',
                                fluidRow(
                                  box(
                                    title = 'Slider Input', width = 6, status = 'primary',
                                    sliderInput('slider', 'Slider:', min=0, max=1000, value=500)),
                                  box(
                                    title = 'Numberic Input', width = 6, status = 'warning',
                                    numericInput('numeric', 'Numeric:', 10))),
                                fluidRow(
                                  box(
                                    title = 'Radio Button', width = 4, solidHeader = TRUE, status = 'success',
                                    radioButtons('radio', 'Please select:', list('Choice1' = 'choice1', 'Choice2' = 'choice2'))),
                                  box(
                                    title = 'Select Input', width = 4, solidHeader = TRUE, status = 'info',
                                    selectInput('select', 'Please select:', choices = c('choice1', 'choice2', 'choice3'))),
                                  box(
                                    title = 'Text Input', width = 4, solidHeader = TRUE, background = 'maroon',
                                    textInput('text', 'Please enter：', value = 'enter...')))
                                ),
                        
                        tabItem(tabName = 'output',
                                fluidRow(
                                  box(
                                    title = 'Verbatim Text Output', width = 6, solidHeader = TRUE, status = 'primary',
                                    verbatimTextOutput('text_iris')),
                                  box(
                                    title = 'Plot Output', width = 6, solidHeader = TRUE, status = 'success',
                                    plotOutput('plot_iris', height = 250))),
                                box(
                                  title = 'Table Output', width = 12, solidHeader = TRUE, status = 'warning',
                                  tableOutput('table_iris'))
                                ),
                        
                        tabItem(tabName = 'kable',
                                box(
                                  title = tagList(icon('sign-in'), 'Input'), width = 2, background = 'black',
                                  radioButtons('input_kable_1', 'Please select:', list('Show row names' = 'TRUE', 'Hide row names' = 'FALSE')),
                                  submitButton('Update')),
                                box(
                                  title = 'knitr::kable', width = 10, solidHeader = TRUE, status = 'primary',
                                  tableOutput('example_kable')),
                                br(),
                                br(),
                                h1('---------------------------------------------------------------------------------------------------------')
                                ),
                        
                        tabItem(tabName = 'datatable',
                                box(
                                  title = tagList(icon('sign-in'), 'Input'), width = 2, background = 'black',
                                  radioButtons('input_datatable_1', 'Please select:', list('Show filter' = 'top', 'Hide filter' = 'none')),
                                  submitButton('Update')),
                                box(
                                  title = 'DT::datatable', width = 10, solidHeader = TRUE, status = 'primary',
                                  dataTableOutput('example_datatable'))
                        ),
                        
                        tabItem(tabName = 'highcharter',
                                box(
                                  title = tagList(icon('sign-in'), 'Input'), width = 3, background = 'black',
                                  selectInput('input_highcharter_1', 'Please select the type of plot:', choices = c('Scatter', 'Line', 'Histogram', 'Density', 'Graph', 'Stock', 'Map', 'Custom')),
                                  submitButton('Update')),
                                box(
                                  title = 'highcharter', width = 9, solidHeader = TRUE, status = 'primary',
                                  highchartOutput('example_highcharter', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'dygraphs',
                                box(
                                  title = 'dygraphs', width = 12, solidHeader = TRUE, status = 'primary',
                                  dygraphOutput('example_dygraphs', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'visNetwork',
                                box(
                                  title = tagList(icon('sign-in'), 'Input'), width = 3, background = 'black',
                                  selectInput('input_visNetwork_1', 'Please select the type of plot:', choices = c('Image', 'Icon', 'Custom')),
                                  submitButton('Update')),
                                box(
                                  title = 'visNetwork', width = 9, solidHeader = TRUE, status = 'primary',
                                  visNetworkOutput('example_visNetwork', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'networkD3',
                                box(
                                  title = 'networkD3', width = 12, solidHeader = TRUE, status = 'primary',
                                  sankeyNetworkOutput('example_networkD3', height = '600px'))
                        ),
                        
                        tabItem(tabName = 'references',
                                br(),
                                br(),
                                br(),
                                br(),
                                tags$div(
                                  tags$ul(
                                    tags$li(h3('For more information, please check the following resources')),
                                    tags$ul(tags$li(a(h4('R shiny'), href = 'http://shiny.rstudio.com/tutorial/', target = 'black')), 
                                            tags$li(a(h4('shinydashboard'), href = 'http://rstudio.github.io/shinydashboard/index.html', target = 'black')),
                                            tags$li(h4('Table')),
                                            tags$ul(tags$li(a(h5('knitr'), href = 'https://github.com/yihui/knitr', target = 'black')),
                                                    tags$li(a(h5('DT'), href = 'https://rstudio.github.io/DT/', target = 'black'))), 
                                            tags$li(h4('Plot')),
                                            tags$ul(tags$li(a(h5('highcharter'), href = 'http://jkunst.com/highcharter/', target = 'black')),
                                                    tags$li(a(h5('dygraphs'), href = 'https://rstudio.github.io/dygraphs/', target = 'black')),
                                                    tags$li(a(h5('visNetwork'), href = 'https://datastorm-open.github.io/visNetwork/', target = 'black')),
                                                    tags$li(a(h5('networkD3'), href = 'https://christophergandrud.github.io/networkD3/', target = 'black'))))))
                                )
                        )
                    )
)








#### Server
server <- function(input, output) {
  
  output$text_iris <- renderPrint({
    summary(iris)})
  
  output$table_iris <- renderTable({
    head(iris)})
  
  output$plot_iris <- renderPlot({
    plot(iris)})
  
  output$example_kable <- function() {
    kable(iris, row.names = input$input_kable_1, 'html') %>%
      kable_styling('striped', full_width = F)
  }
  
  output$example_datatable <- renderDataTable({
    datatable(iris, filter = input$input_datatable_1)
  })
  
  output$example_highcharter <- renderHighchart({
    if(input$input_highcharter_1 == 'Scatter') {
      hchart(mpg, 'scatter', hcaes(x = displ, y = hwy, group = class))
    } else if(input$input_highcharter_1 == 'Line') {
      data(citytemp)
      highchart() %>% 
        hc_xAxis(categories = citytemp$month) %>% 
        hc_add_series(name = 'Tokyo', data = citytemp$tokyo) %>% 
        hc_add_series(name = 'London', data = citytemp$london) %>% 
        hc_add_series(name = 'Other city', data = (citytemp$tokyo + citytemp$london)/2)
    } else if(input$input_highcharter_1 == 'Histogram') {
      hchart(diamonds$price, name = 'You can zoom in') 
    } else if(input$input_highcharter_1 == 'Density') {
      hchart(density(diamonds$price), type = 'area', color = '#B71C1C', name = 'Price')
    } else if(input$input_highcharter_1 == 'Graph') {
      N <- 40
      net <- sample_gnp(N, p = 2/N)
      wc <- cluster_walktrap(net)
      V(net)$label <- seq(N)
      V(net)$name <- paste('Node ', seq(N))
      V(net)$page_rank <- round(page.rank(net)$vector, 2)
      V(net)$betweenness <- round(betweenness(net), 2)
      V(net)$degree <- degree(net)
      V(net)$size <- V(net)$degree
      V(net)$comm <- membership(wc)
      V(net)$color <- colorize(membership(wc))
      hchart(net, layout = layout_with_fr)
    } else if(input$input_highcharter_1 == 'Stock') {
      SPY <- getSymbols('SPY', from = Sys.Date() - lubridate::years(1), auto.assign = FALSE)
      SPY <- adjustOHLC(SPY)
      SPY.SMA.10 <- SMA(Cl(SPY), n = 5)
      SPY.SMA.200 <- SMA(Cl(SPY), n = 100)
      SPY.RSI.14 <- RSI(Cl(SPY))
      SPY.RSI.SellLevel <- xts(rep(70, NROW(SPY)), index(SPY))
      SPY.RSI.BuyLevel <- xts(rep(30, NROW(SPY)), index(SPY))
      
      highchart(type = 'stock') %>% 
        # create axis
        hc_yAxis_multiples(create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)) %>% 
        # series
        hc_add_series(SPY, yAxis = 0, name = 'SPY') %>% 
        hc_add_series(SPY.SMA.10, yAxis = 0, name = 'Fast MA') %>% 
        hc_add_series(SPY.SMA.200, yAxis = 0, name = 'Slow MA') %>% 
        hc_add_series(SPY$SPY.Volume, color = 'gray', yAxis = 1, type = 'column', name = 'Volume') %>% 
        hc_add_series(SPY.RSI.14, yAxis = 2, color = hex_to_rgba('green', 0.7), name = 'Osciallator') %>%
        hc_add_series(SPY.RSI.SellLevel, color = hex_to_rgba('red', 0.7), yAxis = 2, name = 'Sell level') %>% 
        hc_add_series(SPY.RSI.BuyLevel, color = hex_to_rgba('blue', 0.7), yAxis = 2, name = 'Buy level') 
    } else if(input$input_highcharter_1 == 'Map') {
      mapdata <- get_data_from_map(download_map_data('countries/us/us-all'))
      set.seed(1234)
      data_fake <- mapdata %>% 
        select(code = `hc-a2`) %>% 
        mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))
      
      hcmap('countries/us/us-all', data = data_fake, value = 'value',
            joinBy = c('hc-a2', 'code'), name = 'Fake data',
            dataLabels = list(enabled = TRUE, format = '{point.name}'),
            borderColor = '#FAFAFA', borderWidth = 0.1,
            tooltip = list(valueDecimals = 2, valuePrefix = '$', valueSuffix = ' USD')) 
    } else {
      colors <- c('#FB1108', '#FD150B', '#FA7806', '#FBE426', '#FCFB8F', '#F3F5E7', '#C7E4EA','#ABD6E6','#9AD2E1')
      stars$color <- colorize(log(stars$temp), colors)
      x <- c('Luminosity', 'Temperature', 'Distance')
      y <- sprintf('{point.%s:.2f}', c('lum', 'temp', 'distance'))
      tltip <- tooltip_table(x, y)
      
      hchart(stars, 'scatter', hcaes(temp, lum, size = radiussun, color = color)) %>% 
        hc_chart(backgroundColor = 'black') %>% 
        hc_xAxis(type = 'logarithmic', reversed = TRUE) %>% 
        hc_yAxis(type = 'logarithmic', gridLineWidth = 0) %>% 
        hc_title(text = 'Our nearest Stars') %>% 
        hc_subtitle(text = 'In a Hertzsprung-Russell diagram') %>% 
        hc_tooltip(useHTML = TRUE, headerFormat = '', pointFormat = tltip) %>% 
        hc_size(height = 600)
    }
  })
  
  output$example_dygraphs <- renderDygraph({
    lungDeaths <- cbind(mdeaths, fdeaths)
    dygraph(lungDeaths) %>%
      dySeries('mdeaths', label = 'Male') %>%
      dySeries('fdeaths', label = 'Female') %>%
      dyOptions(stackedGraph = TRUE) %>%
      dyRangeSelector(height = 20)
  })
  
  output$example_visNetwork <- renderVisNetwork({
    if(input$input_visNetwork_1 == 'Image') {
      path_to_images <- 'https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/img/indonesia/'
      nodes <- data.frame(id = 1:4, shape = c('image', 'circularImage'), image = paste0(path_to_images, 1:4, '.png'), label = 'I am an image')
      edges <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))
      
      visNetwork(nodes, edges, width = '100%') %>% 
        visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
        visLayout(randomSeed = 2)
    } else if(input$input_visNetwork_1 == 'Icon') {
      nb <- 10
      nodes <- data.frame(id = 1:nb, label = paste('Label', 1:nb),
                          group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
                          title = paste0('<p>', 1:nb,'<br>Tooltip !</p>'), stringsAsFactors = FALSE)
      edges <- data.frame(from = c(8,2,7,6,1,8,9,4,6,2),
                          to = c(3,7,2,7,9,1,5,3,2,9),
                          value = rnorm(nb, 10), label = paste('Edge', 1:nb),
                          title = paste0('<p>', 1:nb,'<br>Edge Tooltip !</p>'))
      
      visNetwork(nodes, edges, height = '500px', width = '100%') %>% 
        visGroups(groupname = 'A', shape = 'icon', icon = list(code = 'f0c0', size = 75)) %>%
        visGroups(groupname = 'B', shape = 'icon', icon = list(code = 'f007', color = 'red')) %>%
        visGroups(groupname = 'C', shape = 'icon', icon = list(code = 'f1b9', color = 'black')) %>%
        visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = T)) %>%
        addFontAwesome() %>%
        visLayout(randomSeed = 123)
    } else {
      nodes <- fromJSON('https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/nodes_miserables.json')
      edges <- fromJSON('https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/edges_miserables.json')
      
      visNetwork(nodes, edges, height = '700px', width = '100%') %>%
        visOptions(selectedBy = 'group', highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visPhysics(stabilization = FALSE)
    }
  })
  
  output$example_networkD3 <- renderSankeyNetwork({
    URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/', 'master/JSONdata/energy.json')
    Energy <- fromJSON(URL)
  
    sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = 'source',
                  Target = 'target', Value = 'value', NodeID = 'name',
                  units = 'TWh', fontSize = 12, nodeWidth = 30)
  })
  
}




#### Run App
shinyApp(ui, server)



