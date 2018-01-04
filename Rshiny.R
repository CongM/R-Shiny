######################################
## Rshiny.R
## Sun Yat-sen University
## Made by: Cong Mu,Yukang Jiang
######################################

library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(DT)
library(lattice)
library(ggplot2)
library(ggthemes)
library(rCharts)
library(networkD3)
library(wordcloud2)
library(plotly)

#由于数据保密性的需要，rda文件未予公开，敬请谅解
load('data_shiny.rda')
load('ggplot.rda')
load('sankey_data.rda')

data(Chem97, package = "mlmRev")
data(Oats, package = "MEMSS")

index_time<-c('00:00-01:00','01:00-02:00','02:00-03:00','03:00-04:00',
              '04:00-05:00','05:00-06:00','06:00-07:00','07:00-08:00',
              '08:00-09:00','09:00-10:00','10:00-11:00','11:00-12:00',
              '12:00-13:00','13:00-14:00','14:00-15:00','15:00-16:00',
              '16:00-17:00','17:00-18:00','18:00-19:00','19:00-20:00',
              '20:00-21:00','21:00-22:00','22:00-23:00')

hot.pro = as.character(hot$program)
t = 1:48
time = paste(c(rep('0', 19), rep('', 29)), t%/%2, ':', 3*(t%%2), '0', sep = '')

# 筛选频道
dat.pro = function(ind) {
  dat.hot.pro = hot[ind, ]
  hot.chan = as.character(dat.hot.pro[seq(2, ncol(dat.hot.pro) - 1, 2)])
  hot.rate = as.character(dat.hot.pro[seq(3, ncol(dat.hot.pro), 2)])
  hot.new = data.frame('时间' = time, '节目' = hot.chan, '收视率' = hot.rate)
}


# 绘图
p1=ggplot(data=channel_id,aes(x=last,y=id))+
  geom_point()+stat_smooth()+theme_economist()+
  labs(x='停留总时长',y='观看家庭数')+ggtitle('观看家庭数与停留总时长的关系（以频道为分析单位）')

p11=ggplot(data=channel_id1,aes(x=last,y=id))+
  geom_point()+stat_smooth()+theme_economist()+
  labs(x='停留总时长',y='观看家庭数')+ggtitle('00:00-05:00观看家庭数与停留总时长的关系（以频道为分析单位）')

p12=ggplot(data=channel_id2,aes(x=last,y=id))+
  geom_point()+stat_smooth()+theme_economist()+
  labs(x='停留总时长',y='观看家庭数')+ggtitle('06:00-11:00观看家庭数与停留总时长的关系（以频道为分析单位）')

p13=ggplot(data=channel_id3,aes(x=last,y=id))+
  geom_point()+stat_smooth()+theme_economist()+
  labs(x='停留总时长',y='观看家庭数')+ggtitle('12:00-17:00观看家庭数与停留总时长的关系（以频道为分析单位）')

p14=ggplot(data=channel_id4,aes(x=last,y=id))+
  geom_point()+stat_smooth()+theme_economist()+
  labs(x='停留总时长',y='观看家庭数')+ggtitle('18:00-23:00观看家庭数与停留总时长的关系（以频道为分析单位）')

p2=ggplot(data=program_id,aes(x=last,y=id))+
  geom_point()+stat_smooth()+theme_economist()+
  labs(x='停留总时长',y='观看家庭数')+ggtitle('观看家庭数与停留总时长的关系（以节目为分析单位）')

p21=ggplot(data=program_id1,aes(x=last,y=id))+
  geom_point()+stat_smooth()+theme_economist()+
  labs(x='停留总时长',y='观看家庭数')+ggtitle('00:00-05:00观看家庭数与停留总时长的关系（以节目为分析单位）')

p22=ggplot(data=program_id2,aes(x=last,y=id))+
  geom_point()+stat_smooth()+theme_economist()+
  labs(x='停留总时长',y='观看家庭数')+ggtitle('06:00-11:00观看家庭数与停留总时长的关系（以节目为分析单位）')

p23=ggplot(data=program_id3,aes(x=last,y=id))+
  geom_point()+stat_smooth()+theme_economist()+
  labs(x='停留总时长',y='观看家庭数')+ggtitle('12:00-17:00观看家庭数与停留总时长的关系（以节目为分析单位）')

p24=ggplot(data=program_id4,aes(x=last,y=id))+
  geom_point()+stat_smooth()+theme_economist()+
  labs(x='停留总时长',y='观看家庭数')+ggtitle('18:00-23:00观看家庭数与停留总时长的关系（以节目为分析单位）')






ui <- dashboardPage(skin = 'green',
                    dashboardHeader(title = '利用R语言进行交互'),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem('布局', tabName = 'layout', icon = icon('th'),
                                 menuItem("输入", tabName = 'input1', icon = icon('sign-in')),
                                 menuItem("输出", tabName = 'output1', icon = icon('sign-out')) ),
                        menuItem('收视数据表格', tabName = 'table', icon = icon('table')),
                        menuItem('图', tabName = 'plot1', icon = icon('picture-o'),
                                 menuItem("简介",tabName='plot1intro'),
                                 menuItem("示例",tabName='plot1exam') ),
                        menuItem('更美的图', tabName = 'plot2', icon = icon('picture-o'),
                                 menuItem("lattice",tabName='lattice',
                                          menuItem("简介",tabName='latticeintro'),
                                          menuItem("示例",tabName='latticeexam') ),
                                 menuItem("ggplot2",tabName='ggplot2',
                                          menuItem("简介",tabName='ggplot2intro'),
                                          menuItem("示例",tabName='ggplot2exam') ) ),
                        menuItem('再美一些的图', tabName = 'plot3', icon = icon('picture-o'),
                                 menuItem("plotly",tabName='Plotly'),
                                 menuItem("rCharts",tabName='rCharts',
                                          menuItem("简介",tabName='rChartsintro'),
                                          menuItem("示例",tabName='rChartsexam') ),
                                 menuItem("networkD3",tabName='networkD3',
                                          menuItem("简介",tabName='networkD3intro'),
                                          menuItem("示例",tabName='networkD3exam') ),
                                 menuItem("wordcloud2",tabName='wordcloud2',
                                          menuItem("简介",tabName='wordcloud2intro'),
                                          menuItem("示例",tabName='wordcloud2exam') ) ),
                        menuItem('最美的图', tabName = 'plot4', icon = icon('picture-o'),
                                 badgeLabel = "Best!", badgeColor = "yellow")
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'input1',
                                fluidRow(
                                  box(
                                    title = "滑动输入", width = 6, status = "primary",
                                    sliderInput("integer", "Integer:",
                                                min=0, max=1000, value=500)
                                  ),
                                  box(
                                    title = "手动输入", width = 6, status = "warning",
                                    numericInput("integer", "Integer:", 10)
                                  ) 
                                ),
                                
                                fluidRow(
                                  box(
                                    title = "选项卡", width = 4, solidHeader = TRUE, status = "success",
                                    radioButtons('select', "请选择:",
                                                 list("选项1" = "select1",
                                                      "选项2" = "select2"))
                                  ),
                                  box(
                                    title = "选项列表", width = 4, solidHeader = TRUE, status = "info",
                                    selectInput('select', "请选择:", 
                                                choices = c("选项1", "选项2", "选项3"))
                                  ),
                                  box(
                                    title = "文字输入", width = 4, solidHeader = TRUE, background = 'maroon',
                                    textInput("text", "请输入：", value = '输入...')
                                  )
                                )
                        ),
                        
                        
                        tabItem(tabName = 'output1',
                                fluidRow(
                                  box(
                                    title = "结果输出", width = 6, solidHeader = TRUE, status = "primary",
                                    verbatimTextOutput("text.iris")
                                  ),
                                  box(
                                    title = "Plot输出", width = 6, solidHeader = TRUE, status = "success",
                                    plotOutput("plot.iris", height = 250)
                                  )
                                ),
                                box(
                                  title = "表格输出", width = 12, solidHeader = TRUE,status = "warning",
                                  tableOutput("table.iris")
                                )
                        ),
                        
                        tabItem(tabName = 'table',
                                tabsetPanel(
                                  tabPanel("普通表格",
                                           tableOutput("table.hot")
                                  ),
                                  
                                  tabPanel("交互表格",
                                           box(
                                             title = tagList(icon("sign-in"), "输入"),
                                             width = 4, background = "black",
                                             radioButtons('select1', "请选择:",
                                                          list("按频道" = "1",
                                                               "按时间" = "2")
                                             ),
                                             selectInput('program', "请选择频道:", 
                                                         choices = hot.pro),
                                             selectInput('time.sel', "请选择时间:", 
                                                         choices = time),
                                             submitButton("更新视图")
                                           ),
                                           
                                           tabBox(
                                             title = tagList(icon("table"), "表格展现"),
                                             width = 8, 
                                             tabPanel("使用View()", 
                                                      tableOutput("output.table1")
                                             ),
                                             tabPanel("使用DT包",
                                                      dataTableOutput("output.table2")
                                             )
                                           )
                                           
                                  )
                                )
                        ),
                        
                        
                        tabItem(tabName = 'plot1intro',
                                
                                box(
                                  title = "基本绘图也可以很强大", width = 12, solidHeader = TRUE, status = "success",
                                  textOutput("plot1introduce")
                                ),
                                
                                box(
                                  width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("plot1plot1", height = 250)
                                ),
                                
                                box(
                                  width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("plot1plot2", height = 250)
                                ),
                                
                                box(
                                  width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("plot1plot3", height = 250)
                                ),
                                
                                box(
                                  width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("plot1plot4", height = 250)
                                ),
                                
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                h3('特别鸣谢：白中流、吴张斌'),
                                br(),
                                br(),
                                
                                h1('_____________________________________________________________'),
                                br()
                                
                                
                                
                        ),
                        
                        
                        tabItem(tabName = 'plot1exam',
                                
                                h1('plot+polygon+rect'),
                                br(),
                                box(
                                  title = "Step1.根据数据绘制线图", width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("plot1exam1", height = 250)
                                ),
                                
                                box(
                                  title = "Step2.添加直线x=0绘制多边形并涂色", width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("plot1exam2", height = 250)
                                ),
                                
                                box(
                                  title = "Step3.用白色矩形遮住下方", width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("plot1exam3", height = 250)
                                ),
                                
                                box(
                                  title = "Step4.再次绘制线条", width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("plot1exam4", height = 250)
                                ),
                                
                                h1('Step1.'),
                                h2('>set.seed(1120)'),
                                h2('>x = rnorm(40)  #产生40个正态随机数'),
                                h2('>plot(x, xlab = "", type = "l")  #画线图'),
                                h1('Step2.'),
                                h2('>polygon(c(1, 1:40, 40), c(0, x, 0), col = "gray")'),
                                h1('Step3.'),
                                h2('>xy = par("usr")  #图像所在矩形区域的各个顶点坐标'),
                                h2('>rect(xy[1], xy[3], xy[2], 0, col = "white")  #用白色矩形挡住了0以下的部分'),
                                h1('Step4.'),
                                h2('>lines(x)  #重画一遍x的线条'),
                                
                                h1('_____________________________________________________________'),
                                br()
                                
                                
                                
                                
                        ),
                        
                        
                        
                        tabItem(tabName = 'latticeintro',
                                
                                h1('lattice：多维数据展示比较的利器'),
                                h2('常用函数：xyplot、barchart、histogram、dotplot、bwplot等'),
                                br(),
                                
                                box(
                                  title = "分面", width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("latticeintro1", height = 250)
                                ),
                                
                                box(
                                  title = "分组", width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("latticeintro2", height = 250)
                                ),
                                
                                box(
                                  title = "多维分面", width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("latticeintro3")
                                ),
                                
                                box(
                                  title = "分组与分面结合", width = 6, solidHeader = TRUE, status = "success",
                                  plotOutput("latticeintro4")
                                ),
                                
                                h1('_____________________________________________________________'),
                                br()
                                
                                
                                
                        ),
                        
                        
                        tabItem(tabName = 'latticeexam',
                                
                                h1('分面'),
                                h2('>histogram( ~ gcsescore | factor(score), data = Chem97)'),
                                h2('#score为离散变量，“｜”之后的元素用来分面'),
                                br(),
                                
                                box(
                                  title = "分面", width = 12, solidHeader = TRUE, status = "success",
                                  plotOutput("latticeexam1", height = 250)
                                ),
                                
                                br(),
                                h1('分组'),
                                h2('>densityplot(~ gcsescore, data = Chem97, groups = score, plot.points = FALSE, ref = TRUE,
                                   auto.key = list(columns = 3))'),
                                h2('#group代表同一面中的不同线条，auto.key表示图例排列方式'),
                                br(),
                                
                                box(
                                  title = "分组", width = 12, solidHeader = TRUE, status = "success",
                                  plotOutput("latticeexam2", height = 250)
                                ),
                                
                                br(),
                                h1('多维分面'),
                                h2('>xyplot(yield ~ nitro | Variety + Block, data = Oats, type = "o")'),
                                h2('#多维分面采取“＋”方案'),
                                br(),
                                
                                box(
                                  title = "多维分面", width = 12, solidHeader = TRUE, status = "success",
                                  plotOutput("latticeexam3")
                                ),
                                
                                h1('_____________________________________________________________'),
                                br()
                                
                                
                                
                                
                        ),
                        
                        
                        tabItem(tabName = 'ggplot2intro',
                                
                                h1('ggplot2：功能强大，美观丰富，语法层次较为清晰，细节调整稍显复杂'),
                                h2('基本思想：图形叠加'),
                                h3('数据：常常需结合dplyr，reshape2调整成合适的数据结构'),
                                h3('映射：aes参数，将数据与图形元素映射'),
                                h3('几何形状：geom_xx，决定图形类型（点、线等）'),
                                h3('统计变换：stat_xx，展示数据的统计变换'),
                                h3('位置调整：position参数，扰动图，柱状图位置调整等'),
                                h3('......'),
                                br(),
                                
                                
                                h1('_____________________________________________________________'),
                                br()
                                
                                
                                
                        ),
                        
                        
                        
                        tabItem(tabName = 'ggplot2exam',
                                tabsetPanel(
                                  tabPanel("收视率总表",
                                           box(title = "每个时刻观众最多综合榜",
                                               width = 12,solidHeader = TRUE,
                                               background = "light-blue", plotOutput("plot.hot"))
                                  ),
                                  tabPanel("观看家庭数~停留总时长",
                                           column(4,selectInput(inputId='unit',label='请选择分析单位',
                                                                choices=c('频道','节目'),selected='频道')
                                           ),
                                           
                                           column(4,selectInput(inputId='interval',label='请选择时段',
                                                                choices=c('00:00-23:00','00:00-05:00','06:00-11:00','12:00-17:00','18:00-23:00'),selected='00:00-23:00')
                                           ),
                                           
                                           column(4,submitButton("更新视图")),
                                           
                                           box(
                                             title = "观看家庭数与停留总时长的关系",width = 12,
                                             solidHeader = TRUE, status = "success",
                                             plotOutput("last_id")
                                           ),
                                           
                                           br(),
                                           h2('p1=ggplot(data=channel_id,aes(x=last,y=id)) #基础图层'),
                                           h2('p1=p1+geom_point() #点图'),
                                           h2('p1=p1+stat_smooth() #加拟合曲线'),
                                           h2('p1=p1+theme_economist() #使用ggthemes包中的预设主题'),
                                           h2('p1=p1+labs(x="停留总时长",y="观看家庭数") #坐标轴指示'),
                                           h2('p1=p1+ggtitle("观看家庭数与停留总时长的关系（以频道为分析单位）") #标题'),
                                           
                                           h1('_____________________________________________________________'),
                                           br()
                                  ),
                                  tabPanel("央视节目种类",
                                           column(6,selectInput(inputId='time.pro1',label='请选择时间',
                                                                choices=c('00:00','08:00','12:00','19:00','21:00','22:00'))
                                           ),
                                           
                                           column(6,submitButton("更新视图")),
                                           
                                           box(
                                             title = "",width = 12,
                                             solidHeader = TRUE, status = "success",
                                             plotOutput("plot.pro1")
                                           )
                                  )
                                  
                                )
                                
                                
                        ),
                        
                        
                        tabItem(tabName = 'Plotly',
                                tabsetPanel(
                                  tabPanel("收视率总表",
                                           box(title = "每个时刻观众最多综合榜",
                                               width = 12,solidHeader = TRUE,
                                               background = "light-blue", plotlyOutput("plotly.hot"))
                                  ),
                                  
                                  tabPanel("观看家庭数~停留总时长",
                                           
                                           box(
                                             title = "观看家庭数与停留总时长的关系（基于频道）",width = 12,
                                             solidHeader = TRUE, status = "success",
                                             plotlyOutput("last_id.1")
                                           ),
                                           
                                           box(
                                             title = "观看家庭数与停留总时长的关系（基于节目）",width = 12,
                                             solidHeader = TRUE, status = "success",
                                             plotlyOutput("last_id.2")
                                           ),
                                           
                                           h1('_____________________________________________________________'),
                                           br()
                                           
                                  )
                                  
                                )
                                
                                
                        ),
                        
                        
                        
                        
                        tabItem(tabName = 'rChartsintro',
                                
                                h1('rCharts：在R中生成基于D3的Web界面'),
                                h2('安装：'),
                                h3('>require(devtools)'),
                                h3('>install_github("rCharts", "ramnathv") or install_github("ramnathv/rCharts")'),
                                h2('常用函数：nPlot、hPlot、mPlot'),
                                br(),
                                
                                box(
                                  bsolidHeader = TRUE,collapsible = TRUE,width=12,
                                  showOutput("rChartsintro1","nvd3")
                                ),
                                
                                box(
                                  bsolidHeader = TRUE,collapsible = TRUE,width=12,
                                  showOutput("rChartsintro2","highcharts")
                                ),
                                
                                
                                h1('_____________________________________________________________'),
                                br()
                                
                                
                                
                        ),
                        
                        
                        
                        
                        tabItem(tabName = 'rChartsexam',
                                
                                h1('收视数据：不同时段观看电视的家庭数'),
                                br(),
                                
                                column(4,selectInput(inputId='type',label='请选择分析类型',
                                                     choices=c('总览','基于不同类型频道','基于不同类型节目'),selected='总览')
                                ),
                                
                                column(4,submitButton("更新视图")),
                                
                                box(
                                  title = "不同时段观看电视的家庭数",width=12,
                                  bsolidHeader = TRUE,collapsible = TRUE,
                                  showOutput("hour_id","highcharts")
                                ),
                                
                                br(),
                                h3('>p3=hPlot(id~time,data=temp3,group="program_type",color="program_type",type="line")'),
                                
                                h1('_____________________________________________________________'),
                                br()
                        ),
                        
                        
                        tabItem(tabName = 'networkD3intro',
                                
                                h1('networkD3：网络关系图'),
                                h1('常用函数：chordNetwork、forceNetwork、sankeyNetwork'),
                                br(),
                                
                                box(
                                  bsolidHeader = TRUE,collapsible = TRUE,width=12,
                                  forceNetworkOutput("networkD3intro1")
                                ),
                                
                                
                                h1('_____________________________________________________________'),
                                br()
                        ),
                        
                        
                        
                        tabItem(tabName = 'networkD3exam',
                                
                                h1('收视数据：分时段观众流动情况'),
                                br(),
                                br(),
                                br(),
                                
                                tabsetPanel(
                                  tabPanel("1小时流动",
                                           box(
                                             title = "选择您关注的时段时段",width=4,background = 'black',
                                             selectInput('time.sel.1', "请选择时段:", 
                                                         choices = index_time),
                                             submitButton("更新视图")
                                           ),
                                           
                                           box(
                                             title = "1小时观众流动情况",width=8,
                                             chordNetworkOutput("networkD3exam1")
                                           ),
                                           
                                           br(),
                                           h2('>chordNetwork(as.matrix(flow[[1]]),labels=c("央视","卫视","江苏","其他"),fontSize=20)'),
                                           br(),
                                           h1('_____________________________________________________________'),
                                           br()
                                           
                                           
                                           ),
                                  
                                  tabPanel("多小时流动",
                                           box(
                                             title = "17:00——21:00观众流动情况",width=12,
                                             sankeyNetworkOutput("sankey")
                                           ),
                                           
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           
                                           h3('特别鸣谢：李媛媛'),
                                           br(),
                                           br(),
                                           h1('_____________________________________________________________'),
                                           br()
                                  )
                                )
                                
                                         
                               
                        ),
                        
                        
                        
                        
                        
                        
                        tabItem(tabName = 'wordcloud2intro',
                                
                                h1('wordcloud2：个性化词云'),
                                h1('常用函数：wordcloud2、letterCloud'),
                                br(),
                                
                                
                                box(
                                  bsolidHeader = TRUE,collapsible = TRUE,width=12,
                                  wordcloud2Output("wordcloud2intro1")
                                ),
                                
                                h1('_____________________________________________________________'),
                                br()
                        ),
                        
                        
                        
                        tabItem(tabName = 'wordcloud2exam',
                                
                                h1('收视数据：不同类型用户喜爱的节目'),
                                br(),
                                box(
                                  title = "选择您绘制的形状",width=4,background = 'black',
                                  selectInput('type1', "请选择用户类型:", 
                                              choices = c('第1类用户', '第2类用户', '第3类用户')),
                                  selectInput('shape1', "请选择形状:", 
                                              choices = c('circle 圆形', 'diamond 菱形', 'triangle 三角形', 'pentagon 五边形', 'cordioid 心形')),
                                  submitButton("更新视图")
                                ),
                                
                                box(title = "不同类型用户喜爱的节目",
                                    bsolidHeader = TRUE,collapsible = TRUE,width=8,
                                    wordcloud2Output("wordcloud2exam1")
                                ),
                                
                                
                                
                                
                                br(),
                                br(),
                                h2('>wordcloud2(temp[1:100,],size=0.2)'),
                                h2('>letterCloud(temp[1:100,],word="R",size=0.15)'),
                                br(),
                                
                                h1('_____________________________________________________________'),
                                br()
                        ),
                        
                        
                        
                        
                        tabItem(tabName = 'plot4',
                                
                                br(),
                                br(),
                                
                                img(src='SYSU.png'),
                                br(),
                                br(),
                                h2('参考资料：'),
                                br(),
                                a(h3('http://shiny.rstudio.com/tutorial/'),href="http://shiny.rstudio.com/tutorial/",target="black"),
                                br(),
                                a(h3('http://rstudio.github.io/shinydashboard/index.html'),href="http://rstudio.github.io/shinydashboard/index.html",target="black"),
                                
                                h1('_____________________________________________________________'),
                                br()
                                
                                
                                
                        )
                        
                        
                        
                        
                        
                        
                      )
                    )
)



server <- function(input, output) {
  output$text.iris = renderPrint({
    summary(iris) })
  output$table.iris = renderTable({
    iris })
  output$plot.iris = renderPlot({
    plot(iris) })
  
  
  output$table.hot = renderTable({
    hot })
  output$output.table1 = renderTable({
    #收视数据实例
    time.sel = input$time.sel
    program = input$program
    ind = which(hot$program == program)
    
    dat.pro1 = dat.pro(ind)
    
    ind.t = which(time.sel == time)
    dat.pro2 = hot[, c(1, ind.t + 1, ind.t + 2)]
    names(dat.pro2) = c('频道', '节目', '收视率')
    if(input$select1 == '1') output.table = dat.pro1 else output.table = dat.pro2
    output.table
  })
  
  output$output.table2 = renderDataTable({
    #收视数据实例
    time.sel = input$time.sel
    program = input$program
    
    ind = which(hot$program == program)
    
    dat.pro1 = dat.pro(ind)
    
    ind.t = which(time.sel == time)
    dat.pro2 = hot[, c(1, ind.t + 1, ind.t + 2)]
    names(dat.pro2) = c('频道', '节目', '收视率')
    
    if(input$select1 == '1') output.table = dat.pro1 else output.table = dat.pro2
    output.table
  })
  
  
  output$plot1plot1 = renderPlot({
    
    xx = c(1912, 1912:1971, 1971)
    yy = c(min(nhtemp), nhtemp, min(nhtemp))
    plot(xx, yy, type = "n", xlab = "Year", ylab = "Temperatures")
    
    for(i in seq(255, 0, -3)) 
    {
      yy = c(45, nhtemp - (nhtemp - min(nhtemp)) * (1 - i/255), 45)
      polygon(xx, yy, col = rgb(i/255, 1 , 0), border = NA) 
      Sys.sleep(0.1)
      
    }
    
    
  })
  
  
  
  output$plot1plot2 = renderPlot({
    
    par(mar = c(0.2, 0.2, 0.2, 0.2), mfrow = c(2, 2))
    
    for(n in c(60, 70, 80, 90)) 
    {
      set.seed(1120)
      plot.new()
      size = c(replicate(n, 1/rbeta(2, 1.5, 4)))
      center = t(replicate(n, runif(2)))
      center = center[rep(1:n, each = 2), ]
      color = apply(replicate(2 * n, sample(c(0:9,LETTERS[1:6]), 8, TRUE)), 2, function(x) sprintf("#%s",paste(x, collapse = "")))
      points(center, cex = size, pch = rep(20:21, n),col = color)
      
    }
    
    
  })
  
  
  
  
  
  output$plot1plot3 = renderPlot({
    
    fourfoldplot(UCBAdmissions, mfcol = c(2, 3))
    
  })
  
  
  output$plot1plot4 = renderPlot({
    
    heatmap(as.matrix(mtcars), col = brewer.pal(9, "RdYlBu"),scale = "column", margins = c(4, 8))
    
  })
  
  output$plot1introduce = renderText({
    
    text1<-'常用函数：plot、points、line、abline、polygon、rect、title、legendhist、pie、barplot、fourfoldplot、heatmap等'
    print(text1)
  })
  
  
  
  output$plot1exam1 = renderPlot({
    
    set.seed(1120)
    x = rnorm(40)
    plot(x, xlab = "", type = "l")
    
  })
  
  output$plot1exam2 = renderPlot({
    
    set.seed(1120)
    x = rnorm(40)
    plot(x, xlab = "", type = "l")
    
    polygon(c(1, 1:40, 40), c(0, x, 0), col = "gray")
    
  })
  
  
  
  
  output$plot1exam3 = renderPlot({
    
    set.seed(1120)
    x = rnorm(40)
    plot(x, xlab = "", type = "l")
    
    polygon(c(1, 1:40, 40), c(0, x, 0), col = "gray")
    
    xy = par("usr")
    rect(xy[1], xy[3], xy[2], 0, col = "white")
    
  })
  
  output$plot1exam4 = renderPlot({
    
    set.seed(1120)
    x = rnorm(40)
    plot(x, xlab = "", type = "l")
    
    polygon(c(1, 1:40, 40), c(0, x, 0), col = "gray")
    
    xy = par("usr")
    rect(xy[1], xy[3], xy[2], 0, col = "white")
    
    lines(x)
    
  })
  
  output$latticeintro1 = renderPlot({
    
    histogram(~ gcsescore | factor(score), data = Chem97)
    
  })
  
  
  output$latticeintro2 = renderPlot({
    
    densityplot(~ gcsescore, data = Chem97, groups = score, plot.points = FALSE, ref = TRUE, auto.key = list(columns = 3))
    
  })
  
  output$latticeintro3 = renderPlot({
    
    xyplot(yield ~ nitro | Variety + Block, data = Oats, type = "o")
    
  })
  
  output$latticeintro4 = renderPlot({
    
    dotplot(variety ~ yield | site,barley, layout = c(1, 6), aspect = c(0.4),type="p", groups = year, auto.key = list(space = "right"))
    
  })
  
  
  output$latticeexam1 = renderPlot({
    
    histogram(~ gcsescore | factor(score), data = Chem97)
    
  })
  
  
  output$latticeexam2 = renderPlot({
    
    densityplot(~ gcsescore, data = Chem97, groups = score, plot.points = FALSE, ref = TRUE, auto.key = list(columns = 3))
    
  })
  
  output$latticeexam3 = renderPlot({
    
    xyplot(yield ~ nitro | Variety + Block, data = Oats, type = "o")
    
  })
  
  output$plot.hot <- renderPlot({
    ggplot(plot,aes(x=p1,y=y,fill=ind))+geom_bar(stat="identity")+
      geom_text(aes(label = z,vjust = .2,hjust = 0,color=ind),show.legend = F)+
      geom_text(aes(label = z1,vjust = .2,hjust = 1),show.legend = F)+
      coord_flip()+labs(x='时刻',y='收视率')+ggtitle('每个时刻观众最多综合榜')
  })
  
  output$last_id = renderPlot ({
    
    if(input$unit=='频道' & input$interval=='00:00-23:00')
      print(p1)
    
    if(input$unit=='频道' & input$interval=='00:00-05:00')
      print(p11)
    
    if(input$unit=='频道' & input$interval=='06:00-11:00')
      print(p12)
    
    if(input$unit=='频道' & input$interval=='12:00-17:00')
      print(p13)
    
    if(input$unit=='频道' & input$interval=='18:00-23:00')
      print(p14)
    
    if(input$unit=='节目' & input$interval=='00:00-23:00')
      print(p2)
    
    if(input$unit=='节目' & input$interval=='00:00-05:00')
      print(p21)
    
    if(input$unit=='节目' & input$interval=='06:00-11:00')
      print(p22)
    
    if(input$unit=='节目' & input$interval=='12:00-17:00')
      print(p23)
    
    if(input$unit=='节目' & input$interval=='18:00-23:00')
      print(p24)
    
  })
  
  output$plot.pro1 <- renderPlot({
    ind.plot.pro1 = match(input$time.pro1, c('00:00','08:00','12:00','19:00','21:00','22:00'))
    ind.plot.pro1.1 = ((ind.plot.pro1 - 1) * 6 + 1) : (ind.plot.pro1 * 6)
    ggplot(pro1[ind.plot.pro1.1,])+geom_bar(aes(x=种类,y=Freq,fill=种类),stat="identity")+
      coord_polar()+labs(x='',y='')+ggtitle('电视节目种类收视率比较')
  })
  
  output$plotly.hot <- renderPlotly({
    p.hot1 = ggplot(plot,aes(x=p1,y=y,fill=ind))+geom_bar(stat="identity")+
      geom_text(aes(label = z,vjust = .2,hjust = 0,color=ind),show.legend = F)+
      geom_text(aes(label = z1,vjust = .2,hjust = 1),show.legend = F)+
      coord_flip()+labs(x='时刻',y='收视率')+ggtitle('每个时刻观众最多综合榜')
    ggplotly(p.hot1)
  })
  
  output$last_id.1 = renderPlotly({
    
    ggplotly(p1)
    
  })
  
  output$last_id.2 = renderPlotly({
    
    ggplotly(p2)
    
  })
  

  
  
  
  output$rChartsintro1 = renderChart ({
    pp1 = nPlot(yield ~ Block, group = "Variety", data = Oats, type = "multiBarChart")
    pp1$chart(color = c('#7fc97f', '#beaed4', '#fdc086'))
    pp1$addParams(dom="rChartsintro1")
    return(pp1)
  })
  
  output$rChartsintro2 = renderChart ({
    a = hPlot(gcsescore ~ age, data = Chem97[1:50, ], type = "bubble", title = "气泡图", size = "gcsescore", group = "gender")
    a$colors('#8dd3c7', '#ffffb3')
    a$chart(zoomType = "xy")
    a$exporting(enabled = T)
    a$addParams(dom="rChartsintro2")
    return(a)
    
  })
  
  
  output$hour_id = renderChart ({
    
    temp1 = id_hour
    temp1$hour = as.factor(temp1$hour)
    p1 = hPlot(id~hour,data=temp1,type='line')
    p1$addParams(dom="hour_id")
    
    temp2 = data.frame(time=rep(count$time,4),id=c(count$央视,count$卫视,count$江苏,count$其他),channel_type=c(rep('CCTV',24),rep('LocalTV',24),rep('JiangsuTV',24),rep('OtherTV',24)))
    temp2$time = as.factor(temp2$time)
    p2 = hPlot(id~time,data=temp2,group='channel_type',color='channel_type',type='line')
    p2$addParams(dom="hour_id")
    
    temp3 = data.frame(time=rep(count2$time,6),id=c(count2$新闻类,count2$文艺类,count2$体育类,count2$影视类,count2$教育类,count2$服务类),program_type=c(rep('News',24),rep('Entertainment',24),rep('Sports',24),rep('Movies',24),rep('Education',24),rep('Services',24)))
    temp3$time = as.factor(temp3$time)
    p3 = hPlot(id~time,data=temp3,group='program_type',color='program_type',type='line')
    p3$addParams(dom="hour_id")
    
    if(input$type=='总览')
    {
      return(p1)
    }
    
    if(input$type=='基于不同类型频道')
    {
      return(p2)
    }
    
    if(input$type=='基于不同类型节目')
    {
      return(p3)
    }
    
  })
  
  
  
  output$networkD3intro1 = renderForceNetwork ({
    
    forceNetwork(Links = MisLinks, Nodes = MisNodes,Source = "source", Target = "target", Value = "value", NodeID = "name", Group = "group", opacity = 0.8)
    
  })
  
  
  output$networkD3exam1 = renderchordNetwork ({
    t1 = input$time.sel.1
    t1.fin = as.numeric(strsplit(t1,'\\:|\\-')[[1]][3])
    chordNetwork(as.matrix(flow[[t1.fin]]),labels=c('央视','卫视','江苏','其他'),fontSize=20)
    
  })
  
  output$sankey = renderSankeyNetwork ({
    
    sankeyNetwork(Links = flow_links, Nodes = flow_nodes, Source = "source",  
                  Target = "target", Value = "value", NodeID = "names",NodeGroup = "nodegroup",
                  fontSize = 10, nodeWidth = 30)
    
  })
  
  
  output$wordcloud2intro1 = renderWordcloud2 ({
    
    wordcloud2(demoFreqC[1:300, ])
    
  })
  
  
  output$wordcloud2exam1 = renderWordcloud2 ({
    
    
    type = input$type1
    type.fin = as.numeric(gsub('第|类|用|户','',type))
    dat = recommendprogram[[type.fin]]
    temp = dat[-1,]
    temp$program = as.character(temp$program)
    shape1 = input$shape1
    shape1.fin = strsplit(shape1,' ')[[1]][1]
    wordcloud2(temp[1:100,],size=0.2,shape=shape1.fin)
    
  })
  
  
 
  
}



shinyApp(ui, server)
