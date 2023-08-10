#---user interface--------------------------------------------------  
ui <- dashboardPage(skin = 'red',
                    header=dashboardHeaderPlus(title='My Creek View'),
                    
                    dashboardSidebar(
                      selectInput("SelectSite", "Please select Field Site:",
                                  choices = c("SYCA", "ARIK","BARC","BIGC","BLDE","BLUE","BLWA","CARI","COMO","CRAM","CUPE","FLNT","GUIL","HOPB","KING","LECO","LEWI","LIRO","MART","MAYF","MCDI","MCRA","OKSR","POSE","PRIN","PRLA","PRPO","REDB","SUGG","TECR","TOMB","TOOK","WALK","WLOU"), selected = F, multiple = T),
                      selectInput("SelectData", "Please select Protocol:",
                                  choices = c("Water Chemistry" = "DP1.20093.001", "MacroInverts Collection" = "DP1.20120.001"), selected = F, multiple = T),
                      #dateRangeInput('dateRange',label='Select Date Range',start=Sys.Date()-21,end=Sys.Date()),
                      submitButton("Process Selection"),
                      
                      sidebarMenu(
                        menuItem('Map View',tabName = 'E',icon=icon('map')),
                        menuItem('Data View',tabName = 'D',icon=icon('laptop-code')),
                        menuItem('QC Report',tabName = 'C',icon=icon('newspaper')),
                        menuItem('Other',tabName = 'dtable',icon=icon('table')),
                        menuItem('Readme',tabName='readme',icon=icon('info-circle'))
                      )
                    ),
                    
                    dashboardBody("May take up to several minutes - looking at SYCA creek data only",
                                  tabItems( # contains all tabs -  
                                    #plot
                                    tabItem(tabName = 'E',
                                            fluidRow(
                                              box(title='Analyte Comparisons',
                                                  footer = 'Relationship through time between analytes taken from Water Chemistry Samples',
                                                  status = 'info',
                                                  collapsible = T,
                                                  collapsed = F,
                                                  solidHeader = F,
                                                  height='650',
                                                  width='12',
                                                  column(12,withSpinner(plotlyOutput('Elab'),
                                                                        image = 'https://i.pinimg.com/originals/52/16/80/5216809ff35e0daf8bcada59fa04f3c4.gif',
                                                                        image.height = '750px', image.width = '1000px', proxy.height = '500px'),
                                                                        style='height:500px;overflow-y:scroll')
                                              )
                                            )
                                    ),
                                    #correlations
                                    tabItem(tabName = 'C',
                                            fluidRow(
                                              box(title='Correlation Report',
                                                  footer = 'Calculating correlations between main analyte to the others',
                                                  status = 'info',
                                                  collapsible = T,
                                                  collapsed = F,
                                                  solidHeader = F,
                                                  height='650',
                                                  width='12',
                                                  column(12,withSpinner(dataTableOutput('patterns'),
                                                                        image = 'https://i.pinimg.com/originals/52/16/80/5216809ff35e0daf8bcada59fa04f3c4.gif', 
                                                                        image.height = '750px', image.width = '1000px', proxy.height = '500px'),
                                                                        style='height:500px;overflow-y:scroll')
                                              )
                                            )
                                    ),
                                    #Pvalue plot
                                    tabItem(tabName = 'D',
                                            fluidRow(
                                              box(title='Analyte Statistical Analysis',
                                                  footer = 'Linear Regresion Plot',
                                                  status = 'info',
                                                  collapsible = T,
                                                  collapsed = F,
                                                  solidHeader = F,
                                                  height='550',
                                                  width='12',
                                                  column(12,withSpinner(plotlyOutput('Dlab'),
                                                                        image = 'https://i.pinimg.com/originals/52/53/35/52533552584f2c81e63ee15a2f4ee468.gif',
                                                                        image.height = '800px', image.width = '1200px'),
                                                                        style='height:400px;overflow-y:scroll')
                                              ),
                                              
                                              box(title='Analyte P-value summary',
                                                  footer = 'Summary of P-value by Analyte Concentrations by Date',
                                                  status = 'info',
                                                  collapsible = T,
                                                  collapsed = F,
                                                  solidHeader = F,
                                                  height='500',
                                                  width='12',
                                                  column(12,withSpinner(verbatimTextOutput('PvalueSUM'),
                                                                        image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                                                        image.height = '600px', image.width = '1200px', proxy.height = '300px'),
                                                                        style='height:400px;overflow-y:scroll')
                                              ),
                                    #table
                                    tabItem(tabName = 'dtable',
                                            fluidRow(
                                              box(title='Data Table',
                                                  footer = 'Data table of relationship being plotted for Analyte Analysis',
                                                  status = 'info',
                                                  collapsible = T,
                                                  collapsed = F,
                                                  solidHeader = F,
                                                  height='650',
                                                  width='12',
                                                  column(12,withSpinner(dataTableOutput('table'),
                                                                        image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                                                        image.height = '600px', image.width = '1000px', proxy.height = '1000px'),
                                                                        style='height:500px;overflow-y:scroll')
                                              )
                                            )
                                    ),
                                    #readme
                                    tabItem(tabName='readme',
                                            htmlOutput('appreadme')
                                    )
                                  )
                                )
                              )
                    )
)

