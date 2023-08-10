#---user interface--------------------------------------------------  
ui <- dashboardPage(#skin = 'red',
                    header=dashboardHeader(title='My Inverts- Sycamore Creek (NEON)'),
                    
                    dashboardSidebar(
                      
                      #selectInput("ui.domain",  label="Select Domain",choices = unique(domain.sites$Domain),selectize=TRUE),
                      #selectInput("SelectSite", label='Select Site',choices=''),
                                  #"Please select Field Site:",
                                  #choices = c("SYCA", "ARIK","BARC","BIGC","BLDE","BLUE","BLWA","CARI","COMO","CRAM","CUPE","FLNT","GUIL","HOPB","KING","LECO","LEWI","LIRO","MART","MAYF","MCDI","MCRA","OKSR","POSE","PRIN","PRLA","PRPO","REDB","SUGG","TECR","TOMB","TOOK","WALK","WLOU"), selected = F, multiple = T),
                      selectInput("ui.domain", label="Select Domain",choices = unique(domain.sites$Domain),selectize=TRUE, selected = F),
                      
                      selectInput("SelectSite", label='Select Site',choices = unique(domain.sites$Site),selectize=TRUE),
                      
                      dateRangeInput('dateRange',label='Select Date Range (YYYY-MM)',format = "yyyy-mm",start=Sys.Date()-(930),end=Sys.Date()-730, startview = "year"),
                      

                      
                      selectInput("SelectData", label ="Please select Protocol:",
                                  choices = c(''), selectize=TRUE),

                      
                      #dateRangeInput('dateRange',label='Select Date Range',start=Sys.Date()-21,end=Sys.Date()),
                      #actionButton('run','Run'),
                      
                      sidebarMenu(
                        menuItem('Information',tabName = 'I',icon=icon('info')),
                        #menuItem('Data View',tabName = 'D',icon=icon('laptop-code')),
                        #menuItem('QC Report',tabName = 'C',icon=icon('newspaper')),
                        menuItem('Invert Captures',tabName = 'dtable',icon=icon('table')),
                        menuItem('By Location',tabName = 'emap',icon=icon('bug')),
                        submitButton('Process Selections',width = '90%'),
                        menuItem('Readme',tabName='readme',icon=icon('info-circle'))
                      )
                    ),
                    
                    dashboardBody(" Macroinvertebrate Collection Data from National Ecological Observatory Network (NEON)",
                                  tabItems( # contains all tabs -  
                                    
                                    #info - protocol
                                    tabItem(tabName = 'I',
                                            fluidRow(
                                              shinydashboard::box(title='Protocol Name',
                                                  #footer = 'Data from selected protocol',
                                                  status = 'info',
                                                  #collapsible = T,
                                                  #collapsed = F,
                                                  solidHeader = F,
                                                  height='90',
                                                  width='6',
                                                  column(12,withSpinner(textOutput('the_info'),
                                                                        image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                                                        image.height = '600px', image.width = '1000px', proxy.height = '1000px'),
                                                         #style='height:50px;overflow-y:scroll'
                                                         )
                                              ),
                                              
                                              shinydashboard::box(title='Protocol Dpid',
                                                  #footer = 'Data from selected protocol',
                                                  status = 'info',
                                                  #collapsible = T,
                                                  #collapsed = F,
                                                  solidHeader = F,
                                                  height='90',
                                                  width='6',
                                                  column(12,withSpinner(textOutput('d_info'),
                                                                       image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                                                       image.height = '600px', image.width = '1000px', proxy.height = '1000px'),
                                                         #style='height:50px;overflow-y:scroll'
                                                         )
                                              ),
                                              
                                              shinydashboard::box(title='Description',
                                                  #footer = 'Data from selected protocol',
                                                  status = 'info',
                                                  #collapsible = T,
                                                  #collapsed = F,
                                                  solidHeader = F,
                                                  height='120',
                                                  width='12',
                                                  column(12,withSpinner(textOutput('p_info'),
                                                                        image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                                                        image.height = '600px', image.width = '1000px', proxy.height = '1000px'),
                                                         #style='height:60px;overflow-y:scroll'
                                                  )
                                              ),
                                              
                                              shinydashboard::box(title='Status',
                                                  #footer = 'Data from selected protocol',
                                                  status = 'info',
                                                  #collapsible = T,
                                                  #collapsed = F,
                                                  solidHeader = F,
                                                  height='90',
                                                  width='12',
                                                  column(12,withSpinner(textOutput('st_info'),
                                                                        image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                                                        image.height = '600px', image.width = '1000px', proxy.height = '1000px'),
                                                         #style='height:25px;overflow-y:scroll'
                                                  )
                                              ),
                                              
                                              # shinydashboard::box(title='First Date',
                                              #     #footer = 'Data from selected protocol',
                                              #     status = 'info',
                                              #     #collapsible = T,
                                              #     #collapsed = F,
                                              #     solidHeader = T,
                                              #     height='90',
                                              #     width='4',
                                              #     column(12,withSpinner(textOutput('f_info'),
                                              #                           image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                              #                           image.height = '600px', image.width = '1000px', proxy.height = '1000px'),
                                              #            #style='height:25px;overflow-y:scroll'
                                              #            )
                                              # ),
                                              # 
                                              # shinydashboard::box(title='Last Date',
                                              #     #footer = 'Data from selected protocol',
                                              #     status = 'info',
                                              #     #collapsible = T,
                                              #     #collapsed = F,
                                              #     solidHeader = T,
                                              #     height='90',
                                              #     width='4',
                                              #     column(12,withSpinner(tableOutput('l_info'),
                                              #                           image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                              #                           image.height = '600px', image.width = '1000px', proxy.height = '1000px'),
                                              #            #style='height:25px;overflow-y:scroll'
                                              #            )
                                              # ),
                                              
                                              shinydashboard::box(title='Science Team',
                                                  #footer = 'Data from selected protocol',
                                                  status = 'info',
                                                  #collapsible = T,
                                                  #collapsed = F,
                                                  solidHeader = F,
                                                  height='90',
                                                  width='12',
                                                  column(12,withSpinner(textOutput('sc_info'),
                                                                        image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                                                        image.height = '600px', image.width = '1000px', proxy.height = '1000px'),
                                                         #style='height:50px;overflow-y:scroll'
                                                         )
                                              )
                                            )
                                    ),
                                    
                                    
                                              #table
                                              tabItem(tabName = 'dtable',
                                                      fluidRow(
                                                        
                                                        shinydashboard::box(width = 3, status = 'primary',
                                                        selectInput("look", label = "Select MacroInvert level",
                                                                    choices = c('family','genus','scientificName'), selectize=TRUE)),
                                                        
                                                        shinydashboard::box(title='Most Captured Inverts',
                                                            footer = 'Data table for selected protocol',
                                                            status = 'info',
                                                            #collapsible = T,
                                                            #collapsed = F,
                                                            solidHeader = F,
                                                            height='650',
                                                            width='9',
                                                            column(12,withSpinner(DT::dataTableOutput('table'),
                                                                                  image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                                                                  image.height = '600px', image.width = '1000px', proxy.height = '1000px'),
                                                                   style='height:500px;overflow-y:scroll')
                                                        )
                                                        
                                                      )
                                              ),
                                    
                                    #map
                                    tabItem(tabName = 'emap',
                                            fluidRow(
                                              
                                              # shinydashboard::box(width = 3, status = 'primary',
                                              #                     selectInput("look", label = "Select MacroInvert level",
                                              #                                 choices = c('family','genus','scientificName'), selectize=TRUE)),
                                              
                                              shinydashboard::box(title='Invert Capture by Location',
                                                                  footer = 'Capture Locations by species between selected dates',
                                                                  status = 'info',
                                                                  #collapsible = T,
                                                                  #collapsed = F,
                                                                  solidHeader = F,
                                                                  height='600',
                                                                  width='12',
                                                                  column(12,withSpinner(leafletOutput('Elab', width = '100%', height = 1800),
                                                                                        image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',
                                                                                        image.height = '600px', image.width = '1200px', proxy.height = '700px'),
                                                                         style='height:590px;overflow-y:scroll')
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

