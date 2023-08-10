##global
#Loading Libraries


library(neonUtilities)  ##Loading Libraries##
library(dplyr)
library(tidyverse)       
library(readr)
library(tidyr)
library(plotly)
library(shiny)
library(shinycssloaders)
library(RColorBrewer)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(data.table)    
library(mlr)
library(DT)
library(jsonlite)
library(httr)
library(downloader)
library(leaflet)
library(mapview)
library(plyr)
library(htmlwidgets)

###------Old code-----------------------
# load.pkg <- function(p) {  #load packages with require(), install any that are not installed
#   if (!is.element(p, installed.packages()[,1]))
#     install.packages(p, dep = TRUE)
#   suppressMessages(require(p, character.only = TRUE))
# }

# load.pkg('neonUtilities')  ##Loading Libraries##
# load.pkg('dplyr')
# load.pkg('tidyverse')       
# load.pkg('readr')
# load.pkg('tidyr')
# load.pkg('plotly')
# load.pkg('shiny')
# load.pkg('shinycssloaders')
# load.pkg('RColorBrewer')
# load.pkg('shinydashboard')
# load.pkg('shinydashboardPlus')
# load.pkg('shinycssloaders')
# load.pkg('data.table')    
# load.pkg('mlr')
# #load.pkg('knitr')
# load.pkg('DT')
# load.pkg('jsonlite')
# load.pkg('httr')
# load.pkg('downloader')
#load.pkg('rjson')
#load.pkg('RJSONIO')
###------------------end of OLD code----------


debug_mode=0

#dpid_cat$`Product ID`
dpid1 <- read_csv("NEON_Data_Catalog.csv")

# only Macroinvert data
dpid_cat <- dpid1 %>% 
  filter(`Product ID` == "DP1.20120.001")

#dpid_cat<- dpid1


domain<-read.csv('DomainSitesALL.csv',header=TRUE,sep=',',stringsAsFactors = FALSE)

# only AOS sites
domain.sites<- domain %>% 
  filter(Type == 'AOS')

# only D14 for now...
domain.sites<- domain.sites %>% 
  filter(Domain == 'D14')
