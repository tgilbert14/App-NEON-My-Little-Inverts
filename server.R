server <- function(input, output, session) {
  
  observe({
    #find sites
    a<-domain.sites$Site[domain.sites$Domain==input$ui.domain]

    if (is.null(a))
      a<- character(0)
    
    #create selection box 
    updateSelectInput(session,"SelectSite", choices = c('',a), selected = F)
  })
  
    observeEvent(input$SelectSite,{
      
    # function to get the science team (TOS, AOS, ect)
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x)-1)
    }
    
    dpid_cat$`Science Team`<- substrRight(dpid_cat$`Science Team`,4)
    site<- input$SelectSite
    # finding site chosen
    the_site<- domain.sites[grep(site,domain.sites$Site),]
    Type<-dpid_cat$Name[dpid_cat$`Science Team` %in% the_site$Type]
    updateSelectInput(session,"SelectData", choices = c('',Type))
  })
  
  # downloading data from dpid selection
  info_selection <- reactive({
    #site.pick<- input$SelectSite # saving site selection
    dp.pick<- input$SelectData # saving protocol selection
    #req(site.pick)
    req(dp.pick)
    
    a<- grep(dp.pick, dpid_cat$Name)
    info<- dpid_cat[a,]
    
  })
    
  name<- reactive({
    info<- info_selection()
    pro_name<- info$Name
})
  
  dpid<- reactive({
    info<- info_selection()
    the_dpid<- info$`Product ID`
  })
  
  p_desc<- reactive({
    info<- info_selection()
    pro_dec<- info$Description
  })
  
  t_info<- reactive({
    info<- info_selection()
    team<- info$`Science Team`
  })
  
  s_info<- reactive({
    info<- info_selection()
    status<- info$Status
  })
    
    # downloading data from dpid selection
    data_select <- reactive({
      site.pick<- input$SelectSite
      info<- info_selection()
      
      dpid.pick<- info$`Product ID`
    
      # for testing
      
      #site.pick<- 'SYCA'
      #dpid.pick<- "DP1.20120.001"
      
      start_d<-format(input$dateRange[1]) # start date
      end_d<-format(input$dateRange[2])  # end date
      
      raw<- loadByProduct(dpID = dpid.pick, site = site.pick, startdate = start_d, enddate = end_d, check.size = F)
      
      ##base::source('server_dpid.R', local=TRUE) 
      data.raw <- as_tibble(raw$inv_taxonomyProcessed)    #Getting raw data
      data.raw$plotID <-  str_sub(data.raw$sampleID, 15, 30) 
      #View(data.raw)
      
      look_at <- input$look
      
      if (look_at == 'family')
        species<- data.raw$family
      if (look_at == 'genus')
        species<- data.raw$genus
      if (look_at == 'scientificName')
        species<- data.raw$scientificName
      
      species<- unique(species)

#      plot<- data.raw$plotID
#      plot<- unique(plot)
#      #plot
      
      c_dates<- data.raw$collectDate
      c_dates<- unique(c_dates)
      #c_dates
      
      mylist<- list()
      
      x=1
      while(x< length(c_dates)+1) {
        i=1
        
        data_new<- data.raw %>% 
          select(collectDate, look_at, individualCount)
        per_sp<- data_new %>% 
          filter(data_new[2] == species[i]) %>% 
          filter(collectDate == c_dates[x]) %>% 
          mutate(totalCount = sum(individualCount)) %>%
          select(collectDate, look_at, totalCount)
        mylist[[x]]<- unique(per_sp)
        #mylist[[x]]
        
        i=2
        while(i< length(species)+1) {
          data_new<- data.raw %>% 
            select(collectDate, look_at, individualCount)
          per_sp<- data_new %>% 
            filter(data_new[2] == species[i]) %>% 
            filter(collectDate == c_dates[x]) %>% 
            mutate(totalCount = sum(individualCount)) %>%
            select(collectDate, look_at, totalCount)
          a<- unique(per_sp)
          mylist[[x]]<- union(mylist[[x]], a)
          i=i+1
        }
        
        if (x == 1) {mydata<- mylist[[1]]}
        
        mydata<- union(mylist[[x]],mydata)
        x=x+1
        
      }
      
      f_data<- (mydata)

      # getting rid of empty columns
      emptycols <- sapply(f_data, function (k) all(is.na(k)))
      final_data <- f_data[!emptycols]

      final_data<- final_data %>% 
        arrange(desc(totalCount))

  })
      
  
  # leaflet map
  plotInput <- reactive({
    #final_data<- data_select()
    site.pick<- input$SelectSite
    info<- info_selection()
    
    dpid.pick<- info$`Product ID`
    
    start_d<-format(input$dateRange[1]) # start date
    end_d<-format(input$dateRange[2])  # end date
    
    raw<- loadByProduct(dpID = dpid.pick, site = site.pick, startdate = start_d, enddate = end_d, check.size = F)
    
    data.coord<- as_tibble(raw$inv_fieldData)
    
    data<- data.coord %>% 
      select(sampleID, decimalLongitude,decimalLatitude) %>% 
      filter(!is.na(sampleID)) %>% 
      filter(!is.na('1'))
    
    colnames(data)[2]<- 'Longitude'
    colnames(data)[3]<- 'Latitude'
    data$plotID <-  str_sub(data$sampleID, 15, 30) 
    
    data<- data %>% 
      select(plotID, Longitude, Latitude) %>% 
      filter(!is.na(""))

    data.raw <- as_tibble(raw$inv_taxonomyProcessed)    #Getting raw data
    data.raw$plotID <-  str_sub(data.raw$sampleID, 15, 30) 

    species<- data.raw$scientificName
    species<- unique(species)

    plot<- data.raw$plotID
    plot<- unique(plot)

    mylist<- list()
    
    x=1
    while(x< length(plot)+1) {
      i=1
      per_sp<- data.raw %>% 
        select(plotID, scientificName, individualCount) %>% 
        filter(scientificName == species[i]) %>% 
        filter(plotID == plot[x]) %>% 
        mutate(totalCount = sum(individualCount)) %>%
        select(plotID, scientificName, totalCount)
      mylist[[x]]<- unique(per_sp)

      i=2
      while(i< length(species)+1) {
        per_sp<- data.raw %>% 
          select(plotID, scientificName, individualCount) %>% 
          filter(scientificName == species[i]) %>% 
          filter(plotID == plot[x]) %>% 
          mutate(totalCount = sum(individualCount)) %>%
          select(plotID, scientificName, totalCount)
        a<- unique(per_sp)
        mylist[[x]]<- union(mylist[[x]], a)
        i=i+1
      }
      
      if (x == 1) {mydata<- mylist[[1]]}
      
      mydata<- union(mylist[[x]],mydata)
      x=x+1
      
    }
 
    geo_per_plot <- left_join(mydata, data, by = 'plotID')

    ##For each species captured per plot
    new_geo<- unique(geo_per_plot)
    
    ##SYCA reach coordinates-- plot location estimation-----
    ##Reach Markers from downstream to up##
    B.long<- -111.508692
    B.lat<- 33.749033
    R1.long<- -111.508122
    R1.lat<- 33.748902
    R2.long<- -111.507101
    R2.lat<- 33.748306
    R3.long<- -111.506717
    R3.lat<-   33.749106
    R4.long<- -111.507325
    R4.lat<- 33.749875
    R5.long<- -111.507921
    R5.lat<- 33.750538
    S2.long<- -111.508124
    S2.lat<- 33.750886
    R6.long<- -111.508489
    R6.lat<- 33.751257
    S1.long<- -111.508597
    S1.lat<- 33.751703
    R7.long<- -111.508647
    R7.lat<- 33.752254
    R8.long<- -111.507854
    R8.lat<- 33.753151
    R9.long<- -111.506881
    R9.lat<- 33.753434
    R10.long<- -111.506025
    R10.lat<- 33.753788
    T.long<- -111.50559
    T.lat<- 33.754084
    #-------
    
    B<- new_geo %>% 
      filter(plotID == plot[1]) %>% 
      mutate(Longitude = B.long) %>% 
      mutate(Latitude = B.lat)
    
    R1<- new_geo %>% 
      filter(plotID == plot[12]) %>% 
      mutate(Longitude = R1.long) %>% 
      mutate(Latitude = R1.lat)
    new_data<- union(B,R1)
    
    R2<- new_geo %>% 
      filter(plotID == plot[13]) %>% 
      mutate(Longitude = R2.long) %>% 
      mutate(Latitude = R2.lat)
    new_data<- union(R2,new_data)
    
    R3<- new_geo %>% 
      filter(plotID == plot[4]) %>% 
      mutate(Longitude = R3.long) %>% 
      mutate(Latitude = R3.lat)
    new_data<- union(R3,new_data)
    
    R4<- new_geo %>% 
      filter(plotID == plot[6]) %>% 
      mutate(Longitude = R4.long) %>% 
      mutate(Latitude = R4.lat)
    new_data<- union(R4,new_data)
    
    R5<- new_geo %>% 
      filter(plotID == plot[11]) %>% 
      mutate(Longitude = R5.long) %>% 
      mutate(Latitude = R5.lat)
    new_data<- union(R5,new_data)
    
    S2<- new_geo %>% 
      filter(plotID == plot[14]) %>% 
      mutate(Longitude = S2.long) %>% 
      mutate(Latitude = S2.lat)
    new_data<- union(S2,new_data)
    
    R6<- new_geo %>% 
      filter(plotID == plot[5]) %>% 
      mutate(Longitude = R6.long) %>% 
      mutate(Latitude = R6.lat)
    new_data<- union(R6,new_data)
    
    S1<- new_geo %>% 
      filter(plotID == plot[2]) %>% 
      mutate(Longitude = S1.long) %>% 
      mutate(Latitude = S1.lat)
    new_data<- union(S1,new_data)
    
    R7<- new_geo %>% 
      filter(plotID == plot[9]) %>% 
      mutate(Longitude = R7.long) %>% 
      mutate(Latitude = R7.lat)
    new_data<- union(R7,new_data)
    
    R8<- new_geo %>% 
      filter(plotID == plot[8]) %>% 
      mutate(Longitude = R8.long) %>% 
      mutate(Latitude = R8.lat)
    new_data<- union(R8,new_data)
    
    R9<- new_geo %>% 
      filter(plotID == plot[7]) %>% 
      mutate(Longitude = R9.long) %>% 
      mutate(Latitude = R9.lat)
    new_data<- union(R9,new_data)
    
    R10<- new_geo %>% 
      filter(plotID == plot[3]) %>% 
      mutate(Longitude = R10.long) %>% 
      mutate(Latitude = R10.lat)
    new_data<- union(R10,new_data)
    
    T1<- new_geo %>% 
      filter(plotID == plot[10]) %>% 
      mutate(Longitude = T.long) %>% 
      mutate(Latitude = T.lat)
    new_data<- union(T1,new_data)
    
    new_geo<- new_data
    
    nb.cols <- length(new_geo$scientificName)
    mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
    
    pal <- colorFactor(
      palette = mycolors,
      domain = new_geo$scientificName)
    
    ###Breakdown of species--
    # Prepare the text for the tooltip:
    mytext2 <- paste(
      "ScientificName: ", new_geo$scientificName, "<br/>", 
      "Count: ", new_geo$totalCount, "<br/>",
      "PlotID: ", new_geo$plotID, "<br/>", 
      "Long: ", new_geo$Longitude,"<br/>",
      "Lat: ", new_geo$Latitude, sep="") %>%
      lapply(htmltools::HTML)
    meanLong<- mean(data$Longitude)
    meanLat<- mean(data$Latitude)
    
    m <- leaflet(new_geo) %>% 
      addTiles()  %>% 
      setView( lat=meanLat, lng=meanLong , zoom=15) %>%
      addProviderTiles("TomTom.Hybrid") %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       fillColor =~pal(new_geo$scientificName), opacity = .7, fillOpacity = .7, radius=~totalCount/6, popup = new_geo$plotID, stroke = T, weight = 1,  color = 'white', 
                       label = mytext2,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>% 
      addLegend( pal=pal, values=~new_geo$scientificName, opacity=0.3, title = "Species Diversity per Plot", group = "circles", position = "bottomright") %>%
      addLayersControl(overlayGroups = c("circles"))
    
    m 
    
  })
  
  output$Elab <- renderLeaflet({
    print(plotInput())
  })

  # Compare plot2
  plotInput2 <- reactive({
    # code here
  })
  
  output$Dlab <- renderPlotly({
    print(plotInput2())
  })

  output$PvalueSUM <- renderPrint({
    # Print output
  })
  
  #output$table <- DT::renderDT({
  #data2<- data_select()
  #shiny::validate(need(nrow(data2)>0,'No data with current selections'))
  #data2
  #})
  
  output$the_info <- renderText({
    pro_name <- name()
  })
  
  output$d_info <- renderText({
    the_dpid <- dpid()
  })
  
  output$p_info <- renderText({
    pro_dec<- p_desc()
  })
  

  output$f_info <- renderText({
    the_end<- the_date()
  
    first_date<- (the_end[1])
    first_date
    #first_date<- substr(the_end,4,10)
    
  })
  
  output$l_info <- renderTable({
    the_end<- the_date()
    
    num<- length(the_end)
    last_date<- (the_end[num])
    last_date
    
    #num<- nchar(the_end)
    
    #last_date<- substr(the_end,num-10,num-3)
    #last_date
    })
  
  output$st_info <- renderText({
    status<- s_info()
  })
  
  output$sc_info <- renderText({
    team<- t_info()
  })
  
  
  output$table <-DT::renderDT({
    final_data<- data_select()
    shiny::validate(need(nrow(final_data)>0,'No data with current selections'))
    
    datafile<-datatable(final_data,
                        style='default',
                        class='compact cell-border hover display',
                        filter=list(position='top',plain=TRUE)
    )
    
  },server=TRUE) #end of datatable

  
  
  
  c_check<- reactive({
    # reactive code
  })
  
  output$patterns<- renderDataTable({
    print(c_check())
  })
  
  #Readme - make into HTML
  output$appreadme<-renderUI({includeHTML('AppReadme.html')})
  
} #end of server