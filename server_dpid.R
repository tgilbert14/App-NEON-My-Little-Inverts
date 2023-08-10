# Server for all dpid selections ---

## AOS-----

AOS_list<- dpid_cat[grep('AOS',dpid_cat$`Science Team`),]
TOS_list<- dpid_cat[grep('TOS',dpid_cat$`Science Team`),]

## NO DATA AVALIABLE YET...
# "Aquatic plant bryophyte chemical properties"
if (dpid.pick == AOS_list$`Product ID`[1]) {
  #data1<- as_tibble(raw$)
  #data2<- as_tibble(raw$)
}

# "Aquatic plant bryophyte macroalgae clip harvest"
if (dpid.pick == AOS_list$`Product ID`[2]) {
  data1<- as_tibble(raw$apl_clipHarvest)
}

# "Aquatic plant, bryophyte, lichen, and macroalgae point counts in wadeable streams"
if (dpid.pick == AOS_list$`Product ID`[3]) {
  data1<- as_tibble(raw$apc_pointTransect)
}

## NO DATA AVALIABLE YET...
# "Bathymetric and morphological maps"
if (dpid.pick == AOS_list$`Product ID`[4]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Benthic microbe community composition"
if (dpid.pick == AOS_list$`Product ID`[5]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Benthic microbe group abundances"
if (dpid.pick == AOS_list$`Product ID`[6]) {
  #data1<- as_tibble(raw$)
}
## NO DATA AVALIABLE YET...
# "Benthic microbe marker gene sequences"
if (dpid.pick == AOS_list$`Product ID`[7]) {
  #data1<- as_tibble(raw$)
}
## NO DATA AVALIABLE YET...
# "Benthic microbe metagenome sequences"
if (dpid.pick == AOS_list$`Product ID`[8]) {
  #data1<- as_tibble(raw$)
}

# "Chemical properties of groundwater"
if (dpid.pick == AOS_list$`Product ID`[9]) {
  data1<- as_tibble(raw$gwc_domainLabData)
  data2<- as_tibble(raw$gwc_fieldData)
  data3<- as_tibble(raw$gwc_fieldSuperParent)
}

# "Chemical properties of surface water"
if (dpid.pick == AOS_list$`Product ID`[10]) {
  data1<- as_tibble(raw$swc_fieldData)
  data2<- as_tibble(raw$swc_domainLabData)
  data3<- as_tibble(raw$swc_externalLabDataByAnalyte)
  data4<- as_tibble(raw$swc_fieldSuperParent)
}
## maybe alter some domain vs external lab names to see diff above...

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[11]) {
  #data1<- as_tibble(raw$)
}

# "Dissolved gases in surface water"
if (dpid.pick == AOS_list$`Product ID`[12]) {
  data1<- as_tibble(raw$sdg_fieldSuperParent)
  data2<- as_tibble(raw$sdg_externalLabData)
  data3<- as_tibble(raw$sdg_fieldDataProc)
  data4<- as_tibble(raw$sdg_fieldDataAir)
}

# "Fish electrofishing, gill netting, and fyke netting counts"
if (dpid.pick == AOS_list$`Product ID`[13]) {
  data1<- as_tibble(raw$fsh_bulkCount)
  data2<- as_tibble(raw$fsh_fieldData)
  data3<- as_tibble(raw$fsh_perFish)
  data4<- as_tibble(raw$fsh_perPass)
}

## NO DATA AVALIABLE YET...
# "Fish sequences DNA barcode"
if (dpid.pick == AOS_list$`Product ID`[14]) {
  #data1<- as_tibble(raw$)
}

# "Macroinvertebrate collection"
if (dpid.pick == AOS_list$`Product ID`[15]) {
  data1<- as_tibble(raw$inv_fieldData)
  data2<- as_tibble(raw$inv_persample)
  data3<- as_tibble(raw$inv_taxonomyProcessed)
}

# "Macroinvertebrate metabarcoding"
if (dpid.pick == AOS_list$`Product ID`[16]) {
  data1<- as_tibble(raw$inv_fieldData)
}

# "Periphyton, seston, and phytoplankton chemical properties"
if (dpid.pick == AOS_list$`Product ID`[17]) {
  data1<- as_tibble(raw$alg_algaeExternalLabDataPerSample)
  data2<- as_tibble(raw$alg_domainLabChemistry)
  data3<- as_tibble(raw$alg_fieldData)
}

# "Periphyton, seston, and phytoplankton collection"
if (dpid.pick == AOS_list$`Product ID`[18]) {
  data1<- as_tibble(raw$alg_biomass)
  data2<- as_tibble(raw$alg_fieldData)
}

# "Reaeration field and lab collection"
if (dpid.pick == AOS_list$`Product ID`[19]) {
  #as_tibble(raw$rea_backgroundFieldCondData)
  data1<- as_tibble(raw$rea_backgroundFieldSaltData)
  data2<- as_tibble(raw$rea_conductivityFieldData)
  data3<- as_tibble(raw$rea_externalLabDataGas)
  data4<- as_tibble(raw$rea_externalLabDataSalt)
  data5<- as_tibble(raw$rea_plateauMeasurementFieldData)
  data6<- as_tibble(raw$rea_widthFieldData)
  
}


# testing
#site.pick<- 'POSE'
#site.pick<- 'SYCA'
#i=19
#AOS_list$Name[i]
#dpid.pick<- AOS_list$`Product ID`[i]
#raw<- loadByProduct(dpID = dpid.pick, site = site.pick, startdate = s_date, check.size = F)


## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[20]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[21]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[22]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[23]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[24]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[25]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[26]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[27]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[28]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[29]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[30]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[31]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[32]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[33]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[34]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[35]) {
  #data1<- as_tibble(raw$)
}

## NO DATA AVALIABLE YET...
# "Depth profile at specific depths"
if (dpid.pick == AOS_list$`Product ID`[36]) {
  #data1<- as_tibble(raw$)
}


### OLD code----
## Water Chem
#if (dpid.pick == 'DP1.20093.001') {
#  data1<- as_tibble(raw$swc_fieldData)
#  data2<- as_tibble(raw$swc_externalLabDataByAnalyte)
#  data3<- as_tibble(raw$swc_fieldSuperParent)
#}

## Invert
#if (dpid.pick == 'DP1.20120.001') {
#  data1<- as_tibble(raw$inv_fieldData)
#  data2<- as_tibble(raw$inv_persample)
#}
#####



## TOS------


# collectDate -- need script to change....

# Processing Data - joining data by collectDate
if(nrow(data1)>0)
{
  f_data<- data1
  
  if(nrow(data1)>0)
    data_names<- intersect(names(data2),names(data1))
  
    # getting rid of duplicate columns except Date
    data_names<- data_names[grep('collectDate',data_names, invert = T)]
  
    data_2<- data2 %>% 
      select(-data_names)
    
    data<- left_join(data1, data_2, by='collectDate')
    # saving over old f_data if more than one data set
    f_data<- data
    
      if(nrow(data3)>0)
        data_names3<- intersect(names(data3),names(data))
        data_names3<- data_names3[grep('collectDate',data_names3, invert = T)]
  
        data_3<- data3 %>% 
          select(-data_names3)
        f_data<- left_join(data, data_3, by='collectDate')
  
          if(nrow(data4)>0)
            data_names4<- intersect(names(data4),names(f_data))
            data_names4<- data_names4[grep('collectDate',data_names4, invert = T)]
  
            data_4<- data4 %>% 
              select(-data_names4)
            f_data<- left_join(f_data, data_4, by='collectDate')
            
            if(nrow(data5)>0)
              data_names5<- intersect(names(data5),names(f_data))
              data_names5<- data_names5[grep('collectDate',data_names5, invert = T)]
            
              data_5<- data5 %>% 
                select(-data_names5)
              f_data<- left_join(f_data, data_5, by='collectDate')
              
              if(nrow(data6)>0)
                data_names6<- intersect(names(data6),names(f_data))
                data_names6<- data_names6[grep('collectDate',data_names6, invert = T)]
              
                data_6<- data6 %>% 
                  select(-data_names6)
                f_data<- left_join(f_data, data_6, by='collectDate')

}  # end of 'if' data1 exists

  