#' Read in GDX and calculate electricity exchange, value of exchange and aggregated transmission capacites, used in convGDX2MIF.R for the reporting
#' 
#' Read in electricity exchange data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportExchange(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie getNames<- getItems<- getItems
#' @export
#' 
reportExchange <- function(gdx) {
  
  # read parameters and sets
  p_taulength <- readGDX(gdx,name="p_taulength",field="l",format="first_found") #number of hours/year per tau
  trans <- readGDX(gdx,name="trans") #set of cross-border transmission lines (numeric)
  tau <- readGDX(gdx,name="tau") #set of tau
  regi <- readGDX(gdx,name="regi") #set of countries
  tt <- readGDX(gdx,name="t") #set of time
  p_tedataconn <- readGDX(gdx,name="p_tedataconn",field="l",format="first_found") #technical parameters of transmission
  t0 <- readGDX(gdx,name="t0",field="l",format="first_found") #initial year
  c_esmdisrate <- readGDX(gdx,name="c_esmdisrate",field="l",format="first_found") #interest rate
  p_ts <- readGDX(gdx,name="p_ts",field="l",format="first_found") #time step
  
  # read variables
  v_transflow1 <- readGDX(gdx,name="v_transflow1",field="l",format="first_found")
  v_transflow2 <- readGDX(gdx,name="v_transflow2",field="l",format="first_found")
  v_captrans <- readGDX(gdx,name="v_captrans",field="l",format="first_found")

  #define the losses
  p_losses <- p_tedataconn[,,"loss"]
  p_length <- p_tedataconn[,,"length"]
  
  #ELECTRICITY PRICES (needed for computing import and export prices)
  # read variables
  m_elecprices <- readGDX(gdx,name="q_sebal",field="m",format="first_found")
  m_elecprices <- m_elecprices[,,"seel"]
  
  # create MagPie object of m_elecprices with iso3 regions
  m_elecprices <- limesMapping(m_elecprices)[,,tau]
  
  # calculate marginal value per tau
  m_elecprices = m_elecprices/p_taulength
  
  #compute factor to discount average marginal values
  f_npv <- as.numeric(p_ts)*exp(-as.numeric(c_esmdisrate)*(as.numeric(tt)-as.numeric(t0)))
  
  #discounting marginal values [Geur/GWh]
  m_elecprices_tmp <- NULL
  for (t2 in 1:length(tt)) { 
    m_elecprices_tmp <- mbind(m_elecprices_tmp,m_elecprices[,t2,]/f_npv[t2])
  }
  
  # settings mapping path
  mappingPath <- system.file("extdata","LIMES_country_ISO_3.csv",package="limes")
  # reading mapping file
  mapping <- read.csv(mappingPath,sep=";")
  
  #Create variables for NET EXPORTS of the same size and forman than v_transflow
  v_netflow1 <- v_transflow1*0
  v_netflow2 <- v_transflow2*0
  
  #----------------------------------------------------------------------------------
  #EXPORTS
  exports<-NULL
  capexports <- NULL
  valueexports <- NULL
  expo_regi <- NULL
  netexpo_regi <- NULL
  cap_regi <- NULL
  
  #estimate aggregated (total) exports from each country
  for (regi2 in regi) {
    
    #initialize variables to aggregate positive flows
    partMagPie1<-0
    cap1<-0
    value1 <- 0
    expo1 <- NULL
    netexpo1 <- NULL
    cap_regi1 <- NULL
    
    #aggregate the positive flows
    conns1 <- trans[which(trans$regi == regi2),]$conn
    if(sum(as.numeric(conns1))!=0) {
    
      for (conns_tmp in conns1) { 
        
        flow_regi1 <- 0
        netflow_regi1 <- 0
        
        for (tau2 in tau) {
        #aggregate positive flows
        flow_tmp1 <- v_transflow1[,,paste0(tau2,".",conns_tmp)]*as.numeric(p_taulength[,,tau2]) #in GWh for value
        flow_regi1 <- flow_regi1 + setNames(flow_tmp1/1000,NULL) #cumulate per border link... in TWh for volume
        partMagPie1 <- partMagPie1 + setNames(flow_tmp1/1000,NULL) #cumulate per country... in TWh for volume
        
        #estimating value of exports (the price paid is the one of country to which is exported [if exports is because marginal costs are lower])
        regi_tmp_iso2 <- trans[match(conns_tmp,trans$conn),]$reg #country to which is exported (ISO2)
        regi_tmp_iso3 <- mapping[match(regi_tmp_iso2,mapping$LIMES_ISO2),]$LIMES_ISO3 #country to which is exported (ISO3)
        price_conn1 <- m_elecprices_tmp[regi_tmp_iso3,,tau2] #price in country to which is exported (price paid for the exports)
        getItems(flow_tmp1, dim = 1) <- regi_tmp_iso3
        value_tmp1 <- setNames(flow_tmp1,NULL)*(1-as.numeric(p_losses[,,conns_tmp]))*setNames(price_conn1,NULL)
        getItems(value_tmp1, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        value1 <- value1 + value_tmp1
        
        #calculate the NET EXPORTS (positive flows)
        v_netflow1[,,paste0(tau2,".",conns_tmp)] <- v_transflow1[,,paste0(tau2,".",conns_tmp)]-v_transflow2[,,paste0(tau2,".",conns_tmp)]*as.numeric(1-p_losses[,,conns_tmp])
        
        #aggregate positive NET flows
        netflow_tmp1 <- v_netflow1[,,paste0(tau2,".",conns_tmp)]*as.numeric(p_taulength[,,tau2]) #in GWh for value
        netflow_regi1 <- netflow_regi1 + setNames(netflow_tmp1/1000,NULL) #cumulate per border link... in TWh for volume
        
        }
        
        cap1 <- cap1 + setNames(v_captrans[,,conns_tmp],NULL)
        
        #exports to every country (positive flows)
        flow_regi <- setNames(flow_regi1,NULL)
        getItems(flow_regi, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        a <- paste0("Exports to ", mapping[match(trans[match(conns_tmp,trans$conn),]$reg,mapping$LIMES_ISO2),]$LIMES_ISO3,"|Electricity (TWh/yr)")
        expo1 <- mbind(expo1,setNames(flow_regi,a))
        
        #net exports to every country (positive flows)
        netflow_regi <- setNames(netflow_regi1,NULL)
        getItems(netflow_regi, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        a <- paste0("Net exports to ", mapping[match(trans[match(conns_tmp,trans$conn),]$reg,mapping$LIMES_ISO2),]$LIMES_ISO3,"|Electricity (TWh/yr)")
        netexpo1 <- mbind(netexpo1,setNames(netflow_regi,a))
        
        #cross-border transmission capacity (positive flows)
        cap_regi_tmp <- v_captrans[,,conns_tmp]
        getItems(cap_regi_tmp, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        a <- paste0("Transmission capacity with ", mapping[match(trans[match(conns_tmp,trans$conn),]$reg,mapping$LIMES_ISO2),]$LIMES_ISO3,"|Electricity (GW)")
        cap_regi1 <- mbind(cap_regi1,setNames(cap_regi_tmp,a))

      }
      
    }
    
    #initialize variables to aggregate negative flows
    partMagPie2<-0
    cap2<-0
    value2<-0
    expo2 <- NULL
    netexpo2 <- NULL
    cap_regi2 <- NULL
    
    #aggreate negative flows
    conns2 <- trans[which(trans$reg == regi2),]$conn
    if(sum(as.numeric(conns2))!=0) { 
    
      for (conns_tmp in conns2) { 
        
        flow_regi2 <- 0
        netflow_regi2 <- 0
        
        for (tau2 in tau) {
        #aggregate negative flows
        flow_tmp2 <- v_transflow2[,,paste0(tau2,".",conns_tmp)]*as.numeric(p_taulength[,,tau2]) #in GWh for value
        flow_regi2 <- flow_regi2 + setNames(flow_tmp2/1000,NULL) #cumulate per border link... in TWh for volume
        partMagPie2 <- partMagPie2 + setNames(flow_tmp2/1000,NULL) #cumulate per country...in TWh for volume
        
        #estimating value of exports (the price paid is the one of country to which is exported [if exports is because marginal costs are lower])
        regi_tmp_iso2 <- trans[match(conns_tmp,trans$conn),]$regi #country to which is exported (ISO2)
        regi_tmp_iso3 <- mapping[match(regi_tmp_iso2,mapping$LIMES_ISO2),]$LIMES_ISO3 #country to which is exported (ISO3)
        price_conn2 <- m_elecprices_tmp[regi_tmp_iso3,,tau2] #price in country to which is exported (price paid for the exports)
        getItems(flow_tmp2, dim = 1) <- regi_tmp_iso3
        #setNames(flow_tmp2,NULL)
        #setNames(price_conn2,NULL)
        value_tmp2 <- setNames(flow_tmp2,NULL)*(1-as.numeric(p_losses[,,conns_tmp]))*setNames(price_conn2, NULL)
        getItems(value_tmp2, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        value2 <- value2 + value_tmp2
        
        #calculate the NET EXPORTS (negative flows)
        v_netflow2[,,paste0(tau2,".",conns_tmp)] <- v_transflow2[,,paste0(tau2,".",conns_tmp)]-v_transflow1[,,paste0(tau2,".",conns_tmp)]*as.numeric(1-p_losses[,,conns_tmp])
        
        #aggregate negative NET flows
        netflow_tmp2 <- v_netflow2[,,paste0(tau2,".",conns_tmp)]*as.numeric(p_taulength[,,tau2]) #in GWh for value
        netflow_regi2 <- netflow_regi2 + setNames(netflow_tmp2/1000,NULL) #cumulate per border link... in TWh for volume
        
        }
        
        cap2 <- cap2 + setNames(v_captrans[,,conns_tmp],NULL)
        
        #exports to every country (negative flows)
        flow_regi <- setNames(flow_regi2,NULL)
        getItems(flow_regi, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        a <- paste0("Exports to ", mapping[match(trans[match(conns_tmp,trans$conn),]$regi,mapping$LIMES_ISO2),]$LIMES_ISO3,"|Electricity (TWh/yr)")
        expo2 <- mbind(expo2,setNames(flow_regi,a))
        
        #net exports to every country (negative flows)
        netflow_regi <- setNames(netflow_regi2,NULL)
        getItems(netflow_regi, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        a <- paste0("Net exports to ", mapping[match(trans[match(conns_tmp,trans$conn),]$regi,mapping$LIMES_ISO2),]$LIMES_ISO3,"|Electricity (TWh/yr)")
        netexpo2 <- mbind(netexpo2,setNames(netflow_regi,a))
        
        #cross-border transmission capacity
        cap_regi_tmp <- v_captrans[,,conns_tmp]
        getItems(cap_regi_tmp, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        a <- paste0("Transmission capacity with ", mapping[match(trans[match(conns_tmp,trans$conn),]$regi,mapping$LIMES_ISO2),]$LIMES_ISO3,"|Electricity (GW)")
        cap_regi2 <- mbind(cap_regi2,setNames(cap_regi_tmp,a))
        
        }
    }
    
    #For cross-border dependent data it is necessary to create the rows for each country (so all the countries have the same dimensions)
    regi_tmp1 <- mapping[match(trans[match(conns1,trans$conn),]$reg,mapping$LIMES_ISO2),]$LIMES_ISO3
    regi_tmp2 <- mapping[match(trans[match(conns2,trans$conn),]$regi,mapping$LIMES_ISO2),]$LIMES_ISO3
    regi_noexp <- setdiff(mapping$LIMES_ISO3,union(regi_tmp1,regi_tmp2))
    expo3 <- NULL
    netexpo3 <- NULL
    cap_regi3 <- NULL
    for (regi_noexp_tmp in regi_noexp) { 
      #exports to every country
      flow_regi <- setNames(flow_tmp2,NULL)*0
      getItems(flow_regi, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
      a <- paste0("Exports to ", regi_noexp_tmp,"|Electricity (TWh/yr)")
      expo3 <- mbind(expo3,setNames(flow_regi,a))
      
      #net exports to every country
      netflow_regi <- setNames(netflow_tmp2,NULL)*0
      getItems(netflow_regi, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
      a <- paste0("Net exports to ", regi_noexp_tmp,"|Electricity (TWh/yr)")
      netexpo3 <- mbind(netexpo3,setNames(netflow_regi,a))
      
      #transmission capacity to every country
      cap_regi_tmp <- cap_regi_tmp*0
      getItems(cap_regi_tmp, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
      a <- paste0("Transmission capacity with ",regi_noexp_tmp,"|Electricity (GW)")
      cap_regi3 <- mbind(cap_regi3,setNames(cap_regi_tmp,a))
      
    }
    
    #concatenate exports (positive, negative and unexistant flows)
    expo <- mbind(expo1,expo2,expo3)
    expo_regi <- mbind(expo_regi,expo)
    
    #concatenate net exports (positive, negative and unexistant flows)
    netexpo <- mbind(netexpo1,netexpo2,netexpo3)
    netexpo_regi <- mbind(netexpo_regi,netexpo)
    
    #concatenate cross-border capacities (positive, negative and unexistant flows)
    cap_regi4 <- mbind(cap_regi1,cap_regi2,cap_regi3)
    cap_regi <- mbind(cap_regi,cap_regi4)
    
    #sum positive and negative flows 
    partMagPie<-partMagPie1+partMagPie2
    cap <- cap1 + cap2
    value <- value1 + value2
    #add country to the names
    getNames(partMagPie)<-regi2
    getNames(cap)<-regi2
    #concatenate data from different countries (regi)
    exports <- mbind(exports,partMagPie)
    capexports <- mbind(capexports,cap)
    valueexports <- mbind(valueexports,value)
  }
  
  # create MagPie object of exports volume and transmission capacity with iso3 regions
  exports<-limesMapping(exports) 
  capexports<-limesMapping(capexports)
  #MagPie object of value of exports was already created

  
  #----------------------------------------------------------------------------------
  #IMPORTS
  imports<-NULL
  capimports <- NULL
  valueimports <- NULL
  impo_regi <- NULL
  capimports_km <- NULL
  
  #estimate aggregated (total) imports from each country
  for (regi2 in regi) {
    
    #initialize variables to aggregate positive flows
    partMagPie1<-0
    cap1 <- 0
    value1 <- 0
    impo1 <- NULL
    cap1_km <- 0
    
    #aggregate the positive flows
    conns1 <- trans[which(trans$reg == regi2),]$conn
    if(length(conns1)!=0) {
      
      for (conns_tmp in conns1) { 
        
        flow_regi1 <- 0
        
        for (tau2 in tau) {
        #aggregate positive flows
        flow_tmp1 <- v_transflow1[,,paste0(tau2,".",conns_tmp)]*as.numeric(p_taulength[,,tau2])*(1-as.numeric(p_losses[,,conns_tmp])) #in GWh for value
        flow_regi1 <- flow_regi1 + setNames(flow_tmp1/1000,NULL) #cumulate per border link... in TWh for volume
        partMagPie1 <- partMagPie1 + setNames(flow_tmp1/1000,NULL) #cumulate per country... in TWh for volume
        
        #estimating value of imports (the price paid is the one of the importer country [if imports is because marginal costs are higher])
        regi_tmp_iso2 <- trans[match(conns_tmp,trans$conn),]$regi #country from which is imported (ISO2)
        regi_tmp_iso3 <- mapping[match(regi_tmp_iso2,mapping$LIMES_ISO2),]$LIMES_ISO3 #country from which is imported (ISO3)
        price_conn1 <- m_elecprices_tmp[mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3,,tau2] #price in country that imports (price paid for the exports)
        getItems(flow_tmp1, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        value_tmp1 <- setNames(flow_tmp1,NULL)*setNames(price_conn1,NULL)
        getItems(value_tmp1, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        value1 <- value1 + value_tmp1
        
      }
        cap1 <- cap1 + setNames(v_captrans[,,conns_tmp],NULL)
        cap1_km <- cap1_km + setNames(v_captrans[,,conns_tmp]*p_length[,,conns_tmp],NULL)
        
        #imports from every country (positive flows)
        flow_regi <- setNames(flow_regi1,NULL)
        getItems(flow_regi, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        a <- paste0("Imports from ", mapping[match(trans[match(conns_tmp,trans$conn),]$regi,mapping$LIMES_ISO2),]$LIMES_ISO3,"|Electricity (TWh/yr)")
        impo1 <- mbind(impo1,setNames(flow_regi,a))
        }
    }
    
    #initialize variables to aggregate negative flows
    partMagPie2<-0
    cap2 <- 0
    value2 <- 0
    impo2 <- NULL
    cap2_km <- 0
    
    #aggreate negative flows
    conns2 <- trans[which(trans$regi == regi2),]$conn
    if(sum(as.numeric(conns2))!=0) { 
      
      for (conns_tmp in conns2) { 
        
        flow_regi2 <- 0
        
        for (tau2 in tau) {
        #aggregate negative flows
        flow_tmp2 <- v_transflow2[,,paste0(tau2,".",conns_tmp)]*as.numeric(p_taulength[,,tau2])*(1-as.numeric(p_losses[,,conns_tmp])) #in GWh for value
        flow_regi2 <- flow_regi2 + setNames(flow_tmp2/1000,NULL) #cumulate per border link... in TWh for volume
        partMagPie2 <- partMagPie2 + setNames(flow_tmp2/1000,NULL) #cumulate per country.. in TWh for volume
        
        #estimating value of imports (the price paid is the one of the importer country [if imports is because marginal costs are higher])
        regi_tmp_iso2 <- trans[match(conns_tmp,trans$conn),]$reg #country from which is imported (ISO2)
        regi_tmp_iso3 <- mapping[match(regi_tmp_iso2,mapping$LIMES_ISO2),]$LIMES_ISO3 #country from which is imported (ISO3)
        price_conn2 <- m_elecprices_tmp[mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3,,tau2] #price in country that imports (price paid for the exports)
        getItems(flow_tmp2, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        value_tmp2 <- setNames(flow_tmp2,NULL)*setNames(price_conn2,NULL)
        getItems(value_tmp2, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        value2 <- value2 + value_tmp2
      }
        cap2 <- cap2 + setNames(v_captrans[,,conns_tmp],NULL)
        cap2_km <- cap2_km + setNames(v_captrans[,,conns_tmp]*p_length[,,conns_tmp],NULL)
        
        #imports from every country (negative flows)
        flow_regi <- setNames(flow_regi2,NULL)
        getItems(flow_regi, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
        a <- paste0("Imports from ",as.vector(mapping[match(trans[match(conns_tmp,trans$conn),]$reg,mapping$LIMES_ISO2),]$LIMES_ISO3),"|Electricity (TWh/yr)")
        impo2 <- mbind(impo2,setNames(flow_regi,a))
        }
    }
    
    #it is necessary to create the rows for each country (so all the countries have the same dimensions)
    regi_tmp1 <- mapping[match(trans[match(conns1,trans$conn),]$regi,mapping$LIMES_ISO2),]$LIMES_ISO3
    regi_tmp2 <- mapping[match(trans[match(conns2,trans$conn),]$reg,mapping$LIMES_ISO2),]$LIMES_ISO3
    regi_noimp <- setdiff(mapping$LIMES_ISO3,union(regi_tmp1,regi_tmp2))
    impo3 <- NULL
    for (regi_noimp_tmp in regi_noimp) { 
      #exports to every country (negative flows)
      flow_regi <- setNames(flow_tmp2/1000,NULL)*0
      getItems(flow_regi, dim = 1) <- mapping[match(regi2,mapping$LIMES_ISO2),]$LIMES_ISO3
      a <- paste0("Imports from ",regi_noimp_tmp,"|Electricity (TWh/yr)")
      impo3 <- mbind(impo3,setNames(flow_regi,a))
    }
    
    #concatenate imports (positive, negative and unexistant flows)
    impo <- mbind(impo1,impo2,impo3)
    impo_regi <- mbind(impo_regi,impo)
    
    #sum positive and negative flows 
    partMagPie<-partMagPie1+partMagPie2
    cap <- cap1 + cap2
    cap_km <- cap1_km + cap2_km
    value <- value1 + value2
    #add country to the names
    getNames(partMagPie)<-regi2
    getNames(cap)<-regi2
    getNames(cap_km)<-regi2
    #concatenate data from different countries (regi)
    imports <- mbind(imports,partMagPie)
    capimports <- mbind(capimports,cap)
    capimports_km <- mbind(capimports_km,cap_km)
    valueimports <- mbind(valueimports,value)
  }
  
  # create MagPie object of imports volume and transmission capacity with iso3 regions
  imports <- limesMapping(imports) 
  capimports <- limesMapping(capimports)
  capimports_km <- limesMapping(capimports_km)
  #MagPie object of value of imports was already created
  
  
  #--------------------------------------------------------------------------------------------------
  #REPORTING THE EXCHANGE (EXPORTS, IMPORTS AND NET EXPORTS)
  #add variable name for the report
  tmp1<-setNames(exports,"Secondary Energy|Electricity|Exports (TWh/yr)")
  tmp2 <- setNames(imports,"Secondary Energy|Electricity|Imports (TWh/yr)")
  
  # add exports and imports
  tmp3 <- mbind(tmp1,tmp2)
  
  
  #----------------------------------------------------------------------------------
  #NET EXPORTS
  tmp4 <- setNames(exports-imports,"Secondary Energy|Electricity|Net Exports (TWh/yr)")
  tmp4 <- mbind(tmp4,setNames(imports-exports,"Secondary Energy|Electricity|Net Imports (TWh/yr)"))
                
  # add exchange values
  tmp5 <- mbind(tmp3,tmp4)
  
  
  #----------------------------------------------------------------------------------
  #REPORTING THE EXPORT AND IMPORT TRANSMISSION CAPACITY
  #add variable name transmission capacity
  tmp6<-setNames(capexports[,as.numeric(tt),],"Export Transmission Capacity|Electricity (GW)")
  tmp7<-setNames(capimports[,as.numeric(tt),],"Import Transmission Capacity|Electricity (GW)")
  
  #add capacity to export and import
  tmp8 <- mbind(tmp6,tmp7)
  
  #concatenate exchange and capacities
  tmp9 <- mbind(tmp5,tmp8)
  
  #transmission capacity (under current configuration, export and import capacity are the same)
  tmp10 <- NULL
  tmp10 <- setNames(tmp7,"Capacity|Electricity|Transmission Grid (GW)")
  tmp10 <- mbind(tmp10, setNames(capimports_km[,as.numeric(tt),],"Capacity|Electricity|Transmission Grid-km (GWkm)"))
  
  #concatenate
  tmp11 <- mbind(tmp9,tmp10)
  
  #REPORTING THE VALUE OF IMPORTS AND EXPORTS 
  tmp12 <- NULL
  tmp12 <- setNames(valueexports,"Revenue|Electricity|Exports (billion eur2010/yr)")
  tmp12 <- mbind(tmp12, setNames(valueimports,"Cost|Electricity|Imports (billion eur2010/yr)"))
  tmp12 <- mbind(tmp12, setNames(valueimports-valueexports,"Total Energy System Cost|Power Sector|Trade Costs (billion eur2010/yr)"))
  
  #concatenate
  tmp13 <- mbind(tmp11,tmp12)
  
  #REPORTING THE EXPORTS; IMPORTS AND NET EXPORTS FOR EACH CROSS-BORDER LINK
  tmp14 <- NULL
  tmp14 <- mbind(expo_regi,impo_regi,netexpo_regi)
  tmp14 <- mbind(tmp14,cap_regi[,as.numeric(tt),])
  # erasing all the data regarding connections that do not exist (created before to fit the size of the variables)
  for (regi2 in getItems(tmp14, dim = 1)) { for (name in getNames(tmp14)){
    if (sum(tmp14[regi2,,name])==0) {
      tmp14[regi2,,name] <- NA
    }
  }}
  
  #concatenate all
  tmp <- mbind(tmp13,tmp14)
  
  return(tmp)
}
  
