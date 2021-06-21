#' Read in GDX and write *.mif reporting
#' 
#' Read in all information from GDX file and create
#' the *.mif reporting
#' 
#' 
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param gdx_ref reference-gdx for policy costs, a GDX as created by readGDX, or the file name of a gdx
#' @param file name of the mif file which will be written, if no name is
#' provided a magpie object containing all the reporting information is
#' returned
#' @param scenario scenario name that is used in the *.mif reporting
#' @param time temporal resolution of the reporting, default: c(seq(2010,2050,5))
#' @author Sebastian Osorio and Renato Rodrigues
#' @examples
#' 
#' \dontrun{convGDX2MIF(gdx,gdx_ref,file="LIMES_generic_default.csv",scenario="default")}
#' 
#' @export
#' @importFrom magclass mbind write.report getRegions getNames


convGDX2MIF <- function(gdx,gdx_ref=NULL,file=NULL,scenario="default",time=as.numeric(readGDX(gdx,name="t"))) {
  #time=as.numeric(readGDX(gdx,name="t"))
  # generate the report
  
  #initialize report variable
  output <- NULL
  
  #adding capacity info to report output
  output <- mbind(output,reportCapacity(gdx)[,c(time),])
  #output <- mbind(output,limes:::reportCapacity(gdx)[,time,]) # run this line when doign just some test
  
  #adding primary energy to report output
  output <- mbind(output,reportPrimaryEnergy(gdx)[,time,])
  
  #adding fuel costs to report output
  output <- mbind(output,reportFuelCosts(gdx)[,time,])
  
  #adding electricity prices info to report output
  output <- mbind(output,reportElectricityPrices(gdx)[,time,])
  
  #adding electricity generation info to report output
  output <- mbind(output,reportGeneration(gdx,output)[,time,]) #dependent on primary energy
  
  #adding demand info to report output
  output <- mbind(output,reportDemand(gdx,output)[,time,]) #dependent on generation
  
  #adding availability factors to report output
  output <- mbind(output,reportLoadFactor(gdx,output)[,time,])
  
  #Adding all the input parameters (except for fuel costs)
  #output <- mbind(output,reportInput(gdx)[,time,])
  
  #adding CO2 price to report output
  output <- mbind(output,reportCO2Price(gdx)[,time,])
  
  #adding emissions info to report output
  output <- mbind(output,reportEmissions(gdx)[,time,])
  
  #adding industry emissions to report output
  output <- mbind(output,reportIndustryEmissions(gdx,output)[,time,]) #depending on CO2 price and emissions
  
  #adding peak demand to report output (now included in reportDemand)
  #output <- mbind(output,reportPeakDemand(gdx)[,time,])
  
  #adding curtailment to report output
  #output <- mbind(output,reportCurtailment(gdx)[,time,]) #now on generation
  
  #adding carbon sequestration to report output
  #output <- mbind(output,reportCarbonSequestration(gdx)[,time,]) #Now on emissions
  
  #adding capacity additions to report output
  output <- mbind(output,reportCapacityAdditions(gdx)[,time,])
  
  #adding capacity disinvestments to report output
  output <- mbind(output,reportDisinvestments(gdx)[,time,])
  
  #adding exchange to report output
  output <- mbind(output,reportExchange(gdx)[,time,])
  
  #adding carbon sequestration to report output
  output <- mbind(output,reportTotalSystemCosts(gdx,output)[,time,]) #depends on reportExchange and reportCO2Price
  
  #adding adequacy contribution to report output
  output <- mbind(output,reportAdequacyContribution(gdx)[,time,])
  
  #adding buildings to report output
  #output <- mbind(output,reportBuildings(gdx)[,time,])
  
  #adding fictitious vars to report output. These variables are later erased and only the aggregated (updated) values are left
  #(this is needed to keep report within the dimensions)
  #An example is the cap for the EU ETS
  output <- mbind(output,reportFictitiousVars(gdx,output)[,time,])
  #Replace NAs by zeros to avoid missing variables
  output[is.na(output)]<-0
  #Save file before aggregation
  output_beforeagg <- output
  
  
  #AGGREGATE (WEIGHTED AVERAGE OF) SOME INTENSIVE VARIABLES (e.g., electricity price)
  output_RegAgg <- limesInt2Ext(output,gdx)
  
  #LOADING LIST OF REGIONS FOR AGGREGATING CERTAIN GROUPS (e.g., EU)
  # settings mapping path
  mappingregiPath <- system.file("extdata","LIMES_country_ISO_3.csv",package="limes")
  # reading mapping file
  mappingregi <- read.csv(mappingregiPath,sep=";")
  
  #aggregating all countries
  output_tot <- dimSums(output,dim=1, na.rm = T)
  #Replacing the aggregated for intensive variables (a sum that makes no sense) by the weighted average calculated above
  output_tot[,,getNames(output_RegAgg)] <- output_RegAgg["GLO",,]
  
  #aggregating only EU-28
  EU<-which(getRegions(output)!="NOR" & getRegions(output)!="CHE" & getRegions(output)!="BAL" & getRegions(output)!="GLO")
  output_EU<-NULL
  output_EU<-dimSums(output[EU,,],dim=1, na.rm = T)
  getRegions(output_EU)<-"EU28"
  output_EU[,,getNames(output_RegAgg)] <- output_RegAgg["EU28",,]
  
  #aggregating only EU-ETS
  EUETS<-which(getRegions(output)!="CHE" & getRegions(output)!="BAL" & getRegions(output)!="GLO")
  output_EUETS<-NULL
  output_EUETS<-dimSums(output[EUETS,,],dim=1, na.rm = T)
  getRegions(output_EUETS)<-"EUETS"
  output_EUETS[,,getNames(output_RegAgg)] <- output_RegAgg["EUETS",,]
  
  #aggregating EUETS-nonDE
  EUETS_nonDE<-which(getRegions(output)!="CHE" & getRegions(output)!="BAL" & getRegions(output)!="DEU")
  output_EUETS_nonDE<-NULL
  output_EUETS_nonDE<-dimSums(output[EUETS_nonDE,,],dim=1, na.rm = T)
  getRegions(output_EUETS_nonDE)<-"EUETS_nonDE"
  
  #Showing KdW results is rarely necessary. Apply a switch (0) No (1) Yes
  show_KdW <- 0
  
  if (show_KdW == 1) {
    #aggregating KdW (coalition of the willing) and nonKdW
    KdW_iso2 <- readGDX(gdx,name="regi_KdW")
    KdW_iso3 <- mappingregi[match(KdW_iso2,mappingregi[,1]),2]
    #KdW <- which(mappingregi$KdW == 1)
    KdW <- KdW_iso3 #Better to take it directly from the GDX file
    output_KdW<-dimSums(output[KdW,,],dim=1, na.rm = T)
    getRegions(output_KdW)<-"KdW"
    
    nonKdW <- which(mappingregi$KdW == 0)
    output_nonKdW<-dimSums(output[nonKdW,,],dim=1, na.rm = T)
    getRegions(output_nonKdW)<-"nonKdW"
    
    #aggregating KdW_EU, KdW_nonEU, nonKdW_EU, nonKdW_nonEU
    KdW_EU <- which(mappingregi$KdW == 1 & mappingregi$EU == 1)
    output_KdW_EU<-dimSums(output[KdW_EU,,],dim=1, na.rm = T)
    getRegions(output_KdW_EU)<-"KdW_EU"
    
    KdW_nonEU <- which(mappingregi$KdW == 1 & mappingregi$EU == 0)
    output_KdW_nonEU<-dimSums(output[KdW_nonEU,,],dim=1, na.rm = T)
    getRegions(output_KdW_nonEU)<-"KdW_nonEU"
    
    nonKdW_EU <- which(mappingregi$KdW == 0 & mappingregi$EU == 1)
    output_nonKdW_EU<-dimSums(output[nonKdW_EU,,],dim=1, na.rm = T)
    getRegions(output_nonKdW_EU)<-"nonKdW_EU"
    
    nonKdW_nonEU <- which(mappingregi$KdW == 0 & mappingregi$EU == 0)
    output_nonKdW_nonEU<-dimSums(output[nonKdW_nonEU,,],dim=1, na.rm = T)
    getRegions(output_nonKdW_nonEU)<-"nonKdW_nonEU"
    
    #totals concerning the KdW
    output <- mbind(output,output_KdW,output_nonKdW,output_KdW_EU,output_KdW_nonEU,output_nonKdW_EU,output_nonKdW_nonEU)
  }
  
  #load regions that implemented a top-up minimum CO2 price
  minP <- readGDX(gdx,name="regi_minP")
  #output_aggminP <- NULL
  #convert these regions to iso3
  if(length(minP) > 0) {
    minP_iso3 <- mappingregi[match(minP,mappingregi[,1]),2]
    
    output_minP<-dimSums(output[minP_iso3,,],dim=1, na.rm = T)
    getRegions(output_minP)<-"minP"
    
    output_nonminP<-output_tot - output_minP
    getRegions(output_nonminP)<-"non_minP"
    
    output_EUnonminP<-dimSums(output[EU,,],dim=1, na.rm = T) - output_minP
    getRegions(output_EUnonminP)<-"EU_non_minP"
    
    output_EUETSnonminP<-dimSums(output[EUETS,,],dim=1, na.rm = T) - output_minP
    getRegions(output_EUETSnonminP)<-"EUETS_non_minP"
    
    output <- mbind(output,output_minP,output_nonminP,output_EUnonminP,output_EUETSnonminP)
  }
  
  
  #CONCATENATING OUTPUT FROM REGIONS AND AGGREGATED DATA
  output <- mbind(output,output_tot,output_EU,output_EUETS,output_EUETS_nonDE)
  #totals concerning countries implementing min CO2 price is done above
  #totals concerning countries KdW is done above
  
  
  #Renaming global variable (not possible so far)
  #getRegions(output["GLO",,])<-"EUR"
  
  
  #INCLUDING ONLY CERTAIN VARIABLES
  # settings mapping path
  mappingvarsPath <- system.file("extdata","MappingVars.csv",package="limes")
  # reading mapping file
  mappingvars <- read.csv(mappingvarsPath,sep=";")
  #   write the *.mif or give back the magpie opject output
  finalvars <- paste0(as.vector(mappingvars$LIMES)," (",as.vector(mappingvars$UnitLIMES) , ")")
  output <- output[,,intersect(finalvars,getNames(output))]
  
  
  #ERASING INTENSIVE VARIABLES (World, EU28, etc) for variables that were not weighted 
  # settings mapping path
  mappingPath <- system.file("extdata","WeightedAverageVars.csv",package="limes")
  # reading mapping file
  mapping_int2ext <- read.csv(mappingPath,sep=";")
  # Write the 2 sets: all intensive variables and those that were not averaged
  #a) All
  pos_intall <- match(mapping_int2ext$int, mappingvars$LIMES)
  pos_intall <- pos_intall[!is.na(pos_intall)]
  IntVars <- paste0(mappingvars$LIMES[pos_intall]," (",mappingvars$UnitLIMES[pos_intall],")")
  #IntVars <- paste0(as.vector(mappingvars$LIMES[pos_intall])," (",as.vector(mappingvars$UnitLIMES[pos_intall]) , ")")
  #Make sure this variable was indeed calculated
  IntVars <- intersect(finalvars,IntVars)
  #b) non-weighted intensive variables
  mapping_int2ext_noweight <- mapping_int2ext[mapping_int2ext$ext == 0,]
  pos_intnoweight <- match(mapping_int2ext_noweight$int, mappingvars$LIMES)
  pos_intnoweight <- pos_intnoweight[!is.na(pos_intnoweight)]
  IntVars_noweight <- paste0(mappingvars$LIMES[pos_intnoweight]," (",mappingvars$UnitLIMES[pos_intnoweight],")")
  #Make sure this variable was indeed calculated
  IntVars_noweight <- intersect(finalvars,IntVars_noweight)
  IntVars_noweight <- setdiff(IntVars_noweight,c("Capacity|Electricity|Transmission Grid (GW)","Capacity|Electricity|Transmission Grid-km (GWkm)"))
  #
  #
  ## settings mapping path
  #IntVarPath <- system.file("extdata","IntensiveVariables.csv",package="limes")
  ## reading mapping file
  #IntVarfile <- read.csv(IntVarPath,sep=";")
  ## Identify the intensive variables that were not averaged
  #pos_tmp <- setdiff(c(1:length(IntVarfile$LIMES)),match(mapping_int2ext$int,IntVarfile$LIMES))
  ## Write the 2 sets: all intensive variables and those that were  averaged
  #IntVars <- paste0(as.vector(IntVarfile$LIMES)," (",as.vector(IntVarfile$UnitLIMES) , ")")
  #IntVars_noweight <- paste0(as.vector(IntVarfile$LIMES[pos_tmp])," (",as.vector(IntVarfile$UnitLIMES[pos_tmp]) , ")")
  if (length(intersect(getNames(output),IntVars)) > 0) {
    output["GLO",,intersect(getNames(output),IntVars_noweight)] <- NA
    output["EU28",,intersect(getNames(output),IntVars_noweight)] <- NA
    output["EUETS",,intersect(getNames(output),IntVars_noweight)] <- NA
    output["EUETS_nonDE",,intersect(getNames(output),IntVars)] <- NA
    
    if (show_KdW == 1) {
      output["KdW",,intersect(getNames(output),IntVars)] <- NA
      output["nonKdW",,intersect(getNames(output),IntVars)] <- NA
      output["KdW_EU",,intersect(getNames(output),IntVars)] <- NA
      output["KdW_nonEU",,intersect(getNames(output),IntVars)] <- NA
      output["nonKdW_EU",,intersect(getNames(output),IntVars)] <- NA
      output["nonKdW_nonEU",,intersect(getNames(output),IntVars)] <- NA
    }
    
    if(length(minP) > 0) {
      output["minP",,intersect(getNames(output),IntVars)] <- NA
      output["non_minP",,intersect(getNames(output),IntVars)] <- NA
      output["EU_non_minP",,intersect(getNames(output),IntVars)] <- NA
      output["EUETS_non_minP",,intersect(getNames(output),IntVars)] <- NA
    }
  }
  
  #Transmission capacity aggregated (special case)
  if(length(intersect(getNames(output),"Capacity|Electricity|Transmission Grid (GW)")) > 0) {
    output[c("GLO","EU28","EUETS"),,"Capacity|Electricity|Transmission Grid (GW)"] <- output[c("GLO","EU28","EUETS"),,"Capacity|Electricity|Transmission Grid (GW)"]/2
    output[c("GLO","EU28","EUETS"),,"Capacity|Electricity|Transmission Grid-km (GWkm)"] <- output[c("GLO","EU28","EUETS"),,"Capacity|Electricity|Transmission Grid-km (GWkm)"]/2
  }
  
  
  #ADDING VARIABLES THAT ONLY EXIST FOR AN AGGREGATED REGION, e.g., the EU ETS cap
  #(not possible to do as default for the region-dependent variables because mbind only works when dimensions do not differ)
  # settings mapping path
  AggVarPath <- system.file("extdata","AggregateVariables.csv",package="limes")
  # reading mapping file
  AggVarfile <- read.csv(AggVarPath,sep=";")
  #  write the *.mif or give back the magpie opject output
  AggVars_tmp <- paste0(as.vector(AggVarfile$LIMES)," (",as.vector(AggVarfile$UnitLIMES) , ")")
  AggVars <- intersect(AggVars_tmp,getNames(output))
  
  # read sets and parameters
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  
  #Check the version: When there is endogenous heating, related emissions should not appear here (to avoid duplicates)
  if(c_LIMESversion >= 2.28) {
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    if(c_heating == 1) {
      AggVars <- AggVars[is.na(match(AggVars,"Emissions|CO2|Energy|Supply|Heat|District Heating (Mt CO2/yr)"))]
    }
  }
  
  #Adding the corresponding values for the EU ETS
  output_EUETSvars <- reportEUETSvars(gdx,output)[,time,]
  output["EUETS",,intersect(AggVars,getNames(output_EUETSvars))] <- output_EUETSvars[,,intersect(AggVars,getNames(output_EUETSvars))]
  output_MSR <- reportMSR(gdx)[,time,]
  output["EUETS",,intersect(AggVars,getNames(output_MSR))] <- output_MSR[,,intersect(AggVars,getNames(output_MSR))]
  #Erase the values from the other fictitious variables for which there are no real values
  output["EUETS",,setdiff(AggVars,union(getNames(output_EUETSvars),getNames(output_MSR)))] <- NA
  #output["EUETS",,AggVars] <- reportEUETSvars(gdx)[,time,AggVars]
  #Erasing the values for the remaining regions
  output[setdiff(getRegions(output),"EUETS"),,AggVars] <- NA
  
  
  #INCLUDE HISTORICAL VALUES FOR THE INDUSTRY
  #(source: "REPORT FROM THE COMMISSION TO THE EUROPEAN PARLIAMENT AND THE COUNCIL Report on the functioning of the European carbon market COM/2019/557 final/2" https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:52019DC0557R(01))
  if(!is.na(match("Emissions|CO2|Industry (Mt CO2/yr)",getNames(output)))) {
    output["EUETS",2010,"Emissions|CO2|Industry (Mt CO2/yr)"] <- NA #715 #Value from 2011 (2010 not available) - emissions reported from data viewer (537.4) might be incomplete as some could be included in combustion
    #output["EUETS",2015,"Emissions|CO2|Industry (Mt CO2/yr)"] <- 771 #emissions reported from data viewer (590.8) might be incomplete as some could be included in combustion
    output["EUETS",2010,"Emissions|CO2|Electricity and Industry (Mt CO2/yr)"] <- NA #output["EUETS",2010,"Emissions|CO2|Industry (Mt CO2/yr)"] + output["EUETS",2010,"Emissions|CO2|Electricity and Industry (Mt CO2/yr)"]
    #output["EUETS",2015,"Emissions|CO2|Electricity and Industry (Mt CO2/yr)"] <- output["EUETS",2015,"Emissions|CO2|Industry (Mt CO2/yr)"] + output["EUETS",2015,"Emissions|CO2|Electricity and Industry (Mt CO2/yr)"]
    
    if(!is.na(match("Emissions|CO2|EU ETS (Mt CO2/yr)",getNames(output)))) {
      output["EUETS",2010,"Emissions|CO2|EU ETS (Mt CO2/yr)"] <- NA #output["EUETS",2010,"Emissions|CO2|Industry (Mt CO2/yr)"] + output["EUETS",2010,"Emissions|CO2|EU ETS (Mt CO2/yr)"]
      #output["EUETS",2015,"Emissions|CO2|EU ETS (Mt CO2/yr)"] <- output["EUETS",2015,"Emissions|CO2|Industry (Mt CO2/yr)"] + output["EUETS",2015,"Emissions|CO2|EU ETS (Mt CO2/yr)"]
      
    }
    
    #To avoid confusion, make sure that industry-related values are not reported for the EU28
    output["EU28",c(2010,2015),"Emissions|CO2|Industry (Mt CO2/yr)"] <- NA
    #output["EU28",2015,"Emissions|CO2|Industry (Mt CO2/yr)"] <- NA 
    output["EU28",c(2010,2015),"Emissions|CO2|Electricity and Industry (Mt CO2/yr)"] <- NA
    #output["EU28",2015,"Emissions|CO2|Electricity and Industry (Mt CO2/yr)"] <- NA
    
    #Add NA for 2010. Because the MAC industry only applies from 2015, the reported price is the sum 
    if(is.na(output[c("GLO"),c(2010),"Emissions|CO2|Industry (Mt CO2/yr)"])) {
      output[c("EU28","EUETS","GLO"),c(2010),"Price|Carbon|Net|Industry (Eur2010/t CO2)"] <- NA
      output[c("EU28","EUETS","GLO"),c(2010),"Price|Carbon|National Climate Target|Industry (Eur2010/t CO2)"] <- NA
    }
  }
  
  #SCALING THE RESULTS ACCORDING TO THE UNITS SPECIFIES FOR THE PROJECT
  tmp <- NULL
  for (i in c(1:length(getNames(output)))) {
    tmp <- mbind(tmp,output[,,i]*mappingvars[match(getNames(output)[i],finalvars),]$ConvFactor)
  }
  output_f <- tmp
  #output<-tmp
  #output_f<-output
  
  
  #MAPPING THE VARIABLES TO THOSE OF A SPECIFIC PROJECT
  posvarsmapping <- match(getNames(output_f),finalvars)
  posvarsmapping <- posvarsmapping[which(posvarsmapping != "NA")]
  mappingvars_project <- paste0(as.vector(mappingvars[posvarsmapping,]$Enavi)," (",as.vector(mappingvars[posvarsmapping,]$UnitEnavi) , ")")
  output_f <- setNames(output_f,mappingvars_project)
  
  
  #WRITE REPORT
  #load the model version
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found") #model version
  if(!is.null(file)) {
    write.report(output_f,model=paste0("LIMES_EU_v",c_LIMESversion),scenario=scenario,file=file,ndigit=7)
  } 
  #if(!is.null(file)) write.reportProject(paste0("LIMES_generic_",scenario,".mif"),mappingvars,model="LIMES_EU",scenario=scenario,file=file,ndigit=7)
  else {
    return(output_f) 
  }  
  
  
  #################################################################
  #ONLY FOR THE REPORT OF INPUTS (run manually)
  #################################################################
  
  ##Always keep this in 0!!!!
  #c_reportinput <- 0
  #
  #if(c_reportinput != 0) {
  #  time=as.numeric(readGDX(gdx,name="t"))
  #    
  #  #Files-related variables
  #  #outputdir <- "C:/Users/osorio/ownCloud/PIK/Data for LIMES/CSV files/output"     # path to the output folder
  #  limes_reporting_file <- path(outputdir,paste0("LIMES_inputparam.mif"))
  #  file=limes_reporting_file
  #  #lastDir <- splitPath[[1]][length(splitPath[[1]])]
  #  #scenario <- lastDir
  #  
  #  #initialize report variable
  #  output <- NULL
  #  
  #  #adding fuel costs to report output
  #  output <- mbind(output,reportFuelCosts(gdx)[,time,])
  #  
  #  #Adding all the input parameters (except for fuel costs) -> turn on switch (c_reportheating) if heat-related input is to be reported
  #  output <- mbind(output,reportInput(gdx)[,time,])
  #  
  #  #Clean the output
  #  output_glo <- NULL
  #  var_dup <- NULL
  #  n_regi <- length(unique(getRegions(output)))
  #  n_years <- length(getYears(output))
  #  for (var_name in getNames(output)) {
  #    #Create array to save whether the number is duplicated for all REGI in one YEAR
  #    dup_year <- new.magpie(cells_and_regions = "GLO", years = getYears(output), names = NULL,
  #                           fill = 0, sort = FALSE, sets = NULL, unit = "unknown")
  #    #Create array to save duplicated values in just one ("GLO") array
  #    #output_tmp <- new.magpie(cells_and_regions = "GLO", years = getYears(output), names = NULL,
  #    #                       fill = 0, sort = FALSE, sets = NULL, unit = "unknown")
  #    
  #    for (tt in getYears(output)) {
  #      #Check if the value is duplicated for all REGI in one YEAR
  #      if(sum(duplicated(output[,tt,var_name])) == n_regi-1) {
  #        dup_year[,tt,] <- 1
  #      }
  #    }
  #    
  #    #Check if duplicates occur over all YEARS
  #    #output_tmp <- NULL
  #    if(dimSums(dup_year[,,],dim=2)==n_years) {
  #      #Allocate DEU values to the unique "GLO" variable
  #      output_tmp <- output["DEU",,var_name]
  #      var_dup <- c(var_dup,var_name)
  #    } else {
  #      output_tmp <- 0*output["DEU",,var_name]
  #    }
  #    getRegions(output_tmp) <- "GLO"
  #    
  #    #Concatenate all "GLO" variables
  #    output_glo <- mbind(output_glo,output_tmp)
  #  }
  #  
  #  #Concatenate "GLO" variables with (surviving) region-dependent parameters
  #  output_f <- mbind(output_glo,output)
  #  
  #  #Clean the file
  #  #Erase region-dependent data for variables with duplicates
  #  output_f[setdiff(getRegions(output_f),"GLO"),,var_dup] <- NA
  #  #Erase "GLO" data for variables without duplicates
  #  output_f["GLO",,setdiff(getNames(output_f),var_dup)] <- NA
  #  
  #  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found") #model version
  #  if(!is.null(file)) write.report(output_f,model=paste0("LIMES_EU_v",c_LIMESversion),scenario=scenario,file=file,ndigit=7)
  #}
  
  
  
}



