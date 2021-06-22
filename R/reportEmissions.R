#' Read in GDX and calculate emissions, used in convGDX2MIF.R for the reporting
#' 
#' Read in emissions data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the emission variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportEmissions(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportEmissions <- function(gdx) {
  
  # read sets
  te <- readGDX(gdx,name="te")
  teel <- readGDX(gdx,name="teel")
  ter <- readGDX(gdx,name="ter")
  tecoal <- readGDX(gdx,name="tecoal") 
  telig <- readGDX(gdx,name="telig") 
  tegas <- readGDX(gdx,name="tegas") 
  tengcc <- readGDX(gdx,name="tengcc")
  tefossil <- readGDX(gdx,name="tefossil") #set of fossil-based generation technologies
  teccs <- readGDX(gdx,name="teccs") #set of generation technologies with CCS
  tebio <- readGDX(gdx,name="tebio") #set of biomass generation technologies
  teoil <- readGDX(gdx,name="teoil") #set of oil generation technologies
  teothers <- readGDX(gdx,name="teothers") #set of other gases generation technologies
  tegas_el <- intersect(tegas,teel)
  tengcc_el <- intersect(tengcc,teel)
  
  # read parameters
  s_c2co2 <- readGDX(gdx,name="s_c2co2",field="l",format="first_found") #conversion factor C -> CO2
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  
  # read variables
  v_emi <- readGDX(gdx,name="v_emi",field="l",format="first_found",restore_zeros = FALSE)
  #v_emi <- readGDX(gdx,name="v_emi",field="l",format="simple",restore_zeros = FALSE, types = "variables")
  
  # create MagPie object of v_emi with iso3 regions
  v_emi <- limesMapping(v_emi)
  
  #take only the co2 and convert from GtC to MtCO2
  v_emi_ccs <- v_emi[,,"cco2"]*as.numeric(s_c2co2)*1000
  v_emi <- v_emi[,,"co2"]*as.numeric(s_c2co2)*1000
  v_emi_el <- v_emi
  
  #Read and transform the v_emifloor; read v_bankemi
  #v_emifloor <- readGDX(gdx,name="v_emifloor",field="l",format="first_found")
  #v_emifloor <- limesMapping(v_emifloor)
  
  #Check the version so to choose the electricity-related variables
  if(c_LIMESversion >= 2.28) {
    #v_emi_el <- v_emi
    v_emi_he <- v_emi[,,"sehe"]
    v_emi_el <- v_emi[,,"seel"]
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    #if(c_heating == 1) {
    #  
    #}
  } 
  
  #annual emissions per primary energy type
  tmp1 <- NULL
  #for (petyex2 in petyex) {
  #  if(petyex2 != "pebio" & petyex2 != "peur" & petyex2 != "pehgen") #keeping only the fossil-fuels
  #  tmp1 <- mbind(tmp1,setNames(dimSums(v_emi[,,petyex2]*s_c2co2*1000,3),paste("Emissions|CO2|Energy|Supply|Electricity|",petyex2,"(Mt CO2/yr)")))
  #}
  
  #annual emissions per country
  tmp2 <- NULL
  
  varList_el <- list(
    #Conventional
    "Emissions|CO2|Energy|Supply|Electricity (Mt CO2/yr)"                        ="seel",
    "Emissions|CO2|Energy|Supply|Electricity|Coal (Mt CO2/yr)"                   =intersect(teel,c(tecoal,telig)),
    "Emissions|CO2|Energy|Supply|Electricity|Coal|w/o CCS (Mt CO2/yr)"           =intersect(teel,setdiff(c(tecoal,telig),teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Coal|w/ CCS (Mt CO2/yr)"            =intersect(teel,intersect(c(tecoal,telig),teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Hard Coal (Mt CO2/yr)"              =intersect(teel,c(tecoal)),
    "Emissions|CO2|Energy|Supply|Electricity|Hard Coal|w/o CCS (Mt CO2/yr)"      =intersect(teel,setdiff(c(tecoal),teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Hard Coal|w/ CCS (Mt CO2/yr)"       =intersect(teel,intersect(c(tecoal),teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Lignite (Mt CO2/yr)"                =intersect(teel,c(telig)),
    "Emissions|CO2|Energy|Supply|Electricity|Lignite|w/o CCS (Mt CO2/yr)"        =intersect(teel,setdiff(c(telig),teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Lignite|w/ CCS (Mt CO2/yr)"         =intersect(teel,intersect(c(telig),teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Oil (Mt CO2/yr)"                    =intersect(teel,c(teoil)),
    "Emissions|CO2|Energy|Supply|Electricity|Gas (Mt CO2/yr)"                    =intersect(teel,c(tegas)),
    "Emissions|CO2|Energy|Supply|Electricity|Gas|w/o CCS (Mt CO2/yr)"            =intersect(teel,setdiff(tegas_el,teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Gas|w/ CCS (Mt CO2/yr)"             =intersect(teel,intersect(tegas_el,teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Gas CC|w/o CCS (Mt CO2/yr)"         =intersect(teel,setdiff(tengcc_el,teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Gas CC|w/ CCS (Mt CO2/yr)"          =intersect(teel,intersect(tengcc_el,teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Gas CC (Mt CO2/yr)"                 =intersect(teel,c(tengcc_el)),
    "Emissions|CO2|Energy|Supply|Electricity|Gas OC (Mt CO2/yr)"                 =intersect(teel,setdiff(tegas_el,tengcc_el)),
    "Emissions|CO2|Energy|Supply|Electricity|Other (Mt CO2/yr)"                  =intersect(teel,c(teothers)),
    "Emissions|CO2|Energy|Supply|Electricity|Waste (Mt CO2/yr)"                  =intersect(teel,c("waste")),
    "Emissions|CO2|Energy|Supply|Electricity|Other Fossil (Mt CO2/yr)"           =intersect(teel,c(teothers,"waste",teoil)),
    
    #general aggregation
    "Emissions|CO2|Energy|Supply|Electricity|Fossil (Mt CO2/yr)"                 =intersect(teel,c(tefossil)),
    "Emissions|CO2|Energy|Supply|Electricity|Fossil|w/o CCS (Mt CO2/yr)"         =intersect(teel,setdiff(tefossil,teccs)),
    "Emissions|CO2|Energy|Supply|Electricity|Fossil|w/ CCS (Mt CO2/yr)"          =intersect(teel,intersect(tefossil,teccs))
  )
  
  for (var in names(varList_el)){
    tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,varList_el[[var]]],dim=3,na.rm = T),var))
  }
  
  # concatenate vars
  tmp3 <- mbind(tmp1,tmp2)
    
  
  #annual emissions withdrawn from the EU ETS
  #If activate this, remember to activate the code in convGDX2MIF to erase the values for the countries for which this variable does not exist
  tmp4 <- NULL
  #tmp4 <- mbind(tmp4,setNames(dimSums(v_emifloor[,,]*s_c2co2*1000,3),"Emissions withdrawn ETS|CO2|Energy|Supply|Electricity (Mt CO2/yr)"))
  if(c_LIMESversion >= 2.33) {
    tewaste <- readGDX(gdx,name="tewaste") #set of waste generation technologies
    
    #Biomass related variables (because there are new biomass technologies from v2.33)
    #tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_el[,,intersect(tebio,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity|Biomass (Mt CO2/yr)")) #might be confusing the fact that is exactly the same as BECCS
    tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_el[,,intersect(tebio,teccs)],dim=3,na.rm = T),"Emissions|CO2|Energy|Supply|Electricity|Biomass|w/ CCS (Mt CO2/yr)"))
    tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_ccs[,,intersect(tebio,teccs)],dim=3,na.rm = T),"Carbon Sequestration|CCS|Electricity|Biomass (Mt CO2/yr)"))
    
    
    if(c_heating == 1) {
      #load heat-related sets
      techp <- readGDX(gdx,name="techp")
      teoel <- readGDX(gdx,name="teoel")
      teohecen <- readGDX(gdx,name="teohecen")
      tedh <- readGDX(gdx,name="tedh")
      tedhelec <- readGDX(gdx,name="tedhelec")
      ternofluc <- readGDX(gdx,name="ternofluc")
      
      
      #1) Emissions FROM DH: CHP AND Heat-only 
      varList_he <- list(
        #1.b) CHP
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP (Mt CO2/yr)"                         =setdiff(techp,c(ter,ternofluc,tedhelec)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP|Coal (Mt CO2/yr)"                    =intersect(techp,c(tecoal,telig)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP|Hard Coal (Mt CO2/yr)"               =intersect(techp,c(tecoal)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP|Lignite (Mt CO2/yr)"                 =intersect(techp,c(telig)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP|Oil (Mt CO2/yr)"                     =intersect(techp,c(teoil)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP|Gas (Mt CO2/yr)"                     =intersect(techp,c(tegas)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP|Gas CC (Mt CO2/yr)"                  =intersect(techp,c(tengcc_el)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP|Gas OC (Mt CO2/yr)"                  =intersect(techp,setdiff(tegas_el,tengcc_el)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP|Other (Mt CO2/yr)"                   =intersect(techp,c(teothers)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP|Other Fossil (Mt CO2/yr)"            =intersect(techp,c(teothers,tewaste,teoil)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|CHP|Fossil (Mt CO2/yr)"                  =intersect(techp,c(tefossil)),
        
        #1.c) Only-heat (centralized boilers)
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Heat-only (Mt CO2/yr)"                   =setdiff(teohecen,c(ter,ternofluc,tedhelec)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Heat-only|Coal (Mt CO2/yr)"              =intersect(teohecen,c(tecoal,telig)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Heat-only|Hard Coal (Mt CO2/yr)"         =intersect(teohecen,c(tecoal)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Heat-only|Lignite (Mt CO2/yr)"           =intersect(teohecen,c(telig)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Heat-only|Oil (Mt CO2/yr)"               =intersect(teohecen,c(teoil)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Heat-only|Gas (Mt CO2/yr)"               =intersect(teohecen,c(tegas)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Heat-only|Other (Mt CO2/yr)"             =intersect(teohecen,c(teothers)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Heat-only|Waste (Mt CO2/yr)"             =intersect(teohecen,c(tewaste)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Heat-only|Other Fossil (Mt CO2/yr)"      =intersect(teohecen,c(teothers,tewaste,teoil)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Heat-only|Fossil (Mt CO2/yr)"            =intersect(teohecen,c(tefossil)),
        
        #1.d) District Heating
        "Emissions|CO2|Energy|Supply|Heat|District Heating (Mt CO2/yr)"                             =setdiff(tedh,c(ter,ternofluc,tedhelec)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Coal (Mt CO2/yr)"                        =intersect(tedh,c(tecoal,telig)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Hard Coal (Mt CO2/yr)"                   =intersect(tedh,c(tecoal)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Lignite (Mt CO2/yr)"                     =intersect(tedh,c(telig)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Oil (Mt CO2/yr)"                         =intersect(tedh,c(teoil)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Gas (Mt CO2/yr)"                         =intersect(tedh,c(tegas)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Other (Mt CO2/yr)"                       =intersect(tedh,c(teothers)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Waste (Mt CO2/yr)"                       =intersect(tedh,c(tewaste)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Other Fossil (Mt CO2/yr)"                =intersect(tedh,c(teothers,tewaste,teoil)),
        "Emissions|CO2|Energy|Supply|Heat|District Heating|Fossil (Mt CO2/yr)"                      =intersect(tedh,c(tefossil))
      )
      
      for (var in names(varList_he)) {
        tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_he[,,varList_he[[var]]],dim=3,na.rm = T),var))
      }
      
      #Electricity and Heat
      varList <- list(
        #Conventional
        "Emissions|CO2|Energy|Supply|Electricity and Heat (Mt CO2/yr)"                  =c("seel","sehe"),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|Biomass (Mt CO2/yr)"          =intersect(te,intersect(tebio,teccs)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|Coal (Mt CO2/yr)"             =intersect(te,c(tecoal,telig)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|Hard Coal (Mt CO2/yr)"        =intersect(te,c(tecoal)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|Lignite (Mt CO2/yr)"          =intersect(te,c(telig)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|Oil (Mt CO2/yr)"              =intersect(te,c(teoil)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|Gas (Mt CO2/yr)"              =intersect(te,c(tegas)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|Other (Mt CO2/yr)"            =intersect(te,c(teothers)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|Waste (Mt CO2/yr)"            =intersect(te,c(tewaste)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|Other Fossil (Mt CO2/yr)"     =intersect(te,c(teothers,tewaste,teoil)),
        
        #general aggregation
        "Emissions|CO2|Energy|Supply|Electricity and Heat|Fossil (Mt CO2/yr)"           =intersect(te,c(tefossil))
      )
      
      for (var in names(varList)){
        tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,varList[[var]]],dim=3,na.rm = T),var))
      }
      
      #Electricity emissions
      varList_el<- list(
        #1.b) CHP
        "Emissions|CO2|Energy|Supply|Electricity|CHP (Mt CO2/yr)"                         =setdiff(techp,c(ter,ternofluc,tedhelec)),
        "Emissions|CO2|Energy|Supply|Electricity|CHP|Coal (Mt CO2/yr)"                    =intersect(techp,c(tecoal,telig)),
        "Emissions|CO2|Energy|Supply|Electricity|CHP|Hard Coal (Mt CO2/yr)"               =intersect(techp,c(tecoal)),
        "Emissions|CO2|Energy|Supply|Electricity|CHP|Lignite (Mt CO2/yr)"                 =intersect(techp,c(telig)),
        "Emissions|CO2|Energy|Supply|Electricity|CHP|Oil (Mt CO2/yr)"                     =intersect(techp,c(teoil)),
        "Emissions|CO2|Energy|Supply|Electricity|CHP|Gas (Mt CO2/yr)"                     =intersect(techp,c(tegas)),
        "Emissions|CO2|Energy|Supply|Electricity|CHP|Gas CC (Mt CO2/yr)"                  =intersect(techp,c(tengcc_el)),
        "Emissions|CO2|Energy|Supply|Electricity|CHP|Gas OC (Mt CO2/yr)"                  =intersect(techp,setdiff(tegas_el,tengcc_el)),
        "Emissions|CO2|Energy|Supply|Electricity|CHP|Other (Mt CO2/yr)"                   =intersect(techp,c(teothers)),
        "Emissions|CO2|Energy|Supply|Electricity|CHP|Other Fossil (Mt CO2/yr)"            =intersect(techp,c(teothers,tewaste,teoil)),
        "Emissions|CO2|Energy|Supply|Electricity|CHP|Fossil (Mt CO2/yr)"                  =intersect(techp,c(tefossil)),
        
        #Electricity-only
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only (Mt CO2/yr)"                        =intersect(teoel,c(tefossil,intersect(tebio,teccs))),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Coal (Mt CO2/yr)"                   =intersect(teoel,c(tecoal,telig)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Coal|w/o CCS (Mt CO2/yr)"           =intersect(teoel,setdiff(c(tecoal,telig),teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Coal|w/ CCS (Mt CO2/yr)"            =intersect(teoel,intersect(c(tecoal,telig),teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Hard Coal (Mt CO2/yr)"              =intersect(teoel,c(tecoal)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Hard Coal|w/o CCS (Mt CO2/yr)"      =intersect(teoel,setdiff(c(tecoal),teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Hard Coal|w/ CCS (Mt CO2/yr)"       =intersect(teoel,intersect(c(tecoal),teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Lignite (Mt CO2/yr)"                =intersect(teoel,c(telig)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Lignite|w/o CCS (Mt CO2/yr)"        =intersect(teoel,setdiff(c(telig),teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Lignite|w/ CCS (Mt CO2/yr)"         =intersect(teoel,intersect(c(telig),teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Oil (Mt CO2/yr)"                    =intersect(teoel,c(teoil)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Gas (Mt CO2/yr)"                    =intersect(teoel,c(tegas)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Gas|w/o CCS (Mt CO2/yr)"            =intersect(teoel,setdiff(tegas_el,teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Gas|w/ CCS (Mt CO2/yr)"             =intersect(teoel,intersect(tegas_el,teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Gas CC|w/o CCS (Mt CO2/yr)"         =intersect(teoel,setdiff(tengcc_el,teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Gas CC|w/ CCS (Mt CO2/yr)"          =intersect(teoel,intersect(tengcc_el,teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Gas CC (Mt CO2/yr)"                 =intersect(teoel,c(tengcc_el)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Gas OC (Mt CO2/yr)"                 =intersect(teoel,setdiff(tegas_el,tengcc_el)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Other (Mt CO2/yr)"                  =intersect(teoel,c(teothers)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Waste (Mt CO2/yr)"                  =intersect(teoel,c(tewaste)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Other Fossil (Mt CO2/yr)"           =intersect(teoel,c(teothers,tewaste,teoil)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Fossil (Mt CO2/yr)"                 =intersect(teoel,c(tefossil)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Fossil|w/o CCS (Mt CO2/yr)"         =intersect(teoel,setdiff(tefossil,teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Fossil|w/ CCS (Mt CO2/yr)"          =intersect(teoel,intersect(tefossil,teccs)),
        "Emissions|CO2|Energy|Supply|Electricity|Electricity-only|Biomass|w/ CCS (Mt CO2/yr)"         =intersect(teoel,intersect(tebio,teccs))
      )
      
      for (var in names(varList_el)) {
        tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_el[,,varList_el[[var]]],dim=3,na.rm = T),var))
      }
      
      #CHP emissions
      varList<- list(
        #1.b) CHP
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP (Mt CO2/yr)"                         =setdiff(techp,c(ter,ternofluc,tedhelec)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP|Coal (Mt CO2/yr)"                    =intersect(techp,c(tecoal,telig)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP|Hard Coal (Mt CO2/yr)"               =intersect(techp,c(tecoal)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP|Lignite (Mt CO2/yr)"                 =intersect(techp,c(telig)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP|Oil (Mt CO2/yr)"                     =intersect(techp,c(teoil)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP|Gas (Mt CO2/yr)"                     =intersect(techp,c(tegas)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP|Gas CC (Mt CO2/yr)"                  =intersect(techp,c(tengcc_el)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP|Gas OC (Mt CO2/yr)"                  =intersect(techp,setdiff(tegas_el,tengcc_el)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP|Other (Mt CO2/yr)"                   =intersect(techp,c(teothers)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP|Other Fossil (Mt CO2/yr)"            =intersect(techp,c(teothers,tewaste,teoil)),
        "Emissions|CO2|Energy|Supply|Electricity and Heat|CHP|Fossil (Mt CO2/yr)"                  =intersect(techp,c(tefossil))
      )
      
      for (var in names(varList)) {
        tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,varList[[var]]],dim=3,na.rm = T),var))
      }
    }
  } 
  
  # concatenate data
  tmp5 <- mbind(tmp3,tmp4)
  
  #Carbon sequestration
  tmp6 <- NULL
  tmp6 <- mbind(tmp6,setNames(dimSums(v_emi_ccs,dim=3,na.rm = T),"Carbon Sequestration|CCS|Electricity (Mt CO2/yr)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_emi_ccs[,,intersect(tefossil,teccs)],dim=3,na.rm = T),"Carbon Sequestration|CCS|Electricity|Fossil (Mt CO2/yr)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_emi_ccs[,,intersect(c(tecoal,telig),teccs)],dim=3,na.rm = T),"Carbon Sequestration|CCS|Electricity|Coal (Mt CO2/yr)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_emi_ccs[,,intersect(tecoal,teccs)],dim=3,na.rm = T),"Carbon Sequestration|CCS|Electricity|Hard Coal (Mt CO2/yr)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_emi_ccs[,,intersect(telig,teccs)],dim=3,na.rm = T),"Carbon Sequestration|CCS|Electricity|Lignite (Mt CO2/yr)"))
  
  # concatenate data
  tmp <- mbind(tmp5,tmp6)

  return(tmp)
}
  
