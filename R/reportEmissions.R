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
  t <- readGDX(gdx,name="t")
  te <- readGDX(gdx,name="te")
  tehe <- readGDX(gdx,name="tehe")
  ter <- readGDX(gdx,name="ter")
  tecoal <- readGDX(gdx,name="tecoal") 
  telig <- readGDX(gdx,name="telig") 
  tegas <- readGDX(gdx,name="tegas") 
  tengcc <- readGDX(gdx,name="tengcc")
  tefossil <- readGDX(gdx,name="tefossil") #set of fossil-based generation technologies
  teccs <- readGDX(gdx,name="teccs") #set of generation technologies with CCS
  tehgen <- readGDX(gdx,name="tehgen") #set of hydrogen generation technologies
  tebio <- readGDX(gdx,name="tebio") #set of biomass generation technologies
  teoil <- readGDX(gdx,name="teoil") #set of oil generation technologies
  teothers <- readGDX(gdx,name="teothers") #set of other gases generation technologies
  tegas_el <- setdiff(tegas,"ngcc_heat")
  tengcc_el <- setdiff(tengcc,"ngcc_heat")
  
  # read parameters
  s_c2co2 <- readGDX(gdx,name="s_c2co2",field="l",format="first_found") #conversion factor C -> CO2
  c_bankemi_EU <- readGDX(gdx,name="c_bankemi_EU",field="l",format="first_found")
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  
  # read variables
  v_emi <- readGDX(gdx,name="v_emi",field="l",format="first_found")
  
  # create MagPie object of v_emi with iso3 regions
  v_emi <- limesMapping(v_emi)
  
  #take only the co2 and convert from GtC to MtCO2
  v_emi_ccs <- v_emi[,,"cco2"]*s_c2co2*1000
  v_emi <- v_emi[,,"co2"]*s_c2co2*1000
  v_emi_el <- v_emi
  
  #Read and transform the v_emifloor; read v_bankemi
  #v_emifloor <- readGDX(gdx,name="v_emifloor",field="l",format="first_found")
  #v_emifloor <- limesMapping(v_emifloor)
  
  #Check the version so to choose the electricity-related variables
  if(c_LIMESversion >= 2.28) {
    v_emi_el <- v_emi
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    if(c_heating == 1) {
      v_emi_he <- v_emi[,,"sehe"]
      v_emi_el <- v_emi[,,"seel"]
    }
  } 
  
  #annual emissions per primary energy type
  tmp1 <- NULL
  #for (petyex2 in petyex) {
  #  if(petyex2 != "pebio" & petyex2 != "peur" & petyex2 != "pehgen") #keeping only the fossil-fuels
  #  tmp1 <- mbind(tmp1,setNames(dimSums(v_emi[,,petyex2]*s_c2co2*1000,3),paste("Emissions|CO2|Energy|Supply|Electricity|",petyex2,"(Mt CO2/yr)")))
  #}
  
  #annual emissions per country
  tmp2 <- NULL
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,],3),"Emissions|CO2|Energy|Supply|Electricity (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c(telig,tecoal)],3),"Emissions|CO2|Energy|Supply|Electricity|Coal (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c(setdiff(telig,teccs),setdiff(tecoal,"pcc"))],3),"Emissions|CO2|Energy|Supply|Electricity|Coal|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,intersect(c(tecoal,telig),teccs)],3),"Emissions|CO2|Energy|Supply|Electricity|Coal|w/ CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c(tecoal)],3),"Emissions|CO2|Energy|Supply|Electricity|Hard Coal (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c(setdiff(tecoal,"pcc"))],3),"Emissions|CO2|Energy|Supply|Electricity|Hard Coal|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,intersect(tecoal,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity|Hard Coal|w/ CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c(telig)],3),"Emissions|CO2|Energy|Supply|Electricity|Lignite (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c(setdiff(telig,teccs))],3),"Emissions|CO2|Energy|Supply|Electricity|Lignite|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,intersect(telig,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity|Lignite|w/ CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c(teoil)],3),"Emissions|CO2|Energy|Supply|Electricity|Oil (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c(tegas_el)],3),"Emissions|CO2|Energy|Supply|Electricity|Gas (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,setdiff(tegas_el,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity|Gas|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,intersect(tengcc_el,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity|Gas|w/ CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,setdiff(tengcc_el,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity|Gas CC|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,setdiff(tegas_el,c(tengcc,teccs))],3),"Emissions|CO2|Energy|Supply|Electricity|Gas OC|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c("pewaste")],3),"Emissions|CO2|Energy|Supply|Electricity|Waste (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c(teothers)],3),"Emissions|CO2|Energy|Supply|Electricity|Other (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi_el[,,c(teothers,"waste",teoil)],3),"Emissions|CO2|Energy|Supply|Electricity|Other Fossil (Mt CO2/yr)"))
  
  # concatenate vars
  tmp3 <- mbind(tmp1,tmp2)
  
  #annual emissions withdrawn from the EU ETS
  #If activate this, remember to activate the code in convGDX2MIF to erase the values for the countries for which this variable does not exist
  tmp4 <- NULL
  #tmp4 <- mbind(tmp4,setNames(dimSums(v_emifloor[,,]*s_c2co2*1000,3),"Emissions withdrawn ETS|CO2|Energy|Supply|Electricity (Mt CO2/yr)"))
  if(c_LIMESversion >= 2.33) {
    tewaste <- readGDX(gdx,name="tewaste") #set of waste generation technologies
    if(c_heating == 1) {
      #Heat 
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_he[,,],3),"Emissions|CO2|Energy|Supply|Heat (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_he[,,intersect(c(tecoal,telig),tehe)],3),"Emissions|CO2|Energy|Supply|Heat|Coal (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_he[,,intersect(tecoal,tehe)],3),"Emissions|CO2|Energy|Supply|Heat|Hard Coal (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_he[,,intersect(telig,tehe)],3),"Emissions|CO2|Energy|Supply|Heat|Lignite (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_he[,,intersect(teoil,tehe)],3),"Emissions|CO2|Energy|Supply|Heat|Oil (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_he[,,intersect(tegas,tehe)],3),"Emissions|CO2|Energy|Supply|Heat|Gas (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_he[,,intersect(teothers,tehe)],3),"Emissions|CO2|Energy|Supply|Heat|Other (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_he[,,intersect(tewaste,tehe)],3),"Emissions|CO2|Energy|Supply|Heat|Waste (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi_he[,,intersect(c(teothers,tewaste,teoil),tehe)],3),"Emissions|CO2|Energy|Supply|Heat|Other Fossil (Mt CO2/yr)"))
      
      
      #Electricity and Heat
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,],3),"Emissions|CO2|Energy|Supply|Electricity and Heat (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(telig,tecoal)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Coal (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(setdiff(telig,teccs),setdiff(tecoal,"pcc"))],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Coal|w/o CCS (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,intersect(c(tecoal,telig),teccs)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Coal|w/ CCS (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(tecoal)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Hard Coal (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(setdiff(tecoal,"pcc"))],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Hard Coal|w/o CCS (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,intersect(tecoal,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Hard Coal|w/ CCS (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(telig)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Lignite (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(setdiff(telig,teccs))],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Lignite|w/o CCS (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,intersect(telig,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Lignite|w/ CCS (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(teoil)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Oil (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(tegas)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Gas (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,setdiff(tegas,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Gas|w/o CCS (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,intersect(tengcc,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Gas|w/ CCS (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,setdiff(tengcc,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Gas CC|w/o CCS (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,setdiff(tegas,c(tengcc,teccs))],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Gas OC|w/o CCS (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(tewaste)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Waste (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(teothers)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Other (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4,setNames(dimSums(v_emi[,,c(teothers,tewaste,teoil)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Other Fossil (Mt CO2/yr)"))
      
    }
  } 
  
  # concatenate data
  tmp5 <- mbind(tmp3,tmp4)
  
  #Carbon sequestration
  tmp6 <- NULL
  tmp6 <- mbind(tmp6,setNames(dimSums(v_emi_ccs,3),"Carbon Sequestration|CCS|Electricity (Mt CO2/yr)"))
  
  #Biomass related variables (because there are new biomass technologies from v2.33)
  
  if(c_LIMESversion >= 2.33) {
    tmp6 <- mbind(tmp6,setNames(dimSums(v_emi_el[,,intersect(tebio,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity|Biomass (Mt CO2/yr)"))
    tmp6 <- mbind(tmp6,setNames(dimSums(v_emi_el[,,intersect(tebio,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity|Biomass w/ CCS (Mt CO2/yr)"))
    tmp6 <- mbind(tmp6,setNames(dimSums(v_emi[,,intersect(tebio,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Biomass (Mt CO2/yr)"))
    tmp6 <- mbind(tmp6,setNames(dimSums(v_emi[,,intersect(tebio,teccs)],3),"Emissions|CO2|Energy|Supply|Electricity and Heat|Biomass w/ CCS (Mt CO2/yr)"))
    tmp6 <- mbind(tmp6,setNames(dimSums(v_emi_ccs[,,intersect(tefossil,teccs)],3),"Carbon Sequestration|CCS|Electricity|Fossil (Mt CO2/yr)"))
    tmp6 <- mbind(tmp6,setNames(dimSums(v_emi_ccs[,,intersect(tebio,teccs)],3),"Carbon Sequestration|CCS|Electricity|Biomass (Mt CO2/yr)"))
    
  } 
  
  # concatenate data
  tmp <- mbind(tmp5,tmp6)

  return(tmp)
}
  
