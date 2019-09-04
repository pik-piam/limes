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
  petyex <- readGDX(gdx,name="petyex") #set of exhaustible primary energy
  tegas <- readGDX(gdx,name="tegas") #set of gas generation technologies
  telig <- readGDX(gdx,name="telig") #set of lignite generation technologies
  tecoal <- readGDX(gdx,name="tecoal") #set of hard coal generation technologies
  tengcc <- readGDX(gdx,name="tengcc") #set of NGCC generation technologies
  
  # read parameters
  s_c2co2 <- readGDX(gdx,name="s_c2co2",field="l",format="first_found") #conversion factor C -> CO2
  c_bankemi_EU <- readGDX(gdx,name="c_bankemi_EU",field="l",format="first_found")
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  
  # read variables
  v_emi <- readGDX(gdx,name="v_emi",field="l",format="first_found")
  
  # create MagPie object of v_emi with iso3 regions
  v_emi <- limesMapping(v_emi)
  
  #take only the co2 and convert from GtC to MtCO2
  v_emi <- v_emi[,,"co2"]*s_c2co2*1000
  
  #Read and transform the v_emifloor; read v_bankemi
  v_emifloor <- readGDX(gdx,name="v_emifloor",field="l",format="first_found")
  v_emifloor <- limesMapping(v_emifloor)
  
  #annual emissions per primary energy type
  tmp1 <- NULL
  for (petyex2 in petyex) {
    if(petyex2 != "pebio" & petyex2 != "peur" & petyex2 != "pehgen") #keeping only the fossil-fuels
    tmp1 <- mbind(tmp1,setNames(dimSums(v_emi[,,petyex2]*s_c2co2*1000,3),paste("Emissions|CO2|Energy|Supply|Electricity|",petyex2,"(Mt CO2/yr)")))
  }
  
  #annual emissions per country
  tmp2 <- NULL
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,],3),"Emissions|CO2|Energy|Supply|Electricity (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c(telig,tecoal)],3),"Emissions|CO2|Energy|Supply|Electricity|Coal (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c(setdiff(telig,"lpcc"),setdiff(tecoal,"pcc"))],3),"Emissions|CO2|Energy|Supply|Electricity|Coal|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c("pcc","lpcc")],3),"Emissions|CO2|Energy|Supply|Electricity|Coal|w/ CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c(tecoal)],3),"Emissions|CO2|Energy|Supply|Electricity|Hard Coal (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c(setdiff(tecoal,"pcc"))],3),"Emissions|CO2|Energy|Supply|Electricity|Hard Coal|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,"pcc"],3),"Emissions|CO2|Energy|Supply|Electricity|Hard Coal|w/ CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c(telig)],3),"Emissions|CO2|Energy|Supply|Electricity|Lignite (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c(setdiff(telig,"lpcc"))],3),"Emissions|CO2|Energy|Supply|Electricity|Lignite|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,"lpcc"],3),"Emissions|CO2|Energy|Supply|Electricity|Lignite|w/ CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c("oil")],3),"Emissions|CO2|Energy|Supply|Electricity|Oil (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c(tegas)],3),"Emissions|CO2|Energy|Supply|Electricity|Gas (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c(setdiff(tegas,"ngccc"))],3),"Emissions|CO2|Energy|Supply|Electricity|Gas|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c("ngccc")],3),"Emissions|CO2|Energy|Supply|Electricity|Gas|w/ CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c(setdiff(tengcc,"ngccc"))],3),"Emissions|CO2|Energy|Supply|Electricity|Gas CC|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,"ngt"],3),"Emissions|CO2|Energy|Supply|Electricity|Gas OC|w/o CCS (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c("pewaste")],3),"Emissions|CO2|Energy|Supply|Electricity|Waste (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c("others")],3),"Emissions|CO2|Energy|Supply|Electricity|Other (Mt CO2/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_emi[,,c("others","waste","oil")],3),"Emissions|CO2|Energy|Supply|Electricity|Other Fossil (Mt CO2/yr)"))
  
  # concatenate vars
  tmp3 <- mbind(tmp1,tmp2)
  
  #annual emissions withdrawn from the EU ETS
  #If activate this, remember to activate the code in convGDX2MIF to erase the values for the countries for which this variable does not exist
  tmp4 <- NULL
  #tmp4 <- mbind(tmp4,setNames(dimSums(v_emifloor[,,]*s_c2co2*1000,3),"Emissions withdrawn ETS|CO2|Energy|Supply|Electricity (Mt CO2/yr)"))
  
  # concatenate data
  tmp <- mbind(tmp3,tmp4)

  return(tmp)
}
  
