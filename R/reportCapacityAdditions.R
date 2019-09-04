#' Read in GDX and calculate capacity additions, used in convGDX2MIF.R for the reporting
#' 
#' Read in capacity additions from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportCapacityAdditions(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#'

reportCapacityAdditions <- function(gdx) {
  
  # read sets
  teel <- readGDX(gdx,name="teel") #set of electricity generation technologies (non-storage)
  ter <- readGDX(gdx,name="ter") #set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx,name="ternofluc") #set of non-variable (non-fluctuating) renewable electricity generation technologies
  tefossil <- readGDX(gdx,name="tefossil") #set of fossil-based electricity generation technologies
  tenr <- readGDX(gdx,name="tenr") #set of non-renewable electricity generation technologies (includes storage)

  # read variables
  v_deltacap <- readGDX(gdx,name="v_deltacap",field="l",format="first_found")
  
  # create MagPie object of v_deltacap with iso3 regions
  v_deltacap <- limesMapping(v_deltacap)
  
  # total installed capacity
  tmp1 <- NULL
  #for (tech in teel) {
  #  tmp1 <- mbind(tmp1,setNames(v_deltacap[,,tech],paste("Capacity Additions|Electricity|",tech,"(GW/yr)")))
  #}
  
  #aggregated technologies
  tmp2 <- NULL
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("tnr")],dim=3),"Capacity Additions|Electricity|Nuclear (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("spv","csp")],dim=3),"Capacity Additions|Electricity|Solar (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("windon","windoff")],dim=3),"Capacity Additions|Electricity|Wind (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("hydro","ror","hs")],dim=3),"Capacity Additions|Electricity|Hydro (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("pc","pc80","pc95","pc10","pcc","lpc","lpc80","lpc95","lpc10","lpcc")],dim=3),"Capacity Additions|Electricity|Coal (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("pc","pc80","pc95","pc10","pcc")],dim=3),"Capacity Additions|Electricity|Hard Coal (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("lpc","lpc80","lpc95","lpc10","lpcc")],dim=3),"Capacity Additions|Electricity|Lignite (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("ngcc","ngcc80","ngcc95","ngcc10","ngt","ngccc")],dim=3),"Capacity Additions|Electricity|Gas (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("biolcigcc")],dim=3),"Capacity Additions|Electricity|Biomass (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("biolcigcc")],dim=3),"Capacity Additions|Electricity|Biomass|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("oil")],dim=3),"Capacity Additions|Electricity|Oil (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("oil")],dim=3),"Capacity Additions|Electricity|Oil|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("waste")],dim=3),"Capacity Additions|Electricity|Waste (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("hcc","hct","hfc")],dim=3),"Capacity Additions|Electricity|Hydrogen (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("others")],dim=3),"Capacity Additions|Electricity|Other (GW/yr)"))

  #aggregated coal
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("pc","pc80","pc95","pc10","lpc","lpc80","lpc95","lpc10")],dim=3),"Capacity Additions|Electricity|Coal|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("pcc","lpcc")],dim=3),"Capacity Additions|Electricity|Coal|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("pc","pc80","pc95","pc10")],dim=3),"Capacity Additions|Electricity|Hard Coal|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("pcc")],dim=3),"Capacity Additions|Electricity|Hard Coal|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("lpc","lpc80","lpc95","lpc10")],dim=3),"Capacity Additions|Electricity|Lignite|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("lpcc")],dim=3),"Capacity Additions|Electricity|Lignite|w/ CCS (GW/yr)"))

  #aggregated gas
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("ngcc","ngcc80","ngcc95","ngcc10","ngccc")],dim=3),"Capacity Additions|Electricity|Gas CC (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("ngcc","ngcc80","ngcc95","ngcc10")],dim=3),"Capacity Additions|Electricity|Gas CC|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("ngcc","ngcc80","ngcc95","ngcc10","ngt")],dim=3),"Capacity Additions|Electricity|Gas|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("ngccc")],dim=3),"Capacity Additions|Electricity|Gas|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("ngccc")],dim=3),"Capacity Additions|Electricity|Gas CC|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("ngt")],dim=3),"Capacity Additions|Electricity|Gas OC|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("ngt")],dim=3),"Capacity Additions|Electricity|Gas OC (GW/yr)"))
  
  #Hydrogen
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("hfc")],dim=3),"Capacity Additions|Electricity|Hydrogen FC (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("hct")],dim=3),"Capacity Additions|Electricity|Hydrogen OC (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("hcc")],dim=3),"Capacity Additions|Electricity|Hydrogen CC (GW/yr)"))

  #Renewables
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("spv")],dim=3),"Capacity Additions|Electricity|Solar|PV (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("csp")],dim=3),"Capacity Additions|Electricity|Solar|CSP (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("windoff")],dim=3),"Capacity Additions|Electricity|Wind|Offshore (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("windon")],dim=3),"Capacity Additions|Electricity|Wind|Onshore (GW/yr)"))
  
  #general aggregation
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c(ter)],dim=3),"Capacity Additions|Electricity|Variable renewable (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c(ternofluc)],dim=3),"Capacity Additions|Electricity|Non-variable renewable (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c(ter,ternofluc)],dim=3),"Capacity Additions|Electricity|Renewable (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c(tefossil)],dim=3),"Capacity Additions|Electricity|Fossil (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,tenr)],dim=3),"Capacity Additions|Electricity|Non-renewable (GW/yr)")) #this does not include storage
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("psp","batteries","helec")],dim=3),"Capacity Additions|Electricity|Storage (GW/yr)"))
  
  #Storage
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("psp")],dim=3),"Capacity Additions|Electricity|Storage|Pump Hydro (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("batteries")],dim=3),"Capacity Additions|Electricity|Storage|Stat Batteries (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("helec")],dim=3),"Capacity Additions|Electricity|Storage|Hydrogen electrolysis (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c("psp","batteries")],dim=3),"Capacity Additions|Electricity|Storage|Intra-day (GW/yr)"))
  
  #combine aggregated capacity with brake-down of technologies
  tmp3 <- mbind(tmp1,tmp2)
  
  # append global values to the national ones
  tmp4 <- NULL
  tmp4 <- setNames(dimSums(v_deltacap,dim=3),"Capacity Additions|Electricity (GW/yr)")
  
  #combine aggregated capacity with brake-down of technologies
  tmp <- mbind(tmp3,tmp4)

  return(tmp)
}
  
