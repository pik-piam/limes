#' Read in GDX and calculate Disinvestment, used in convGDX2MIF.R for the reporting
#' 
#' Read in Disinvestment from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportCapacityAdditions(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#'

reportDisinvestments <- function(gdx) {
  
  # read sets
  t <- readGDX(gdx,name="t",field="l",format="first_found") #time set
  teel <- readGDX(gdx,name="teel") #set of electricity generation technologies (non-storage)
  ter <- readGDX(gdx,name="ter") #set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx,name="ternofluc") #set of non-variable (non-fluctuating) renewable electricity generation technologies
  tefossil <- readGDX(gdx,name="tefossil") #set of fossil-based electricity generation technologies
  tenr <- readGDX(gdx,name="tenr") #set of non-renewable electricity generation technologies (includes storage)

  # read variables
  v_disinvest <- readGDX(gdx,name="v_disinvest",field="l",format="first_found")
  
  # create MagPie object of v_disinvest with iso3 regions
  v_disinvest <- limesMapping(v_disinvest)
  
  #need to add the year 2010 to v_disinvest (depending on the scenario)
  if (length(getYears(v_disinvest)) < length(t)) {
    for (t2 in setdiff(t,getYears(v_disinvest))) {
      tmp<- v_disinvest[,1,]*0
      getYears(tmp)<- t2
      v_disinvest<-mbind(tmp,v_disinvest)
    }
  }
  
  
  # total disinvestments
  tmp1 <- NULL
  #for (tech in teel) {
  #  tmp1 <- mbind(tmp1,setNames(v_disinvest[,,tech],paste("Disinvestment|Electricity|",tech,"(GW/yr)")))
  #}
  
  #aggregated technologies
  tmp2 <- NULL
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("tnr")],dim=3),"Disinvestment|Electricity|Nuclear (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("spv","csp")],dim=3),"Disinvestment|Electricity|Solar (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("windon","windoff")],dim=3),"Disinvestment|Electricity|Wind (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("hydro","ror","hs")],dim=3),"Disinvestment|Electricity|Hydro (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("pc","pc80","pc95","pc10","pcc","lpc","lpc80","lpc95","lpc10","lpcc")],dim=3),"Disinvestment|Electricity|Coal (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("pc","pc80","pc95","pc10","pcc")],dim=3),"Disinvestment|Electricity|Hard Coal (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("lpc","lpc80","lpc95","lpc10","lpcc")],dim=3),"Disinvestment|Electricity|Lignite (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("ngcc","ngcc80","ngcc95","ngcc10","ngt","ngccc")],dim=3),"Disinvestment|Electricity|Gas (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("biolcigcc")],dim=3),"Disinvestment|Electricity|Biomass (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("biolcigcc")],dim=3),"Disinvestment|Electricity|Biomass|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("oil")],dim=3),"Disinvestment|Electricity|Oil|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("waste")],dim=3),"Disinvestment|Electricity|Waste (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("hcc","hct","hfc")],dim=3),"Disinvestment|Electricity|Hydrogen (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("others")],dim=3),"Disinvestment|Electricity|Other (GW/yr)"))
#
  #aggregated coal
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("pc","pc80","pc95","pc10","lpc","lpc80","lpc95","lpc10")],dim=3),"Disinvestment|Electricity|Coal|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("pcc","lpcc")],dim=3),"Disinvestment|Electricity|Coal|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("pc","pc80","pc95","pc10")],dim=3),"Disinvestment|Electricity|Hard Coal|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("pcc")],dim=3),"Disinvestment|Electricity|Hard Coal|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("lpc","lpc80","lpc95","lpc10")],dim=3),"Disinvestment|Electricity|Lignite|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("lpcc")],dim=3),"Disinvestment|Electricity|Lignite|w/ CCS (GW/yr)"))
#
  #aggregated gas
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("ngcc","ngcc80","ngcc95","ngcc10","ngccc")],dim=3),"Disinvestment|Electricity|Gas CC (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("ngcc","ngcc80","ngcc95","ngcc10")],dim=3),"Disinvestment|Electricity|Gas CC|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("ngcc","ngcc80","ngcc95","ngcc10","ngt")],dim=3),"Disinvestment|Electricity|Gas|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("ngccc")],dim=3),"Disinvestment|Electricity|Gas|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("ngccc")],dim=3),"Disinvestment|Electricity|Gas CC|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("ngt")],dim=3),"Disinvestment|Electricity|Gas OC|w/o CCS (GW/yr)"))
  
  #Hydrogen
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("hfc")],dim=3),"Disinvestment|Electricity|Hydrogen FC (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("hct")],dim=3),"Disinvestment|Electricity|Hydrogen OC (GW/yr)"))
  
  #Renewables
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("spv")],dim=3),"Disinvestment|Electricity|Solar|PV (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("csp")],dim=3),"Disinvestment|Electricity|Solar|CSP (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("windoff")],dim=3),"Disinvestment|Electricity|Wind|Offshore (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("windon")],dim=3),"Disinvestment|Electricity|Wind|Onshore (GW/yr)"))
  
  #Storage
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("psp","batteries","helec")],dim=3),"Disinvestment|Electricity|Storage (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("psp")],dim=3),"Disinvestment|Electricity|Storage|Pump Hydro (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("batteries")],dim=3),"Disinvestment|Electricity|Storage|Stat Batteries (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_disinvest[,,c("helec")],dim=3),"Disinvestment|Electricity|Storage|Hydrogen electrolysis (GW/yr)"))
  
  #combine aggregated capacity with brake-down of technologies
  tmp3 <- mbind(tmp1,tmp2)
  
  # append global values to the national ones
  tmp4 <- NULL
  tmp4 <- setNames(dimSums(v_disinvest,dim=3),"Disinvestment|Electricity (GW/yr)")
  
  #combine aggregated capacity with brake-down of technologies
  tmp <- mbind(tmp3,tmp4)

  return(tmp)
}
  
