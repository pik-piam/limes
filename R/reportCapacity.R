#' Read in GDX and calculate capacities, used in convGDX2MIF.R for the reporting
#' 
#' Read in capacity information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportCapacity(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#'
#' @export

reportCapacity <- function(gdx) {
  
  # read sets
  t <- readGDX(gdx,name="t")
  te <- readGDX(gdx,name="te") 
  tecoal <- readGDX(gdx,name="tecoal") 
  telig <- readGDX(gdx,name="telig") 
  tegas <- readGDX(gdx,name="tegas") 
  tengcc <- readGDX(gdx,name="tengcc") 

  # read variables
  v_cap <- readGDX(gdx,name="v_cap",field="l",format="first_found")
  v_capreserve <- readGDX(gdx,name="v_capreserve",field="l",format="first_found")
  
  # create MagPie object of v_cap with iso3 regions
  v_cap <- limesMapping(v_cap)
  v_capreserve <- limesMapping(v_capreserve)
  
  # total installed capacity
  tmp1 <- NULL
  #for (tech in te) {
  #  tmp1 <- mbind(tmp1,setNames(v_cap[,,tech],paste("Capacity|Electricity|",tech,"(GW)")))
  #}
  
  #aggregated technologies
  tmp2 <- NULL
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("tnr")],dim=3),"Capacity|Electricity|Nuclear (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("spv","csp")],dim=3),"Capacity|Electricity|Solar (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("windon","windoff")],dim=3),"Capacity|Electricity|Wind (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("hydro","ror","hs")],dim=3),"Capacity|Electricity|Hydro (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("pc","pc80","pc95","pc10","pcc","lpc","lpc80","lpc95","lpc10","lpcc")],dim=3),"Capacity|Electricity|Coal (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("pc","pc80","pc95","pc10","pcc")],dim=3),"Capacity|Electricity|Hard Coal (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("lpc","lpc80","lpc95","lpc10","lpcc")],dim=3),"Capacity|Electricity|Lignite (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("ngcc","ngcc80","ngcc95","ngcc10","ngt","ngccc")],dim=3),"Capacity|Electricity|Gas (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("biolcigcc")],dim=3),"Capacity|Electricity|Biomass (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("biolcigcc")],dim=3),"Capacity|Electricity|Biomass|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("psp","batteries","helec")],dim=3),"Capacity|Electricity|Storage (GW)")) 
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("oil")],dim=3),"Capacity|Electricity|Oil (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("oil")],dim=3),"Capacity|Electricity|Oil|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("waste")],dim=3),"Capacity|Electricity|Waste (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("hcc","hct","hfc")],dim=3),"Capacity|Electricity|Hydrogen (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("others")],dim=3),"Capacity|Electricity|Other (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("others","waste","oil")],dim=3),"Capacity|Electricity|Other Fossil (GW)"))

  #aggregated coal
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("pc","pc80","pc95","pc10","lpc","lpc80","lpc95","lpc10")],dim=3),"Capacity|Electricity|Coal|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("pcc","lpcc")],dim=3),"Capacity|Electricity|Coal|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("pc","pc80","pc95","pc10")],dim=3),"Capacity|Electricity|Hard Coal|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("pcc")],dim=3),"Capacity|Electricity|Hard Coal|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("lpc","lpc80","lpc95","lpc10")],dim=3),"Capacity|Electricity|Lignite|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("lpcc")],dim=3),"Capacity|Electricity|Lignite|w/ CCS (GW)"))

  #aggregated gas
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("ngcc","ngcc80","ngcc95","ngcc10","ngccc")],dim=3),"Capacity|Electricity|Gas CC (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("ngcc","ngcc80","ngcc95","ngcc10")],dim=3),"Capacity|Electricity|Gas CC|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("ngcc","ngcc80","ngcc95","ngcc10","ngt")],dim=3),"Capacity|Electricity|Gas|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("ngccc")],dim=3),"Capacity|Electricity|Gas|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("ngccc")],dim=3),"Capacity|Electricity|Gas CC|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("ngt")],dim=3),"Capacity|Electricity|Gas OC|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("ngt")],dim=3),"Capacity|Electricity|Gas OC (GW)"))
  
  #Hydrogen
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("hfc")],dim=3),"Capacity|Electricity|Hydrogen FC (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("hct")],dim=3),"Capacity|Electricity|Hydrogen OC (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("hcc")],dim=3),"Capacity|Electricity|Hydrogen CC (GW)"))
  
  #Renewables
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("spv")],dim=3),"Capacity|Electricity|Solar|PV (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("csp")],dim=3),"Capacity|Electricity|Solar|CSP (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("windoff")],dim=3),"Capacity|Electricity|Wind|Offshore (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("windon")],dim=3),"Capacity|Electricity|Wind|Onshore (GW)"))
  
  #Storage
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("psp")],dim=3),"Capacity|Electricity|Storage|Pump Hydro (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("batteries")],dim=3),"Capacity|Electricity|Storage|Stat Batteries (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("helec")],dim=3),"Capacity|Electricity|Storage|Hydrogen electrolysis (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("psp","batteries")],dim=3),"Capacity|Electricity|Storage|Intra-day (GW)"))
  
  #combine aggregated capacity with brake-down of technologies
  tmp3 <- mbind(tmp1,tmp2)
  
  # append global values to the national ones
  tmp4 <- setNames(dimSums(v_cap,dim=3),"Capacity|Electricity (GW)")
  
  #combine aggregated capacity with brake-down of technologies
  tmp5 <- mbind(tmp4,tmp3)
  
  #Reserves
  #These were also included in reportAdequacyContribution, but here they appear under a different name
  tmp6 <- NULL
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve,dim=3),"Capacity|Electricity|Reserve Plants (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c(tecoal)],dim=3),"Capacity|Electricity|Reserve Plants|Hard Coal (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c(telig)],dim=3),"Capacity|Electricity|Reserve Plants|Lignite (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c(tecoal,telig)],dim=3),"Capacity|Electricity|Reserve Plants|Coal (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c(tegas)],dim=3),"Capacity|Electricity|Reserve Plants|Gas (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c(tengcc)],dim=3),"Capacity|Electricity|Reserve Plants|Gas CC (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c("ngt")],dim=3),"Capacity|Electricity|Reserve Plants|Gas OC (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c("biolcigcc")],dim=3),"Capacity|Electricity|Reserve Plants|Biomass (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c("oil")],dim=3),"Capacity|Electricity|Reserve Plants|Oil (GW)"))
  
  
  #combine aggregated capacity with brake-down of technologies
  tmp <- mbind(tmp5[,as.numeric(c(t)),],tmp6)

  return(tmp)
}
  
