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
  t <- readGDX(gdx,name="t")
  te <- readGDX(gdx,name="te") 
  tehe <- readGDX(gdx,name="tehe")
  tecoal <- readGDX(gdx,name="tecoal") 
  telig <- readGDX(gdx,name="telig") 
  tegas <- readGDX(gdx,name="tegas") 
  tengcc <- readGDX(gdx,name="tengcc")
  tehgen <- readGDX(gdx,name="tehgen")
  tehydro <- readGDX(gdx,name="tehydro")
  tebio <- readGDX(gdx,name="tebio")
  teoil <- readGDX(gdx,name="teoil")
  techp <- readGDX(gdx,name="techp")
  teccs <- readGDX(gdx,name="teccs")
  testore <- readGDX(gdx,name="testore")
  teothers <- readGDX(gdx,name="teothers")
  tereserve <- readGDX(gdx,name="tereserve")
  tegas_el <- intersect(tegas,teel)
  tengcc_el <- intersect(tengcc,teel)

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
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(tehydro))],dim=3),"Capacity Additions|Electricity|Hydro (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(telig,tecoal))],dim=3),"Capacity Additions|Electricity|Coal (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(tecoal))],dim=3),"Capacity Additions|Electricity|Hard Coal (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(telig))],dim=3),"Capacity Additions|Electricity|Lignite (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(tegas))],dim=3),"Capacity Additions|Electricity|Gas (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(tebio))],dim=3),"Capacity Additions|Electricity|Biomass (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,setdiff(tebio,teccs))],dim=3),"Capacity Additions|Electricity|Biomass|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(teoil))],dim=3),"Capacity Additions|Electricity|Oil (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,setdiff(teoil,teccs))],dim=3),"Capacity Additions|Electricity|Oil|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c("waste"))],dim=3),"Capacity Additions|Electricity|Waste (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,tehgen)],dim=3),"Capacity Additions|Electricity|Hydrogen (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(teothers))],dim=3),"Capacity Additions|Electricity|Other (GW/yr)"))

  #aggregated coal
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c(setdiff(telig,teccs),setdiff(tecoal,teccs))],dim=3),"Capacity Additions|Electricity|Coal|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(c(tecoal,telig),teccs)],dim=3),"Capacity Additions|Electricity|Coal|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,setdiff(tecoal,teccs)],dim=3),"Capacity Additions|Electricity|Hard Coal|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(tecoal,teccs)],dim=3),"Capacity Additions|Electricity|Hard Coal|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,setdiff(telig,teccs)],dim=3),"Capacity Additions|Electricity|Lignite|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(telig,teccs)],dim=3),"Capacity Additions|Electricity|Lignite|w/ CCS (GW/yr)"))

  #aggregated gas
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c(tengcc_el)],dim=3),"Capacity Additions|Electricity|Gas CC (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,setdiff(tengcc_el,teccs)],dim=3),"Capacity Additions|Electricity|Gas CC|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,setdiff(tegas_el,teccs)],dim=3),"Capacity Additions|Electricity|Gas|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(tegas_el,teccs)],dim=3),"Capacity Additions|Electricity|Gas|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(tengcc_el,teccs)],dim=3),"Capacity Additions|Electricity|Gas CC|w/ CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,setdiff(setdiff(tegas_el,tengcc_el),tengcc_el)],dim=3),"Capacity Additions|Electricity|Gas OC|w/o CCS (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,setdiff(tegas_el,tengcc_el)],dim=3),"Capacity Additions|Electricity|Gas OC (GW/yr)"))
  
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
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(ter))],dim=3),"Capacity Additions|Electricity|Variable renewable (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(ternofluc))],dim=3),"Capacity Additions|Electricity|Non-variable renewable (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(ter,ternofluc))],dim=3),"Capacity Additions|Electricity|Renewable (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,c(tefossil))],dim=3),"Capacity Additions|Electricity|Fossil (GW/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(teel,tenr)],dim=3),"Capacity Additions|Electricity|Non-renewable (GW/yr)")) #this does not include storage
  tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,c(testore)],dim=3),"Capacity Additions|Electricity|Storage (GW/yr)"))
  
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
  
