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
  teel <- readGDX(gdx,name="teel")
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
  tefossil <- readGDX(gdx,name="tefossil")
  ter <- readGDX(gdx,name="ter")
  ternofluc <- readGDX(gdx,name="ternofluc")
  tenr <- readGDX(gdx,name="tenr")
  tegas_el <- intersect(tegas,teel)
  tengcc_el <- intersect(tengcc,teel)
  
  # Read parameters
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")

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
  
  #aggregated technologies (w/o CHP) - depending on LIMES version, they will be considered
  tmp2 <- NULL
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c("tnr"))],dim=3),"Capacity|Electricity|Nuclear (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c("spv","csp"))],dim=3),"Capacity|Electricity|Solar (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c("windon","windoff"))],dim=3),"Capacity|Electricity|Wind (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(tehydro))],dim=3),"Capacity|Electricity|Hydro (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c("waste"))],dim=3),"Capacity|Electricity|Waste (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,tehgen)],dim=3),"Capacity|Electricity|Hydrogen (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(telig,tecoal))],dim=3),"Capacity|Electricity|Coal (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(tecoal))],dim=3),"Capacity|Electricity|Hard Coal (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(telig))],dim=3),"Capacity|Electricity|Lignite (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(tegas_el))],dim=3),"Capacity|Electricity|Gas (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(tebio))],dim=3),"Capacity|Electricity|Biomass (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,setdiff(tebio,teccs))],dim=3),"Capacity|Electricity|Biomass|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c(testore)],dim=3),"Capacity|Electricity|Storage (GW)")) 
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(teoil))],dim=3),"Capacity|Electricity|Oil (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,setdiff(teoil,teccs))],dim=3),"Capacity|Electricity|Oil|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(teothers))],dim=3),"Capacity|Electricity|Other (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(teoil,teothers,"waste"))],dim=3),"Capacity|Electricity|Other Fossil (GW)"))
  
  #when there is exogenous heating
  if(c_LIMESversion >= 2.33) {
    tewaste <- readGDX(gdx,name="tewaste") #set of waste generation technologies
    tedh <- readGDX(gdx,name="tedh") #set of District Heating generation technologies
    tedhelec <- readGDX(gdx,name="tedhelec") #set of electric District Heating generation technologies
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    if(c_heating == 1) {
      #CHP
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c(techp)],dim=3),"Capacity|Electricity|CHP (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(c(tecoal,telig),techp)],dim=3),"Capacity|Electricity|CHP|Coal (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tecoal,techp)],dim=3),"Capacity|Electricity|CHP|Hard Coal (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(telig,techp)],dim=3),"Capacity|Electricity|CHP|Lignite (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tegas,techp)],dim=3),"Capacity|Electricity|CHP|Gas (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tengcc,techp)],dim=3),"Capacity|Electricity|CHP|Gas CC (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(techp,setdiff(tegas,tengcc))],dim=3),"Capacity|Electricity|CHP|Gas OC (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tebio,techp)],dim=3),"Capacity|Electricity|CHP|Biomass (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teoil,techp)],dim=3),"Capacity|Electricity|CHP|Oil (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teothers,techp)],dim=3),"Capacity|Electricity|CHP|Other (GW)"))
      
      #Electricity-only
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,setdiff(teel,techp)],dim=3),"Capacity|Electricity|Electricity-only (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),c(telig,tecoal))],dim=3),"Capacity|Electricity|Electricity-only|Coal (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),tecoal)],dim=3),"Capacity|Electricity|Electricity-only|Hard Coal (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),telig)],dim=3),"Capacity|Electricity|Electricity-only|Lignite (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),tegas)],dim=3),"Capacity|Electricity|Electricity-only|Gas (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),tengcc)],dim=3),"Capacity|Electricity|Electricity-only|Gas CC (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),setdiff(tegas,tengcc))],dim=3),"Capacity|Electricity|Electricity-only|Gas OC (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),tebio)],dim=3),"Capacity|Electricity|Electricity-only|Biomass (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),teoil)],dim=3),"Capacity|Electricity|Electricity-only|Oil (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),teothers)],dim=3),"Capacity|Electricity|Electricity-only|Other (GW)"))
      
      #District Heating
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c(tedh)],dim=3),"Capacity|Heat|District Heating (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(c(tecoal,telig),tedh)],dim=3),"Capacity|Heat|District Heating|Coal (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tecoal,tedh)],dim=3),"Capacity|Heat|District Heating|Hard Coal (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(telig,tedh)],dim=3),"Capacity|Heat|District Heating|Lignite (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tegas,tedh)],dim=3),"Capacity|Heat|District Heating|Gas (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tebio,tedh)],dim=3),"Capacity|Heat|District Heating|Biomass (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teoil,tedh)],dim=3),"Capacity|Heat|District Heating|Oil (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teothers,tedh)],dim=3),"Capacity|Heat|District Heating|Other (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tewaste,tedh)],dim=3),"Capacity|Heat|District Heating|Waste (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect("sol_heat",tedh)],dim=3),"Capacity|Heat|District Heating|Solar (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect("geo_heat",tedh)],dim=3),"Capacity|Heat|District Heating|Geothermal (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tedhelec,tedh)],dim=3),"Capacity|Heat|District Heating|Electricity (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect("hpump",tedh)],dim=3),"Capacity|Heat|District Heating|Electricity|Heat Pump (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect("elboil",tedh)],dim=3),"Capacity|Heat|District Heating|Electricity|Electric Boiler (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tefossil,tedh)],dim=3),"Capacity|Heat|District Heating|Fossil (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tenr,tedh)],dim=3),"Capacity|Heat|District Heating|Non-renewable (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(c(ter,ternofluc),tedh)],dim=3),"Capacity|Heat|District Heating|Renewable (GW)"))
    }
    
    #Biomass w/ CCS
    tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tebio,teccs)],dim=3),"Capacity|Electricity|Biomass|w/ CCS (GW)"))
  }
  
  #aggregated coal
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c(setdiff(telig,teccs),setdiff(tecoal,teccs))],dim=3),"Capacity|Electricity|Coal|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(c(tecoal,telig),teccs)],dim=3),"Capacity|Electricity|Coal|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,setdiff(tecoal,teccs)],dim=3),"Capacity|Electricity|Hard Coal|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tecoal,teccs)],dim=3),"Capacity|Electricity|Hard Coal|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,setdiff(telig,teccs)],dim=3),"Capacity|Electricity|Lignite|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(telig,teccs)],dim=3),"Capacity|Electricity|Lignite|w/ CCS (GW)"))

  #aggregated gas
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c(tengcc_el)],dim=3),"Capacity|Electricity|Gas CC (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,setdiff(tengcc_el,teccs)],dim=3),"Capacity|Electricity|Gas CC|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,setdiff(tegas_el,teccs)],dim=3),"Capacity|Electricity|Gas|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tegas_el,teccs)],dim=3),"Capacity|Electricity|Gas|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(tengcc_el,teccs)],dim=3),"Capacity|Electricity|Gas CC|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,setdiff(setdiff(tegas_el,tengcc_el),tengcc_el)],dim=3),"Capacity|Electricity|Gas OC|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,setdiff(tegas_el,tengcc_el)],dim=3),"Capacity|Electricity|Gas OC (GW)"))
  
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
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c(tegas_el)],dim=3),"Capacity|Electricity|Reserve Plants|Gas (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c(tengcc)],dim=3),"Capacity|Electricity|Reserve Plants|Gas CC (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c("ngt")],dim=3),"Capacity|Electricity|Reserve Plants|Gas OC (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c(tebio)],dim=3),"Capacity|Electricity|Reserve Plants|Biomass (GW)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c(teoil)],dim=3),"Capacity|Electricity|Reserve Plants|Oil (GW)"))
  
  #when there is exogenous heating
  if(c_LIMESversion >= 2.33) {
    if(c_heating == 1) {
      #CHP
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c(techp)],dim=3),"Capacity|Electricity|Reserve Plants|CHP (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,intersect(c(tecoal,telig),techp)],dim=3),"Capacity|Electricity|Reserve Plants|CHP|Coal (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,intersect(tecoal,techp)],dim=3),"Capacity|Electricity|Reserve Plants|CHP|Hard Coal (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,intersect(telig,techp)],dim=3),"Capacity|Electricity|Reserve Plants|CHP|Lignite (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,intersect(tegas_el,techp)],dim=3),"Capacity|Electricity|Reserve Plants|CHP|Gas (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,intersect(tengcc_el,techp)],dim=3),"Capacity|Electricity|Reserve Plants|CHP|Gas CC (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,c("ngt_chp")],dim=3),"Capacity|Electricity|Reserve Plants|CHP|Gas OC (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,intersect(tebio,techp)],dim=3),"Capacity|Electricity|Reserve Plants|CHP|Biomass (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,intersect(teoil,techp)],dim=3),"Capacity|Electricity|Reserve Plants|CHP|Oil (GW)"))
      #Electricity-only
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,setdiff(tereserve,techp)],dim=3),"Capacity|Electricity|Reserve Plants|Electricity-only (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,setdiff(c(tecoal,telig),techp)],dim=3),"Capacity|Electricity|Reserve Plants|Electricity-only|Coal (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,setdiff(tecoal,techp)],dim=3),"Capacity|Electricity|Reserve Plants|Electricity-only|Hard Coal (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,setdiff(telig,techp)],dim=3),"Capacity|Electricity|Reserve Plants|Electricity-only|Lignite (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,setdiff(tegas_el,techp)],dim=3),"Capacity|Electricity|Reserve Plants|Electricity-only|Gas (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,setdiff(tengcc_el,techp)],dim=3),"Capacity|Electricity|Reserve Plants|Electricity-only|Gas CC (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,setdiff(tegas_el,tengcc_el)],dim=3),"Capacity|Electricity|Reserve Plants|Electricity-only|Gas OC (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,setdiff(tebio,techp)],dim=3),"Capacity|Electricity|Reserve Plants|Electricity-only|Biomass (GW)"))
      tmp6 <- mbind(tmp6,setNames(dimSums(v_capreserve[,,setdiff(teoil,techp)],dim=3),"Capacity|Electricity|Reserve Plants|Electricity-only|Oil (GW)"))
    }
  }
  
  tmp7 <- mbind(tmp5[,as.numeric(c(t)),],tmp6)
  
  #Energy storage (reservoir) capacity
  tmp8 <- NULL
  if(c_LIMESversion >= 2.34) {
    v_storecap <- readGDX(gdx,name="v_storecap",field="l",format="first_found")[,,testore]
    v_storecap <- limesMapping(v_storecap)
    tmp8 <- mbind(tmp8,setNames(dimSums(v_storecap[,,],dim=3),"Capacity|Electricity|Storage Reservoir (GWh)"))
    tmp8 <- mbind(tmp8,setNames(dimSums(v_storecap[,,c("psp")],dim=3),"Capacity|Electricity|Storage Reservoir|Pump Hydro (GWh)"))
    tmp8 <- mbind(tmp8,setNames(dimSums(v_storecap[,,c("batteries")],dim=3),"Capacity|Electricity|Storage Reservoir|Stat Batteries (GWh)"))
    tmp8 <- mbind(tmp8,setNames(dimSums(v_storecap[,,c("helec")],dim=3),"Capacity|Electricity|Storage Reservoir|Hydrogen electrolysis (GWh)"))
    tmp8 <- mbind(tmp8,setNames(dimSums(v_storecap[,,c("psp","batteries")],dim=3),"Capacity|Electricity|Storage Reservoir|Intra-day (GWh)"))
  }
  
  #combine aggregated capacity with brake-down of technologies
  tmp <- mbind(tmp7,tmp8[,as.numeric(c(t)),])

  return(tmp)
}