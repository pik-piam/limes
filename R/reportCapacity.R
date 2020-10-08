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
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(teothers))],dim=3),"Capacity|Electricity|Other (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(teoil,teothers,"waste"))],dim=3),"Capacity|Electricity|Other Fossil (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(teel,c(ter))],dim=3),"Capacity|Electricity|Variable renewable (GW)"))
  
  #when there is exogenous heating
  if(c_LIMESversion >= 2.33) {
    tewaste <- readGDX(gdx,name="tewaste") #set of waste generation technologies
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    
    if(c_heating == 1) {
      #load some required sets
      tedh <- readGDX(gdx,name="tedh") #set of District Heating generation technologies
      tedhelec <- readGDX(gdx,name="tedhelec") #set of electric District Heating generation technologies
      teohecen <- readGDX(gdx,name="teohecen") #set of centralized only-heating generation technologies
      tehedec <- readGDX(gdx,name="tehedec") #set of decentralized only-heating generation technologies
      
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
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),intersect(c(telig,tecoal),teccs))],dim=3),"Capacity|Electricity|Electricity-only|Coal|w/ CCS (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),setdiff(c(telig,tecoal),teccs))],dim=3),"Capacity|Electricity|Electricity-only|Coal|w/o CCS (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),tecoal)],dim=3),"Capacity|Electricity|Electricity-only|Hard Coal (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),intersect(tecoal,teccs))],dim=3),"Capacity|Electricity|Electricity-only|Hard Coal|w/ CCS (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),setdiff(tecoal,teccs))],dim=3),"Capacity|Electricity|Electricity-only|Hard Coal|w/o CCS (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),telig)],dim=3),"Capacity|Electricity|Electricity-only|Lignite (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),intersect(telig,teccs))],dim=3),"Capacity|Electricity|Electricity-only|Lignite|w/ CCS (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),setdiff(telig,teccs))],dim=3),"Capacity|Electricity|Electricity-only|Lignite|w/o CCS (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),tegas)],dim=3),"Capacity|Electricity|Electricity-only|Gas (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),intersect(tegas,teccs))],dim=3),"Capacity|Electricity|Electricity-only|Gas|w/ CCS (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),setdiff(tegas,teccs))],dim=3),"Capacity|Electricity|Electricity-only|Gas|w/o CCS (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),tengcc)],dim=3),"Capacity|Electricity|Electricity-only|Gas CC (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),setdiff(tegas,tengcc))],dim=3),"Capacity|Electricity|Electricity-only|Gas OC (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),tebio)],dim=3),"Capacity|Electricity|Electricity-only|Biomass (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),intersect(tebio,teccs))],dim=3),"Capacity|Electricity|Electricity-only|Biomass|w/ CCS (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),setdiff(tebio,teccs))],dim=3),"Capacity|Electricity|Electricity-only|Biomass|w/o CCS (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),teoil)],dim=3),"Capacity|Electricity|Electricity-only|Oil (GW)"))
      tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,intersect(setdiff(teel,techp),teothers)],dim=3),"Capacity|Electricity|Electricity-only|Other (GW)"))
      
      #Heat-related capacities
      varList_he <- list(
        #1.a) Only-heat (centralized boilers)
        "Capacity|Heat|District Heating|Heat-only (TWh/yr)"                               =c(teohecen),
        "Capacity|Heat|District Heating|Heat-only|Biomass (TWh/yr)"                       =intersect(teohecen,tebio),
        "Capacity|Heat|District Heating|Heat-only|Coal (TWh/yr)"                          =intersect(teohecen,c(tecoal,telig)),
        "Capacity|Heat|District Heating|Heat-only|Hard Coal (TWh/yr)"                     =intersect(teohecen,c(tecoal)),
        "Capacity|Heat|District Heating|Heat-only|Lignite (TWh/yr)"                       =intersect(teohecen,c(telig)),
        "Capacity|Heat|District Heating|Heat-only|Oil (TWh/yr)"                           =intersect(teohecen,c(teoil)),
        "Capacity|Heat|District Heating|Heat-only|Gas (TWh/yr)"                           =intersect(teohecen,c(tegas)),
        "Capacity|Heat|District Heating|Heat-only|Other (TWh/yr)"                         =intersect(teohecen,c(teothers)),
        "Capacity|Heat|District Heating|Heat-only|Waste (TWh/yr)"                         =intersect(teohecen,c(tewaste)),
        "Capacity|Heat|District Heating|Heat-only|Other Fossil (TWh/yr)"                  =intersect(teohecen,c(teothers,tewaste,teoil)),
        "Capacity|Heat|District Heating|Heat-only|Electricity (TWh/yr)"                   =intersect(teohecen,c(tedhelec)),
        "Capacity|Heat|District Heating|Heat-only|Electricity|Heat Pump (TWh/yr)"         =intersect(teohecen,"hp_large"),
        "Capacity|Heat|District Heating|Heat-only|Electricity|Electric Boiler (TWh/yr)"   =intersect(teohecen,"elboil_large"),
        "Capacity|Heat|District Heating|Heat-only|Solar (TWh/yr)"                         =intersect(teohecen,c("sol_heat")),
        "Capacity|Heat|District Heating|Heat-only|Geothermal (TWh/yr)"                    =intersect(teohecen,c("geo_heat")),
        "Capacity|Heat|District Heating|Heat-only|Fossil (TWh/yr)"                        =intersect(teohecen,c(tefossil)),
        "Capacity|Heat|District Heating|Heat-only|Renewable (TWh/yr)"                     =intersect(teohecen,c(ter,ternofluc)),
        "Capacity|Heat|District Heating|Heat-only|Non-renewable (TWh/yr)"                 =intersect(teohecen,tenr),
        
        #1.b) District Heating
        "Capacity|Heat|District Heating (TWh/yr)"                             =c(tedh),
        "Capacity|Heat|District Heating|Biomass (TWh/yr)"                     =intersect(tedh,tebio),
        "Capacity|Heat|District Heating|Coal (TWh/yr)"                        =intersect(tedh,c(tecoal,telig)),
        "Capacity|Heat|District Heating|Hard Coal (TWh/yr)"                   =intersect(tedh,c(tecoal)),
        "Capacity|Heat|District Heating|Lignite (TWh/yr)"                     =intersect(tedh,c(telig)),
        "Capacity|Heat|District Heating|Oil (TWh/yr)"                         =intersect(tedh,c(teoil)),
        "Capacity|Heat|District Heating|Gas (TWh/yr)"                         =intersect(tedh,c(tegas)),
        "Capacity|Heat|District Heating|Other (TWh/yr)"                       =intersect(tedh,c(teothers)),
        "Capacity|Heat|District Heating|Waste (TWh/yr)"                       =intersect(tedh,c(tewaste)),
        "Capacity|Heat|District Heating|Other Fossil (TWh/yr)"                =intersect(tedh,c(teothers,tewaste,teoil)),
        "Capacity|Heat|District Heating|Electricity (TWh/yr)"                 =intersect(tedh,c(tedhelec)),
        "Capacity|Heat|District Heating|Electricity|Heat Pump (TWh/yr)"       =intersect(tedh,"hp_large"),
        "Capacity|Heat|District Heating|Electricity|Electric Boiler (TWh/yr)" =intersect(tedh,"elboil_large"),
        "Capacity|Heat|District Heating|Solar (TWh/yr)"                       =intersect(tedh,c("sol_heat")),
        "Capacity|Heat|District Heating|Geothermal (TWh/yr)"                  =intersect(tedh,c("geo_heat")),
        "Capacity|Heat|District Heating|Fossil (TWh/yr)"                      =intersect(tedh,c(tefossil)),
        "Capacity|Heat|District Heating|Renewable (TWh/yr)"                   =intersect(tedh,c(ter,ternofluc)),
        "Capacity|Heat|District Heating|Non-renewable (TWh/yr)"               =intersect(tedh,tenr)
        
      )
      
      for (var in names(varList_he)){
        tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,varList_he[[var]]],dim=3),var))
      }
      
      c_buildings <- readGDX(gdx,name="c_buildings",field="l",format="first_found") #switch on buildings module
      if(c_buildings == 1) {
        varList_he <- list(
          #1.c) Decentralized heating (only electricity-based)
          "Capacity|Heat|Decentralized (TWh/yr)"                             =c(tehedec),
          "Capacity|Heat|Decentralized|Heat Pump (TWh/yr)"                   =intersect(tehedec,c("hp_sh_dec","hp_wh_dec")),
          "Capacity|Heat|Decentralized|Resistive electric heater (TWh/yr)"   =intersect(tehedec,"resheat_dec"),
          "Capacity|Heat|Decentralized|Conventional heater (TWh/yr)"         =intersect(tehedec,"convheat_dec"),
          "Capacity|Heat|Decentralized|Conventional water heater (TWh/yr)"   =intersect(tehedec,"convwh_dec")
        )
        
        for (var in names(varList_he)){
          tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,varList_he[[var]]],dim=3),var))
        }
        
      }
      
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
  tmp2 <- mbind(tmp2,setNames(dimSums(v_cap[,,c("helec")],dim=3),"Capacity|Electricity|Storage|Hydrogen electrolysis [input] (GW)"))
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
  
  #Energy storage (reservoir) capacity and ratios
  tmp8 <- NULL
  if(c_LIMESversion >= 2.34) {
    v_storecap <- readGDX(gdx,name="v_storecap",field="l",format="first_found")[,,testore]
    v_storecap <- limesMapping(v_storecap)
    
    varList_st <- list(
      "Capacity|Electricity|Storage Reservoir (GWh)"                       =setdiff(testore,c("heat_sto")),
      "Capacity|Electricity|Storage Reservoir|Intra-day (GWh)"             ="psp",
      "Capacity|Electricity|Storage Reservoir|Pump Hydro (GWh)"            ="psp",           
      "Capacity|Electricity|Storage Reservoir|Stat Batteries (GWh)"        ="batteries",               
      "Capacity|Electricity|Storage Reservoir|Hydrogen electrolysis (GWh)" ="helec"
    )
    
    for (var in names(varList_st)){
      tmp8 <- mbind(tmp8,setNames(dimSums(v_storecap[,,varList_st[[var]]],dim=3),var))
    }
    
    #tmp8 <- mbind(tmp8,setNames(dimSums(v_storecap[,,setdiff(testore,c("heat_sto"))],dim=3),"Capacity|Electricity|Storage Reservoir (GWh)"))
    #tmp8 <- mbind(tmp8,setNames(dimSums(v_storecap[,,c("psp","batteries")],dim=3),"Capacity|Electricity|Storage Reservoir|Intra-day (GWh)"))
    #tmp8 <- mbind(tmp8,setNames(v_storecap[,,c("psp")],"Capacity|Electricity|Storage Reservoir|Pump Hydro (GWh)"))
    #tmp8 <- mbind(tmp8,setNames(v_storecap[,,c("batteries")],"Capacity|Electricity|Storage Reservoir|Stat Batteries (GWh)"))
    #tmp8 <- mbind(tmp8,setNames(v_storecap[,,c("helec")],"Capacity|Electricity|Storage Reservoir|Hydrogen electrolysis (GWh)"))
    
    
    #Number of storing hours
    tmp8 <- mbind(tmp8,setNames(v_storecap[,,c("psp")]/v_cap[,,c("psp")],"Discharge duration|Pump Hydro (h)"))
    tmp8 <- mbind(tmp8,setNames(v_storecap[,,c("batteries")]/v_cap[,,c("batteries")],"Discharge duration|Stat Batteries (h)"))
    tmp8 <- mbind(tmp8,setNames(v_storecap[,,c("helec")]/v_cap[,,c("helec")],"Discharge duration|Hydrogen electrolysis (h)"))
    
    if(c_heating == 1) {
      tmp8 <- mbind(tmp8,setNames(dimSums(v_storecap[,,c("heat_sto")],dim=3),"Capacity|Heat|Storage Reservoir (GWh)"))
    }
    
  }
  
  #combine aggregated capacity with brake-down of technologies
  tmp <- mbind(tmp7,tmp8[,as.numeric(c(t)),])

  return(tmp)
}