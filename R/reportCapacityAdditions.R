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
  
  # Read parameters
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")

  # read variables
  v_deltacap <- readGDX(gdx,name="v_deltacap",field="l",format="first_found")
  
  # create MagPie object of v_deltacap with iso3 regions
  v_deltacap <- limesMapping(v_deltacap)
  
  # total installed capacity
  tmp1 <- NULL
  #for (tech in teel) {
  #  tmp1 <- mbind(tmp1,setNames(v_deltacap[,,tech],paste("Capacity Additions|Electricity|",tech,"(GW/yr)")))
  #}
  
  #aggregated technologies (w/o CHP) - depending on LIMES version, they will be considered
  varList_el <- list(
    #Conventional
    "Capacity Additions|Electricity (GW/yr)"                        =c(teel),
    "Capacity Additions|Electricity|Biomass (GW/yr)"                =intersect(teel,tebio),
    "Capacity Additions|Electricity|Biomass|w/o CCS (GW/yr)"        =intersect(teel,setdiff(tebio,teccs)),
    "Capacity Additions|Electricity|Coal (GW/yr)"                   =intersect(teel,c(tecoal,telig)),
    "Capacity Additions|Electricity|Coal|w/o CCS (GW/yr)"           =intersect(teel,setdiff(c(tecoal,telig),teccs)),
    "Capacity Additions|Electricity|Coal|w/ CCS (GW/yr)"            =intersect(teel,intersect(c(tecoal,telig),teccs)),
    "Capacity Additions|Electricity|Hard Coal (GW/yr)"              =intersect(teel,c(tecoal)),
    "Capacity Additions|Electricity|Hard Coal|w/o CCS (GW/yr)"      =intersect(teel,setdiff(c(tecoal),teccs)),
    "Capacity Additions|Electricity|Hard Coal|w/ CCS (GW/yr)"       =intersect(teel,intersect(c(tecoal),teccs)),
    "Capacity Additions|Electricity|Lignite (GW/yr)"                =intersect(teel,c(telig)),
    "Capacity Additions|Electricity|Lignite|w/o CCS (GW/yr)"        =intersect(teel,setdiff(c(telig),teccs)),
    "Capacity Additions|Electricity|Lignite|w/ CCS (GW/yr)"         =intersect(teel,intersect(c(telig),teccs)),
    "Capacity Additions|Electricity|Oil (GW/yr)"                    =intersect(teel,c(teoil)),
    "Capacity Additions|Electricity|Gas (GW/yr)"                    =intersect(teel,c(tegas)),
    "Capacity Additions|Electricity|Gas|w/o CCS (GW/yr)"            =intersect(teel,setdiff(tegas_el,teccs)),
    "Capacity Additions|Electricity|Gas|w/ CCS (GW/yr)"             =intersect(teel,intersect(tegas_el,teccs)),
    "Capacity Additions|Electricity|Gas CC|w/o CCS (GW/yr)"         =intersect(teel,setdiff(tengcc_el,teccs)),
    "Capacity Additions|Electricity|Gas CC|w/ CCS (GW/yr)"          =intersect(teel,intersect(tengcc_el,teccs)),
    "Capacity Additions|Electricity|Gas CC (GW/yr)"                 =intersect(teel,c(tengcc_el)),
    "Capacity Additions|Electricity|Gas OC (GW/yr)"                 =intersect(teel,setdiff(tegas_el,tengcc_el)),
    "Capacity Additions|Electricity|Other (GW/yr)"                  =intersect(teel,c(teothers)),
    "Capacity Additions|Electricity|Hydrogen (GW/yr)"               =intersect(teel,c(tehgen)),
    "Capacity Additions|Electricity|Hydrogen FC (GW/yr)"            =intersect(teel,c("hfc")),
    "Capacity Additions|Electricity|Hydrogen OC (GW/yr)"            =intersect(teel,c("hct")),
    "Capacity Additions|Electricity|Hydrogen CC (GW/yr)"            =intersect(teel,c("hcc")),
    "Capacity Additions|Electricity|Nuclear (GW/yr)"                =intersect(teel,c("tnr")),
    "Capacity Additions|Electricity|Waste (GW/yr)"                  =intersect(teel,c("waste")),
    "Capacity Additions|Electricity|Other Fossil (GW/yr)"           =intersect(teel,c(teothers,"waste",teoil)),
    
    #general aggregation
    "Capacity Additions|Electricity|Fossil (GW/yr)"                 =intersect(teel,c(tefossil)),
    "Capacity Additions|Electricity|Fossil|w/o CCS (GW/yr)"         =intersect(teel,setdiff(tefossil,teccs)),
    "Capacity Additions|Electricity|Fossil|w/ CCS (GW/yr)"          =intersect(teel,intersect(tefossil,teccs)),
    "Capacity Additions|Electricity|Variable renewable (GW/yr)"     =intersect(teel,c(ter)),
    "Capacity Additions|Electricity|Non-variable renewable (GW/yr)" =intersect(teel,c(ternofluc)),
    "Capacity Additions|Electricity|Renewable (GW/yr)"              =intersect(teel,c(ter,ternofluc)),
    "Capacity Additions|Electricity|Non-renewable (GW/yr)"          =intersect(teel,tenr), #this does not include storage
    
    #Renewable
    "Capacity Additions|Electricity|Wind (GW/yr)"                                          =intersect(teel,c("windon","windoff")),       
    "Capacity Additions|Electricity|Wind|Onshore (GW/yr)"                                  =intersect(teel,c("windon")),   
    "Capacity Additions|Electricity|Wind|Offshore (GW/yr)"                                 =intersect(teel,c("windoff")),      
    "Capacity Additions|Electricity|Solar (GW/yr)"                                         =intersect(teel,c("spv","csp")),
    "Capacity Additions|Electricity|Solar|PV (GW/yr)"                                      =intersect(teel,c("spv")), 
    "Capacity Additions|Electricity|Solar|CSP (GW/yr)"                                     =intersect(teel,c("csp")),  
    "Capacity Additions|Electricity|Hydro (GW/yr)"                                         =intersect(teel,c(tehydro)),
    #"Capacity Additions|Electricity|Hydro|Hydro storage (GW/yr)"        =intersect(teel,"hs"),
    #"Capacity Additions|Electricity|Hydro|Run of river (GW/yr)"        =intersect(teel,"ror")
    
    #Storage
    "Capacity Additions|Electricity|Storage (GW/yr)"                                       =setdiff(testore,c("heat_sto")),
    "Capacity Additions|Electricity|Storage|Stat Batteries (GW/yr)"                        =c("batteries"),
    "Capacity Additions|Electricity|Storage|Pump Hydro (GW/yr)"                            =c("psp"),
    "Capacity Additions|Electricity|Storage|Hydrogen electrolysis [input] (GW/yr)"         =c("helec"),
    "Capacity Additions|Electricity|Storage|Intra-day (GW/yr)"                             =c("batteries","psp")
  )
  
  tmp2 <- NULL
  for (var in names(varList_el)){
    tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,varList_el[[var]]],dim=3),var))
  }
  
  if(c_LIMESversion >= 2.33) {
    tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,intersect(tebio,teccs)],dim=3),"Capacity Additions|Electricity|Biomass|w/ CCS (GW/yr)"))
  }
  
  #concatenate
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
