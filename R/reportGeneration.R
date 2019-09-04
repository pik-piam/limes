#' Read in GDX and calculate electricity generation, used in convGDX2MIF.R for the reporting
#' 
#' Read in electricity generation data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the Generation variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportGeneration(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportGeneration <- function(gdx) {
  
  # read sets
  teel <- readGDX(gdx,name="teel") #set of electricity generation technologies (non-storage)
  ter <- readGDX(gdx,name="ter") #set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx,name="ternofluc") #set of non-variable (non-fluctuating) renewable electricity generation technologies
  tefossil <- readGDX(gdx,name="tefossil") #set of fossil-based electricity generation technologies
  tenr <- readGDX(gdx,name="tenr") #set of non-renewable electricity generation technologies (includes storage)
  tegas <- readGDX(gdx,name="tegas") #set of gas generation technologies
  telig <- readGDX(gdx,name="telig") #set of lignite generation technologies
  tecoal <- readGDX(gdx,name="tecoal") #set of hard coal generation technologies
  tengcc <- readGDX(gdx,name="tengcc") #set of NGCC generation technologies
  tau <- readGDX(gdx,name="tau") #set of time slices
  pety <- readGDX(gdx,name="pety") #set of primary energies
  
  # read parameters and sets
  p_taulength <- readGDX(gdx,name="p_taulength",field="l",format="first_found") #number of hours/year per tau
  p_tedata <- readGDX(gdx,name="p_tedata",field="l",format="first_found") #parameter per technology
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  
  # read variables
  v_seprod <- readGDX(gdx,name="v_seprod",field="l",format="first_found")[,,tau]
  v_storeout <- readGDX(gdx,name="v_storeout",field="l",format="first_found")[,,tau]
  v_storein <- readGDX(gdx,name="v_storein",field="l",format="first_found")[,,tau]
  v_exdemand <- readGDX(gdx,name="v_exdemand",field="l",format="first_found")[,,tau] #demand
  
  #Make sure only the "right" tau are taken -> to avoid info from gdx that might be stuck in the file
  #v_seprod <- v_seprod[,,tau]
  v_seprod <- v_seprod[,,pety]
  v_seprod <- v_seprod[,,"seel"]
  #p_taulength <- p_taulength[,,tau]
  #v_exdemand <- v_exdemand[,,tau]
  p_autocons <- p_tedata[,,"autocons"]

  # create MagPie object of variables with iso3 regions
  v_seprod <- limesMapping(v_seprod)
  v_storeout <- limesMapping(v_storeout)
  v_storein <- limesMapping(v_storein)
  v_exdemand <- limesMapping(v_exdemand)
  p_autocons <- limesMapping(p_autocons)
  
  #Check the version so to choose the electricity-related variables
  if(c_LIMESversion >= 2.28) {
    p_eldemand <- v_exdemand[,,"seel"]
  } else {
    p_eldemand <- v_exdemand
  }
  
  #generation per technology ('te') per country
  #and converting from GWh to TWh
  tmp1 <- NULL
  #for (teel2 in teel) {
  #  tmp1 <- mbind(tmp1,setNames(dimSums(v_seprod[,,teel2]*p_taulength/1000,3),paste("Secondary Energy|Electricity|",teel2,"(TWh/yr)")))
  #}
  
  #generation per aggregated technologies per country
  tmp2 <- NULL
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("tnr")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Nuclear (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(telig,tecoal)]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Coal (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(setdiff(telig,"lpcc"),setdiff(tecoal,"pcc"))]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Coal|w/o CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("pcc","lpcc")]*p_taulength/1000,3),"Secondary Energy|Electricity|Coal|w/ CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(telig)]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Lignite (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(setdiff(telig,"lpcc"))]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Lignite|w/o CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("lpcc")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Lignite|w/ CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(tecoal)]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Hard Coal (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(setdiff(tecoal,"pcc"))]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Hard Coal|w/o CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("pcc")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Hard Coal|w/ CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(tegas)]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Gas (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(setdiff(tegas,"ngccc"))]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Gas|w/o CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(setdiff(tengcc,"ngccc"))]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Gas CC|w/o CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(tengcc)]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Gas CC (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("ngccc")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Gas|w/ CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("ngccc")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Gas CC|w/ CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("ngt")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Gas OC|w/o CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("ngt")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Gas OC (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("oil")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Oil (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("oil")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Oil|w/o CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("windon","windoff")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Wind (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("windon")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Wind|Onshore (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("windoff")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Wind|Offshore (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("spv","csp")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Solar (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("spv")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Solar|PV (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("csp")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Solar|CSP (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("biolcigcc")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Biomass (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("biolcigcc")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Biomass|w/o CCS (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("hydro","ror","hs")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Hydro (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("waste")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Waste (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("hcc","hct","hfc")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Hydrogen (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("others")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Other (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("hfc")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Hydrogen FC (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("hct")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Hydrogen OC (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("hcc")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Hydrogen CC (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("others","waste","oil")]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Other Fossil (TWh/yr)"))
  
  #general aggregation
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(tefossil)]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Fossil (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(ter)]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Variable renewable (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(ternofluc)]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Non-variable renewable (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(ter,ternofluc)]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Renewable (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,intersect(teel,tenr)]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Non-renewable (TWh/yr)")) #this does not include storage
  tmp2 <- mbind(tmp2,setNames((dimSums(v_seprod[,,c(teel)]*p_taulength,dim=3)+dimSums(v_storeout*p_taulength,dim=3))/1000,"Secondary Energy|Electricity (TWh/yr)"))
  tmp2 <- mbind(tmp2,setNames((dimSums(v_seprod[,,c(teel)]*p_taulength,dim=3)+dimSums(v_storeout*p_taulength,dim=3)-dimSums(v_storein*p_taulength,dim=3))/1000,"Secondary Energy||Electricity|w/o losses (TWh/yr)"))

  # add global values
  tmp3 <- mbind(tmp1,tmp2)
  
  #Storage
  tmp4 <- NULL
  tmp4 <- mbind(tmp4,setNames(dimSums(v_storeout*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Storage (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_storeout[,,"psp"]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Storage|Pump Hydro (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_storeout[,,"batteries"]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Storage|Stat Batteries (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_storeout[,,"helec"]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Storage|Hydrogen electrolysis (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_storein*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Storage Consumption (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_storein[,,"psp"]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Storage Consumption|Pump Hydro (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_storein[,,"batteries"]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Storage Consumption|Stat Batteries (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_storein[,,"helec"]*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Storage Consumption|Hydrogen electrolysis (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums((v_storein-v_storeout)*p_taulength/1000,dim=3),"Secondary Energy|Electricity|Storage Losses (TWh/yr)"))
  
  tmp5 <- mbind(tmp3,tmp4)
  
  #Gross demand
  tmp6 <- NULL
  #Need to create a variable with t,regi,te for gross production (v_capreserve has these indexes)
  p_grossprod <- v_seprod*0
  #p_grossprod <- limesMapping(p_grossprod)
  for (teel2 in teel) {
    p_grossprod[,,teel2] <- v_seprod[,,teel2]*(1+p_autocons[,,teel2])
    tmp6 <- mbind(tmp6,setNames(dimSums(p_grossprod[,,teel2]*p_taulength/1000,dim=3),paste("Gross Production|Electricity|",teel2,"(TWh/yr)")))
  } 
  tmp6 <- mbind(tmp6,setNames(dimSums(p_grossprod[,,c(telig)]*p_taulength/1000,dim=3),"Gross Production|Electricity|Lignite (TWh/yr)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(p_grossprod[,,c(tecoal)]*p_taulength/1000,dim=3),"Gross Production|Electricity|Coal (TWh/yr)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(p_grossprod[,,c(tegas)]*p_taulength/1000,dim=3),"Gross Production|Electricity|Gas (TWh/yr)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(p_grossprod[,,c(tefossil)]*p_taulength/1000,dim=3),"Gross Production|Electricity|Fossil (TWh/yr)"))
  tmp6 <- mbind(tmp6,setNames(dimSums(p_grossprod[,,c("tnr")]*p_taulength/1000,dim=3),"Gross Production|Electricity|Nuclear (TWh/yr)"))
  
  #Net imports
  p_netimpots <- p_eldemand*0
  p_netimpots <- dimSums(p_eldemand*p_taulength/1000,dim=3) - dimSums(v_seprod*p_taulength/1000,dim=3) + dimSums((v_storein-v_storeout)*p_taulength/1000,dim=3)
  
  #Gross demand (gross electricity production + net imports), following official german statistics procedure
  p_grossdem <- dimSums(p_grossprod*p_taulength/1000,dim=3) + p_netimpots
  tmp6 <- mbind(tmp6,setNames(p_grossdem,"Secondary Energy|Electricity|Gross Demand (TWh/yr)"))
  
  #Shares
  tmp7 <- NULL
  tmp7 <- mbind(tmp7,setNames(dimSums(p_grossprod[,,c(ter,ternofluc)]*p_taulength/1000,dim=3)/p_grossdem,"Secondary Energy|Electricity|Share of renewables in gross demand (--)"))
  
  tmp8 <- mbind(tmp6,tmp7)
  
  # add global values
  tmp <- mbind(tmp5,tmp8)

  return(tmp)
}
  
