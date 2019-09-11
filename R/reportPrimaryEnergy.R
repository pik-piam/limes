#' Read in GDX and calculate primary energy, used in convGDX2MIF.R for the reporting
#' 
#' Read in primary energy data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportPrimaryEnergy(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportPrimaryEnergy <- function(gdx) {
  
  # read sets
  t <- readGDX(gdx,name="t")
  te <- readGDX(gdx,name="te") 
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
  tegas_el <- setdiff(tegas,"ngcc_heat")
  tengcc_el <- setdiff(tengcc,"ngcc_heat")
  petyex <- readGDX(gdx,name="petyex")
  pety <- readGDX(gdx,name="pety") #set of primary energies
  petyren <- readGDX(gdx,name="petyren") #set of primary energies
  tau <- readGDX(gdx,name="tau") #set of time slices
  pe2se <- readGDX(gdx,name="pe2se")
  pe2se <- paste0(pe2se[,1],".",pe2se[,2],".",pe2se[,3])
  
  # read parameters and variables
  p_taulength <- readGDX(gdx,name="p_taulength",field="l",format="first_found")[,,tau]
  v_pedem <- readGDX(gdx,name="v_pedem",field="l",format="first_found")[,,tau]
  v_seprod <- readGDX(gdx,name="v_seprod",field="l",format="first_found")[,,tau]
  
  #Make sure only the "right" sets are taken -> to avoid info from gdx that might be stuck in the file
  #v_seprod <- v_seprod[,,c(pe2se)]
  v_seprod <- v_seprod[,,c(petyren)]
  v_seprod <- v_seprod[,,c(ter,"hydro","ror","hs")]
  v_seprod <- v_seprod[,,"seel"]
  v_pedem <- v_pedem[,,petyex]
  
  # create MagPie object of v_pedem with iso3 regions
  v_pedem <- limesMapping(v_pedem)
  v_seprod <- limesMapping(v_seprod)

  #use of exhaustible primary energy types per country
  #and convert from GWh to TWh
  tmp1 <- NULL
  #for (petyex2 in petyex) {
  #  tmp1 <- mbind(tmp1,setNames(dimSums(v_pedem[,,petyex2]*p_taulength/1000,3),paste("Primary Energy|Electricity|",petyex2,"(TWh/yr)")))
  #}
  
  #aggregated use of primary energy types in all countries
  tmp2 <- NULL
  tmp2 <- setNames(dimSums(v_pedem[,,]*p_taulength/1000,3),paste("Primary Energy|Electricity [exhaustible resources] (TWh/yr)"))
  
  # add global values
  tmp3 <- mbind(tmp1,tmp2)
  
  #use of exhaustible primary energy types per country (aggregated per sources)
  tmp4 <- NULL
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,setdiff(tebio,"bio_ccs")]*p_taulength/1000,3),"Primary Energy|Electricity|Biomass w/o CCS (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c(tebio)]*p_taulength/1000,3),"Primary Energy|Electricity|Biomass (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,setdiff(c(telig,tecoal),teccs)]*p_taulength/1000,3),"Primary Energy|Electricity|Coal w/o CCS (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c("pcc","lpcc")]*p_taulength/1000,3),"Primary Energy|Electricity|Coal w/ CCS (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c(tecoal,telig)]*p_taulength/1000,3),"Primary Energy|Electricity|Coal (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c(tecoal)]*p_taulength/1000,3),"Primary Energy|Electricity|Hard Coal (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c(telig)]*p_taulength/1000,3),"Primary Energy|Electricity|Lignite (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,setdiff(teoil,"oil_ccs")]*p_taulength/1000,3),"Primary Energy|Electricity|Oil w/o CCS (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c(teoil)]*p_taulength/1000,3),"Primary Energy|Electricity|Oil (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c(tegas)]*p_taulength/1000,3),"Primary Energy|Electricity|Gas (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,setdiff(tegas_el,teccs)]*p_taulength/1000,3),"Primary Energy|Electricity|Gas w/o CCS (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c("ngccc")]*p_taulength/1000,3),"Primary Energy|Electricity|Gas w/ CCS (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c("pewaste")]*p_taulength/1000,3),"Primary Energy|Electricity|Waste (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c(tefossil)]*p_taulength/1000,3),"Primary Energy|Electricity|Fossil (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(((dimSums(v_pedem[,,c(tefossil)]*p_taulength,3)-dimSums(v_pedem[,,c(teccs)]*p_taulength,3))/1000),"Primary Energy|Electricity|Fossil w/o CCS (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c(teccs)]*p_taulength/1000,3),"Primary Energy|Electricity|Fossil w/ CCS (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c("tnr")]*p_taulength/1000,3),"Primary Energy|Electricity|Nuclear (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c("others","others_chp")]*p_taulength/1000,3),"Primary Energy|Electricity|Other (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_pedem[,,c(tehgen)]*p_taulength/1000,3),"Primary Energy|Electricity|Hydrogen (TWh/yr)"))
  
  #For these technologies, primary energy variable does not exist in LIMES, so it is calculated based on their generation
  tmp4 <- mbind(tmp4,setNames(dimSums(v_seprod[,,c("spv","csp")]*p_taulength/1000,3),"Primary Energy|Electricity|Solar (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_seprod[,,c("windon","windoff")]*p_taulength/1000,3),"Primary Energy|Electricity|Wind (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(v_seprod[,,c("hydro","ror","hs")]*p_taulength/1000,3),"Primary Energy|Electricity|Hydro (TWh/yr)"))
  
  #combining all the primary sources
  tmp5 <- setNames(dimSums(v_pedem[,,]*p_taulength/1000,3)+dimSums(v_seprod[,,c("spv","csp","windon","windoff","hydro","ror","hs")]*p_taulength/1000,3),paste("Primary Energy|Electricity (TWh/yr)"))
  
  # add global values
  tmp <- mbind(tmp3,tmp4,tmp5)

  return(tmp)
}
  
