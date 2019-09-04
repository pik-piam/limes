#' Read in GDX and calculate the load factor, used in convGDX2MIF.R for the reporting
#' 
#' Read in electricity generation data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportLoadFactor(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportLoadFactor <- function(gdx) {
  
  # read parameters and sets
  p_taulength <- readGDX(gdx,name="p_taulength",field="l",format="first_found") #number of hours/year per tau
  tau <- readGDX(gdx,name="tau") #set of time slices
  t <- readGDX(gdx,name="t") 
  pety <- readGDX(gdx,name="pety") #set of primary energies
  teel <- readGDX(gdx,name="teel") #set of electricity generation technologies (non-storage)
  ter <- readGDX(gdx,name="ter") #set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx,name="ternofluc") #set of non-variable (non-fluctuating) renewable electricity generation technologies
  tefossil <- readGDX(gdx,name="tefossil") #set of fossil-based electricity generation technologies
  tenr <- readGDX(gdx,name="tenr") #set of non-renewable electricity generation technologies (includes storage)
  tegas <- readGDX(gdx,name="tegas") #set of gas generation technologies
  telig <- readGDX(gdx,name="telig") #set of lignite generation technologies
  tecoal <- readGDX(gdx,name="tecoal") #set of hard coal generation technologies
  tengcc <- readGDX(gdx,name="tengcc") #set of NGCC generation technologies

  # read variables
  v_seprod <- readGDX(gdx,name="v_seprod",field="l",format="first_found")[,,tau]
  v_seprod <- v_seprod[,,pety]
  v_seprod <- v_seprod[,,"seel"]
  v_cap <- readGDX(gdx,name="v_cap",field="l",format="first_found")

  # create MagPie object of v_cap and v_seprod with iso3 regions
  v_seprod <- limesMapping(v_seprod)
  v_cap <- limesMapping(v_cap)
  
  #take only the years in t to make it compatible with v_seprod
  v_cap <- v_cap[,as.numeric(t),]

  #load factor per technology ('teel') per country
  tmp1 <- NULL
  #for (teel2 in teel) {
  #  if(teel2 != "pc_old" & teel2 != "lpc_old" & teel2 != "ngcc_old") #ignoring the old-fossil sources
  #    tmp1 <- mbind(tmp1,setNames(dimSums(v_seprod[,,teel2]*p_taulength,3)/(8760*v_cap[,as.numeric(t),teel2]),paste("Load Factor|Electricity|",teel2,"(--)")))
  #  }
  
  #load factor for aggregate technologies
  tmp2 <- NULL
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("tnr")]*p_taulength,dim=3)/dimSums((v_cap[,,c("tnr")])*8760,dim=3),"Load Factor|Electricity|Nuclear (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(telig,tecoal)]*p_taulength,dim=3)/dimSums(v_cap[,,c(telig,tecoal)]*8760,dim=3),"Load Factor|Electricity|Coal (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(setdiff(telig,"lpcc"),setdiff(tecoal,"pcc"))]*p_taulength,dim=3)/dimSums(v_cap[,,c(setdiff(telig,"lpcc"),setdiff(tecoal,"pcc"))]*8760,dim=3),"Load Factor|Electricity|Coal|w/o CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("pcc","lpcc")]*p_taulength,3)/dimSums(v_cap[,,c("pcc","lpcc")]*8760,dim=3),"Load Factor|Electricity|Coal|w/ CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(telig)]*p_taulength,dim=3)/dimSums(v_cap[,,c(telig)]*8760,dim=3),"Load Factor|Electricity|Lignite (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(setdiff(telig,"lpcc"))]*p_taulength,dim=3)/dimSums(v_cap[,,c(setdiff(telig,"lpcc"))]*8760,dim=3),"Load Factor|Electricity|Lignite|w/o CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("lpcc")]*p_taulength,dim=3)/dimSums(v_cap[,,c("lpcc")]*8760,dim=3),"Load Factor|Electricity|Lignite|w/ CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(tecoal)]*p_taulength,dim=3)/dimSums(v_cap[,,c(tecoal)]*8760,dim=3),"Load Factor|Electricity|Hard Coal (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(setdiff(tecoal,"pcc"))]*p_taulength,dim=3)/dimSums(v_cap[,,c(setdiff(tecoal,"pcc"))]*8760,dim=3),"Load Factor|Electricity|Hard Coal|w/o CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("pcc")]*p_taulength,dim=3)/dimSums(v_cap[,,c("pcc")]*8760,dim=3),"Load Factor|Electricity|Hard Coal|w/ CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(tegas)]*p_taulength,dim=3)/dimSums(v_cap[,,c(tegas)]*8760,dim=3),"Load Factor|Electricity|Gas (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(setdiff(tegas,"ngccc"))]*p_taulength,dim=3)/dimSums(v_cap[,,c(setdiff(tegas,"ngccc"))]*8760,dim=3),"Load Factor|Electricity|Gas|w/o CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(setdiff(tengcc,"ngccc"))]*p_taulength,dim=3)/dimSums(v_cap[,,c(setdiff(tengcc,"ngccc"))]*8760,dim=3),"Load Factor|Electricity|Gas CC|w/o CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c(tengcc)]*p_taulength,dim=3)/dimSums(v_cap[,,c(tengcc)]*8760,dim=3),"Load Factor|Electricity|Gas CC (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("ngccc")]*p_taulength,dim=3)/dimSums(v_cap[,,c("ngccc")]*8760,dim=3),"Load Factor|Electricity|Gas|w/ CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("ngccc")]*p_taulength,dim=3)/dimSums(v_cap[,,c("ngccc")]*8760,dim=3),"Load Factor|Electricity|Gas CC|w/ CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("ngt")]*p_taulength,dim=3)/dimSums(v_cap[,,c("ngt")]*8760,dim=3),"Load Factor|Electricity|Gas OC|w/o CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("ngt")]*p_taulength,dim=3)/dimSums(v_cap[,,c("ngt")]*8760,dim=3),"Load Factor|Electricity|Gas OC (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("oil")]*p_taulength,dim=3)/dimSums(v_cap[,,c("oil")]*8760,dim=3),"Load Factor|Electricity|Oil (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("oil")]*p_taulength,dim=3)/dimSums(v_cap[,,c("oil")]*8760,dim=3),"Load Factor|Electricity|Oil|w/o CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("windon","windoff")]*p_taulength,dim=3)/dimSums(v_cap[,,c("windon","windoff")]*8760,dim=3),"Load Factor|Electricity|Wind (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("windon")]*p_taulength,dim=3)/dimSums(v_cap[,,c("windon")]*8760,dim=3),"Load Factor|Electricity|Wind|Onshore (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("windoff")]*p_taulength,dim=3)/dimSums(v_cap[,,c("windoff")]*8760,dim=3),"Load Factor|Electricity|Wind|Offshore (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("spv","csp")]*p_taulength,dim=3)/dimSums(v_cap[,,c("spv","csp")]*8760,dim=3),"Load Factor|Electricity|Solar (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("spv")]*p_taulength,dim=3)/dimSums(v_cap[,,c("spv")]*8760,dim=3),"Load Factor|Electricity|Solar|PV (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("csp")]*p_taulength,dim=3)/dimSums(v_cap[,,c("csp")]*8760,dim=3),"Load Factor|Electricity|Solar|CSP (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("biolcigcc")]*p_taulength,dim=3)/dimSums(v_cap[,,c("biolcigcc")]*8760,dim=3),"Load Factor|Electricity|Biomass (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("biolcigcc")]*p_taulength,dim=3)/dimSums(v_cap[,,c("biolcigcc")]*8760,dim=3),"Load Factor|Electricity|Biomass|w/o CCS (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("hydro","ror","hs")]*p_taulength,dim=3)/dimSums(v_cap[,,c("hydro","ror","hs")]*8760,dim=3),"Load Factor|Electricity|Hydro (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("waste")]*p_taulength,dim=3)/dimSums(v_cap[,,c("waste")]*8760,dim=3),"Load Factor|Electricity|Waste (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("hcc","hct","hfc")]*p_taulength,dim=3)/dimSums(v_cap[,,c("hcc","hct","hfc")]*8760,dim=3),"Load Factor|Electricity|Hydrogen (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("others")]*p_taulength,dim=3)/dimSums(v_cap[,,c("others")]*8760,dim=3),"Load Factor|Electricity|Other (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("hfc")]*p_taulength,dim=3)/dimSums(v_cap[,,c("hfc")]*8760,dim=3),"Load Factor|Electricity|Hydrogen FC (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("hct")]*p_taulength,dim=3)/dimSums(v_cap[,,c("hct")]*8760,dim=3),"Load Factor|Electricity|Hydrogen OC (--)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(v_seprod[,,c("hcc")]*p_taulength,dim=3)/dimSums(v_cap[,,c("hcc")]*8760,dim=3),"Load Factor|Electricity|Hydrogen CC (--)"))
  
  # add global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
