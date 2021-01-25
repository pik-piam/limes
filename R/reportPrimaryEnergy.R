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
  te <- readGDX(gdx,name="te")
  teel <- readGDX(gdx,name="teel")
  tehe <- readGDX(gdx,name="tehe")
  ter <- readGDX(gdx,name="ter")
  tecoal <- readGDX(gdx,name="tecoal") 
  telig <- readGDX(gdx,name="telig") 
  tegas <- readGDX(gdx,name="tegas") 
  tengcc <- readGDX(gdx,name="tengcc")
  tefossil <- readGDX(gdx,name="tefossil") #set of fossil-based generation technologies
  teccs <- readGDX(gdx,name="teccs") #set of generation technologies|with CCS
  tehgen <- readGDX(gdx,name="tehgen") #set of hydrogen generation technologies
  tebio <- readGDX(gdx,name="tebio") #set of biomass generation technologies
  teoil <- readGDX(gdx,name="teoil") #set of oil generation technologies
  teothers <- readGDX(gdx,name="teothers") #set of other gases generation technologies
  tegas_el <- intersect(tegas,teel)
  tengcc_el <- intersect(tengcc,teel)
  petyex <- readGDX(gdx,name="petyex")
  pety <- readGDX(gdx,name="pety") #set of primary energies
  petyren <- readGDX(gdx,name="petyren") #set of primary energies
  tau <- readGDX(gdx,name="tau") #set of time slices
  pe2se <- readGDX(gdx,name="pe2se")
  pe2se <- paste0(pe2se[,1],".",pe2se[,2],".",pe2se[,3])
  
  # read parameters and variables
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  p_taulength <- readGDX(gdx,name="p_taulength",field="l",format="first_found")[,,tau]
  #v_pedem <- readGDX(gdx,name="v_pedem",field="l",format="first_found")[,,tau]
  v_pedem <- readGDX(gdx,name="v_pedem",field="l",format="first_found",restore_zeros = FALSE)
  #v_seprod <- readGDX(gdx,name="v_seprod",field="l",format="first_found")[,,tau]
  
  #Make sure only the "right" sets are taken -> to avoid info from gdx that might be stuck in the file
  #v_seprod <- v_seprod[,,c(pe2se)]
  #v_seprod <- v_seprod[,,c(petyren)]
  #v_seprod <- v_seprod[,,c(ter,"hydro","ror","hs")]
  #v_seprod <- v_seprod[,,"seel"]
  v_pedem <- v_pedem[,,petyex]
  #v_pedem[is.na(v_pedem)] <- 0
  
  # create MagPie object of v_pedem|with iso3 regions
  v_pedem <- limesMapping(v_pedem)
  #v_seprod <- limesMapping(v_seprod)
  
  #sum over tau
  
  
  v_pedem_he <- new.magpie(cells_and_regions = getRegions(v_pedem), years = getYears(v_pedem), names = NULL,
             fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  
  #Check the version so to choose the electricity-related variables
  if(c_LIMESversion >= 2.28) {
    v_pedem_el <- v_pedem[,,"seel"]
    
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    if(c_heating == 1) {
      v_pedem_he <- v_pedem[,,"sehe"]
    }
  } else {
    v_pedem_el <- v_pedem
    
  }
  
  v_pedem_el <- collapseNames(v_pedem_el)
  v_pedem_he <- collapseNames(v_pedem_he)
  
  #PRIMARY ENERGY FOR vRES and hydro is in reportGeneration
  
  #use of exhaustible primary energy types per country
  #and convert from GWh to TWh
  tmp1 <- NULL
  
  varList_el <- list(
    "Primary Energy|Exhaustible resources|Electricity (TWh/yr)"   =c(petyex), #all
    "Primary Energy|Biomass|Electricity (TWh/yr)"                 =intersect(teel,tebio),
    "Primary Energy|Biomass|Electricity|w/o CCS (TWh/yr)"         =intersect(teel,setdiff(tebio,teccs)),
    "Primary Energy|Coal|Electricity (TWh/yr)"                    =intersect(teel,c(tecoal,telig)),
    "Primary Energy|Coal|Electricity|w/o CCS (TWh/yr)"            =intersect(teel,setdiff(c(tecoal,telig),teccs)),
    "Primary Energy|Coal|Electricity|w/ CCS (TWh/yr)"             =intersect(teel,intersect(c(tecoal,telig),teccs)),
    "Primary Energy|Hard Coal|Electricity (TWh/yr)"               =intersect(teel,c(tecoal)),
    "Primary Energy|Hard Coal|Electricity|w/o CCS (TWh/yr)"       =intersect(teel,setdiff(c(tecoal),teccs)),
    "Primary Energy|Hard Coal|Electricity|w/ CCS (TWh/yr)"        =intersect(teel,intersect(c(tecoal),teccs)),
    "Primary Energy|Lignite|Electricity (TWh/yr)"                 =intersect(teel,c(telig)),
    "Primary Energy|Lignite|Electricity|w/o CCS (TWh/yr)"         =intersect(teel,setdiff(c(telig),teccs)),
    "Primary Energy|Lignite|Electricity|w/ CCS (TWh/yr)"          =intersect(teel,intersect(c(telig),teccs)),
    "Primary Energy|Oil|Electricity (TWh/yr)"                     =intersect(teel,c(teoil)),
    "Primary Energy|Gas|Electricity (TWh/yr)"                     =intersect(teel,c(tegas)),
    "Primary Energy|Gas|Electricity|w/o CCS (TWh/yr)"             =intersect(teel,setdiff(tegas_el,teccs)),
    "Primary Energy|Gas|Electricity|w/ CCS (TWh/yr)"              =intersect(teel,intersect(tegas_el,teccs)),
    "Primary Energy|Fossil|Electricity (TWh/yr)"                  =intersect(teel,c(tefossil)),
    "Primary Energy|Fossil|Electricity|w/o CCS (TWh/yr)"          =intersect(teel,setdiff(tefossil,teccs)),
    "Primary Energy|Fossil|Electricity|w/ CCS (TWh/yr)"           =intersect(teel,intersect(tefossil,teccs)),
    "Primary Energy|Other|Electricity (TWh/yr)"                   =intersect(teel,c(teothers)),
    "Primary Energy|Hydrogen|Electricity (TWh/yr)"                =intersect(teel,c(tehgen)),
    "Primary Energy|Nuclear|Electricity (TWh/yr)"                 =intersect(teel,c("tnr")),
    "Primary Energy|Waste|Electricity (TWh/yr)"                   =intersect(teel,c("waste"))
  )
  
  for (var in names(varList_el)) {
    tmp1 <- mbind(tmp1,setNames(dimSums(dimSums(v_pedem_el[,,varList_el[[var]]],dim=c(3.2,3.3))*p_taulength,dim=3)/1000,var))
  }
  
  tmp2 <- NULL
  #when there is endogenous heating
  if(c_LIMESversion >= 2.33) {
    tewaste <- readGDX(gdx,name="tewaste") #set of|waste generation technologies
    
    #Electricity (new technologies)
    tmp2 <- mbind(tmp2,setNames(dimSums(dimSums(v_pedem_el[,,intersect(tebio,teccs)],dim=c(3.2,3.3))*p_taulength,dim=3)/1000,"Primary Energy|Biomass|Electricity|w/ CCS (TWh/yr)"))
    
    if(c_heating == 1) {
      #Heat
      varList_he <- list(
        #"Primary Energy|Heat (TWh/yr)"                        =NA,
        "Primary Energy|Exhaustible resources|Heat (TWh/yr)"  =setdiff(petyex,c("pehgen", "peur")),                    
        "Primary Energy|Biomass|Heat (TWh/yr)"                =intersect(tebio,tehe),
        "Primary Energy|Coal|Heat (TWh/yr)"                   =intersect(c(tecoal,telig),tehe),
        "Primary Energy|Hard Coal|Heat (TWh/yr)"              =intersect(tecoal,tehe),
        "Primary Energy|Lignite|Heat (TWh/yr)"                =intersect(telig,tehe),
        "Primary Energy|Oil|Heat (TWh/yr)"                    =intersect(teoil,tehe),
        "Primary Energy|Gas|Heat (TWh/yr)"                    =intersect(tegas,tehe),
        "Primary Energy|Fossil|Heat (TWh/yr)"                 =intersect(tefossil,tehe),
        "Primary Energy|Other|Heat (TWh/yr)"                  =intersect(teothers,tehe),
        "Primary Energy|Waste|Heat (TWh/yr)"                  =intersect(tewaste,tehe)
      )
      for (var in names(varList_he)){
        tmp2 <- mbind(tmp2,setNames(dimSums(dimSums(v_pedem_he[,,varList_he[[var]]],dim=c(3.2,3.3))*p_taulength,dim=3)/1000,var))
      }
      
      #Load additional sets
      techp <- readGDX(gdx,name="techp") #set of chp generation technologies
      
      #Electricity and Heat
      varList_elhe <- list(
        "Primary Energy|Exhaustible resources|Electricity and Heat (TWh/yr)"   =c(petyex),
        "Primary Energy|Biomass|Electricity and Heat (TWh/yr)"                 =c(tebio),
        "Primary Energy|Coal|Electricity and Heat (TWh/yr)"                    =c(tecoal,telig),
        "Primary Energy|Hard Coal|Electricity and Heat (TWh/yr)"               =c(tecoal),
        "Primary Energy|Lignite|Electricity and Heat (TWh/yr)"                 =c(telig),
        "Primary Energy|Oil|Electricity and Heat (TWh/yr)"                     =c(teoil),
        "Primary Energy|Gas|Electricity and Heat (TWh/yr)"                     =c(tegas),
        "Primary Energy|Waste|Electricity and Heat (TWh/yr)"                   =c(tewaste),
        "Primary Energy|Fossil|Electricity and Heat (TWh/yr)"                  =c(tefossil),
        "Primary Energy|Nuclear|Electricity and Heat (TWh/yr)"                 =c("tnr"),
        "Primary Energy|Other|Electricity and Heat (TWh/yr)"                   =c(teothers),
        "Primary Energy|Hydrogen|Electricity and Heat (TWh/yr)"                =c(tehgen),
        
        #CHP
        "Primary Energy|Exhaustible resources|CHP (TWh/yr)"   =intersect(techp,te),
        "Primary Energy|Biomass|CHP (TWh/yr)"                 =intersect(techp,tebio),
        "Primary Energy|Coal|CHP (TWh/yr)"                    =intersect(techp,c(tecoal,telig)),
        "Primary Energy|Hard Coal|CHP (TWh/yr)"               =intersect(techp,tecoal),
        "Primary Energy|Lignite|CHP (TWh/yr)"                 =intersect(techp,telig),
        "Primary Energy|Oil|CHP (TWh/yr)"                     =intersect(techp,teoil),
        "Primary Energy|Gas|CHP (TWh/yr)"                     =intersect(techp,tegas),
        "Primary Energy|Gas|CHP|Gas CC (TWh/yr)"              =intersect(techp,tengcc),
        "Primary Energy|Gas|CHP|Gas OC (TWh/yr)"              =intersect(techp,setdiff(tegas,tengcc)),
        "Primary Energy|Fossil|CHP (TWh/yr)"                  =intersect(techp,tefossil),
        "Primary Energy|Other|CHP (TWh/yr)"                   =intersect(techp,teothers)
      )
      for (var in names(varList_elhe)){
        tmp2 <- mbind(tmp2,setNames(dimSums(dimSums(v_pedem[,,varList_elhe[[var]]],dim=c(3.2,3.3,3.4))*p_taulength,dim=3)/1000,var))
      }
        
    }
  }
  
  
  # add global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
