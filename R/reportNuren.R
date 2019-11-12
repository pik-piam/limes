#' Read in GDX and calculate availability factors, used in convGDX2MIF.R for the reporting
#' 
#' Read in availability factors information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the availability factors
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportNuren(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportNuren <- function(gdx) {
  
  # read parameters and sets
  #p_taulength <- readGDX(gdx,name="p_taulength",field="l",format="first_found") #number of hours/year per tau
  #p_nuren_adj <- readGDX(gdx,name="p_nuren_adj",field="l",format="first_found") #availability factor for RES
  p_nurenannual_adj <- readGDX(gdx,name="p_nurenannual_adj",field="l",format="first_found") #annual availability factor for RES (per grade)
  p_nurenannual_adj2 <- readGDX(gdx,name="p_nurenannual_adj2",field="l",format="first_found") #annual availability factor for RES
  f_capmax <- readGDX(gdx,name="f_capmax",field="l",format="first_found") #capacity potential (per grade)
  ter <- readGDX(gdx,name="ter") #set of variable renewable generation technologies
  grade <- readGDX(gdx,name="grade") #set of grades (quality of RES potential)
  tau <- readGDX(gdx,name="tau") #set of time slices
  
  #Make sure only the "right" tau are taken -> to avoid info from gdx that might be stuck in the file
  #p_nuren <- p_nuren[,,tau]
  #p_taulength <- p_taulength[,,tau]
  
  # create MagPie object of demand with iso3 regions
  #p_nuren_adj <- limesMapping(p_nuren_adj[,,c(ter)])
  p_nurenannual_adj <- limesMapping(p_nurenannual_adj[,,c(ter)])
  p_nurenannual_adj2 <- limesMapping(p_nurenannual_adj2[,,c(ter)])
  f_capmax <- limesMapping(f_capmax[,,c(ter)])
  
  #Allocate years for expansion potential
  o_capmax <- limesAllocateYears(f_capmax,gdx)
  
  #REPORT ONLY ELECTRICITY-RELATED FACTORS
  ter <- setdiff(ter,"sol_heat")
  
  #Availability and expansion potential technologies per grade
  tmp1 <- NULL
  for (ter2 in ter) { 
    if(ter2 == "spv") {ter_name <- "Solar|PV"}
    if(ter2 == "csp") {ter_name <- "Solar|CSP"}
    if(ter2 == "windon") {ter_name <- "Wind|Onshore"}
    if(ter2 == "windoff") {ter_name <- "Wind|Offshore"}
    
    tmp1 <- mbind(tmp1,setNames(p_nurenannual_adj2[,,ter2],as.character(paste0("Availability factor|Electricity|",ter_name," (--)"))))
    #tmp1 <- mbind(tmp1,setNames(dimSums(o_capmax[,,ter2],dim=3),as.character(paste0("Expansion Potential||Electricity|",ter_name," (GW)"))))
    
    for (grade2 in grade) {
      tmp1 <- mbind(tmp1,setNames(p_nurenannual_adj[,,paste0(ter2,".",grade2)],paste0("Availability factor|Electricity|",ter_name,"|Grade|",grade2," (--)")))
      #tmp1 <- mbind(tmp1,setNames(o_capmax[,,paste0(ter2,".",grade2)],paste0("Expansion Potential|Electricity|",ter_name,"|Grade|",grade2," (GW)")))
    }
  }
  
  tmp2 <- NULL
  
  # add global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
