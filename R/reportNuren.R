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
  p_taulength <- readGDX(gdx,name="p_taulength",field="l",format="first_found") #number of hours/year per tau
  p_nuren <- readGDX(gdx,name="p_nuren",field="l",format="first_found") #availability factor for RES
  ter <- readGDX(gdx,name="ter") #set of variable renewable generation technologies
  grade <- readGDX(gdx,name="grade") #set of grades (quality of RES potential)
  tau <- readGDX(gdx,name="tau") #set of time slices
  
  #Make sure only the "right" tau are taken -> to avoid info from gdx that might be stuck in the file
  #p_nuren <- p_nuren[,,tau]
  #p_taulength <- p_taulength[,,tau]
  
  # create MagPie object of demand with iso3 regions
  p_nuren <- limesMapping(p_nuren)
  
  #single technologies per grade
  tmp1 <- NULL
  for (ter2 in ter) { for (grade2 in grade) {
    tmp1 <- mbind(tmp1,setNames((dimSums(p_nuren[,,paste0(ter2,".",grade2)]*p_taulength,dim=3)/dimSums(p_taulength,dim=3)),paste0("Electricity|Availability factor|",ter2,"|Grade|",grade2,"(--)")))
  }}
  
  #allocate the year
  tmp2<-limesAllocateYears(tmp1,gdx)
  
  #single technologies
  tmp3 <- NULL
  
  # add global values
  tmp <- mbind(tmp2,tmp3)

  return(tmp)
}
  
