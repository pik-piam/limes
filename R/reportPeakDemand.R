#' Read in GDX and calculate annual peak demand, used in convGDX2MIF.R for the reporting
#' 
#' Read in gross demand data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportPeakDemand(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportPeakDemand <- function(gdx) {

  #Loading sets and parameters from convGDX2MIF parent function 
  regi <- readGDX(gdx,name="regi") #set of countries
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  v_exdemand <- readGDX(gdx,name="v_exdemand",field="l",format="first_found") #demand
  
  # create MagPie object of demand with iso3 regions
  v_exdemand <- limesMapping(v_exdemand)
  
  #Check the version so to choose the electricity-related variables
  if(c_LIMESversion >= 2.28) {
    p_eldemand <- v_exdemand[,,"seel"]
  } else {
    p_eldemand <- v_exdemand
  }
  
  #single countries
  tmp1 <- NULL
  tmp1 <- setNames(as.magpie(apply(p_eldemand,1:2,max)),"Capacity|Electricity|Peak Demand (GW)")
  
  tmp2 <- NULL
  
  #concatenating peak demand data
  tmp <- mbind(tmp1,tmp2)
  
  return(tmp)
}
  
