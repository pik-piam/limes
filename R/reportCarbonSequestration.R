#' Read in GDX and calculate carbon sequestred, used in convGDX2MIF.R for the reporting
#' 
#' Read in carbon sequestred data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportCarbonSequestration(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportCarbonSequestration <- function(gdx) {
  
  # read parameters and sets
  petyex <- readGDX(gdx,name="petyex") #set of exhaustible primary energy
  teccs <- readGDX(gdx,name="teccs") #set of technologies with CCS
  s_c2co2 <- readGDX(gdx,name="s_c2co2",field="l",format="first_found") #conversion factor C -> CO2
  
  # read variables
  v_emi <- readGDX(gdx,name="v_emi",field="l",format="first_found") #both emitted and sequestred emissions

  # create MagPie object of v_emi with iso3 regions
  v_emi <- limesMapping(v_emi)
  
  #take only the sequestred carbon ('cco2')
  v_emi <- v_emi[,,"cco2"]

  #annual sequestration per primary energy type
  tmp1 <- NULL
  
  #annual sequestration with some level of aggregation
  tmp2 <- NULL
  
  # add global values
  tmp3 <- mbind(tmp1,tmp2)
  
  #annual emissions per country
  tmp4 <- NULL
  
  tmp <- mbind(tmp3,tmp4)

  return(tmp)
}
  
