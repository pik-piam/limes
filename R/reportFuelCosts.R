#' Read in GDX and report fuel costs, used in convGDX2MIF.R for the reporting
#' 
#' Read in fuel costs information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the fuel costs
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportFuelCosts(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames getSets getSets<- as.magpie
#' @export
#' 
reportFuelCosts <- function(gdx) {
  
  # read parameters and sets
  p_datafuelcost <- readGDX(gdx,name="p_datafuelcost",field="l",format="first_found") #fuel costs over time
  petyex <- readGDX(gdx,name="petyex")
  
  # create MagPie object of demand with iso3 regions
  p_datafuelcost <- limesMapping(p_datafuelcost)
  
  #single primary energies
  #original data in [Geur/GWh]. Convert to €/GJ
  tmp1 <- NULL
  #for (petyex2 in petyex) { 
  #  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,petyex2]/(3600*1.e-9),paste0("Fuel costs|",petyex2,"(Eur2010/GJ)")))
  #}
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"pebio"]/(3600*1.e-9),"Price|Primary Energy|Biomass (Eur2010/GJ)"))
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"pegas"]/(3600*1.e-9),"Price|Primary Energy|Gas (Eur2010/GJ)"))
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"pelig"]/(3600*1.e-9),"Price|Primary Energy|Lignite (Eur2010/GJ)"))
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"pecoal"]/(3600*1.e-9),"Price|Primary Energy|Hard Coal (Eur2010/GJ)"))
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"peoil"]/(3600*1.e-9),"Price|Primary Energy|Oil (Eur2010/GJ)"))
  
  tmp2 <- NULL
  
  # add global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
