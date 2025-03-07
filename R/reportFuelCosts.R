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
  tt <- readGDX(gdx, name = "t")
  p_datafuelcost <- readGDX(gdx,name="p_datafuelcost",field="l",format="first_found") #fuel costs over time
  p_PriceInput_ind <- readGDX(gdx,name="p_PriceInput_ind",field="l",
                              format="first_found", react = 'silent', restore_zeros = T)
  petyex <- readGDX(gdx,name="petyex")

  # create MagPie object of demand with iso3 regions
  p_datafuelcost <- limesMapping(p_datafuelcost)

  #single primary energies
  #original data in [Geur/GWh]. Convert to €/GJ
  tmp1 <- NULL
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"pebio"]/(3600*1.e-9),"Price|Primary Energy|Biomass (Eur2010/GJ)"))
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"pegas"]/(3600*1.e-9),"Price|Primary Energy|Gas (Eur2010/GJ)"))
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"pelig"]/(3600*1.e-9),"Price|Primary Energy|Lignite (Eur2010/GJ)"))
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"pecoal"]/(3600*1.e-9),"Price|Primary Energy|Hard Coal (Eur2010/GJ)"))
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"peoil"]/(3600*1.e-9),"Price|Primary Energy|Oil (Eur2010/GJ)"))
  tmp1 <- mbind(tmp1,setNames(p_datafuelcost[,,"pehgen"]/(3600*1.e-9),"Price|Primary Energy|Hydrogen [external] (Eur2010/GJ)"))

  tmp2 <- NULL

  if(all(dim(p_PriceInput_ind) > 0)) { #check if all the dimensions are positive, otherwise it means, the parameter is not defined
    #Create magpie object with all the countries. Data from p_PriceInput_ind does not have spatial granularity
    .price_coke <- new.magpie(cells_and_regions = getItems(p_datafuelcost, dim  = 1),
                             years = getYears(p_PriceInput_ind),
                             names = "Price|Primary Energy|Coke (Eur2010/GJ)",
                             fill=0)

    .price_coke[,,] <- p_PriceInput_ind["GLO",,"coke"]/(3600*1.e-9)
    tmp2 <- mbind(tmp2,.price_coke)
  }



  # add global values
  tmp <- mbind(tmp1[, as.numeric(tt), ],tmp2)

  return(tmp)
}

