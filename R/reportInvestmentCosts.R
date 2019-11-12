#' Read in GDX and report the investment costs, used in convGDX2MIF.R for the reporting
#' 
#' Read in investment costs information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the investment costs
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportInvestmentCosts(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames
#' @export
#' 
reportInvestmentCosts <- function(gdx) {
  
  # read sets and parameters
  te <- readGDX(gdx,name="te") #set of technologies
  p_incoall <- readGDX(gdx,name="p_incoall",field="l",format="first_found") #investment consts over time
  
  # create MagPie object of demand with iso3 regions
  p_incoall <- limesMapping(p_incoall)
  
  #single technologies
  tmp1 <- NULL
  
  #adding the name of the variable and the technology
  #and convert Geur/GW to eur/kW
  for (te2 in setdiff(te,"hvacline")) { #ignoring the transmission data
    tmp1 <- mbind(tmp1,setNames(p_incoall[,,te2]*1000,paste0("Investment costs|",te2,"(eur/kW)")))
    }
  
  #aggregated technologies
  tmp2 <- NULL
  
  # add global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
