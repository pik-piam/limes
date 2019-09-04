#' Read in GDX and report the Variable O&M costs, used in convGDX2MIF.R for the reporting
#' 
#' Read in Variable O&M information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the Variable O&M
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportOMV(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames
#' @export
#' 
reportOMV <- function(gdx) {
  
  # read sets and parameters
  p_tedata <- readGDX(gdx,name="p_tedata",field="l",format="first_found") #technology data
  te <- readGDX(gdx,name="te") #set of technologies
  
  # create MagPie object of demand with iso3 regions
  p_tedata <- limesMapping(p_tedata)
  
  #single technologies
  tmp1 <- NULL
  
  ##adding the name of the variable and the technology
  ##and converting from Geur/GWh to eur/MWh
  #for (te2 in te) { 
  #  if(te2 != "hvacline") #ignoring the transmission data
  #  tmp1 <- mbind(tmp1,setNames(p_tedata[,,paste0("omv.",te2)]*1e6,paste0("Variable O&M|",te2,"(Eur/MWh)")))
  #}
  #
  ##allocating the years
  #tmp1 <- limesAllocateYears(tmp1,gdx)
  
  #aggregated technologies
  tmp2 <- NULL
  
  # add global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
