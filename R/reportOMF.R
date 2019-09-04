#' Read in GDX and report the Fixed O&M costs, used in convGDX2MIF.R for the reporting
#' 
#' Read in Fixed O&M costs information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the Fixed O&M costs
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportOMF(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames
#' @export
#' 
reportOMF <- function(gdx) {
  
  # read sets and parameters
  te <- readGDX(gdx,name="te") #set of technologies
  p_incoall <- readGDX(gdx,name="p_incoall",field="l",format="first_found") #investment costs over time
  p_tedata <- readGDX(gdx,name="p_tedata",field="l",format="first_found") #technology data
  
  # create MagPie object of demand with iso3 regions
  p_incoall <- limesMapping(p_incoall)
  p_tedata <- limesMapping(p_tedata)
  
  #single technologies
  tmp1 <- NULL
  
  ##adding the name of the variable and the technology
  ##and convert investment costs from Geur/GW to eur/kW
  #for (te2 in te) { 
  #  if(te2 != "hvacline") #ignoring the transmission data
  #  tmp1 <- mbind(tmp1,setNames(p_incoall[,,te2]*1000*p_tedata[,,paste0("omf.",te2)],paste0("Fixed O&M costs|",te2,"(--)")))
  #}
  
  #aggregated technologies
  tmp2 <- NULL
  
  # add global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
