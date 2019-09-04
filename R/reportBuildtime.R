#' Read in GDX and report the plant buildtime, used in convGDX2MIF.R for the reporting
#' 
#' Read in buildtime information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the buildtimes
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportBuildtime(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames
#' @export
#' 
reportBuildtime <- function(gdx) {
  
  # read sets and parameters
  p_tedata <- readGDX(gdx,name="p_tedata",field="l",format="first_found") #technology data
  te <- readGDX(gdx,name="te") #set of technologies
  
  # create MagPie object of demand with iso3 regions
  p_tedata <- limesMapping(p_tedata)
  
  #single technologies
  tmp1 <- NULL
  
  ##adding the name of the variable and the technology
  #for (te2 in te) { 
  #  if(te2 != "hvacline") #ignoring the transmission data
  #  tmp1 <- mbind(tmp1,setNames(p_tedata[,,paste0("buildtime.",te2)],paste0("Buildtime|",te2,"(Year)")))
  #  }
  #
  ##allocating the years
  #tmp1 <- limesAllocateYears(tmp1,gdx)
  
  #aggregated technologies
  tmp2 <- NULL
  
  # add global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
