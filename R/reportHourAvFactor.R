#' Read in GDX and report the max availability factor per hour (for nonRES), used in convGDX2MIF.R for the reporting
#' 
#' Read in max availability factors per hour (for nonRES) information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the max availability factors per hour (for nonRES)
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportHourAvFactor(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames
#' @export
#' 
reportHourAvFactor <- function(gdx) {
  
  # read set of technologies
  p_tedata <- readGDX(gdx,name="p_tedata",field="l",format="first_found") #technology data
  te <- readGDX(gdx,name="te") #set of generation technologies
  ter <- readGDX(gdx,name="ter") #set of variable renewable generation technologies
  
  # create MagPie object of demand with iso3 regions
  p_tedata <- limesMapping(p_tedata)
  
  #single technologies
  tmp1 <- NULL
  
  #adding the name of the variable and the technology
  for (te2 in te) { 
    if(te2 != "hvacline" & sum(which(as.vector(ter)==te2))==0) #ignoring the transmission data
    tmp1 <- mbind(tmp1,setNames(p_tedata[,,paste0("nu.",te2)],paste0("Max availability factor per hour|",te2,"(--)")))
    }
  
  #allocating the years
  tmp1 <- limesAllocateYears(tmp1,gdx)
  
  #aggregated technologies
  tmp2 <- NULL
  
  # add global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
