#' Read in GDX and import different parameters used in convGDX2MIF.R for intermediate calculations in
#' the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{importParameters(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' 
importParameters <- function(gdx) {
  
  # read parameters
  p_taulength <- readGDX(gdx,name="p_taulength",field="l",format="first_found")
  p_exdemand <- readGDX(gdx,name="p_exdemand",field="l",format="first_found")
  c_demandscale <- readGDX(gdx,name="c_demandscale",field="l",format="first_found")
  s_c2co2 <- readGDX(gdx,name="s_c2co2",field="l",format="first_found")
  p_nuren <- readGDX(gdx,name="p_nuren",field="l",format="first_found")
  p_datafuelcost <- readGDX(gdx,name="p_datafuelcost",field="l",format="first_found")
  p_tedata <- readGDX(gdx,name="p_tedata",field="l",format="first_found")
  p_incoall <- readGDX(gdx,name="p_incoall",field="l",format="first_found")
   
  # create MagPie object of demand with iso3 regions
  p_exdemand <- limesMapping(p_exdemand)
  p_nuren <- limesMapping(p_nuren)
  p_datafuelcost <- limesMapping(p_datafuelcost)
  p_tedata <- limesMapping(p_tedata)
  p_incoall <- limesMapping(p_incoall)
  
  #Distinguish electricity demand ("seel") 
  p_eldemand <- p_exdemand[,,"seel"]
  
  #
  #p_nuren_best <- p_nuren[,,"1"]
  
  # parReturn <- mbind(p_taulength,p_exdemand,c_demandscale,p_eldemand)
  returnList <- list("p_taulength" = p_taulength ,
                     "c_demandscale" = c_demandscale, 
                     "p_eldemand" = p_eldemand,
                     "s_c2co2" = s_c2co2,
                     "p_nuren" = p_nuren,
                     "p_datafuelcost" = p_datafuelcost,
                     "p_tedata" = p_tedata,
                     "p_incoall" = p_incoall)
  
  return(returnList)
}
  
