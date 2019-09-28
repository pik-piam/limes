#' Read in GDX and calculate (net and gross) annual demand, used in convGDX2MIF.R for the reporting
#' 
#' Read in (net and gross) demand data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportDemand(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportDemand <- function(gdx) {
  
  #Loading parameters from convGDX2MIF parent function 
  p_taulength <- readGDX(gdx,name="p_taulength",field="l",format="first_found") #number of hours/year per tau
  v_exdemand <- readGDX(gdx,name="v_exdemand",field="l",format="first_found") #demand
  c_demandscale <- readGDX(gdx,name="c_demandscale",field="l",format="first_found") #factor for scaling net demand
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  tau <- readGDX(gdx,name="tau") #set of time slices
  
  #Make sure only the "right" tau are taken -> to avoid info from gdx that might be stuck in the file
  v_exdemand <- v_exdemand[,,tau]
  p_taulength <- p_taulength[,,tau]
  
  # create MagPie object of demand with iso3 regions
  v_exdemand <- limesMapping(v_exdemand)
  p_hedemand <- new.magpie(cells_and_regions = getRegions(v_exdemand), years = getYears(v_exdemand), names = tau,
                           fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  
  #Check the version so to choose the electricity-related variables
  if(c_LIMESversion >= 2.28) {
    p_eldemand <- v_exdemand[,,"seel"]
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    if(c_heating == 1) {
      p_hedemand <- v_exdemand[,,"sehe"]
    }
  } else {
    p_eldemand <- v_exdemand
    
  }
  
  #single countries
  tmp1 <- NULL
  tmp1 <- mbind(tmp1,setNames(dimSums(p_eldemand*p_taulength,dim=3)/1000,"Gross Energy|Electricity  (TWh)"))
  
  #
  tmp2 <- NULL
  
  #concatenating gross demand data
  tmp3 <- NULL
  tmp3 <- mbind(tmp1,tmp2)
  
  #net demand 
  
  #single countries
  tmp4 <- NULL
  #tmp4 <- mbind(tmp4,setNames(dimSums(tmp1/c_demandscale,dim=3),"Net demand|Electricity (TWh)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(p_eldemand*p_taulength/c_demandscale,dim=3)/1000,"Final Energy|Electricity (TWh/yr)"))
  tmp4 <- mbind(tmp4,setNames(dimSums(p_hedemand*p_taulength,dim=3)/1000,"Final Energy|Heat (TWh/yr)"))
  
  #
  tmp5 <- NULL
  
  #concatenating net demand data
  tmp6 <- NULL
  tmp6 <- mbind(tmp4,tmp5)
  
  #concatenating net and gross demand data
  tmp <- mbind(tmp3,tmp6)

  return(tmp)
}
  
