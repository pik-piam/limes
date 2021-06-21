#' Read in GDX and calculate buildings variables, used in convGDX2MIF.R for the reporting
#' 
#' Read in (net and gross) demand data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains buildings module variables
#' @author Sebastian Osorio
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportBuildings(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportBuildings <- function(gdx) {
  
  #Loading parameters from convGDX2MIF parent function 
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  
  #Check the version so to choose the electricity-related variables
  if(c_LIMESversion >= 2.38) {
    
    #Loading sets and switches from convGDX2MIF parent function 
    #c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    c_buildings <- readGDX(gdx,name="c_buildings",field="l",format="first_found") #switch on buildings module
    
    #Loading parameters and variables
    p_bd_heatdem_ue <- readGDX(gdx,name="p_bd_heatdem_ue",field="l",format="first_found",restore_zeros = FALSE) #heat demand per sector
    p_othersec_demDH_sec_ue <- readGDX(gdx,name="p_othersec_demDH_sec_ue",field="l",format="first_found",restore_zeros = T) #heat demand per sector
    p_bd_area <- readGDX(gdx,name="p_bd_area",field="l",format="first_found",restore_zeros = FALSE) #heat demand per sector
    
    # create MagPie object of demand with iso3 regions
    p_bd_heatdem_ue <- limesMapping(p_bd_heatdem_ue)
    p_othersec_demDH_sec_ue <- limesMapping(p_othersec_demDH_sec_ue)
    p_bd_area <- limesMapping(p_bd_area)
    
    
    if(c_buildings == 1) {
      tmp1 <- NULL
      tmp1 <- mbind(tmp1,setNames(p_bd_area[,,"resid"],"Useful Area|Residential (Mm2)"))
      tmp1 <- mbind(tmp1,setNames(p_bd_area[,,"nonresid"],"Useful Area|Non-residential (Mm2)"))
      tmp1 <- mbind(tmp1,setNames(p_bd_heatdem_ue[,,"resid.space_heat"]/1000,"Useful Energy Available for Final Consumption|Heat|Residential|Space-heating (TWh/yr)"))
      tmp1 <- mbind(tmp1,setNames(p_bd_heatdem_ue[,,"resid.water_heat"]/1000,"Useful Energy Available for Final Consumption|Heat|Residential|Water-heating (TWh/yr)"))
      tmp1 <- mbind(tmp1,setNames(p_bd_heatdem_ue[,,"nonresid.space_heat"]/1000,"Useful Energy Available for Final Consumption|Heat|Non-residential|Space-heating (TWh/yr)"))
      tmp1 <- mbind(tmp1,setNames(p_bd_heatdem_ue[,,"nonresid.water_heat"]/1000,"Useful Energy Available for Final Consumption|Heat|Non-residential|Water-heating (TWh/yr)"))
      tmp1 <- mbind(tmp1,setNames(p_othersec_demDH_sec_ue[,,"industry"]/1000,"Useful Energy Available for Final Consumption|Heat|Industry (TWh/yr)"))
      tmp1 <- mbind(tmp1,setNames(p_othersec_demDH_sec_ue[,,"agric"]/1000,"Useful Energy Available for Final Consumption|Heat|Agriculture (TWh/yr)"))
      
      
    }
    
  }
  
  tmp2 <- NULL
  
  
  
  #concatenating net and gross demand data
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
