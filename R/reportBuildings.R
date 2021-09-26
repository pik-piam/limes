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
#' \dontrun{
#' reportBuildings(gdx)
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#'
reportBuildings <- function(gdx) {

  # Loading parameters from convGDX2MIF parent function
  c_LIMESversion <- readGDX(gdx, name = "c_LIMESversion", field = "l", format = "first_found")
  
  tmp1 <- NULL
  # Check the version so to choose the electricity-related variables
  if (c_LIMESversion >= 2.38) {

    # Loading sets and switches from convGDX2MIF parent function
    # c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    c_buildings <- readGDX(gdx, name = "c_buildings", field = "l", format = "first_found") # switch on buildings module


    if (c_buildings == 1) {
      # Loading parameters and variables
      p_bd_heatdem_ue <- readGDX(gdx, name = "p_bd_heatdem_ue", field = "l", format = "first_found", restore_zeros = FALSE) # heat demand per sector
      p_othersec_demDH_sec_ue <- readGDX(gdx, name = "p_othersec_demDH_sec_ue", field = "l", format = "first_found", restore_zeros = T) # heat demand per sector
      p_bd_area <- readGDX(gdx, name = "p_bd_area", field = "l", format = "first_found", restore_zeros = FALSE) # heat demand per sector
      v_bd_heatdem_ESR <- readGDX(gdx, name = "v_bd_heatdem_ESR", field = "l", format = "first_found") # heat that is covered by the ESR [annual data per sector]
      v_bd_heatdem_ETS <- readGDX(gdx, name = "v_bd_heatdem_ETS", field = "l", format = "first_found") # heat that is covered by the ETS [annual data per sector]
      p_othersec_demDH_ue <- readGDX(gdx, name = "p_othersec_demDH_ue", field = "l", format = "first_found") # heat that is provided by DH to other sectors (industry and agriculture) [annual data per sector]
      p_bd_ratio_ue2fe <- readGDX(gdx, name = "p_bd_ratio_ue2fe", field = "l", format = "first_found") # Ratio useful energy to final energy [--] - same for all DH technologies

      # create MagPie object of demand with iso3 regions
      p_bd_heatdem_ue <- limesMapping(p_bd_heatdem_ue)
      p_othersec_demDH_sec_ue <- limesMapping(p_othersec_demDH_sec_ue)
      p_bd_area <- limesMapping(p_bd_area)
      v_bd_heatdem_ESR <- limesMapping(v_bd_heatdem_ESR)
      v_bd_heatdem_ETS <- limesMapping(v_bd_heatdem_ETS)
      p_othersec_demDH_ue <- limesMapping(p_othersec_demDH_ue)
      p_bd_ratio_ue2fe <- limesMapping(p_bd_ratio_ue2fe)

      # Surface area
      tmp1 <- mbind(tmp1, setNames(p_bd_area[, , "resid"], "Useful Area|Residential (Mm2)"))
      tmp1 <- mbind(tmp1, setNames(p_bd_area[, , "nonresid"], "Useful Area|Non-residential (Mm2)"))

      # Useful energy
      tmp1 <- mbind(tmp1, setNames(p_bd_heatdem_ue[, , "resid.space_heat"] / 1000, "Useful Energy Available for Final Consumption|Heat|Residential|Space-heating (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(p_bd_heatdem_ue[, , "resid.water_heat"] / 1000, "Useful Energy Available for Final Consumption|Heat|Residential|Water-heating (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(p_bd_heatdem_ue[, , "nonresid.space_heat"] / 1000, "Useful Energy Available for Final Consumption|Heat|Non-residential|Space-heating (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(p_bd_heatdem_ue[, , "nonresid.water_heat"] / 1000, "Useful Energy Available for Final Consumption|Heat|Non-residential|Water-heating (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(p_othersec_demDH_sec_ue[, , "industry"] / 1000, "Useful Energy Available for Final Consumption|Heat|Industry|District heating (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(p_othersec_demDH_sec_ue[, , "agric"] / 1000, "Useful Energy Available for Final Consumption|Heat|Agriculture|District heating (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(dimSums(v_bd_heatdem_ETS, dim = 3) / 1000, "Useful Energy Available for Final Consumption|Heat|Buildings|ETS-covered (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(dimSums(v_bd_heatdem_ESR, dim = 3) / 1000, "Useful Energy Available for Final Consumption|Heat|Buildings|non-ETS-covered (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames((dimSums(v_bd_heatdem_ETS, dim = 3) + dimSums(v_bd_heatdem_ESR, dim = 3)) / 1000, "Useful Energy Available for Final Consumption|Heat|Buildings (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(p_othersec_demDH_ue / 1000, "Useful Energy Available for Final Consumption|Heat|Other sectors|District heating (TWh/yr)"))

      # Final energy
      tmp1 <- mbind(tmp1, setNames(tmp1[, , "Useful Energy Available for Final Consumption|Heat|Buildings (TWh/yr)"] / p_bd_ratio_ue2fe, "Final Energy|Heat|Buildings (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(tmp1[, , "Useful Energy Available for Final Consumption|Heat|Buildings|ETS-covered (TWh/yr)"] / p_bd_ratio_ue2fe, "Final Energy|Heat|Buildings|ETS-covered (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(tmp1[, , "Useful Energy Available for Final Consumption|Heat|Buildings|non-ETS-covered (TWh/yr)"] / p_bd_ratio_ue2fe, "Final Energy|Heat|Buildings|non-ETS-covered (TWh/yr)"))
      tmp1 <- mbind(tmp1, setNames(tmp1[, , "Useful Energy Available for Final Consumption|Heat|Other sectors|District heating (TWh/yr)"] / p_bd_ratio_ue2fe, "Final Energy|Heat|Other sectors|District heating (TWh/yr)"))


    }

  }

  tmp2 <- NULL



  # concatenating net and gross demand data
  tmp <- mbind(tmp1, tmp2)

  return(tmp)
}
