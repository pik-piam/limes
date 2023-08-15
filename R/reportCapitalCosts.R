#' Read in GDX and report fuel costs, used in convGDX2MIF.R for the reporting
#'
#' Read in fuel costs information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the fuel costs
#' @author Sebastian Osorio
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportFuelCosts(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames getSets getSets<- as.magpie
#' @export
#'
reportCapitalCosts <- function(gdx) {

  # read parameters and sets
  p_incoall <- readGDX(gdx,name="p_incoall",field="l",format="first_found") #capital costs
  p_incostall <- readGDX(gdx,name="p_incostall",field="l",format="first_found") #energy storage costs
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found") #energy storage costs

  if(c_LIMESversion >= 2.38) {
   heating <- .readHeatingCfg(gdx) #switch heating
    c_buildings <- readGDX(gdx, name = c("c_buildings", "report_c_buildings"),
                           field = "l", format = "first_found") #switch on buildings module
  }

  # create MagPie object of demand with iso3 regions
  p_incoall <- limesMapping(p_incoall)
  p_incostall <- limesMapping(p_incostall)

  #single primary energies
  #original data in [Geur/GWh]. Convert to €/GJ
  tmp1 <- NULL

  if (heating != "fullDH" || c_LIMESversion < 2.38) {
    varList_el <- list(
      #Electricity-only
      "Capital Costs|Electricity|Biomass (Eur2010/kW)"          = "biolcigcc",
      "Capital Costs|Electricity|Hard Coal (Eur2010/kW)"        = "pc",
      "Capital Costs|Electricity|Lignite (Eur2010/kW)"          = "lpc",
      "Capital Costs|Electricity|Oil (Eur2010/kW)"              = "oil",
      "Capital Costs|Electricity|Gas CC (Eur2010/kW)"           = "ngcc",
      "Capital Costs|Electricity|Gas OC (Eur2010/kW)"           = "ngt",
      "Capital Costs|Electricity|Other (Eur2010/kW)"            = "others",
      "Capital Costs|Electricity|Waste (Eur2010/kW)"            = "waste"
    )

    for (var in names(varList_el)) {
      tmp1 <- mbind(tmp1, setNames(p_incoall[, , varList_el[[var]]] * 1000, var)) #convert from GEur/GW to eur/kW
    }

  }

  varList_el <- list(
    # Conventional
    "Capital Costs|Electricity|Biomass|w/ CCS (Eur2010/kW)"   = "bio_ccs",
    "Capital Costs|Electricity|Hard Coal|w/ CCS (Eur2010/kW)" = "pcc",
    "Capital Costs|Electricity|Lignite|w/ CCS (Eur2010/kW)"   = "lpcc",
    "Capital Costs|Electricity|Gas CC|w/ CCS (Eur2010/kW)"    = "ngccc",
    "Capital Costs|Electricity|Hydrogen FC (Eur2010/kW)"      = "hfc",
    "Capital Costs|Electricity|Hydrogen OC (Eur2010/kW)"      = "hct",
    "Capital Costs|Electricity|Hydrogen CC (Eur2010/kW)"      = "hcc",
    "Capital Costs|Electricity|Nuclear (Eur2010/kW)"          = "tnr",

    # Renewable
    "Capital Costs|Electricity|Wind|Onshore (Eur2010/kW)"     = "windon",
    "Capital Costs|Electricity|Wind|Offshore (Eur2010/kW)"    = "windoff",
    "Capital Costs|Electricity|Solar|PV (Eur2010/kW)"         = "spv",
    "Capital Costs|Electricity|Solar|CSP (Eur2010/kW)"        = "csp",
    "Capital Costs|Electricity|Hydro (Eur2010/kW)"            = "hydro",

    #Storage power costs
    "Power Costs|Electricity|Storage|Pump Hydro (Eur2010/kW)"            = "psp",
    "Power Costs|Electricity|Storage|Stat Batteries (Eur2010/kW)"        = "batteries",
    "Power Costs|Electricity|Storage|Hydrogen electrolysis (Eur2010/kW)" = "helec"

  )

  for (var in names(varList_el)) {
    tmp1 <- mbind(tmp1, setNames(p_incoall[, , varList_el[[var]]] * 1000, var)) #convert from GEur/GW to eur/kW
  }

  varList_elst <- list(
    #Storage energy costs
    "Energy Costs|Electricity|Storage|Pump Hydro (Eur2010/kWh)"            = "psp",
    "Energy Costs|Electricity|Storage|Stat Batteries (Eur2010/kWh)"        = "batteries",
    "Energy Costs|Electricity|Storage|Hydrogen electrolysis (Eur2010/kWh)" = "helec"

  )

  for (var in names(varList_elst)) {
    tmp1 <- mbind(tmp1, setNames(p_incostall[, , varList_elst[[var]]] * 1000, var)) #convert from GEur/GWh to eur/kWh
  }


  ##When heating is switched on
  tmp2 <- NULL

  if(c_LIMESversion >= 2.38) {
    if (heating == "fullDH") {
      varList_he <- list(
        #Electricity-only
        "Capital Costs|Electricity|Electricity-only|Biomass (Eur2010/kW)"          = "biolcigcc",
        "Capital Costs|Electricity|Electricity-only|Hard Coal (Eur2010/kW)"        = "pc",
        "Capital Costs|Electricity|Electricity-only|Lignite (Eur2010/kW)"          = "lpc",
        "Capital Costs|Electricity|Electricity-only|Oil (Eur2010/kW)"              = "oil",
        "Capital Costs|Electricity|Electricity-only|Gas CC (Eur2010/kW)"           = "ngcc",
        "Capital Costs|Electricity|Electricity-only|Gas OC (Eur2010/kW)"           = "ngt",
        "Capital Costs|Electricity|Electricity-only|Other (Eur2010/kW)"            = "others",
        "Capital Costs|Electricity|Electricity-only|Waste (Eur2010/kW)"            = "waste",

        #CHP
        "Capital Costs|Electricity|CHP|Biomass (Eur2010/kW)"        = "bio_chp",
        "Capital Costs|Electricity|CHP|Waste (Eur2010/kW)"          = "waste_chp",
        "Capital Costs|Electricity|CHP|Hard Coal (Eur2010/kW)"      = "pc_chp",
        "Capital Costs|Electricity|CHP|Lignite (Eur2010/kW)"        = "lpc_chp",
        "Capital Costs|Electricity|CHP|Oil (Eur2010/kW)"            = "oil_chp",
        "Capital Costs|Electricity|CHP|Gas CC (Eur2010/kW)"         = "ngcc_chp",
        "Capital Costs|Electricity|CHP|Gas OC (Eur2010/kW)"         = "ngt_chp",
        "Capital Costs|Electricity|CHP|Hydrogen (Eur2010/kW)"       = "hgen_chp",
        "Capital Costs|Electricity|CHP|Other (Eur2010/kW)"          = "others_chp",

        #Heat-only
        "Capital Costs|Heat|District Heating|Heat-only|Biomass (Eur2010/kW)"          = "bio_heat",
        "Capital Costs|Heat|District Heating|Heat-only|Waste (Eur2010/kW)"            = "waste_heat",
        "Capital Costs|Heat|District Heating|Heat-only|Hard Coal (Eur2010/kW)"        = "pc_heat",
        "Capital Costs|Heat|District Heating|Heat-only|Lignite (Eur2010/kW)"          = "lpc_heat",
        "Capital Costs|Heat|District Heating|Heat-only|Oil (Eur2010/kW)"              = "oil_heat",
        "Capital Costs|Heat|District Heating|Heat-only|Gas (Eur2010/kW)"              = "gas_heat",
        "Capital Costs|Heat|District Heating|Heat-only|Other (Eur2010/kW)"            = "others_heat",
        "Capital Costs|Heat|District Heating|Heat-only|Heat Pump (Eur2010/kW)"        = "hp_large",
        "Capital Costs|Heat|District Heating|Heat-only|Electric Boiler (Eur2010/kW)"  = "elboil_large",
        "Capital Costs|Heat|District Heating|Heat-only|Solar (Eur2010/kW)"            = "sol_heat",
        "Capital Costs|Heat|District Heating|Heat-only|Geothermal (Eur2010/kW)"       = "geo_heat",

        #Storage
        "Power Costs|Heat|Storage (Eur2010/kW)"     = "heat_sto"
      )

      if("hgen_heat" %in% getNames(p_incoall)) { #Hydrogen is a new technology, need to ensure it works with older versions
        varList_he <- c(
          varList_he,
          list("Capital Costs|Heat|District Heating|Heat-only|Hydrogen (Eur2010/kW)"                       = "hgen_heat")
        )
      }

      for (var in names(varList_he)) {
        tmp2 <- mbind(tmp2, setNames(p_incoall[, , varList_he[[var]]] * 1000, var)) #convert from GEur/GWh to eur/kWh
      }

      varList_he <- list(
        #Storage
        "Energy Costs|Heat|Storage (Eur2010/kWh)"   = "heat_sto"
      )

      for (var in names(varList_he)) {
        tmp2 <- mbind(tmp2, setNames(p_incostall[, , varList_he[[var]]] * 1000, var)) #convert from GEur/GWh to eur/kWh
      }

    }

    if (c_buildings == 1) {
      varList_he <- list(
        #Heat-only
        "Capital Costs|Heat|Decentralized|Heat Pump space heating (Eur2010/kW)"    = "hp_sh_dec",
        "Capital Costs|Heat|Decentralized|Heat Pump water heating (Eur2010/kW)"    = "hp_wh_dec",
        "Capital Costs|Heat|Decentralized|Resistance (Eur2010/kW)"                 = "resheat_dec",
        "Capital Costs|Heat|Decentralized|Conventional space heater (Eur2010/kW)"  = "convheat_dec",
        "Capital Costs|Heat|Decentralized|Conventional water heater (Eur2010/kW)"  = "convwh_dec"
      )

      for (var in names(varList_he)) {
        tmp2 <- mbind(tmp2, setNames(p_incoall[, , varList_he[[var]]] * 1000, var)) #convert from GEur/GWh to eur/kWh
      }

    }
  }

  # add global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}

