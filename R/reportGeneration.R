#' Read in GDX and calculate electricity generation, used in convGDX2MIF.R for the reporting
#'
#' Read in electricity generation data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param reporting_tau boolean determining whether to generate the tau report
#'  reporting at the time slice level (TRUE) or at the yearly level (FALSE, default)
#' @return MAgPIE object - contains the Generation variables
#' @author Sebastian Osorio, Renato Rodrigues, Antoine Levesque
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' \dontrun{
#' reportGeneration(gdx)
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie getItems collapseDim getRegions magpie_expand
#' @importFrom stringr str_replace
#' @export
#'
reportGeneration <- function(gdx, reporting_tau = FALSE) {

  # read sets
  tt <- readGDX(gdx, name = "t", field = "l", format = "first_found") # time set
  t0 <- tt[1]
  teel <- readGDX(gdx, name = "teel") # set of electricity generation technologies (non-storage)
  ter <- readGDX(gdx, name = "ter") # set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx, name = "ternofluc") # set of non-variable (non-fluctuating) renewable electricity generation technologies
  tefossil <- readGDX(gdx, name = "tefossil") # set of fossil-based electricity generation technologies
  tenr <- readGDX(gdx, name = "tenr") # set of non-renewable electricity generation technologies (includes storage)
  tegas <- readGDX(gdx, name = "tegas") # set of gas generation technologies
  telig <- readGDX(gdx, name = "telig") # set of lignite generation technologies
  tecoal <- readGDX(gdx, name = "tecoal") # set of hard coal generation technologies
  tengcc <- readGDX(gdx, name = "tengcc") # set of NGCC generation technologies
  tehydro <- readGDX(gdx, name = "tehydro") # set of hydropower generation technologies
  tehgen <- readGDX(gdx, name = "tehgen")
  tehydro <- readGDX(gdx, name = "tehydro")
  tebio <- readGDX(gdx, name = "tebio")
  teoil <- readGDX(gdx, name = "teoil")
  techp <- readGDX(gdx, name = "techp")
  teccs <- readGDX(gdx, name = "teccs")
  teothers <- readGDX(gdx, name = "teothers")
  tau <- readGDX(gdx, name = "tau") # set of time slices
  tegas_el <- intersect(tegas, teel)
  tengcc_el <- intersect(tengcc, teel)
  testore <- readGDX(gdx, name = "testore")
  pe2se <- readGDX(gdx, name = "pe2se")
  #pety <- readGDX(gdx, name = "pety") # set of primary energies
  pety <- unique(pe2se[, 1])
  tewaste <- readGDX(gdx, name = "tewaste", format = "first_found", react = 'silent') # set of waste generation technologies
  if(is.null(tewaste)) {tewaste <- "waste"} #in old model versions this set was not defined and only the tech 'waste' existed

  # read parameters
  c_esmdisrate <- readGDX(gdx, name = "c_esmdisrate", field = "l", format = "first_found") # interest rate
  p_ts <- readGDX(gdx, name = "p_ts", field = "l", format = "first_found") # time-step
  p_taulength <- readGDX(gdx, name = c("p_taulength", "pm_taulength"), field = "l", format = "first_found")[, , tau] # number of hours/year per tau
  p_tedata <- readGDX(gdx, name = "p_tedata", field = "l", format = "first_found") # parameter per technology
  o_netimports_tau <- readGDX(gdx, name = "o_netimports_tau", format = "first_found", react = 'silent') # electricity net imports
  c_LIMESversion <- readGDX(gdx, name = "c_LIMESversion", field = "l", format = "first_found")
  c_buildings <- readGDX(gdx, name = c("c_buildings", "report_c_buildings"),
                         field = "l", format = "first_found") #switch on buildings module

  # read variables
  v_seprod <- readGDX(gdx, name = "v_seprod", field = "l", format = "first_found", restore_zeros = FALSE)[, , tau]
  v_storeout <- readGDX(gdx, name = "v_storeout", field = "l", format = "first_found", restore_zeros = FALSE)[, , tau]
  v_storein <- readGDX(gdx, name = "v_storein", field = "l", format = "first_found", restore_zeros = FALSE)[, , tau]
  v_exdemand <- readGDX(gdx, name = "v_exdemand", field = "l", format = "first_found", restore_zeros = FALSE)[, , tau] # demand
  p_othersec_exdemand_DH <- readGDX(gdx, name = "p_othersec_exdemand_DH", field = "l", format = "first_found", react = 'silent') # heat demand provided by DH to other sectors (industry and agriculture) [annual data per sector]

  # Make sure only the sets -> to reduce the size of the variables
  v_seprod <- v_seprod[, , pety]
  p_autocons <- p_tedata[, , "autocons"]

  # create MagPie object of variables with iso3 regions
  v_seprod <- limesMapping(v_seprod)
  v_storeout <- limesMapping(v_storeout)
  v_storein <- limesMapping(v_storein)
  v_exdemand <- limesMapping(v_exdemand)
  p_autocons <- limesMapping(p_autocons)
  if (!is.null(o_netimports_tau)) {
    o_netimports_tau <- limesMapping(o_netimports_tau)
  }
  if (!is.null(p_othersec_exdemand_DH)) {
    p_othersec_exdemand_DH <- limesMapping(p_othersec_exdemand_DH)[,getYears(v_seprod),]
  }

  # give explicit set names
  if(attributes(dimnames(v_storeout)[2]) != "t") {
    getSets(v_storeout) <- c("region", "t", "tau", "enty2", "te")
    getSets(v_storein) <- c("region", "t", "tau", "enty2", "te")
  }

  # Check the version so to choose the electricity-related variables
  if (c_LIMESversion >= 2.28) {

    v_seprod_el <- v_seprod[, , "seel"]
    heating <- .readHeatingCfg(gdx)

    if (heating == "fullDH") {
      v_seprod_he <- v_seprod[, , "sehe"]
      v_seprod_he <- collapseDim(v_seprod_he, dim = 3.3)
      p_eldemand <- v_exdemand[, , "seel"]
      # v_seprod_el <- v_seprod[,,"seel"]

    } else {
      p_eldemand <- v_exdemand
      # v_seprod_el <- v_seprod
      v_storein_el <- v_storein
      v_storeout_el <- v_storeout
    }

  } else { #=if c_LIMESversion < 2.26
    p_eldemand <- v_exdemand
    v_seprod_el <- v_seprod
  }

  if (c_LIMESversion >= 2.37) { # First version with heat storage
    if (length(grep("heat_sto", getNames(v_storein))) > 0) {
      v_storein_el <- v_storein[, , "seel"]
      v_storein_el <- v_storein_el[, , setdiff(testore, c("heat_sto"))]

      v_storein_he <- v_storein[, , "sehe"]
      v_storein_he <- collapseDim(v_storein_he, dim = 3.2) # first collapse (to keep the technology name)
      # v_storein_he <- v_storein_he[,,"heat_sto"] #then filter by technology
    }

    if (length(grep("heat_sto", getNames(v_storeout))) > 0) {
      v_storeout_el <- v_storeout[, , "seel"]
      v_storeout_el <- v_storeout_el[, , setdiff(testore, c("heat_sto"))]

      v_storeout_he <- v_storeout[, , "sehe"]
      v_storeout_he <- collapseDim(v_storeout_he, dim = 3.2)
      # v_storeout_he <- v_storeout_he[,,"heat_sto"] #then filter by technology
    }

  #End of if c_LIMESversion >= 2.37
  }

  # Collapse names to avoid some problems
  p_eldemand <- collapseDim(p_eldemand, dim = 3.2)
  v_seprod_el <- collapseDim(v_seprod_el, dim = 3.2)
  v_storein_el <- collapseDim(v_storein_el, dim = 3.2)
  v_storeout_el <- collapseDim(v_storeout_el, dim = 3.2)

  # generation per aggregated technology per country

  varList_el <- list(
    # Conventional
    "Secondary Energy|Electricity (TWh/yr)"                  = c(teel),
    "Secondary Energy|Electricity|Biomass (TWh/yr)"          = intersect(teel, tebio),
    "Secondary Energy|Electricity|Biomass|w/o CCS (TWh/yr)"  = intersect(teel, setdiff(tebio, teccs)),
    "Secondary Energy|Electricity|Coal (TWh/yr)"             = intersect(teel, c(tecoal, telig)),
    "Secondary Energy|Electricity|Coal|w/o CCS (TWh/yr)"     = intersect(teel, setdiff(c(tecoal, telig), teccs)),
    "Secondary Energy|Electricity|Coal|w/ CCS (TWh/yr)"      = intersect(teel, intersect(c(tecoal, telig), teccs)),
    "Secondary Energy|Electricity|Hard Coal (TWh/yr)"        = intersect(teel, c(tecoal)),
    "Secondary Energy|Electricity|Hard Coal|w/o CCS (TWh/yr)" = intersect(teel, setdiff(c(tecoal), teccs)),
    "Secondary Energy|Electricity|Hard Coal|w/ CCS (TWh/yr)" = intersect(teel, intersect(c(tecoal), teccs)),
    "Secondary Energy|Electricity|Lignite (TWh/yr)"          = intersect(teel, c(telig)),
    "Secondary Energy|Electricity|Lignite|w/o CCS (TWh/yr)"  = intersect(teel, setdiff(c(telig), teccs)),
    "Secondary Energy|Electricity|Lignite|w/ CCS (TWh/yr)"   = intersect(teel, intersect(c(telig), teccs)),
    "Secondary Energy|Electricity|Oil (TWh/yr)"              = intersect(teel, c(teoil)),
    "Secondary Energy|Electricity|Gas (TWh/yr)"              = intersect(teel, c(tegas)),
    "Secondary Energy|Electricity|Gas|w/o CCS (TWh/yr)"      = intersect(teel, setdiff(tegas_el, teccs)),
    "Secondary Energy|Electricity|Gas|w/ CCS (TWh/yr)"       = intersect(teel, intersect(tegas_el, teccs)),
    "Secondary Energy|Electricity|Gas CC|w/o CCS (TWh/yr)"   = intersect(teel, setdiff(tengcc_el, teccs)),
    "Secondary Energy|Electricity|Gas CC|w/ CCS (TWh/yr)"    = intersect(teel, intersect(tengcc_el, teccs)),
    "Secondary Energy|Electricity|Gas CC (TWh/yr)"           = intersect(teel, c(tengcc_el)),
    "Secondary Energy|Electricity|Gas OC (TWh/yr)"           = intersect(teel, setdiff(tegas_el, tengcc_el)),
    "Secondary Energy|Electricity|Other (TWh/yr)"            = intersect(teel, c(teothers)),
    "Secondary Energy|Electricity|Hydrogen (TWh/yr)"         = intersect(teel, c(tehgen)),
    "Secondary Energy|Electricity|Hydrogen FC (TWh/yr)"      = intersect(teel, c("hfc")),
    "Secondary Energy|Electricity|Hydrogen OC (TWh/yr)"      = intersect(teel, c("hct")),
    "Secondary Energy|Electricity|Hydrogen CC (TWh/yr)"      = intersect(teel, c("hcc")),
    "Secondary Energy|Electricity|Nuclear (TWh/yr)"          = intersect(teel, c("tnr")),
    "Secondary Energy|Electricity|Waste (TWh/yr)"            = intersect(teel, c(tewaste)),
    "Secondary Energy|Electricity|Other Fossil (TWh/yr)"     = intersect(teel, c(teothers, tewaste, teoil)),

    # general aggregation
    "Secondary Energy|Electricity|Fossil (TWh/yr)"                 = intersect(teel, c(tefossil)),
    "Secondary Energy|Electricity|Fossil|w/o CCS (TWh/yr)"         = intersect(teel, setdiff(tefossil, teccs)),
    "Secondary Energy|Electricity|Fossil|w/ CCS (TWh/yr)"          = intersect(teel, intersect(tefossil, teccs)),
    "Secondary Energy|Electricity|Variable renewable (TWh/yr)"     = intersect(teel, c(ter)),
    "Secondary Energy|Electricity|Non-variable renewable (TWh/yr)" = intersect(teel, c(ternofluc)),
    "Secondary Energy|Electricity|Renewable (TWh/yr)"              = intersect(teel, c(ter, ternofluc)),
    "Secondary Energy|Electricity|Non-renewable (TWh/yr)"          = intersect(teel, tenr), # this does not include storage

    # Renewable
    "Secondary Energy|Electricity|Wind (TWh/yr)"         = intersect(teel, c("windon", "windoff")),
    "Secondary Energy|Electricity|Wind|Onshore (TWh/yr)" = intersect(teel, c("windon")),
    "Secondary Energy|Electricity|Wind|Offshore (TWh/yr)" = intersect(teel, c("windoff")),
    "Secondary Energy|Electricity|Solar (TWh/yr)"        = intersect(teel, c("spv", "csp")),
    "Secondary Energy|Electricity|Solar|PV (TWh/yr)"     = intersect(teel, c("spv")),
    "Secondary Energy|Electricity|Solar|CSP (TWh/yr)"    = intersect(teel, c("csp")),
    "Secondary Energy|Electricity|Hydro (TWh/yr)"        = intersect(teel, c(tehydro))
  )

  varList_stGen <- list(
    "Secondary Energy|Electricity|Storage (TWh/yr)"                       = setdiff(testore, c("heat_sto")),
    "Secondary Energy|Electricity|Storage|Pump Hydro (TWh/yr)"            = "psp",
    "Secondary Energy|Electricity|Storage|Stat Batteries (TWh/yr)"        = "batteries",
    "Secondary Energy|Electricity|Storage|Hydrogen electrolysis (TWh/yr)" = "helec"
  )

  varList_stCons <- list(
    "Secondary Energy|Electricity|Storage Consumption (TWh/yr)"                       = setdiff(testore, c("heat_sto")),
    "Secondary Energy|Electricity|Storage Consumption|Pump Hydro (TWh/yr)"            = "psp",
    "Secondary Energy|Electricity|Storage Consumption|Stat Batteries (TWh/yr)"        = "batteries",
    "Secondary Energy|Electricity|Storage Consumption|Hydrogen electrolysis (TWh/yr)" = "helec",
    "Primary Energy|Electricity|Hydrogen (TWh/yr)"                                    = "helec"
  )

  #Heat-related lists
  if (c_LIMESversion >= 2.33) {

    if (heating == "fullDH") {
      # Additional sets needed
      teoel <- readGDX(gdx, name = "teoel") # set of electricity-only generation technologies
      tedh <- readGDX(gdx, name = "tedh") # set of District Heating generation technologies
      tedhelec <- readGDX(gdx, name = "tedhelec") # set of electric District Heating generation technologies
      teheelec <- readGDX(gdx, name = "teheelec") # set of electric-based Heating generation technologies
      teohecen <- readGDX(gdx, name = "teohecen") # set of centralized heat-only generation technologies
      tehedec <- readGDX(gdx, name = "tehedec") # set of decentralized heat generation technologies

      # 1) HEAT FROM DH: CHP AND Heat-only

      v_heatwaste_DH <- readGDX(gdx, name = "v_heatwaste_DH", field = "l", format = "first_found",  restore_zeros  =  FALSE)[, , tau]
      if(is.null(v_heatwaste_DH)) { #No split of heat demand

        v_heatwaste_DH <- 0 #Assume all the wasted heat (v_heatwaste in the previous sbal equation) occurs in decentral
      } else { #split of heat demand between DH and decentral P2H

        v_heatwaste_DH <- limesMapping(v_heatwaste_DH) #[GWh]

      }

      #total useful energy generated by Dh
      o_seprodtotalDH_UE <-
        dimSums(v_seprod_he[,,tedh], dim = c(3.2, 3.3)) -
        collapseNames(v_storein_he) +
        collapseNames(v_storeout_he)
      #Total useful energy generate by DH to buildings
      o_bd_seprodtotalDH_UE <- o_seprodtotalDH_UE - p_othersec_exdemand_DH

      # 1 (cont) Decentralized heat
      if (c_buildings == 1) {

        # Useful energy
        varList_UE_P2H <- list(
          # 1.a) ALL heat production
          # "Useful Energy|Heat|Electricity (TWh/yr)"          =intersect(tehe, c(teheelec)),

          # 1.d) Decentralized heating (only electricity-based)
          "Useful Energy|Heating|Buildings|Decentral|Electricity|Total (TWh/yr)"                       = intersect(tehedec, teheelec),
          "Useful Energy|Heating|Buildings|Decentral|Electricity|Heat Pump (TWh/yr)"                   = intersect(tehedec, c("hp_sh_dec", "hp_wh_dec")),
          "Useful Energy|Heating|Buildings|Decentral|Electricity|Resistance (TWh/yr)"                  = intersect(tehedec, "resheat_dec"),
          "Useful Energy|Heating|Buildings|Decentral|Electricity|Conventional (TWh/yr)"                = intersect(tehedec, c("convheat_dec", "convwh_dec")),
          "Useful Energy|Heating|Buildings|Decentral|Electricity|Conventional space heater (TWh/yr)"   = intersect(tehedec, "convheat_dec"),
          "Useful Energy|Heating|Buildings|Decentral|Electricity|Conventional water heater (TWh/yr)"   = intersect(tehedec, "convwh_dec")
        )
      }

      # Gross heat (disaggregated between heat-only and CHP technologies)
      # Load additional parameters
      p_DH_losses <- readGDX(gdx, name = "p_DH_losses", field = "l", format = "first_found") # District heating losses [--] - same for all DH technologies
      p_DH_losses <- limesMapping(p_DH_losses)
      p_bd_ratio_ue2fe_DH <- readGDX(gdx, name = "p_bd_ratio_ue2fe_DH", field = "l", format = "first_found") # Ratio useful energy to final energy [--] - same for all DH technologies
      p_bd_ratio_ue2fe_DH <- limesMapping(p_bd_ratio_ue2fe_DH)
      # Need to create a variable with t, regi, te for gross production (v_seprod has these indexes)
      o_gross2ue <- new.magpie(cells_and_regions = getItems(v_seprod_he, dim = 1), years = getYears(v_seprod_he), names = NULL,
                               fill = 0, sort = FALSE, sets = NULL)
      o_grossprod_he <- new.magpie(cells_and_regions = getItems(v_seprod_he, dim = 1), years = getYears(v_seprod_he), names = c(tedh),
                                   fill = 0, sort = FALSE, sets = NULL)
      # Value from 2015 is used to scale the heat efficiency (etah), then we need a matrix with the same sets as v_seprod_he to estimate gross heat later
      o_gross2ue[, as.numeric(tt), ] <- ((1 - p_DH_losses) * p_bd_ratio_ue2fe_DH)

      varList_GrossHe_DH <- list(
        # 1.b) CHP
        "Secondary Energy|Gross|Heat|District Heating|CHP (TWh/yr)"                                         = c(techp),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Biomass (TWh/yr)"                                 = intersect(techp, tebio),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Coal (TWh/yr)"                                    = intersect(techp, c(tecoal, telig)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Hard Coal (TWh/yr)"                               = intersect(techp, c(tecoal)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Lignite (TWh/yr)"                                 = intersect(techp, c(telig)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Oil (TWh/yr)"                                     = intersect(techp, c(teoil)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Gas (TWh/yr)"                                     = intersect(techp, c(tegas)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Gas CC (TWh/yr)"                                  = intersect(techp, c(tengcc_el)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Gas OC (TWh/yr)"                                  = intersect(techp, setdiff(tegas_el, tengcc_el)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Hydrogen (TWh/yr)"                                = intersect(techp, tehgen),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Other (TWh/yr)"                                   = intersect(techp, c(teothers)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Waste (TWh/yr)"                                   = intersect(techp, tewaste),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Other Fossil (TWh/yr)"                            = intersect(techp, c(teothers, tewaste, teoil)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Fossil (TWh/yr)"                                  = intersect(techp, c(tefossil)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Renewable (TWh/yr)"                               = intersect(techp, c(ter, ternofluc)),
        "Secondary Energy|Gross|Heat|District Heating|CHP|Non-renewable (TWh/yr)"                           = intersect(techp, tenr),

        # 1.c) Only-heat (centralized boilers)
        "Secondary Energy|Gross|Heat|District Heating|Heat-only (TWh/yr)"                                = c(teohecen),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Biomass (TWh/yr)"                        = intersect(teohecen, tebio),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Coal (TWh/yr)"                           = intersect(teohecen, c(tecoal, telig)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Hard Coal (TWh/yr)"                      = intersect(teohecen, c(tecoal)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Lignite (TWh/yr)"                        = intersect(teohecen, c(telig)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Oil (TWh/yr)"                            = intersect(teohecen, c(teoil)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Gas (TWh/yr)"                            = intersect(teohecen, c(tegas)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Other (TWh/yr)"                          = intersect(teohecen, c(teothers)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Waste (TWh/yr)"                          = intersect(teohecen, c(tewaste)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Other Fossil (TWh/yr)"                   = intersect(teohecen, c(teothers, tewaste, teoil)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Electricity (TWh/yr)"                    = intersect(teohecen, c(tedhelec)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Electricity|Heat Pump (TWh/yr)"          = intersect(teohecen, "hp_large"),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Electricity|Electric Boiler (TWh/yr)"    = intersect(teohecen, "elboil_large"),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Solar (TWh/yr)"                          = intersect(teohecen, c("sol_heat")),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Geothermal (TWh/yr)"                     = intersect(teohecen, c("geo_heat")),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Fossil (TWh/yr)"                         = intersect(teohecen, c(tefossil)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Renewable (TWh/yr)"                      = intersect(teohecen, c(ter, ternofluc)),
        "Secondary Energy|Gross|Heat|District Heating|Heat-only|Non-renewable (TWh/yr)"                  = intersect(teohecen, tenr),

        # 1.d) District Heating
        "Secondary Energy|Gross|Heat|District Heating (TWh/yr)"                             = c(tedh),
        "Secondary Energy|Gross|Heat|District Heating|Biomass (TWh/yr)"                     = intersect(tedh, tebio),
        "Secondary Energy|Gross|Heat|District Heating|Coal (TWh/yr)"                        = intersect(tedh, c(tecoal, telig)),
        "Secondary Energy|Gross|Heat|District Heating|Hard Coal (TWh/yr)"                   = intersect(tedh, c(tecoal)),
        "Secondary Energy|Gross|Heat|District Heating|Lignite (TWh/yr)"                     = intersect(tedh, c(telig)),
        "Secondary Energy|Gross|Heat|District Heating|Oil (TWh/yr)"                         = intersect(tedh, c(teoil)),
        "Secondary Energy|Gross|Heat|District Heating|Gas (TWh/yr)"                         = intersect(tedh, c(tegas)),
        "Secondary Energy|Gross|Heat|District Heating|Hydrogen (TWh/yr)"                    = intersect(tedh, c(tehgen)),
        "Secondary Energy|Gross|Heat|District Heating|Other (TWh/yr)"                       = intersect(tedh, c(teothers)),
        "Secondary Energy|Gross|Heat|District Heating|Waste (TWh/yr)"                       = intersect(tedh, c(tewaste)),
        "Secondary Energy|Gross|Heat|District Heating|Other Fossil (TWh/yr)"                = intersect(tedh, c(teothers, tewaste, teoil)),
        "Secondary Energy|Gross|Heat|District Heating|Electricity (TWh/yr)"                 = intersect(tedh, c(tedhelec)),
        "Secondary Energy|Gross|Heat|District Heating|Electricity|Heat Pump (TWh/yr)"       = intersect(tedh, "hp_large"),
        "Secondary Energy|Gross|Heat|District Heating|Electricity|Electric Boiler (TWh/yr)" = intersect(tedh, "elboil_large"),
        "Secondary Energy|Gross|Heat|District Heating|Solar (TWh/yr)"                       = intersect(tedh, c("sol_heat")),
        "Secondary Energy|Gross|Heat|District Heating|Geothermal (TWh/yr)"                  = intersect(tedh, c("geo_heat")),
        "Secondary Energy|Gross|Heat|District Heating|Fossil (TWh/yr)"                      = intersect(tedh, c(tefossil)),
        "Secondary Energy|Gross|Heat|District Heating|Renewable (TWh/yr)"                   = intersect(tedh, c(ter, ternofluc)),
        "Secondary Energy|Gross|Heat|District Heating|Non-renewable (TWh/yr)"               = intersect(tedh, tenr)
      )

      if("hgen_heat" %in% tehgen) { #Hydrogen is a new technology, need to ensure it works with older versions
        varList_GrossHe_DH <- c(
          varList_GrossHe_DH,
          list("Secondary Energy|Gross|Heat|District Heating|Heat-only|Hydrogen (TWh/yr)"                       = intersect(teohecen, c(tehgen)))
        )
      }

    }
  }




# Standard Reporting ------------------------------------------------------


  if (!reporting_tau) { # for normal reporting

    # and converting from GWh to TWh
    tmp1 <- NULL

    for (var in names(varList_el)) {
      tmp1 <- mbind(tmp1, setNames(dimSums(dimSums(v_seprod_el[, , varList_el[[var]]], dim = c(3.2, 3.3))
                                          * p_taulength, dim = 3.1)
                                  / 1000,
                                  var))
    }

    # general aggregation
    tmp1 <- mbind(tmp1, setNames(dimSums((dimSums(v_seprod_el[, , c(teel)], dim = c(3.2, 3.3))
                                         + dimSums(v_storeout_el, dim = c(3.2)))
                                        * p_taulength, dim = 3.1)
                                / 1000,
                                "Secondary Energy|Electricity|w/ storage (TWh/yr)"))
    tmp1 <- mbind(tmp1, setNames(dimSums((dimSums(v_seprod_el[, , c(teel)], dim = c(3.2, 3.3))
                                         + dimSums(v_storeout_el, dim = c(3.2))
                                         - dimSums(v_storein_el, dim = c(3.2)))
                                        * p_taulength, dim = 3.1)
                                / 1000,
                                "Secondary Energy|Electricity|w/o losses (TWh/yr)"))

    tmp2 <- NULL
    # when there is endogenous heating switch
    if (c_LIMESversion >= 2.33) {

      # Electricity (new technologies)
      tmp2 <- mbind(tmp2, setNames(dimSums(dimSums(v_seprod_el[, , intersect(tebio, teccs)], dim = c(3.2, 3.3)) * p_taulength, dim = 3.1) / 1000,
                                   "Secondary Energy|Electricity|Biomass|w/ CCS (TWh/yr)"))

      if (heating == "fullDH") {

        # EXPLANATION OF TERMINOLOGY (for DH):
        # Transformation input ----(etah)--->>>> transformation output -----(distribution losses)---->>> final energy|heat -------(ratio of energy service to energy consumption)------>>>> useful energy
        # Transformation input = primary energy/final energy (in the case of electricity) (in reporttPrimaryEnergy)
        # Transformation output = gross energy (in this file)
        # Final energy

        #Total useful energy from DH (reported as total and not per tech)
        tmp2 <- mbind(tmp2, setNames(dimSums(o_seprodtotalDH_UE * p_taulength, dim = 3) / 1000,
                                     "Useful Energy|Heating|Heat|Total (TWh/yr)"))


        # 1 (cont) Decentralized heat
        if (c_buildings == 1) {
          for (var in names(varList_UE_P2H)) {
            tmp2 <- mbind(tmp2, setNames(dimSums(dimSums(v_seprod_he[, , varList_UE_P2H[[var]]], dim = c(3.2, 3.3)) * p_taulength, dim = 3) / 1000, var))
          }

        #End of if c_buildings == 1
        }

        # 2. ELECTRICITY FROM CHP AND ELECTRICITY-ONLY PLANTS
        varList_el <- list(
          # 2.a) CHP
          "Secondary Energy|Electricity|CHP (TWh/yr)"               = c(techp),
          "Secondary Energy|Electricity|CHP|Biomass (TWh/yr)"       = intersect(techp, tebio),
          "Secondary Energy|Electricity|CHP|Waste (TWh/yr)"         = intersect(techp, tewaste),
          "Secondary Energy|Electricity|CHP|Coal (TWh/yr)"          = intersect(techp, c(tecoal, telig)),
          "Secondary Energy|Electricity|CHP|Hard Coal (TWh/yr)"     = intersect(techp, c(tecoal)),
          "Secondary Energy|Electricity|CHP|Lignite (TWh/yr)"       = intersect(techp, c(telig)),
          "Secondary Energy|Electricity|CHP|Oil (TWh/yr)"           = intersect(techp, c(teoil)),
          "Secondary Energy|Electricity|CHP|Gas (TWh/yr)"           = intersect(techp, c(tegas)),
          "Secondary Energy|Electricity|CHP|Gas CC (TWh/yr)"        = intersect(techp, c(tengcc_el)),
          "Secondary Energy|Electricity|CHP|Gas OC (TWh/yr)"        = intersect(techp, setdiff(tegas_el, tengcc_el)),
          "Secondary Energy|Electricity|CHP|Hydrogen (TWh/yr)"      = intersect(techp, tehgen),
          "Secondary Energy|Electricity|CHP|Other (TWh/yr)"         = intersect(techp, c(teothers)),
          "Secondary Energy|Electricity|CHP|Other Fossil (TWh/yr)"  = intersect(techp, c(teothers, tewaste, teoil)),
          "Secondary Energy|Electricity|CHP|Fossil (TWh/yr)"        = intersect(techp, c(tefossil)),
          "Secondary Energy|Electricity|CHP|Renewable (TWh/yr)"     = intersect(techp, c(ter, ternofluc)),
          "Secondary Energy|Electricity|CHP|Non-renewable (TWh/yr)" = intersect(techp, tenr),

          # 2.b) Electricity-only
          "Secondary Energy|Electricity|Electricity-only (TWh/yr)"                  = c(teoel),
          "Secondary Energy|Electricity|Electricity-only|Biomass (TWh/yr)"          = intersect(teoel, tebio),
          "Secondary Energy|Electricity|Electricity-only|Biomass|w/o CCS (TWh/yr)"  = intersect(teoel, setdiff(tebio, teccs)),
          "Secondary Energy|Electricity|Electricity-only|Coal (TWh/yr)"             = intersect(teoel, c(tecoal, telig)),
          "Secondary Energy|Electricity|Electricity-only|Coal|w/o CCS (TWh/yr)"     = intersect(teoel, setdiff(c(tecoal, telig), teccs)),
          "Secondary Energy|Electricity|Electricity-only|Coal|w/ CCS (TWh/yr)"      = intersect(teoel, intersect(c(tecoal, telig), teccs)),
          "Secondary Energy|Electricity|Electricity-only|Hard Coal (TWh/yr)"        = intersect(teoel, c(tecoal)),
          "Secondary Energy|Electricity|Electricity-only|Hard Coal|w/o CCS (TWh/yr)" = intersect(teoel, setdiff(c(tecoal), teccs)),
          "Secondary Energy|Electricity|Electricity-only|Hard Coal|w/ CCS (TWh/yr)" = intersect(teoel, intersect(c(tecoal), teccs)),
          "Secondary Energy|Electricity|Electricity-only|Lignite (TWh/yr)"          = intersect(teoel, c(telig)),
          "Secondary Energy|Electricity|Electricity-only|Lignite|w/o CCS (TWh/yr)"  = intersect(teoel, setdiff(c(telig), teccs)),
          "Secondary Energy|Electricity|Electricity-only|Lignite|w/ CCS (TWh/yr)"   = intersect(teoel, intersect(c(telig), teccs)),
          "Secondary Energy|Electricity|Electricity-only|Oil (TWh/yr)"              = intersect(teoel, c(teoil)),
          "Secondary Energy|Electricity|Electricity-only|Gas (TWh/yr)"              = intersect(teoel, c(tegas)),
          "Secondary Energy|Electricity|Electricity-only|Gas|w/o CCS (TWh/yr)"      = intersect(teoel, setdiff(tegas_el, teccs)),
          "Secondary Energy|Electricity|Electricity-only|Gas|w/ CCS (TWh/yr)"       = intersect(teoel, intersect(tegas_el, teccs)),
          "Secondary Energy|Electricity|Electricity-only|Gas CC (TWh/yr)"           = intersect(teoel, c(tengcc_el)),
          "Secondary Energy|Electricity|Electricity-only|Gas OC (TWh/yr)"           = intersect(teoel, setdiff(tegas_el, tengcc_el)),
          "Secondary Energy|Electricity|Electricity-only|Other (TWh/yr)"            = intersect(teoel, c(teothers)),
          "Secondary Energy|Electricity|Electricity-only|Hydrogen (TWh/yr)"         = intersect(teoel, c(tehgen)),
          "Secondary Energy|Electricity|Electricity-only|Waste (TWh/yr)"            = intersect(teoel, c(tewaste)),
          "Secondary Energy|Electricity|Electricity-only|Other Fossil (TWh/yr)"     = intersect(teoel, c(teothers, tewaste, teoil)),
          "Secondary Energy|Electricity|Electricity-only|Fossil (TWh/yr)"           = intersect(teoel, c(tefossil)),
          "Secondary Energy|Electricity|Electricity-only|Fossil|w/o CCS (TWh/yr)"   = intersect(teoel, setdiff(tefossil, teccs)),
          "Secondary Energy|Electricity|Electricity-only|Fossil|w/ CCS (TWh/yr)"    = intersect(teoel, intersect(tefossil, teccs)),
          "Secondary Energy|Electricity|Electricity-only|Renewable (TWh/yr)"        = intersect(teoel, c(ter, ternofluc)),
          "Secondary Energy|Electricity|Electricity-only|Non-renewable (TWh/yr)"    = intersect(teoel, tenr) # this does not include storage
        )

        for (var in names(varList_el)) {
          tmp2 <- mbind(tmp2, setNames(dimSums(dimSums(v_seprod_el[, , varList_el[[var]]], dim = c(3.2, 3.3)) * p_taulength, dim = 3) / 1000, var))
        }

        #End of if heating == "fullDH"
      }

      #End of if c_LIMESversion >= 2.33
    }

    # add global values
    tmp3 <- mbind(tmp1, tmp2)


    # STORAGE-RELATED
    tmp4 <- NULL

    # Storage generation
    for (var in names(varList_stGen)) {
      tmp4 <- mbind(tmp4, setNames(dimSums(dimSums(v_storeout_el[, , varList_stGen[[var]]], dim = c(3.2))
                                          * p_taulength, dim = 3.1)
                                  / 1000,
                                  var))
    }

    # Storage consumption
    for (var in names(varList_stCons)) {
      tmp4 <- mbind(tmp4, setNames(dimSums(dimSums(v_storein_el[, , varList_stCons[[var]]], dim = c(3.2))
                                          * p_taulength, dim = 3.1)
                                  / 1000,
                                  var))
    }

    # Storage losses
    tmp4 <- mbind(tmp4, setNames(dimSums((dimSums(v_storein_el, dim = c(3.2)) -
                                            dimSums(v_storeout_el, dim = c(3.2))) * p_taulength / 1000,
                                         dim = 3.1),
                                 "Secondary Energy|Electricity|Storage Losses (TWh/yr)"))

    # aggregate tmp
    tmp5 <- mbind(tmp3, tmp4)

    # Gross production and demand
    tmp6 <- NULL
    # Need to create a variable with t, regi, te for gross production (v_seprod has these indexes)
    o_grossprod <- new.magpie(cells_and_regions = getItems(v_seprod_el, dim = 1), years = getYears(v_seprod_el), names = c(teel),
                              fill = 0, sort = FALSE, sets = NULL)
    # Estimate annual gross production for different technologies (in TWh/yr)
    for (teel2 in teel) {
      o_grossprod[, , teel2] <- dimSums(collapseDim(dimSums(v_seprod_el[, , teel2], dim = c(3.2)), dim = 3.2) * p_taulength / 1000, dim = 3) / (1 - p_autocons[, , teel2])
    }

    varList_elGr <- list(
      # Conventional
      "Secondary Energy|Gross|Electricity (TWh/yr)"                  = c(teel),
      "Secondary Energy|Gross|Electricity|Biomass (TWh/yr)"          = intersect(teel, tebio),
      "Secondary Energy|Gross|Electricity|Biomass|w/o CCS (TWh/yr)"  = intersect(teel, setdiff(tebio, teccs)),
      "Secondary Energy|Gross|Electricity|Coal (TWh/yr)"             = intersect(teel, c(tecoal, telig)),
      "Secondary Energy|Gross|Electricity|Coal|w/o CCS (TWh/yr)"     = intersect(teel, setdiff(c(tecoal, telig), teccs)),
      "Secondary Energy|Gross|Electricity|Coal|w/ CCS (TWh/yr)"      = intersect(teel, intersect(c(tecoal, telig), teccs)),
      "Secondary Energy|Gross|Electricity|Hard Coal (TWh/yr)"        = intersect(teel, c(tecoal)),
      "Secondary Energy|Gross|Electricity|Hard Coal|w/o CCS (TWh/yr)" = intersect(teel, setdiff(c(tecoal), teccs)),
      "Secondary Energy|Gross|Electricity|Hard Coal|w/ CCS (TWh/yr)" = intersect(teel, intersect(c(tecoal), teccs)),
      "Secondary Energy|Gross|Electricity|Lignite (TWh/yr)"          = intersect(teel, c(telig)),
      "Secondary Energy|Gross|Electricity|Lignite|w/o CCS (TWh/yr)"  = intersect(teel, setdiff(c(telig), teccs)),
      "Secondary Energy|Gross|Electricity|Lignite|w/ CCS (TWh/yr)"   = intersect(teel, intersect(c(telig), teccs)),
      "Secondary Energy|Gross|Electricity|Oil (TWh/yr)"              = intersect(teel, c(teoil)),
      "Secondary Energy|Gross|Electricity|Gas (TWh/yr)"              = intersect(teel, c(tegas)),
      "Secondary Energy|Gross|Electricity|Gas|w/o CCS (TWh/yr)"      = intersect(teel, setdiff(tegas_el, teccs)),
      "Secondary Energy|Gross|Electricity|Gas|w/ CCS (TWh/yr)"       = intersect(teel, intersect(tegas_el, teccs)),
      # "Secondary Energy|Gross|Electricity|Gas CC|w/o CCS (TWh/yr)"   =intersect(teel, setdiff(tengcc_el, teccs)),
      # "Secondary Energy|Gross|Electricity|Gas CC|w/ CCS (TWh/yr)"    =intersect(teel, intersect(tengcc_el, teccs)),
      "Secondary Energy|Gross|Electricity|Gas CC (TWh/yr)"           = intersect(teel, c(tengcc_el)),
      "Secondary Energy|Gross|Electricity|Gas OC (TWh/yr)"           = intersect(teel, setdiff(tegas_el, tengcc_el)),
      "Secondary Energy|Gross|Electricity|Other (TWh/yr)"            = intersect(teel, c(teothers)),
      "Secondary Energy|Gross|Electricity|Hydrogen (TWh/yr)"         = intersect(teel, c(tehgen)),
      # "Secondary Energy|Gross|Electricity|Hydrogen FC (TWh/yr)"      =intersect(teel, c("hfc")),
      # "Secondary Energy|Gross|Electricity|Hydrogen OC (TWh/yr)"      =intersect(teel, c("hct")),
      # "Secondary Energy|Gross|Electricity|Hydrogen CC (TWh/yr)"      =intersect(teel, c("hcc")),
      "Secondary Energy|Gross|Electricity|Nuclear (TWh/yr)"          = intersect(teel, c("tnr")),
      "Secondary Energy|Gross|Electricity|Waste (TWh/yr)"            = intersect(teel, c(tewaste)),
      "Secondary Energy|Gross|Electricity|Other Fossil (TWh/yr)"     = intersect(teel, c(teothers, tewaste, teoil)),

      # general aggregation
      "Secondary Energy|Gross|Electricity|Fossil (TWh/yr)"                 = intersect(teel, c(tefossil)),
      "Secondary Energy|Gross|Electricity|Fossil|w/o CCS (TWh/yr)"         = intersect(teel, setdiff(tefossil, teccs)),
      "Secondary Energy|Gross|Electricity|Fossil|w/ CCS (TWh/yr)"          = intersect(teel, intersect(tefossil, teccs)),
      "Secondary Energy|Gross|Electricity|Variable renewable (TWh/yr)"     = intersect(teel, c(ter)),
      "Secondary Energy|Gross|Electricity|Non-variable renewable (TWh/yr)" = intersect(teel, c(ternofluc)),
      "Secondary Energy|Gross|Electricity|Renewable (TWh/yr)"              = intersect(teel, c(ter, ternofluc)),
      "Secondary Energy|Gross|Electricity|Non-renewable (TWh/yr)"          = intersect(teel, tenr), # this does not include storage

      # Renewable
      "Secondary Energy|Gross|Electricity|Wind (TWh/yr)"         = intersect(teel, c("windon", "windoff")),
      "Secondary Energy|Gross|Electricity|Wind|Onshore (TWh/yr)" = intersect(teel, c("windon")),
      "Secondary Energy|Gross|Electricity|Wind|Offshore (TWh/yr)" = intersect(teel, c("windoff")),
      "Secondary Energy|Gross|Electricity|Solar (TWh/yr)"        = intersect(teel, c("spv", "csp")),
      "Secondary Energy|Gross|Electricity|Solar|PV (TWh/yr)"     = intersect(teel, c("spv")),
      "Secondary Energy|Gross|Electricity|Solar|CSP (TWh/yr)"    = intersect(teel, c("csp")),
      "Secondary Energy|Gross|Electricity|Hydro (TWh/yr)"        = intersect(teel, c(tehydro))
    )

    if (c_LIMESversion >= 2.33) {
      varList_elGr <- append(varList_elGr, list("Secondary Energy|Gross|Electricity|Biomass|w/ CCS (TWh/yr)" = intersect(teccs, tebio)))
    }

    for (var in names(varList_elGr)) {
      tmp6 <- mbind(tmp6, setNames(dimSums(o_grossprod[, , varList_elGr[[var]]], 3), var)) # o_grossprod is already in TWh/yr
    }


    if (heating == "fullDH") {

      # Gross electricity (split between electricity-only and CHP technologies)
      varList_elGrEoChp <- list(
        # 2.a) CHP
        "Secondary Energy|Gross|Electricity|CHP (TWh/yr)"               = c(techp),
        "Secondary Energy|Gross|Electricity|CHP|Biomass (TWh/yr)"       = intersect(techp, tebio),
        "Secondary Energy|Gross|Electricity|CHP|Waste (TWh/yr)"         = intersect(techp, tewaste),
        "Secondary Energy|Gross|Electricity|CHP|Coal (TWh/yr)"          = intersect(techp, c(tecoal, telig)),
        "Secondary Energy|Gross|Electricity|CHP|Hard Coal (TWh/yr)"     = intersect(techp, c(tecoal)),
        "Secondary Energy|Gross|Electricity|CHP|Lignite (TWh/yr)"       = intersect(techp, c(telig)),
        "Secondary Energy|Gross|Electricity|CHP|Oil (TWh/yr)"           = intersect(techp, c(teoil)),
        "Secondary Energy|Gross|Electricity|CHP|Gas (TWh/yr)"           = intersect(techp, c(tegas)),
        "Secondary Energy|Gross|Electricity|CHP|Gas CC (TWh/yr)"        = intersect(techp, c(tengcc_el)),
        "Secondary Energy|Gross|Electricity|CHP|Gas OC (TWh/yr)"        = intersect(techp, setdiff(tegas_el, tengcc_el)),
        "Secondary Energy|Gross|Electricity|CHP|Hydrogen (TWh/yr)"      = intersect(techp, tehgen),
        "Secondary Energy|Gross|Electricity|CHP|Other (TWh/yr)"         = intersect(techp, c(teothers)),
        "Secondary Energy|Gross|Electricity|CHP|Other Fossil (TWh/yr)"  = intersect(techp, c(teothers, tewaste, teoil)),
        "Secondary Energy|Gross|Electricity|CHP|Fossil (TWh/yr)"        = intersect(techp, c(tefossil)),
        "Secondary Energy|Gross|Electricity|CHP|Renewable (TWh/yr)"     = intersect(techp, c(ter, ternofluc)),
        "Secondary Energy|Gross|Electricity|CHP|Non-renewable (TWh/yr)" = intersect(techp, tenr),

        # 2.b) Electricity-only
        "Secondary Energy|Gross|Electricity|Electricity-only (TWh/yr)"                  = c(teoel),
        "Secondary Energy|Gross|Electricity|Electricity-only|Biomass (TWh/yr)"          = intersect(teoel, tebio),
        "Secondary Energy|Gross|Electricity|Electricity-only|Biomass|w/o CCS (TWh/yr)"  = intersect(teoel, setdiff(tebio, teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Coal (TWh/yr)"             = intersect(teoel, c(tecoal, telig)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Coal|w/o CCS (TWh/yr)"     = intersect(teoel, setdiff(c(tecoal, telig), teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Coal|w/ CCS (TWh/yr)"      = intersect(teoel, intersect(c(tecoal, telig), teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Hard Coal (TWh/yr)"        = intersect(teoel, c(tecoal)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Hard Coal|w/o CCS (TWh/yr)" = intersect(teoel, setdiff(c(tecoal), teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Hard Coal|w/ CCS (TWh/yr)" = intersect(teoel, intersect(c(tecoal), teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Lignite (TWh/yr)"          = intersect(teoel, c(telig)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Lignite|w/o CCS (TWh/yr)"  = intersect(teoel, setdiff(c(telig), teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Lignite|w/ CCS (TWh/yr)"   = intersect(teoel, intersect(c(telig), teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Oil (TWh/yr)"              = intersect(teoel, c(teoil)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Gas (TWh/yr)"              = intersect(teoel, c(tegas)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Gas|w/o CCS (TWh/yr)"      = intersect(teoel, setdiff(tegas_el, teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Gas|w/ CCS (TWh/yr)"       = intersect(teoel, intersect(tegas_el, teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Gas CC (TWh/yr)"           = intersect(teoel, c(tengcc_el)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Gas OC (TWh/yr)"           = intersect(teoel, setdiff(tegas_el, tengcc_el)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Other (TWh/yr)"            = intersect(teoel, c(teothers)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Hydrogen (TWh/yr)"         = intersect(teoel, c(tehgen)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Waste (TWh/yr)"            = intersect(teoel, c(tewaste)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Other Fossil (TWh/yr)"     = intersect(teoel, c(teothers, tewaste, teoil)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Fossil (TWh/yr)"           = intersect(teoel, c(tefossil)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Fossil|w/o CCS (TWh/yr)"   = intersect(teoel, setdiff(tefossil, teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Fossil|w/ CCS (TWh/yr)"    = intersect(teoel, intersect(tefossil, teccs)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Renewable (TWh/yr)"        = intersect(teoel, c(ter, ternofluc)),
        "Secondary Energy|Gross|Electricity|Electricity-only|Non-renewable (TWh/yr)"    = intersect(teoel, tenr) # this does not include storage
      )

      for (var in names(varList_elGrEoChp)) {
        tmp6 <- mbind(tmp6, setNames(dimSums(o_grossprod[, , varList_elGrEoChp[[var]]], 3), var)) # o_grossprod is already in TWh/yr
      }

      # Gross heat
      # Estimate annual gross production for different technologies (in TWh/yr)
      for (tehe2 in getNames(o_grossprod_he)) {
        o_grossprod_he[, , tehe2] <- dimSums(collapseDim(v_seprod_he[, , tehe2], dim = c(3.2, 3.3)) * p_taulength / 1000, dim = 3) / o_gross2ue
      }

      # Gross heat (disaggregated between heat-only and CHP technologies)
      for (var in names(varList_GrossHe_DH)) {
        tmp6 <- mbind(tmp6, setNames(dimSums(o_grossprod_he[, , varList_GrossHe_DH[[var]]], 3), var)) # o_grossprod_he is already in TWh/yr
      }


      # Final energy (Final Energy|Heat|*)
      # Take estimations from useful energy, divide by the ue2fe factor, and change name
      items <- getItems(tmp5, dim = 3)[grep("Useful Energy|District Heating", getItems(tmp5, dim = 3))]
      items <- items[items != "Useful Energy|Heating|Storage Consumption (TWh/yr)" & items != "Useful Energy|Heating|Storage Losses (TWh/yr)"]
      for (var_name in items) {
        tmp6 <- mbind(tmp6, setNames(tmp5[, , var_name] / p_bd_ratio_ue2fe_DH, str_replace(var_name, "Useful", "Final")))
      }

      #End of heating if
    }

    # Net imports
    o_netimpots <- new.magpie(cells_and_regions = getItems(p_eldemand, dim = 1), years = getYears(p_eldemand), names = tau,
                              fill = 0, sort = FALSE, sets = NULL)
    o_netimpots <- dimSums(p_eldemand * p_taulength / 1000, dim = 3) - setNames(tmp1[, , "Secondary Energy|Electricity|w/o losses (TWh/yr)"], NULL)

    # Gross demand (gross electricity production + net imports), following official german statistics procedure
    o_grossdem <- dimSums(o_grossprod, dim = c(3.1)) + o_netimpots
    tmp6 <- mbind(tmp6, setNames(o_grossdem, "Secondary Energy|Electricity|Gross Demand (TWh/yr)"))
    tmp6 <- mbind(tmp6, setNames(dimSums(o_grossprod[, , intersect(teel, c(ter, ternofluc))], dim = 3) / o_grossdem,
                                 "Secondary Energy|Electricity|Share of renewables in gross demand (--)"))
    tmp6 <- mbind(tmp6, setNames(dimSums(o_grossprod[, , intersect(teel, c(ter))], dim = 3) / o_grossdem,
                                 "Secondary Energy|Electricity|Share of variable renewables in gross demand (--)"))
    tmp6 <- mbind(tmp6, setNames(dimSums(o_grossprod[, , intersect(teel, c("windon","windoff"))], dim = 3) / o_grossdem,
                                 "Secondary Energy|Electricity|Share of wind energy in gross demand (--)"))
    tmp6 <- mbind(tmp6, setNames(dimSums(o_grossprod[, , intersect(teel, c("spv","csp"))], dim = 3) / o_grossdem,
                                 "Secondary Energy|Electricity|Share of solar energy in gross demand (--)"))

    # merge tmp's
    tmp <- mbind(tmp5, tmp6)

  } else { #if not the reportTau

# Tau reporting -----------------------------------------------------------
    f_renameTau <- function(vecOriginal) {
      vecModif <- vecOriginal
      names(vecModif) <- gsub("Secondary Energy", "Load", names(vecModif))
      names(vecModif) <- gsub("Useful Energy", "Load|Useful", names(vecModif))
      names(vecModif) <- gsub("TWh/yr", "GW", names(vecModif))
      return(vecModif)

    }

    f_computeTauVec <- function(varName, varList, data) {

      sets2Sum <- setdiff(getSets(data), c("region", "t", "tau"))
      if (!is.null(varList[[varName]])) {
        .tmp <- dimSums(data[, , varList[[varName]]],
                        dim = sets2Sum)
      } else {
        .tmp <- dimSums(data,
                        dim = sets2Sum)
      }


      .nm <- paste(varName, getNames(.tmp), sep = "___")
      .nm <- gsub("^(.*)( \\(.*\\))___(.*)$", "\\1|\\3\\2", .nm)
      .tmp <- setNames(.tmp, .nm)
      return(.tmp)
    }

    f_computeTau <- function(varList, data) {

      out <- do.call("mbind",
                     lapply(names(varList), f_computeTauVec, varList, data))
      return(out)
    }

    #Add BECCS to the list
    if(!is.null((intersect(tebio, teccs)))) {
      varList_el <- c(varList_el,
                         "Secondary Energy|Electricity|Biomass|w/ CCS (TWh/yr)"  = intersect(teel, intersect(tebio, teccs))
                      )
    }

    # Reporting tau
    varList_elTau <- f_renameTau(varList_el)
    varList_stGenTau <- f_renameTau(varList_stGen)
    varList_stConsTau <- f_renameTau(varList_stCons)
    varList_stLoss <-
      list("Load|Electricity|Storage Net Charging (GW)"                           = setdiff(testore, c("heat_sto")),
           "Load|Electricity|Storage Net Charging|Pump Hydro (GW)"                = "psp",
           "Load|Electricity|Storage Net Charging|Stat Batteries (GW)"            = "batteries",
           "Load|Electricity|Storage Net Charging|Hydrogen electrolysis (GW)"     = "helec"
           )

    tmp1 <- mbind(
      f_computeTau(varList_elTau, v_seprod_el),
      f_computeTau(varList_stGenTau, v_storeout_el),
      f_computeTau(varList_stConsTau, v_storein_el),
      f_computeTau(varList_stLoss, v_storein_el - v_storeout_el)
    )

    if (!is.null(o_netimports_tau)) {
      varList_Imports <- list("Secondary Energy|Electricity|Net Imports (TWh/yr)" = NULL)
      varList_Imports <- f_renameTau(varList_Imports)
      tmp1 <- mbind(tmp1,
                   f_computeTau(varList_Imports, o_netimports_tau))
    }


    x <- new.magpie(getRegions(tmp1), getYears(tmp1), getNames(p_taulength))
    tmp2 <- setNames(magpie_expand(p_taulength, x),
                     paste0("Tau length|", getNames(p_taulength), " (h)")
    )
    tmp3 <- mbind(tmp1, tmp2)

    #Heat related
    tmp4 <- NULL
    if (heating == "fullDH") {

      #Total UE generated by DH
      varList_totalDH_UE <- list("Load|Useful|Heating|Heat|Total (GW)" = NULL)
      varList_totalDH_UE <- f_renameTau(varList_totalDH_UE)
      tmp4 <- mbind(tmp4,
                    f_computeTau(varList_totalDH_UE, o_seprodtotalDH_UE))
      varList_GrossStHeat <- list("Load|Gross|Heat|Storage Net Charging (GW)" = "heat_sto")
      varList_GrossStHeat <- f_renameTau(varList_GrossStHeat)
      tmp4 <- mbind(tmp4,
                    f_computeTau(varList_GrossStHeat,
                                 (v_storein_he - v_storeout_he) / o_gross2ue)) #v_storein

      # Reporting tau
      varList_GrossHe_DHTau <- f_renameTau(varList_GrossHe_DH)

      tmp4 <- mbind(tmp4,
        f_computeTau(varList_GrossHe_DHTau, v_seprod_he / o_gross2ue)
      )

      if (c_buildings == 1) {

        ## Reporting tau
        #Power to heat
        varList_UE_P2HTau <- f_renameTau(varList_UE_P2H)
        tmp4 <- mbind(tmp4,
          f_computeTau(varList_UE_P2HTau, v_seprod_he)
        )
        #Dh for buildings
        varList_bd_totalDH_UE <- list("Load|Useful|Heating|Buildings|Heat|Total (GW)" = NULL)
        varList_bd_totalDH_UE <- f_renameTau(varList_bd_totalDH_UE)
        tmp4 <- mbind(tmp4,
                      f_computeTau(varList_bd_totalDH_UE, o_bd_seprodtotalDH_UE))

      } #end if buildings

    } #end if fullDH

    tmp <- mbind(tmp3, tmp4)


  }
  return(tmp)
}
