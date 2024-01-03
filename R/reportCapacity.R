#' Read in GDX and calculate capacities,  used in convGDX2MIF.R for the reporting
#'
#' Read in capacity information from GDX file,  information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX,  or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio,  Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportCapacity(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#'
#' @export

reportCapacity <- function(gdx) {

  # read sets
  tt <- readGDX(gdx, name = "t")
  teel <- readGDX(gdx, name = "teel")
  tecoal <- readGDX(gdx, name = "tecoal")
  telig <- readGDX(gdx, name = "telig")
  tegas <- readGDX(gdx, name = "tegas")
  tengcc <- readGDX(gdx, name = "tengcc")
  tehgen <- readGDX(gdx, name = "tehgen")
  tehydro <- readGDX(gdx, name = "tehydro")
  tebio <- readGDX(gdx, name = "tebio")
  teoil <- readGDX(gdx, name = "teoil")
  teccs <- readGDX(gdx, name = "teccs")
  testore <- readGDX(gdx, name = "testore")
  teothers <- readGDX(gdx, name = "teothers")
  tereserve <- readGDX(gdx, name = "tereserve")
  tefossil <- readGDX(gdx, name = "tefossil")
  ter <- readGDX(gdx, name = "ter")
  ternofluc <- readGDX(gdx, name = "ternofluc")
  tenr <- readGDX(gdx, name = "tenr")
  tegas_el <- intersect(tegas, teel)
  tengcc_el <- intersect(tengcc, teel)
  tewaste <- readGDX(gdx, name = "tewaste", format = "first_found", react = 'silent') # set of waste generation technologies
  if(is.null(tewaste)) {tewaste <- "waste"} #in old model versions this set was not defined and only the tech 'waste' existed

  # Read parameters
  c_LIMESversion <- readGDX(gdx, name = "c_LIMESversion", field = "l", format = "first_found")
  c_buildings <- readGDX(gdx, name = c("c_buildings", "report_c_buildings"),
                         field = "l", format = "first_found") #switch on buildings module

  # read variables
  v_cap <- readGDX(gdx, name = c("v_cap", "vm_cap"), field = "l", format = "first_found")
  v_capreserve <- readGDX(gdx, name = "v_capreserve", field = "l", format = "first_found")

  # create MagPie object of v_cap with iso3 regions
  v_cap <- limesMapping(v_cap)
  v_capreserve <- limesMapping(v_capreserve)

  # total installed capacity
  tmp1 <- NULL
  #for (tech in te) {
  #  tmp1 <- mbind(tmp1, setNames(v_cap[, , tech], paste("Capacity|Electricity|", tech, "(GW)")))
  #}

  #aggregated technologies (w/o CHP) - depending on LIMES version,  they will be considered
  varList_el <- list(
    #Conventional
    "Capacity|Electricity (GW)"                   = c(teel),
    "Capacity|Electricity|Biomass (GW)"           = intersect(teel, tebio),
    "Capacity|Electricity|Biomass|w/o CCS (GW)"   = intersect(teel, setdiff(tebio, teccs)),
    "Capacity|Electricity|Coal (GW)"              = intersect(teel, c(tecoal, telig)),
    "Capacity|Electricity|Coal|w/o CCS (GW)"      = intersect(teel, setdiff(c(tecoal, telig), teccs)),
    "Capacity|Electricity|Coal|w/ CCS (GW)"       = intersect(teel, intersect(c(tecoal, telig), teccs)),
    "Capacity|Electricity|Hard Coal (GW)"         = intersect(teel, c(tecoal)),
    "Capacity|Electricity|Hard Coal|w/o CCS (GW)" = intersect(teel, setdiff(c(tecoal), teccs)),
    "Capacity|Electricity|Hard Coal|w/ CCS (GW)"  = intersect(teel, intersect(c(tecoal), teccs)),
    "Capacity|Electricity|Lignite (GW)"           = intersect(teel, c(telig)),
    "Capacity|Electricity|Lignite|w/o CCS (GW)"   = intersect(teel, setdiff(c(telig), teccs)),
    "Capacity|Electricity|Lignite|w/ CCS (GW)"    = intersect(teel, intersect(c(telig), teccs)),
    "Capacity|Electricity|Oil (GW)"               = intersect(teel, c(teoil)),
    "Capacity|Electricity|Gas (GW)"               = intersect(teel, c(tegas)),
    "Capacity|Electricity|Gas|w/o CCS (GW)"       = intersect(teel, setdiff(tegas_el, teccs)),
    "Capacity|Electricity|Gas|w/ CCS (GW)"        = intersect(teel, intersect(tegas_el, teccs)),
    "Capacity|Electricity|Gas CC|w/o CCS (GW)"    = intersect(teel, setdiff(tengcc_el, teccs)),
    "Capacity|Electricity|Gas CC|w/ CCS (GW)"     = intersect(teel, intersect(tengcc_el, teccs)),
    "Capacity|Electricity|Gas CC (GW)"            = intersect(teel, c(tengcc_el)),
    "Capacity|Electricity|Gas OC (GW)"            = intersect(teel, setdiff(tegas_el, tengcc_el)),
    "Capacity|Electricity|Other (GW)"             = intersect(teel, c(teothers)),
    "Capacity|Electricity|Hydrogen (GW)"          = intersect(teel, c(tehgen)),
    "Capacity|Electricity|Hydrogen FC (GW)"       = intersect(teel, c("hfc")),
    "Capacity|Electricity|Hydrogen OC (GW)"       = intersect(teel, c("hct")),
    "Capacity|Electricity|Hydrogen CC (GW)"       = intersect(teel, c("hcc")),
    "Capacity|Electricity|Nuclear (GW)"           = intersect(teel, c("tnr")),
    "Capacity|Electricity|Waste (GW)"             = intersect(teel, c(tewaste)),
    "Capacity|Electricity|Other Fossil (GW)"      = intersect(teel, c(teothers, tewaste, teoil)),

    #general aggregation
    "Capacity|Electricity|Fossil (GW)"                  = intersect(teel, c(tefossil)),
    "Capacity|Electricity|Fossil|w/o CCS (GW)"          = intersect(teel, setdiff(tefossil, teccs)),
    "Capacity|Electricity|Fossil|w/ CCS (GW)"           = intersect(teel, intersect(tefossil, teccs)),
    "Capacity|Electricity|Variable renewable (GW)"      = intersect(teel, c(ter)),
    "Capacity|Electricity|Non-variable renewable (GW)"  = intersect(teel, c(ternofluc)),
    "Capacity|Electricity|Renewable (GW)"               = intersect(teel, c(ter, ternofluc)),
    "Capacity|Electricity|Non-renewable (GW)"           = intersect(teel, tenr),  #this does not include storage

    #Renewable
    "Capacity|Electricity|Wind (GW)"                                           = intersect(teel, c("windon", "windoff")),
    "Capacity|Electricity|Wind|Onshore (GW)"                                   = intersect(teel, c("windon")),
    "Capacity|Electricity|Wind|Offshore (GW)"                                  = intersect(teel, c("windoff")),
    "Capacity|Electricity|Solar (GW)"                                          = intersect(teel, c("spv", "csp")),
    "Capacity|Electricity|Solar|PV (GW)"                                       = intersect(teel, c("spv")),
    "Capacity|Electricity|Solar|CSP (GW)"                                      = intersect(teel, c("csp")),
    "Capacity|Electricity|Hydro (GW)"                                          = intersect(teel, c(tehydro)),
    #"Capacity|Electricity|Hydro|Hydro storage (GW)"         = intersect(teel, "hs"),
    #"Capacity|Electricity|Hydro|Run of river (GW)"         = intersect(teel, "ror")

    #Storage
    "Capacity|Electricity|Storage (GW)"                                        = setdiff(testore, c("heat_sto")),
    "Capacity|Electricity|Storage|Stat Batteries (GW)"                         = c("batteries"),
    "Capacity|Electricity|Storage|Pump Hydro (GW)"                             = c("psp"),
    "Capacity|Electricity|Storage|Hydrogen electrolysis [input] (GW)"          = c("helec"),
    "Capacity|Electricity|Storage|Intra-day [input] (GW)"                      = c("batteries", "psp")
  )

  tmp2 <- NULL
  for (var in names(varList_el)){
    tmp2 <- mbind(tmp2, setNames(dimSums(v_cap[, , varList_el[[var]]], dim = 3), var))
  }


  #when there is exogenous heating
  if(c_LIMESversion >=  2.33) {
    heating <- .readHeatingCfg(gdx)

    if(heating == "fullDH") {
      #load some required sets
      techp <- readGDX(gdx, name = "techp")
      tedhelec <- readGDX(gdx, name = "tedhelec") #set of electric District Heating generation technologies
      teohecen <- readGDX(gdx, name = "teohecen") #set of centralized only-heating generation technologies
      tehedec <- readGDX(gdx, name = "tehedec") #set of decentralized only-heating generation technologies
      teoel <- readGDX(gdx, name = "teoel") #set of electricity-only generation technologies

      #Some additional parameters
      p_tedata <- readGDX(gdx, name = "p_tedata", field = "l", format = "first_found")
      o_autocons <- p_tedata[, , "autocons"]
      o_cb_coeff <- p_tedata[, , "cb_coeff"]
      o_cv_coeff <- p_tedata[, , "cv_coeff"]

      o_autocons <- collapseDim(limesMapping(o_autocons),  dim  =  3.1)
      o_cb_coeff <- collapseDim(limesMapping(o_cb_coeff),  dim  =  3.1)
      o_cv_coeff <- collapseDim(limesMapping(o_cv_coeff),  dim  =  3.1)

      varList_el <- list(
        #2.a) CHP
        "Capacity|Electricity|CHP (GW)"                = c(techp),
        "Capacity|Electricity|CHP|Biomass (GW)"        = intersect(techp, tebio),
        "Capacity|Electricity|CHP|Waste (GW)"          = intersect(techp, tewaste),
        "Capacity|Electricity|CHP|Coal (GW)"           = intersect(techp, c(tecoal, telig)),
        "Capacity|Electricity|CHP|Hard Coal (GW)"      = intersect(techp, c(tecoal)),
        "Capacity|Electricity|CHP|Lignite (GW)"        = intersect(techp, c(telig)),
        "Capacity|Electricity|CHP|Oil (GW)"            = intersect(techp, c(teoil)),
        "Capacity|Electricity|CHP|Gas (GW)"            = intersect(techp, c(tegas)),
        "Capacity|Electricity|CHP|Gas CC (GW)"         = intersect(techp, c(tengcc_el)),
        "Capacity|Electricity|CHP|Gas OC (GW)"         = intersect(techp, setdiff(tegas_el, tengcc_el)),
        "Capacity|Electricity|CHP|Hydrogen (GW)"       = intersect(techp, tehgen),
        "Capacity|Electricity|CHP|Other (GW)"          = intersect(techp, c(teothers)),
        "Capacity|Electricity|CHP|Other Fossil (GW)"   = intersect(techp, c(teothers, tewaste, teoil)),
        "Capacity|Electricity|CHP|Fossil (GW)"         = intersect(techp, c(tefossil)),
        "Capacity|Electricity|CHP|Renewable (GW)"      = intersect(techp, c(ter, ternofluc)),
        "Capacity|Electricity|CHP|Non-renewable (GW)"  = intersect(techp, tenr)
      )

      for (var in names(varList_el)){
        tmp2 <- mbind(tmp2, setNames(dimSums(v_cap[, , varList_el[[var]]], dim = 3), var)) #In limes CHP capacities are gross,  need to estimate net capacities
      }

      varList_el <- list(
        #2.b) Electricity-only
        "Capacity|Electricity|Electricity-only (GW)"                   = c(teoel),
        "Capacity|Electricity|Electricity-only|Biomass (GW)"           = intersect(teoel, tebio),
        "Capacity|Electricity|Electricity-only|Biomass|w/o CCS (GW)"   = intersect(teoel, setdiff(tebio, teccs)),
        "Capacity|Electricity|Electricity-only|Coal (GW)"              = intersect(teoel, c(tecoal, telig)),
        "Capacity|Electricity|Electricity-only|Coal|w/o CCS (GW)"      = intersect(teoel, setdiff(c(tecoal, telig), teccs)),
        "Capacity|Electricity|Electricity-only|Coal|w/ CCS (GW)"       = intersect(teoel, intersect(c(tecoal, telig), teccs)),
        "Capacity|Electricity|Electricity-only|Hard Coal (GW)"         = intersect(teoel, c(tecoal)),
        "Capacity|Electricity|Electricity-only|Hard Coal|w/o CCS (GW)" = intersect(teoel, setdiff(c(tecoal), teccs)),
        "Capacity|Electricity|Electricity-only|Hard Coal|w/ CCS (GW)"  = intersect(teoel, intersect(c(tecoal), teccs)),
        "Capacity|Electricity|Electricity-only|Lignite (GW)"           = intersect(teoel, c(telig)),
        "Capacity|Electricity|Electricity-only|Lignite|w/o CCS (GW)"   = intersect(teoel, setdiff(c(telig), teccs)),
        "Capacity|Electricity|Electricity-only|Lignite|w/ CCS (GW)"    = intersect(teoel, intersect(c(telig), teccs)),
        "Capacity|Electricity|Electricity-only|Oil (GW)"               = intersect(teoel, c(teoil)),
        "Capacity|Electricity|Electricity-only|Gas (GW)"               = intersect(teoel, c(tegas)),
        "Capacity|Electricity|Electricity-only|Gas|w/o CCS (GW)"       = intersect(teoel, setdiff(tegas_el, teccs)),
        "Capacity|Electricity|Electricity-only|Gas|w/ CCS (GW)"        = intersect(teoel, intersect(tegas_el, teccs)),
        "Capacity|Electricity|Electricity-only|Gas CC (GW)"            = intersect(teoel, c(tengcc_el)),
        "Capacity|Electricity|Electricity-only|Gas OC (GW)"            = intersect(teoel, setdiff(tegas_el, tengcc_el)),
        "Capacity|Electricity|Electricity-only|Other (GW)"             = intersect(teoel, c(teothers)),
        "Capacity|Electricity|Electricity-only|Hydrogen (GW)"          = intersect(teoel, c(tehgen)),
        "Capacity|Electricity|Electricity-only|Waste (GW)"             = intersect(teoel, c(tewaste)),
        "Capacity|Electricity|Electricity-only|Other Fossil (GW)"      = intersect(teoel, c(teothers, tewaste, teoil)),
        "Capacity|Electricity|Electricity-only|Fossil (GW)"            = intersect(teoel, c(tefossil)),
        "Capacity|Electricity|Electricity-only|Fossil|w/o CCS (GW)"    = intersect(teoel, setdiff(tefossil, teccs)),
        "Capacity|Electricity|Electricity-only|Fossil|w/ CCS (GW)"     = intersect(teoel, intersect(tefossil, teccs)),
        "Capacity|Electricity|Electricity-only|Renewable (GW)"         = intersect(teoel, c(ter, ternofluc)),
        "Capacity|Electricity|Electricity-only|Non-renewable (GW)"     = intersect(teoel, tenr) #this does not include storage
      )

      for (var in names(varList_el)){
        tmp2 <- mbind(tmp2, setNames(dimSums(v_cap[, , varList_el[[var]]],  dim = 3),  var))
      }

      #Heat-related capacities
      varList_he <- list(
        #1.a) Only-heat (centralized boilers)
        "Capacity|Gross|Heat|District Heating|Heat-only (GW)"                                = c(teohecen),
        "Capacity|Gross|Heat|District Heating|Heat-only|Biomass (GW)"                        = intersect(teohecen, tebio),
        "Capacity|Gross|Heat|District Heating|Heat-only|Coal (GW)"                           = intersect(teohecen, c(tecoal, telig)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Hard Coal (GW)"                      = intersect(teohecen, c(tecoal)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Lignite (GW)"                        = intersect(teohecen, c(telig)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Oil (GW)"                            = intersect(teohecen, c(teoil)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Gas (GW)"                            = intersect(teohecen, c(tegas)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Other (GW)"                          = intersect(teohecen, c(teothers)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Waste (GW)"                          = intersect(teohecen, c(tewaste)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Other Fossil (GW)"                   = intersect(teohecen, c(teothers, tewaste, teoil)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Electricity (GW)"                    = intersect(teohecen, c(tedhelec)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Electricity|Heat Pump (GW)"          = intersect(teohecen, "hp_large"),
        "Capacity|Gross|Heat|District Heating|Heat-only|Electricity|Electric Boiler (GW)"    = intersect(teohecen, "elboil_large"),
        "Capacity|Gross|Heat|District Heating|Heat-only|Solar (GW)"                          = intersect(teohecen, c("sol_heat")),
        "Capacity|Gross|Heat|District Heating|Heat-only|Geothermal (GW)"                     = intersect(teohecen, c("geo_heat")),
        "Capacity|Gross|Heat|District Heating|Heat-only|Fossil (GW)"                         = intersect(teohecen, c(tefossil)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Renewable (GW)"                      = intersect(teohecen, c(ter, ternofluc)),
        "Capacity|Gross|Heat|District Heating|Heat-only|Non-renewable (GW)"                  = intersect(teohecen, tenr)

        ##1.b) District Heating
        #"Capacity|Heat|District Heating (GW)"                              = c(tedh),
        #"Capacity|Heat|District Heating|Biomass (GW)"                      = intersect(tedh, tebio),
        #"Capacity|Heat|District Heating|Coal (GW)"                         = intersect(tedh, c(tecoal, telig)),
        #"Capacity|Heat|District Heating|Hard Coal (GW)"                    = intersect(tedh, c(tecoal)),
        #"Capacity|Heat|District Heating|Lignite (GW)"                      = intersect(tedh, c(telig)),
        #"Capacity|Heat|District Heating|Oil (GW)"                          = intersect(tedh, c(teoil)),
        #"Capacity|Heat|District Heating|Gas (GW)"                          = intersect(tedh, c(tegas)),
        #"Capacity|Heat|District Heating|Other (GW)"                        = intersect(tedh, c(teothers)),
        #"Capacity|Heat|District Heating|Waste (GW)"                        = intersect(tedh, c(tewaste)),
        #"Capacity|Heat|District Heating|Other Fossil (GW)"                 = intersect(tedh, c(teothers, tewaste, teoil)),
        #"Capacity|Heat|District Heating|Electricity (GW)"                  = intersect(tedh, c(tedhelec)),
        #"Capacity|Heat|District Heating|Electricity|Heat Pump (GW)"        = intersect(tedh, "hp_large"),
        #"Capacity|Heat|District Heating|Electricity|Electric Boiler (GW)"  = intersect(tedh, "elboil_large"),
        #"Capacity|Heat|District Heating|Solar (GW)"                        = intersect(tedh, c("sol_heat")),
        #"Capacity|Heat|District Heating|Geothermal (GW)"                   = intersect(tedh, c("geo_heat")),
        #"Capacity|Heat|District Heating|Fossil (GW)"                       = intersect(tedh, c(tefossil)),
        #"Capacity|Heat|District Heating|Renewable (GW)"                    = intersect(tedh, c(ter, ternofluc)),
        #"Capacity|Heat|District Heating|Non-renewable (GW)"                = intersect(tedh, tenr)

      )

      if("hgen_heat" %in% tehgen) { #Hydrogen is a new technology, need to ensure it works with older versions
        varList_he <- c(
          varList_he,
          list("Capacity|Gross|Heat|District Heating|Heat-only|Hydrogen (GW)"                       = intersect(teohecen, c(tehgen)))
        )
      }

      for (var in names(varList_he)){
        tmp2 <- mbind(tmp2, setNames(dimSums(v_cap[, , varList_he[[var]]], dim = 3), var))
      }

      varList_he <- list(
        #1.b) CHP
        "Capacity|Gross|Heat|District Heating|CHP (GW)"                                          = c(techp),
        "Capacity|Gross|Heat|District Heating|CHP|Biomass (GW)"                                  = intersect(techp, tebio),
        "Capacity|Gross|Heat|District Heating|CHP|Waste (GW)"                                    = intersect(techp, tewaste),
        "Capacity|Gross|Heat|District Heating|CHP|Coal (GW)"                                     = intersect(techp, c(tecoal, telig)),
        "Capacity|Gross|Heat|District Heating|CHP|Hard Coal (GW)"                                = intersect(techp, c(tecoal)),
        "Capacity|Gross|Heat|District Heating|CHP|Lignite (GW)"                                  = intersect(techp, c(telig)),
        "Capacity|Gross|Heat|District Heating|CHP|Oil (GW)"                                      = intersect(techp, c(teoil)),
        "Capacity|Gross|Heat|District Heating|CHP|Gas (GW)"                                      = intersect(techp, c(tegas)),
        "Capacity|Gross|Heat|District Heating|CHP|Gas CC (GW)"                                   = intersect(techp, c(tengcc_el)),
        "Capacity|Gross|Heat|District Heating|CHP|Gas OC (GW)"                                   = intersect(techp, setdiff(tegas_el, tengcc_el)),
        "Capacity|Gross|Heat|District Heating|CHP|Hydrogen (GW)"                                 = intersect(techp, c(tehgen)),
        "Capacity|Gross|Heat|District Heating|CHP|Other (GW)"                                    = intersect(techp, c(teothers)),
        "Capacity|Gross|Heat|District Heating|CHP|Other Fossil (GW)"                             = intersect(techp, c(teothers, tewaste, teoil)),
        "Capacity|Gross|Heat|District Heating|CHP|Fossil (GW)"                                   = intersect(techp, c(tefossil)),
        "Capacity|Gross|Heat|District Heating|CHP|Renewable (GW)"                                = intersect(techp, c(ter, ternofluc)),
        "Capacity|Gross|Heat|District Heating|CHP|Non-renewable (GW)"                            = intersect(techp, tenr)
      )

      for (var in names(varList_he)){
        tmp2 <- mbind(tmp2, setNames(dimSums((v_cap[, , varList_he[[var]]]/(1-o_autocons[, , varList_he[[var]]]))*(1/(o_cb_coeff[, , varList_he[[var]]] + o_cv_coeff[, , varList_he[[var]]])), dim = 3), var))
      }

      if(c_buildings  ==  1) {
        varList_he <- list(
          #1.c) Decentralized heating (only electricity-based)
          "Capacity|Heat|Decentralized|P2H (GW)"                          = c(tehedec),
          "Capacity|Heat|Decentralized|Heat Pump (GW)"                    = intersect(tehedec, c("hp_sh_dec", "hp_wh_dec")),
          "Capacity|Heat|Decentralized|Resistance (GW)"                   = intersect(tehedec, "resheat_dec"),
          "Capacity|Heat|Decentralized|Conventional (GW)"                 = intersect(tehedec, c("convheat_dec", "convwh_dec")),
          "Capacity|Heat|Decentralized|Conventional space heater (GW)"    = intersect(tehedec, "convheat_dec"),
          "Capacity|Heat|Decentralized|Conventional water heater (GW)"    = intersect(tehedec, "convwh_dec")
        )

        for (var in names(varList_he)){
          tmp2 <- mbind(tmp2, setNames(dimSums(v_cap[, , varList_he[[var]]], dim = 3), var))
        }

      }

    }

    #Biomass w/ CCS
    tmp2 <- mbind(tmp2, setNames(dimSums(v_cap[, , intersect(tebio, teccs)], dim = 3), "Capacity|Electricity|Biomass|w/ CCS (GW)"))
  }

  #combine aggregated capacity with brake-down of technologies
  tmp3 <- mbind(tmp1, tmp2)

  # append global values to the national ones
  tmp4 <- NULL

  #combine aggregated capacity with brake-down of technologies
  tmp5 <- mbind(tmp4, tmp3)

  #Reserves
  #These were also included in reportAdequacyContribution,  but here they appear under a different name
  tmp6 <- NULL
  tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve, dim = 3), "Capacity|Electricity|Reserve Plants (GW)"))
  tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c(tecoal)], dim = 3), "Capacity|Electricity|Reserve Plants|Hard Coal (GW)"))
  tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c(telig)], dim = 3), "Capacity|Electricity|Reserve Plants|Lignite (GW)"))
  tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c(tecoal, telig)], dim = 3), "Capacity|Electricity|Reserve Plants|Coal (GW)"))
  tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c(tegas_el)], dim = 3), "Capacity|Electricity|Reserve Plants|Gas (GW)"))
  tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c(tengcc)], dim = 3), "Capacity|Electricity|Reserve Plants|Gas CC (GW)"))
  tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c("ngt")], dim = 3), "Capacity|Electricity|Reserve Plants|Gas OC (GW)"))
  tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c(tebio)], dim = 3), "Capacity|Electricity|Reserve Plants|Biomass (GW)"))
  tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c(teoil)], dim = 3), "Capacity|Electricity|Reserve Plants|Oil (GW)"))
  tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c(tewaste)], dim = 3), "Capacity|Electricity|Reserve Plants|Waste (GW)"))

  #when there is exogenous heating
  if(c_LIMESversion >=  2.33) {
    if(heating == "fullDH") {
      #CHP
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c(techp)], dim = 3), "Capacity|Electricity|Reserve Plants|CHP (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , intersect(c(tecoal, telig), techp)], dim = 3), "Capacity|Electricity|Reserve Plants|CHP|Coal (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , intersect(tecoal, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|CHP|Hard Coal (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , intersect(telig, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|CHP|Lignite (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , intersect(tegas_el, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|CHP|Gas (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , intersect(tengcc_el, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|CHP|Gas CC (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , c("ngt_chp")], dim = 3), "Capacity|Electricity|Reserve Plants|CHP|Gas OC (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , intersect(tehgen, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|CHP|Hydrogen (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , intersect(tebio, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|CHP|Biomass (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , intersect(teoil, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|CHP|Oil (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , intersect(tewaste, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|CHP|Waste (GW)"))
      #Electricity-only
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , setdiff(tereserve, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|Electricity-only (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , setdiff(c(tecoal, telig), techp)], dim = 3), "Capacity|Electricity|Reserve Plants|Electricity-only|Coal (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , setdiff(tecoal, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|Electricity-only|Hard Coal (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , setdiff(telig, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|Electricity-only|Lignite (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , setdiff(tegas_el, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|Electricity-only|Gas (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , setdiff(tengcc_el, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|Electricity-only|Gas CC (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , setdiff(tegas_el, tengcc_el)], dim = 3), "Capacity|Electricity|Reserve Plants|Electricity-only|Gas OC (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , setdiff(tebio, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|Electricity-only|Biomass (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , setdiff(teoil, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|Electricity-only|Oil (GW)"))
      tmp6 <- mbind(tmp6, setNames(dimSums(v_capreserve[, , setdiff(tewaste, techp)], dim = 3), "Capacity|Electricity|Reserve Plants|Electricity-only|Waste (GW)"))
    }
  }

  tmp7 <- mbind(tmp5[, as.numeric(tt), ], tmp6)

  #Energy storage (reservoir) capacity and ratios
  tmp8 <- NULL
  if(c_LIMESversion >=  2.34) {
    v_storecap <- readGDX(gdx, name = "v_storecap", field = "l", format = "first_found")[, , testore]
    v_storecap <- limesMapping(v_storecap)

    varList_st <- list(
      "Capacity|Electricity|Storage Reservoir (GWh)"                        = setdiff(testore, c("heat_sto")),
      "Capacity|Electricity|Storage Reservoir|Intra-day (GWh)"              = c("psp","batteries"),
      "Capacity|Electricity|Storage Reservoir|Pump Hydro (GWh)"             = "psp",
      "Capacity|Electricity|Storage Reservoir|Stat Batteries (GWh)"         = "batteries",
      "Capacity|Electricity|Storage Reservoir|Hydrogen electrolysis (GWh)"  = "helec"
    )

    for (var in names(varList_st)){
      tmp8 <- mbind(tmp8, setNames(dimSums(v_storecap[, , varList_st[[var]]], dim = 3), var))
    }


    #Number of storing hours
    tmp8 <- mbind(tmp8, setNames(v_storecap[, , c("psp")]/v_cap[, , c("psp")], "Discharge duration|Pump Hydro (h)"))
    tmp8 <- mbind(tmp8, setNames(v_storecap[, , c("batteries")]/v_cap[, , c("batteries")], "Discharge duration|Stat Batteries (h)"))
    tmp8 <- mbind(tmp8, setNames(v_storecap[, , c("helec")]/v_cap[, , c("helec")], "Discharge duration|Hydrogen electrolysis (h)"))

    if(heating == "fullDH") {
      tmp8 <- mbind(tmp8, setNames(dimSums(v_cap[, , c("heat_sto")], dim = 3), "Capacity|Heat|Storage (GW)"))
      tmp8 <- mbind(tmp8, setNames(dimSums(v_storecap[, , c("heat_sto")], dim = 3), "Capacity|Heat|Storage Reservoir (GWh)"))
    }

  }

  #combine aggregated capacity with brake-down of technologies
  tmp9 <- mbind(tmp7, tmp8[, as.numeric(tt), ])

  tmp10 <- NULL
  c_DACCS <- readGDX(gdx, name = c("c_DACCS"), field = "l", format = "first_found", react = 'silent') #heat peak demand in buildings
  if(!is.null(c_DACCS)) {
    if(c_DACCS >= 1) {
      tedaccs <- readGDX(gdx, name = "tedaccs")

      varList_daccs <- list(
        "Capacity|Carbon removal|DACCS (Mt CO2/yr)"                           = c(tedaccs),
        "Capacity|Carbon removal|DACCS|Liquid solvent (Mt CO2/yr)"            = "liquid_daccs",
        "Capacity|Carbon removal|DACCS|Solid solvent (Mt CO2/yr)"             = "solid_daccs",
        "Capacity|Carbon removal|DACCS|CaO ambient weathering (Mt CO2/yr)"    = "caow_daccs"
      )

      for (var in names(varList_daccs)){ #Data is in MtC/yr, convert to MtCO2/yr
        tmp10 <- mbind(tmp10, setNames(dimSums(v_cap[, , varList_daccs[[var]]] * 44/12,  dim = 3),  var))
      }

    }
  }



  tmp <- mbind(tmp9, tmp10[, as.numeric(tt), ])

  return(tmp)
}
