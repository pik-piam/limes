#' Read in GDX and calculate capacity additions, used in convGDX2MIF.R for the reporting
#'
#' Read in capacity additions from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportCapacityAdditions(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#'

reportCapacityAdditions <- function(gdx) {

  # read sets
  teel <- readGDX(gdx,name="teel") #set of electricity generation technologies (non-storage)
  ter <- readGDX(gdx,name="ter") #set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx,name="ternofluc") #set of non-variable (non-fluctuating) renewable electricity generation technologies
  tefossil <- readGDX(gdx,name="tefossil") #set of fossil-based electricity generation technologies
  tenr <- readGDX(gdx,name="tenr") #set of non-renewable electricity generation technologies (includes storage)
  tecoal <- readGDX(gdx,name="tecoal")
  telig <- readGDX(gdx,name="telig")
  tegas <- readGDX(gdx,name="tegas")
  tengcc <- readGDX(gdx,name="tengcc")
  tehgen <- readGDX(gdx,name="tehgen")
  tehydro <- readGDX(gdx,name="tehydro")
  tebio <- readGDX(gdx,name="tebio")
  teoil <- readGDX(gdx,name="teoil")
  techp <- readGDX(gdx,name="techp")
  teccs <- readGDX(gdx,name="teccs")
  testore <- readGDX(gdx,name="testore")
  teothers <- readGDX(gdx,name="teothers")
  tegas_el <- intersect(tegas,teel)
  tengcc_el <- intersect(tengcc,teel)
  tewaste <- readGDX(gdx, name = "tewaste", format = "first_found", react = 'silent') # set of waste generation technologies
  if(is.null(tewaste)) {tewaste <- "waste"} #in old model versions this set was not defined and only the tech 'waste' existed

  # Read parameters
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  c_buildings <- readGDX(gdx, name = c("c_buildings", "report_c_buildings"),
                         field = "l", format = "first_found") #switch on buildings module

  # read variables
  v_deltacap <- readGDX(gdx,name="v_deltacap",field="l",format="first_found")

  # create MagPie object of v_deltacap with iso3 regions
  v_deltacap <- limesMapping(v_deltacap)

  # total installed capacity
  tmp1 <- NULL
  #for (tech in teel) {
  #  tmp1 <- mbind(tmp1,setNames(v_deltacap[,,tech],paste("Capacity Additions|Electricity|",tech,"(GW/yr)")))
  #}

  #aggregated technologies (w/o CHP) - depending on LIMES version, they will be considered
  varList_el <- list(
    #Conventional
    "Capacity Additions|Electricity (GW/yr)"                        =c(teel),
    "Capacity Additions|Electricity|Biomass (GW/yr)"                =intersect(teel,tebio),
    "Capacity Additions|Electricity|Biomass|w/o CCS (GW/yr)"        =intersect(teel,setdiff(tebio,teccs)),
    "Capacity Additions|Electricity|Coal (GW/yr)"                   =intersect(teel,c(tecoal,telig)),
    "Capacity Additions|Electricity|Coal|w/o CCS (GW/yr)"           =intersect(teel,setdiff(c(tecoal,telig),teccs)),
    "Capacity Additions|Electricity|Coal|w/ CCS (GW/yr)"            =intersect(teel,intersect(c(tecoal,telig),teccs)),
    "Capacity Additions|Electricity|Hard Coal (GW/yr)"              =intersect(teel,c(tecoal)),
    "Capacity Additions|Electricity|Hard Coal|w/o CCS (GW/yr)"      =intersect(teel,setdiff(c(tecoal),teccs)),
    "Capacity Additions|Electricity|Hard Coal|w/ CCS (GW/yr)"       =intersect(teel,intersect(c(tecoal),teccs)),
    "Capacity Additions|Electricity|Lignite (GW/yr)"                =intersect(teel,c(telig)),
    "Capacity Additions|Electricity|Lignite|w/o CCS (GW/yr)"        =intersect(teel,setdiff(c(telig),teccs)),
    "Capacity Additions|Electricity|Lignite|w/ CCS (GW/yr)"         =intersect(teel,intersect(c(telig),teccs)),
    "Capacity Additions|Electricity|Oil (GW/yr)"                    =intersect(teel,c(teoil)),
    "Capacity Additions|Electricity|Gas (GW/yr)"                    =intersect(teel,c(tegas)),
    "Capacity Additions|Electricity|Gas|w/o CCS (GW/yr)"            =intersect(teel,setdiff(tegas_el,teccs)),
    "Capacity Additions|Electricity|Gas|w/ CCS (GW/yr)"             =intersect(teel,intersect(tegas_el,teccs)),
    "Capacity Additions|Electricity|Gas CC|w/o CCS (GW/yr)"         =intersect(teel,setdiff(tengcc_el,teccs)),
    "Capacity Additions|Electricity|Gas CC|w/ CCS (GW/yr)"          =intersect(teel,intersect(tengcc_el,teccs)),
    "Capacity Additions|Electricity|Gas CC (GW/yr)"                 =intersect(teel,c(tengcc_el)),
    "Capacity Additions|Electricity|Gas OC (GW/yr)"                 =intersect(teel,setdiff(tegas_el,tengcc_el)),
    "Capacity Additions|Electricity|Other (GW/yr)"                  =intersect(teel,c(teothers)),
    "Capacity Additions|Electricity|Hydrogen (GW/yr)"               =intersect(teel,c(tehgen)),
    "Capacity Additions|Electricity|Hydrogen FC (GW/yr)"            =intersect(teel,c("hfc")),
    "Capacity Additions|Electricity|Hydrogen OC (GW/yr)"            =intersect(teel,c("hct")),
    "Capacity Additions|Electricity|Hydrogen CC (GW/yr)"            =intersect(teel,c("hcc")),
    "Capacity Additions|Electricity|Nuclear (GW/yr)"                =intersect(teel,c("tnr")),
    "Capacity Additions|Electricity|Waste (GW/yr)"                  =intersect(teel,c(tewaste)),
    "Capacity Additions|Electricity|Other Fossil (GW/yr)"           =intersect(teel,c(teothers,tewaste,teoil)),

    #general aggregation
    "Capacity Additions|Electricity|Fossil (GW/yr)"                 =intersect(teel,c(tefossil)),
    "Capacity Additions|Electricity|Fossil|w/o CCS (GW/yr)"         =intersect(teel,setdiff(tefossil,teccs)),
    "Capacity Additions|Electricity|Fossil|w/ CCS (GW/yr)"          =intersect(teel,intersect(tefossil,teccs)),
    "Capacity Additions|Electricity|Variable renewable (GW/yr)"     =intersect(teel,c(ter)),
    "Capacity Additions|Electricity|Non-variable renewable (GW/yr)" =intersect(teel,c(ternofluc)),
    "Capacity Additions|Electricity|Renewable (GW/yr)"              =intersect(teel,c(ter,ternofluc)),
    "Capacity Additions|Electricity|Non-renewable (GW/yr)"          =intersect(teel,tenr), #this does not include storage

    #Renewable
    "Capacity Additions|Electricity|Wind (GW/yr)"                                          =intersect(teel,c("windon","windoff")),
    "Capacity Additions|Electricity|Wind|Onshore (GW/yr)"                                  =intersect(teel,c("windon")),
    "Capacity Additions|Electricity|Wind|Offshore (GW/yr)"                                 =intersect(teel,c("windoff")),
    "Capacity Additions|Electricity|Solar (GW/yr)"                                         =intersect(teel,c("spv","csp")),
    "Capacity Additions|Electricity|Solar|PV (GW/yr)"                                      =intersect(teel,c("spv")),
    "Capacity Additions|Electricity|Solar|CSP (GW/yr)"                                     =intersect(teel,c("csp")),
    "Capacity Additions|Electricity|Hydro (GW/yr)"                                         =intersect(teel,c(tehydro)),
    #"Capacity Additions|Electricity|Hydro|Hydro storage (GW/yr)"        =intersect(teel,"hs"),
    #"Capacity Additions|Electricity|Hydro|Run of river (GW/yr)"        =intersect(teel,"ror")

    #Storage
    "Capacity Additions|Electricity|Storage (GW/yr)"                                       =setdiff(testore,c("heat_sto")),
    "Capacity Additions|Electricity|Storage|Stat Batteries (GW/yr)"                        =c("batteries"),
    "Capacity Additions|Electricity|Storage|Pump Hydro (GW/yr)"                            =c("psp"),
    "Capacity Additions|Electricity|Storage|Hydrogen electrolysis [input] (GW/yr)"         =c("helec"),
    "Capacity Additions|Electricity|Storage|Intra-day (GW/yr)"                             =c("batteries","psp")
  )

  tmp2 <- NULL
  for (var in names(varList_el)){
    tmp2 <- mbind(tmp2,setNames(dimSums(v_deltacap[,,varList_el[[var]]],dim=3),var))
  }

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
        "Capacity Additions|Electricity|CHP (GW/yr)"                = c(techp),
        "Capacity Additions|Electricity|CHP|Biomass (GW/yr)"        = intersect(techp, tebio),
        "Capacity Additions|Electricity|CHP|Waste (GW/yr)"          = intersect(techp, tewaste),
        "Capacity Additions|Electricity|CHP|Coal (GW/yr)"           = intersect(techp, c(tecoal, telig)),
        "Capacity Additions|Electricity|CHP|Hard Coal (GW/yr)"      = intersect(techp, c(tecoal)),
        "Capacity Additions|Electricity|CHP|Lignite (GW/yr)"        = intersect(techp, c(telig)),
        "Capacity Additions|Electricity|CHP|Oil (GW/yr)"            = intersect(techp, c(teoil)),
        "Capacity Additions|Electricity|CHP|Gas (GW/yr)"            = intersect(techp, c(tegas)),
        "Capacity Additions|Electricity|CHP|Gas CC (GW/yr)"         = intersect(techp, c(tengcc_el)),
        "Capacity Additions|Electricity|CHP|Gas OC (GW/yr)"         = intersect(techp, setdiff(tegas_el, tengcc_el)),
        "Capacity Additions|Electricity|CHP|Hydrogen (GW/yr)"       = intersect(techp, tehgen),
        "Capacity Additions|Electricity|CHP|Other (GW/yr)"          = intersect(techp, c(teothers)),
        "Capacity Additions|Electricity|CHP|Other Fossil (GW/yr)"   = intersect(techp, c(teothers, tewaste, teoil)),
        "Capacity Additions|Electricity|CHP|Fossil (GW/yr)"         = intersect(techp, c(tefossil)),
        "Capacity Additions|Electricity|CHP|Renewable (GW/yr)"      = intersect(techp, c(ter, ternofluc)),
        "Capacity Additions|Electricity|CHP|Non-renewable (GW/yr)"  = intersect(techp, tenr)
      )

      for (var in names(varList_el)){
        tmp2 <- mbind(tmp2, setNames(dimSums((v_deltacap[, , varList_el[[var]]]), dim = 3), var)) #In limes CHP capacities are gross,  need to estimate net capacities
      }

      varList_el <- list(
        #2.b) Electricity-only
        "Capacity Additions|Electricity|Electricity-only (GW/yr)"                   = c(teoel),
        "Capacity Additions|Electricity|Electricity-only|Biomass (GW/yr)"           = intersect(teoel, tebio),
        "Capacity Additions|Electricity|Electricity-only|Biomass|w/o CCS (GW/yr)"   = intersect(teoel, setdiff(tebio, teccs)),
        "Capacity Additions|Electricity|Electricity-only|Coal (GW/yr)"              = intersect(teoel, c(tecoal, telig)),
        "Capacity Additions|Electricity|Electricity-only|Coal|w/o CCS (GW/yr)"      = intersect(teoel, setdiff(c(tecoal, telig), teccs)),
        "Capacity Additions|Electricity|Electricity-only|Coal|w/ CCS (GW/yr)"       = intersect(teoel, intersect(c(tecoal, telig), teccs)),
        "Capacity Additions|Electricity|Electricity-only|Hard Coal (GW/yr)"         = intersect(teoel, c(tecoal)),
        "Capacity Additions|Electricity|Electricity-only|Hard Coal|w/o CCS (GW/yr)" = intersect(teoel, setdiff(c(tecoal), teccs)),
        "Capacity Additions|Electricity|Electricity-only|Hard Coal|w/ CCS (GW/yr)"  = intersect(teoel, intersect(c(tecoal), teccs)),
        "Capacity Additions|Electricity|Electricity-only|Lignite (GW/yr)"           = intersect(teoel, c(telig)),
        "Capacity Additions|Electricity|Electricity-only|Lignite|w/o CCS (GW/yr)"   = intersect(teoel, setdiff(c(telig), teccs)),
        "Capacity Additions|Electricity|Electricity-only|Lignite|w/ CCS (GW/yr)"    = intersect(teoel, intersect(c(telig), teccs)),
        "Capacity Additions|Electricity|Electricity-only|Oil (GW/yr)"               = intersect(teoel, c(teoil)),
        "Capacity Additions|Electricity|Electricity-only|Gas (GW/yr)"               = intersect(teoel, c(tegas)),
        "Capacity Additions|Electricity|Electricity-only|Gas|w/o CCS (GW/yr)"       = intersect(teoel, setdiff(tegas_el, teccs)),
        "Capacity Additions|Electricity|Electricity-only|Gas|w/ CCS (GW/yr)"        = intersect(teoel, intersect(tegas_el, teccs)),
        "Capacity Additions|Electricity|Electricity-only|Gas CC (GW/yr)"            = intersect(teoel, c(tengcc_el)),
        "Capacity Additions|Electricity|Electricity-only|Gas OC (GW/yr)"            = intersect(teoel, setdiff(tegas_el, tengcc_el)),
        "Capacity Additions|Electricity|Electricity-only|Other (GW/yr)"             = intersect(teoel, c(teothers)),
        "Capacity Additions|Electricity|Electricity-only|Hydrogen (GW/yr)"          = intersect(teoel, c(tehgen)),
        "Capacity Additions|Electricity|Electricity-only|Waste (GW/yr)"             = intersect(teoel, c(tewaste)),
        "Capacity Additions|Electricity|Electricity-only|Other Fossil (GW/yr)"      = intersect(teoel, c(teothers, tewaste, teoil)),
        "Capacity Additions|Electricity|Electricity-only|Fossil (GW/yr)"            = intersect(teoel, c(tefossil)),
        "Capacity Additions|Electricity|Electricity-only|Fossil|w/o CCS (GW/yr)"    = intersect(teoel, setdiff(tefossil, teccs)),
        "Capacity Additions|Electricity|Electricity-only|Fossil|w/ CCS (GW/yr)"     = intersect(teoel, intersect(tefossil, teccs)),
        "Capacity Additions|Electricity|Electricity-only|Renewable (GW/yr)"         = intersect(teoel, c(ter, ternofluc)),
        "Capacity Additions|Electricity|Electricity-only|Non-renewable (GW/yr)"     = intersect(teoel, tenr) #this does not include storage
      )

      for (var in names(varList_el)){
        tmp2 <- mbind(tmp2, setNames(dimSums(v_deltacap[, , varList_el[[var]]],  dim = 3),  var))
      }

      #Heat-related capacities
      varList_he <- list(
        #1.a) Only-heat (centralized boilers)
        "Capacity Additions|Gross|Heat|District Heating|Heat-only (GW/yr)"                                = c(teohecen),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Biomass (GW/yr)"                        = intersect(teohecen, tebio),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Coal (GW/yr)"                           = intersect(teohecen, c(tecoal, telig)),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Hard Coal (GW/yr)"                      = intersect(teohecen, c(tecoal)),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Lignite (GW/yr)"                        = intersect(teohecen, c(telig)),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Oil (GW/yr)"                            = intersect(teohecen, c(teoil)),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Gas (GW/yr)"                            = intersect(teohecen, c(tegas)),

        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Other (GW/yr)"                          = intersect(teohecen, c(teothers)),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Waste (GW/yr)"                          = intersect(teohecen, c(tewaste)),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Other Fossil (GW/yr)"                   = intersect(teohecen, c(teothers, tewaste, teoil)),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Electricity (GW/yr)"                    = intersect(teohecen, c(tedhelec)),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Electricity|Heat Pump (GW/yr)"          = intersect(teohecen, "hp_large"),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Electricity|Electric Boiler (GW/yr)"    = intersect(teohecen, "elboil_large"),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Solar (GW/yr)"                          = intersect(teohecen, c("sol_heat")),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Geothermal (GW/yr)"                     = intersect(teohecen, c("geo_heat")),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Fossil (GW/yr)"                         = intersect(teohecen, c(tefossil)),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Renewable (GW/yr)"                      = intersect(teohecen, c(ter, ternofluc)),
        "Capacity Additions|Gross|Heat|District Heating|Heat-only|Non-renewable (GW/yr)"                  = intersect(teohecen, tenr)
      )
      if("hgen_heat" %in% tehgen) { #Hydrogen is a new technology, need to ensure it works with older versions
        varList_he <- c(
          varList_he,
          list("Capacity Additions|Gross|Heat|District Heating|Heat-only|Hydrogen (GW/yr)"                       = intersect(teohecen, c(tehgen)))
        )
      }

      for (var in names(varList_he)){
        tmp2 <- mbind(tmp2, setNames(dimSums(v_deltacap[, , varList_he[[var]]], dim = 3), var))
      }

      varList_he <- list(
        #1.b) CHP
        "Capacity Additions|Gross|Heat|District Heating|CHP (GW/yr)"                  = c(techp),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Biomass (GW/yr)"          = intersect(techp, tebio),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Waste (GW/yr)"            = intersect(techp, tewaste),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Coal (GW/yr)"             = intersect(techp, c(tecoal, telig)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Hard Coal (GW/yr)"        = intersect(techp, c(tecoal)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Lignite (GW/yr)"          = intersect(techp, c(telig)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Oil (GW/yr)"              = intersect(techp, c(teoil)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Gas (GW/yr)"              = intersect(techp, c(tegas)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Gas CC (GW/yr)"           = intersect(techp, c(tengcc_el)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Gas OC (GW/yr)"           = intersect(techp, setdiff(tegas_el, tengcc_el)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Hydrogen (GW/yr)"         = intersect(techp, c(tehgen)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Other (GW/yr)"            = intersect(techp, c(teothers)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Other Fossil (GW/yr)"     = intersect(techp, c(teothers, tewaste, teoil)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Fossil (GW/yr)"           = intersect(techp, c(tefossil)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Renewable (GW/yr)"        = intersect(techp, c(ter, ternofluc)),
        "Capacity Additions|Gross|Heat|District Heating|CHP|Non-renewable (GW/yr)"    = intersect(techp, tenr)
      )

      for (var in names(varList_he)){
        tmp2 <- mbind(tmp2, setNames(
          dimSums(
          (v_deltacap[, , varList_he[[var]]] / (1 - o_autocons[, , varList_he[[var]]]))
          *(1 / (o_cb_coeff[, , varList_he[[var]]] + o_cv_coeff[, , varList_he[[var]]]))
          , dim = 3)
          , var)
          )
      }

      if(c_buildings  ==  1) {
        varList_he <- list(
          #1.c) Decentralized heating (only electricity-based)
          "Capacity Additions|Heat|Decentralized|P2H (GW/yr)"                          = c(tehedec),
          "Capacity Additions|Heat|Decentralized|Heat Pump (GW/yr)"                    = intersect(tehedec, c("hp_sh_dec", "hp_wh_dec")),
          "Capacity Additions|Heat|Decentralized|Heat Pump space heating (GW/yr)"      = intersect(tehedec, c("hp_sh_dec")),
          "Capacity Additions|Heat|Decentralized|Heat Pump water heating (GW/yr)"      = intersect(tehedec, c("hp_wh_dec")),
          "Capacity Additions|Heat|Decentralized|Resistance (GW/yr)"                   = intersect(tehedec, "resheat_dec"),
          "Capacity Additions|Heat|Decentralized|Conventional (GW/yr)"                 = intersect(tehedec, c("convheat_dec", "convwh_dec")),
          "Capacity Additions|Heat|Decentralized|Conventional space heater (GW/yr)"    = intersect(tehedec, "convheat_dec"),
          "Capacity Additions|Heat|Decentralized|Conventional water heater (GW/yr)"    = intersect(tehedec, "convwh_dec")
        )

        for (var in names(varList_he)) {
          tmp2 <- mbind(tmp2, setNames(dimSums(v_deltacap[, , varList_he[[var]]], dim = 3), var))
        }

      }

    }

    #Biomass w/ CCS
    tmp2 <- mbind(tmp2, setNames(dimSums(v_deltacap[, , intersect(tebio, teccs)], dim = 3), "Capacity Additions|Electricity|Biomass|w/ CCS (GW/yr)"))
  }

  if(c_LIMESversion >=  2.34) {
    v_deltastorecap <- readGDX(gdx, name = "v_deltastorecap", field = "l", format = "first_found")[, , testore]
    v_deltastorecap <- limesMapping(v_deltastorecap)

    varList_st <- list(
      "Capacity Additions|Electricity|Storage Reservoir (GWh/yr)"                        = setdiff(testore, c("heat_sto")),
      "Capacity Additions|Electricity|Storage Reservoir|Intra-day (GWh/yr)"              = c("psp","batteries"),
      "Capacity Additions|Electricity|Storage Reservoir|Pump Hydro (GWh/yr)"             = "psp",
      "Capacity Additions|Electricity|Storage Reservoir|Stat Batteries (GWh/yr)"         = "batteries",
      "Capacity Additions|Electricity|Storage Reservoir|Hydrogen electrolysis (GWh/yr)"  = "helec"
    )

    for (var in names(varList_st)){
      tmp2 <- mbind(tmp2, setNames(dimSums(v_deltastorecap[, , varList_st[[var]]], dim = 3), var))
    }

    if(heating == "fullDH") {
      tmp2 <- mbind(tmp2, setNames(dimSums(v_deltacap[, , c("heat_sto")], dim = 3), "Capacity Additions|Heat|Storage (GW/yr)"))
      tmp2 <- mbind(tmp2, setNames(dimSums(v_deltastorecap[, , c("heat_sto")], dim = 3), "Capacity Additions|Heat|Storage Reservoir (GWh/yr)"))
    }

  }

  #DACCS
  c_DACCS <- readGDX(gdx, name = c("c_DACCS"), field = "l", format = "first_found", react = 'silent') #heat peak demand in buildings
  if(!is.null(c_DACCS)) {
    if(c_DACCS >= 1) {
      tedaccs <- readGDX(gdx, name = "tedaccs")

      varList_daccs <- list(
        "Capacity Additions|Carbon removal|DACCS (Mt CO2/yr)"                           = c(tedaccs),
        "Capacity Additions|Carbon removal|DACCS|Liquid solvent (Mt CO2/yr)"            = "liquid_daccs",
        "Capacity Additions|Carbon removal|DACCS|Solid sorbent (Mt CO2/yr)"             = "solid_daccs",
        "Capacity Additions|Carbon removal|DACCS|CaO ambient weathering (Mt CO2/yr)"    = "caow_daccs"
      )

      for (var in names(varList_daccs)){ #Data is in MtC/yr, convert to MtCO2/yr
        tmp2 <- mbind(tmp2, setNames(dimSums(v_deltacap[, , varList_daccs[[var]]] * 44/12,  dim = 3),  var))
      }

    }
  }

  #concatenate
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}

