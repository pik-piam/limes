#' Read in GDX and calculate primary energy,  used in convGDX2MIF.R for the reporting
#'
#' Read in primary energy data from GDX file,  information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX,  or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio,  Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportPrimaryEnergy(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie getItems collapseDim
#' @export
#'
reportPrimaryEnergy <- function(gdx) {

  # read sets
  te <- readGDX(gdx, name = "te")
  teel <- readGDX(gdx, name = "teel")
  tehe <- readGDX(gdx, name = "tehe")
  tecoal <- readGDX(gdx, name = "tecoal")
  telig <- readGDX(gdx, name = "telig")
  tegas <- readGDX(gdx, name = "tegas")
  tengcc <- readGDX(gdx, name = "tengcc")
  tefossil <- readGDX(gdx, name = "tefossil") #set of fossil-based generation technologies
  teccs <- readGDX(gdx, name = "teccs") #set of generation technologies|with CCS
  tehgen <- readGDX(gdx, name = "tehgen") #set of hydrogen generation technologies
  tebio <- readGDX(gdx, name = "tebio") #set of biomass generation technologies
  teoil <- readGDX(gdx, name = "teoil") #set of oil generation technologies
  teothers <- readGDX(gdx, name = "teothers") #set of other gases generation technologies
  tenr <- readGDX(gdx, name = "tenr") #set of non-renewable generation technologies
  tegas_el <- intersect(tegas, teel)
  petyex <- readGDX(gdx, name = "petyex")
  tau <- readGDX(gdx, name = "tau") #set of time slices
  pe2se <- readGDX(gdx, name = "pe2se")
  pe2se <- paste0(pe2se[, 1], ".", pe2se[, 2], ".", pe2se[, 3])
  tewaste <- readGDX(gdx, name = "tewaste", format = "first_found", react = 'silent') # set of waste generation technologies
  if(is.null(tewaste)) {tewaste <- "waste"} #in old model versions this set was not defined and only the tech 'waste' existed

  # read parameters and variables
  c_LIMESversion <- readGDX(gdx, name = "c_LIMESversion", field = "l", format = "first_found")
  c_buildings <- readGDX(gdx, name = c("c_buildings", "report_c_buildings"),
                         field = "l", format = "first_found") #switch on buildings module
  p_taulength <- readGDX(gdx, name = c("p_taulength", "pm_taulength"), field = "l", format = "first_found")[, , tau]
  v_pedem <- readGDX(gdx, name = c("v_pedem", "vm_pedem"), field = "l", format = "first_found", restore_zeros  =  FALSE)

  if(length(grep("peel", getNames(v_pedem))) > 0) {
    v_pedem <- v_pedem[, , c(petyex, "peel")]
  } else {
    v_pedem <- v_pedem[, , c(petyex)]
  }

  # create MagPie object of v_pedem|with iso3 regions
  v_pedem <- limesMapping(v_pedem)

  #Check names in v_pedem - in one of the v2.37 versions,  v_pedem becomes annual,  so we need first to aggregate to annual value
  if(length(grep("1[.]", getNames(v_pedem))) > 0) { #check if tau is part of the names
    v_pedem <- dimSums(v_pedem*p_taulength, dim  =  3.1,  na.rm  =  T)
  }

  #create magpie for PE for heating purposes
  v_pedem_he <- new.magpie(cells_and_regions  =  getItems(v_pedem,  dim  =  1),  years  =  getYears(v_pedem),  names  =  NULL,
             fill  =  NA,  sort  =  FALSE,  sets  =  NULL)

  #Check the version so to choose the electricity-related variables
  if(c_LIMESversion >=  2.28) {
    v_pedem_el <- v_pedem[, , "seel"]

    heating <- .readHeatingCfg(gdx)
    if(heating == "fullDH") {
      v_pedem_he <- v_pedem[, , "sehe"]
      v_pedem_he <- collapseDim(v_pedem_he,  dim  =  3.2)
    }
  } else {
    v_pedem_el <- v_pedem

  }

  v_pedem_el <- collapseDim(v_pedem_el,  dim  =  3.2)


  #PRIMARY ENERGY FOR vRES and hydro is in reportGeneration

  #use of exhaustible primary energy types per country
  #and convert from GWh to TWh
  tmp1 <- NULL

  varList_el <- list(
    "Primary Energy|Exhaustible resources|Electricity (TWh/yr)"    = c(petyex),  #all
    "Primary Energy|Biomass|Electricity (TWh/yr)"                  = intersect(teel, tebio),
    "Primary Energy|Biomass|Electricity|w/o CCS (TWh/yr)"          = intersect(teel, setdiff(tebio, teccs)),
    "Primary Energy|Coal|Electricity (TWh/yr)"                     = intersect(teel, c(tecoal, telig)),
    "Primary Energy|Coal|Electricity|w/o CCS (TWh/yr)"             = intersect(teel, setdiff(c(tecoal, telig), teccs)),
    "Primary Energy|Coal|Electricity|w/ CCS (TWh/yr)"              = intersect(teel, intersect(c(tecoal, telig), teccs)),
    "Primary Energy|Hard Coal|Electricity (TWh/yr)"                = intersect(teel, c(tecoal)),
    "Primary Energy|Hard Coal|Electricity|w/o CCS (TWh/yr)"        = intersect(teel, setdiff(c(tecoal), teccs)),
    "Primary Energy|Hard Coal|Electricity|w/ CCS (TWh/yr)"         = intersect(teel, intersect(c(tecoal), teccs)),
    "Primary Energy|Lignite|Electricity (TWh/yr)"                  = intersect(teel, c(telig)),
    "Primary Energy|Lignite|Electricity|w/o CCS (TWh/yr)"          = intersect(teel, setdiff(c(telig), teccs)),
    "Primary Energy|Lignite|Electricity|w/ CCS (TWh/yr)"           = intersect(teel, intersect(c(telig), teccs)),
    "Primary Energy|Oil|Electricity (TWh/yr)"                      = intersect(teel, c(teoil)),
    "Primary Energy|Gas|Electricity (TWh/yr)"                      = intersect(teel, c(tegas)),
    "Primary Energy|Gas|Electricity|w/o CCS (TWh/yr)"              = intersect(teel, setdiff(tegas_el, teccs)),
    "Primary Energy|Gas|Electricity|w/ CCS (TWh/yr)"               = intersect(teel, intersect(tegas_el, teccs)),
    "Primary Energy|Fossil|Electricity (TWh/yr)"                   = intersect(teel, c(tefossil)),
    "Primary Energy|Fossil|Electricity|w/o CCS (TWh/yr)"           = intersect(teel, setdiff(tefossil, teccs)),
    "Primary Energy|Fossil|Electricity|w/ CCS (TWh/yr)"            = intersect(teel, intersect(tefossil, teccs)),
    "Primary Energy|Other|Electricity (TWh/yr)"                    = intersect(teel, c(teothers)),
    "Primary Energy|Hydrogen|Electricity (TWh/yr)"                 = intersect(teel, c(tehgen)),
    "Primary Energy|Nuclear|Electricity (TWh/yr)"                  = intersect(teel, c("tnr")),
    "Primary Energy|Waste|Electricity (TWh/yr)"                    = intersect(teel, c(tewaste))
  )

  for (var in names(varList_el)) {
    tmp1 <- mbind(tmp1, setNames(dimSums(v_pedem_el[, , varList_el[[var]]], dim = 3)/1000, var))
  }

  if(c_LIMESversion >=  2.33) {
    tewaste <- readGDX(gdx, name = "tewaste") #set of|waste generation technologies from this version

    #Electricity (new technologies)
    tmp1 <- mbind(tmp1, setNames(dimSums(v_pedem_el[, , intersect(tebio, teccs)], dim = 3)/1000, "Primary Energy|Biomass|Electricity|w/ CCS (TWh/yr)"))
  }


  tmp2 <- NULL
  #when there is endogenous heating
  if(c_LIMESversion >=  2.38) {

    if(heating == "fullDH") {

      #Load additional sets
      techp <- readGDX(gdx, name = "techp") #set of chp generation technologies
      teoel <- readGDX(gdx, name = "teoel") #set of electricity-only generation technologies
      tedh <- readGDX(gdx, name = "tedh") #set of district heating generation technologies
      tehedec <- readGDX(gdx, name = "tehedec") #set of (electric) decentralized heating generation technologies
      teheelec <- readGDX(gdx, name = "teheelec") #set of P2H generation technologies

      #Heat
      varList_he <- list(
        #"Primary Energy|Heat (TWh/yr)"                                                      = NA,
        "Primary Energy|Exhaustible resources|Heat (TWh/yr)"                                = setdiff(petyex, c("pehgen",  "peur")),
        "Primary Energy|Biomass|Heat (TWh/yr)"                                              = intersect(tebio, tehe),
        "Primary Energy|Coal|Heat (TWh/yr)"                                                 = intersect(c(tecoal, telig), tehe),
        "Primary Energy|Hard Coal|Heat (TWh/yr)"                                            = intersect(tecoal, tehe),
        "Primary Energy|Lignite|Heat (TWh/yr)"                                              = intersect(telig, tehe),
        "Primary Energy|Oil|Heat (TWh/yr)"                                                  = intersect(teoil, tehe),
        "Primary Energy|Gas|Heat (TWh/yr)"                                                  = intersect(tegas, tehe),
        "Primary Energy|Fossil|Heat (TWh/yr)"                                               = intersect(tefossil, tehe),
        "Primary Energy|Other|Heat (TWh/yr)"                                                = intersect(teothers, tehe),
        "Primary Energy|Waste|Heat (TWh/yr)"                                                = intersect(tewaste, tehe)
      )
      for (var in names(varList_he)){
        tmp2 <- mbind(tmp2, setNames(dimSums(v_pedem_he[, , varList_he[[var]]], dim = 3)/1000, var))
      }


      #Electricity and Heat
      varList_elhe <- list(
        "Primary Energy|Exhaustible resources|Electricity and Heat (TWh/yr)"    = c(petyex),
        "Primary Energy|Biomass|Electricity and Heat (TWh/yr)"                  = c(tebio),
        "Primary Energy|Coal|Electricity and Heat (TWh/yr)"                     = c(tecoal, telig),
        "Primary Energy|Hard Coal|Electricity and Heat (TWh/yr)"                = c(tecoal),
        "Primary Energy|Lignite|Electricity and Heat (TWh/yr)"                  = c(telig),
        "Primary Energy|Oil|Electricity and Heat (TWh/yr)"                      = c(teoil),
        "Primary Energy|Gas|Electricity and Heat (TWh/yr)"                      = c(tegas),
        "Primary Energy|Waste|Electricity and Heat (TWh/yr)"                    = c(tewaste),
        "Primary Energy|Fossil|Electricity and Heat (TWh/yr)"                   = c(tefossil),
        "Primary Energy|Nuclear|Electricity and Heat (TWh/yr)"                  = c("tnr"),
        "Primary Energy|Other|Electricity and Heat (TWh/yr)"                    = c(teothers),
        "Primary Energy|Hydrogen|Electricity and Heat (TWh/yr)"                 = c(tehgen),

        #CHP
        "Primary Energy|Exhaustible resources|CHP (TWh/yr)"    = intersect(techp, te),
        "Primary Energy|Biomass|CHP (TWh/yr)"                  = intersect(techp, tebio),
        "Primary Energy|Coal|CHP (TWh/yr)"                     = intersect(techp, c(tecoal, telig)),
        "Primary Energy|Hard Coal|CHP (TWh/yr)"                = intersect(techp, tecoal),
        "Primary Energy|Lignite|CHP (TWh/yr)"                  = intersect(techp, telig),
        "Primary Energy|Oil|CHP (TWh/yr)"                      = intersect(techp, teoil),
        "Primary Energy|Gas|CHP (TWh/yr)"                      = intersect(techp, tegas),
        "Primary Energy|Gas|CHP|Gas CC (TWh/yr)"               = intersect(techp, tengcc),
        "Primary Energy|Gas|CHP|Gas OC (TWh/yr)"               = intersect(techp, setdiff(tegas, tengcc)),
        "Primary Energy|Fossil|CHP (TWh/yr)"                   = intersect(techp, tefossil),
        "Primary Energy|Other|CHP (TWh/yr)"                    = intersect(techp, teothers),
        "Primary Energy|Waste|CHP (TWh/yr)"                    = intersect(techp, tewaste),

        #Electricity-only
        "Primary Energy|Exhaustible resources|Electricity-only (TWh/yr)"    = intersect(teoel, c(tenr, tebio, tehgen)),
        "Primary Energy|Biomass|Electricity-only (TWh/yr)"                  = intersect(teoel, tebio),
        "Primary Energy|Coal|Electricity-only (TWh/yr)"                     = intersect(teoel, c(tecoal, telig)),
        "Primary Energy|Hard Coal|Electricity-only (TWh/yr)"                = intersect(teoel, tecoal),
        "Primary Energy|Lignite|Electricity-only (TWh/yr)"                  = intersect(teoel, telig),
        "Primary Energy|Oil|Electricity-only (TWh/yr)"                      = intersect(teoel, teoil),
        "Primary Energy|Gas|Electricity-only (TWh/yr)"                      = intersect(teoel, tegas),
        "Primary Energy|Gas|Electricity-only|Gas CC (TWh/yr)"               = intersect(teoel, tengcc),
        "Primary Energy|Gas|Electricity-only|Gas OC (TWh/yr)"               = intersect(teoel, setdiff(tegas, tengcc)),
        "Primary Energy|Fossil|Electricity-only (TWh/yr)"                   = intersect(teoel, tefossil),
        "Primary Energy|Other|Electricity-only (TWh/yr)"                    = intersect(teoel, teothers),
        "Primary Energy|Waste|Electricity-only (TWh/yr)"                    = intersect(teoel, tewaste),
        "Primary Energy|Hydrogen|Electricity-only (TWh/yr)"                 = intersect(teoel, tehgen)

      )
      for (var in names(varList_elhe)){
        tmp2 <- mbind(tmp2, setNames(dimSums(v_pedem[, , varList_elhe[[var]]], dim = 3)/1000, var))
      }

      if(length(grep("peel", getNames(v_pedem))) > 0) {
        #Heat
        varList_he <- list(
          #Electricity use in heating
          "Secondary Energy Input|Electricity|District Heating|Heat (TWh/yr)"                           = intersect(tedh, c(teheelec)),
          "Secondary Energy Input|Electricity|District Heating|Heat Pump|Heat (TWh/yr)"                 = intersect(tedh, "hp_large"),
          "Secondary Energy Input|Electricity|District Heating|Electric Boiler|Heat (TWh/yr)"           = intersect(tedh, "elboil_large")
        )
        for (var in names(varList_he)){
          tmp2 <- mbind(tmp2, setNames(dimSums(v_pedem_he[, , varList_he[[var]]], dim = 3)/1000, var))
        }

        if(c_buildings  ==  1) {
          varList_he <- list(

            #Electricity use in heating
            "Secondary Energy Input|Electricity|Heat (TWh/yr)"                                            = intersect(tehe, c(teheelec)),
            "Secondary Energy Input|Electricity|Decentralized|Heat (TWh/yr)"                              = intersect(tehedec, teheelec),
            "Secondary Energy Input|Electricity|Decentralized|Heat Pump|Heat (TWh/yr)"                    = intersect(tehedec, c("hp_sh_dec", "hp_wh_dec")),
            "Secondary Energy Input|Electricity|Decentralized|Conventional|Heat (TWh/yr)"                 = intersect(tehedec, c("convheat_dec", "convwh_dec")),
            "Secondary Energy Input|Electricity|Decentralized|Resistance|Heat (TWh/yr)"                   = intersect(tehedec, "resheat_dec"),
            "Secondary Energy Input|Electricity|Decentralized|Conventional space heater|Heat (TWh/yr)"    = intersect(tehedec, "convheat_dec"),
            "Secondary Energy Input|Electricity|Decentralized|Conventional water heater|Heat (TWh/yr)"    = intersect(tehedec, "convwh_dec")
          )

          for (var in names(varList_he)){
            tmp2 <- mbind(tmp2, setNames(dimSums(v_pedem_he[, , varList_he[[var]]], dim = 3)/1000, var))
          }
        }
      }


    }
  }


  # add global values
  tmp <- mbind(tmp1, tmp2)

  return(tmp)
}

