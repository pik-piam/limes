#' Read in GDX and calculate industry emissions, used in convGDX2MIF.R for the reporting
#'
#' Read in emissions data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the emission variables
#' @author Sebastian Osorio
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportIndustryModule(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#'
reportIndustryModule <- function(gdx) {

  .tmp <- NULL

  # read switch for new industry module
  c_NewIndustry <- readGDX(gdx,name="c_NewIndustry",field="l",format="first_found", react = 'silent')


  if(!is.null(c_NewIndustry)) {
    if(c_NewIndustry >= 1) {

      # read sets
      tt <- readGDX(gdx, name = "t")
      sec_ind <- readGDX(gdx, name = "sec_ind")
      proc_ind <- readGDX(gdx, name = "proc_ind")
      sec2proc <- readGDX(gdx, name = "sec2proc")

      #Read parameters
      s_c2co2 <- readGDX(gdx,name = "s_c2co2",field = "l",format = "first_found") #conversion factor C -> CO2

      #Read variables
      v_Prod_Industry <- readGDX(gdx,name="v_Prod_Industry",field="l",format="first_found")
      o_Emi_IndProc <- readGDX(gdx,name="o_Emi_IndProc",field="l",format="first_found")
      v_Capacity_Industry <- readGDX(gdx,name="v_Capacity_Industry",field="l",format="first_found")
      v_deltaCap_Industry <- readGDX(gdx,name="v_deltaCap_Industry",field="l",format="first_found")
      o_FuelCons_IndProc <- readGDX(gdx,name="o_FuelCons_IndProc",field="l",format="first_found")
      o_InputCons_IndProc <- readGDX(gdx,name="o_InputCons_IndProc",field="l",format="first_found")

      # create MagPie object of v_cap with iso3 regions
      v_Prod_Industry <- limesMapping(v_Prod_Industry)
      o_Emi_IndProc <- limesMapping(o_Emi_IndProc)
      v_Capacity_Industry <- limesMapping(v_Capacity_Industry)
      v_deltaCap_Industry <- limesMapping(v_deltaCap_Industry)
      o_FuelCons_IndProc <- limesMapping(o_FuelCons_IndProc)
      o_InputCons_IndProc <- limesMapping(o_InputCons_IndProc)

      ##Steel

      #Sets of process per sector
      proc_steel <- c(sec2proc$proc_ind[sec2proc$sec_ind == "steel"])

      varList_steel <- list(
        #Conventional
        "Steel"                                                                 = c(proc_steel),
        "Steel|Blast Furnace-Basic Oxygen Furnace"                              = c("BF_BOF","BF_BOF_refurb"),
        "Steel|Electric Arc Furnace"                                            = c("EAF","EAF_refurb"),
        "Steel|Direct Reduced Iron Electric Arc Furnace"                        = c("DRI_EAF_NG","DRI_EAF_H2","DRI_EAF_NG_nocapex","DRI_EAF_H2_nocapex"),
        "Steel|Direct Reduced Iron Electric Arc Furnace with Natural gas"       = c("DRI_EAF_NG","DRI_EAF_NG_nocapex"),
        "Steel|Direct Reduced Iron Electric Arc Furnace with Hydrogen"          = c("DRI_EAF_H2","DRI_EAF_H2_nocapex")
      )

      .tmp1 <- NULL
      #Capacity
      for (var in names(varList_steel)){
        .tmp1 <- mbind(.tmp1, setNames(
          dimSums(v_Capacity_Industry[, , varList_steel[[var]]], dim = 3),
                                       paste0("Capacity|",var," (Million ton)")))
      }

      #Capacity additions
      for (var in names(varList_steel)){
        .tmp1 <- mbind(.tmp1, setNames(
          dimSums(v_deltaCap_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Capacity Additions|",var," (Million ton/yr)")))
      }

      # concatenate vars
      .tmp <- mbind(.tmp,.tmp1[, as.numeric(tt), ])

      .tmp2 <- NULL

      #Production
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Production|",var," (Million ton/yr)")))
      }

      #Emissions
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_Emi_IndProc[, , varList_steel[[var]]], dim = 3) * as.numeric(s_c2co2) * 1000,
          paste0("Emissions|CO2|Industry|",var," (Mt CO2/yr)")))
      }

      # concatenate vars
      .tmp <- mbind(.tmp,.tmp2)


    }
  }



  return(.tmp)
}

