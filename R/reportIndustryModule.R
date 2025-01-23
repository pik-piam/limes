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
      sec2te <- readGDX(gdx, name = "sec2te")

      #Read parameters
      s_c2co2 <- readGDX(gdx,name = "s_c2co2",field = "l",format = "first_found") #conversion factor C -> CO2
      t0 <- tt[1]
      p_ts <- readGDX(gdx, name = "p_ts", field = "l", format = "first_found") #time step
      c_esmdisrate <- readGDX(gdx, name = "c_esmdisrate", field = "l", format = "first_found") #discount rate

      #Read variables
      v_Prod_Industry <- readGDX(gdx,name="v_Prod_Industry",field="l",format="first_found")
      o_Emi_IndProc <- readGDX(gdx,name="o_Emi_IndProc",field="l",format="first_found")
      v_Capacity_Industry <- readGDX(gdx,name="v_Capacity_Industry",field="l",format="first_found")
      v_deltaCap_Industry <- readGDX(gdx,name="v_deltaCap_Industry",field="l",format="first_found")
      o_FuelCons_IndProc <- readGDX(gdx,name="o_FuelCons_IndProc",field="l",format="first_found")
      o_InputCons_IndProc <- readGDX(gdx,name="o_InputCons_IndProc",field="l",format="first_found")
      o_LCOP_IndProc <- readGDX(gdx,name="o_LCOP_IndProc",field="l",format="first_found")
      o_ProdCostMarg_IndProc <- readGDX(gdx,name="o_ProdCostMarg_IndProc",field="l",format="first_found")
      o_ProdCost_IndProc <- readGDX(gdx,name="o_ProdCost_IndProc",field="l",format="first_found")
      o_ProdCost_IndProc_CAPEX <- readGDX(gdx,name="o_ProdCost_IndProc_CAPEX",field="l",format="first_found")
      o_ProdCost_IndProc_Elec <- readGDX(gdx,name="o_ProdCost_IndProc_Elec",field="l",format="first_found")
      o_ProdCost_IndProc_emi <- readGDX(gdx,name="o_ProdCost_IndProc_emi",field="l",format="first_found")
      o_ProdCost_IndProc_FuelPECost <- readGDX(gdx,name="o_ProdCost_IndProc_FuelPECost",field="l",format="first_found")
      o_ProdCost_IndProc_H2 <- readGDX(gdx,name="o_ProdCost_IndProc_H2",field="l",format="first_found")
      o_ProdCost_IndProc_InputMat <- readGDX(gdx,name="o_ProdCost_IndProc_InputMat",field="l",format="first_found")
      o_ProdCost_IndProc_OMCost <- readGDX(gdx,name="o_ProdCost_IndProc_OMCost",field="l",format="first_found")


      # create MagPie object of v_cap with iso3 regions
      v_Prod_Industry <- limesMapping(v_Prod_Industry)
      o_Emi_IndProc <- limesMapping(o_Emi_IndProc)
      v_Capacity_Industry <- limesMapping(v_Capacity_Industry)
      v_deltaCap_Industry <- limesMapping(v_deltaCap_Industry)
      o_FuelCons_IndProc <- limesMapping(o_FuelCons_IndProc)
      o_InputCons_IndProc <- limesMapping(o_InputCons_IndProc)
      o_LCOP_IndProc <- limesMapping(o_LCOP_IndProc)
      o_ProdCostMarg_IndProc <- limesMapping(o_ProdCostMarg_IndProc)
      o_ProdCost_IndProc <- limesMapping(o_ProdCost_IndProc)
      o_ProdCost_IndProc_CAPEX <- limesMapping(o_ProdCost_IndProc_CAPEX)
      o_ProdCost_IndProc_Elec <- limesMapping(o_ProdCost_IndProc_Elec)
      o_ProdCost_IndProc_emi <- limesMapping(o_ProdCost_IndProc_emi)
      o_ProdCost_IndProc_FuelPECost <- limesMapping(o_ProdCost_IndProc_FuelPECost)
      o_ProdCost_IndProc_H2 <- limesMapping(o_ProdCost_IndProc_H2)
      o_ProdCost_IndProc_InputMat <- limesMapping(o_ProdCost_IndProc_InputMat)
      o_ProdCost_IndProc_OMCost <- limesMapping(o_ProdCost_IndProc_OMCost)

      #compute factor to discount average marginal values
      f_npv <- as.numeric(p_ts)*exp(-as.numeric(c_esmdisrate)*(as.numeric(tt)-as.numeric(t0)))

      ##########################################################################

      ##Steel

      #Sets of process per sector
      te_steel <- c(sec2te$te_ind[sec2te$sec_ind == "steel"])

      varList_steel <- list(
        #Conventional
        "Industry|Steel"                                                                 = c(te_steel),
        "Industry|Steel|Blast Furnace-Basic Oxygen Furnace"                              = c("BF_BOF_new","BF_BOF_reline"),
        "Industry|Steel|Electric Arc Furnace"                                            = c("EAF_new","EAF_refurb"),
        "Industry|Steel|Direct Reduced Iron Electric Arc Furnace"                        = c("DRI_EAF_NG_new","DRI_EAF_H2_new","DRI_EAF_NG_refurb","DRI_EAF_H2_refurb"),
        "Industry|Steel|Direct Reduced Iron Electric Arc Furnace with Natural gas"       = c("DRI_EAF_NG_new","DRI_EAF_NG_refurb"),
        "Industry|Steel|Direct Reduced Iron Electric Arc Furnace with Hydrogen"          = c("DRI_EAF_H2_new","DRI_EAF_H2_refurb"),
        "Industry|Steel|Primary route"                                                   = setdiff(te_steel,c("EAF_new","EAF_refurb"))
      )

      .tmp1 <- NULL #keep the following two here because their time granularity is different
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

      for (var in names(varList_steel)){
        #Production
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Production|",var," (Million ton/yr)")))

        #Idle capacity
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(
            v_Capacity_Industry[, as.numeric(tt), varList_steel[[var]]] - v_Prod_Industry[, , varList_steel[[var]]],
            dim = 3),
          paste0("Idle capacity|",var," (Million ton)")))

        #Emissions
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_Emi_IndProc[, , varList_steel[[var]]], dim = 3), #already in MtCO2
          paste0("Emissions|CO2|",var," (Mt CO2/yr)")))

        #Emission factor (no need to convert units, MtCO2 and Mton)
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_Emi_IndProc[, , varList_steel[[var]]], dim = 3) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Emission intensity|CO2|",var," (tCO2/ton)")))

        #Capacity factor
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3) /
            dimSums(v_Capacity_Industry[, as.numeric(tt), varList_steel[[var]]], dim = 3),
          paste0("Capacity factor|",var," (--)")))

        ##Fuel consumption
        #Gas
        o_FuelCons_IndProc_gas <- o_FuelCons_IndProc[,,"pegas"]
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_FuelCons_IndProc_gas[, , varList_steel[[var]]], dim = 3) / 1000, #from GWh to TWh
          paste0("Primary Energy|Gas|",var," (TWh/yr)")))

        #Hard coal
        o_FuelCons_IndProc_coal <- o_FuelCons_IndProc[,,"pecoal"]
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_FuelCons_IndProc_coal[, , varList_steel[[var]]], dim = 3) / 1000, #from GWh to TWh
          paste0("Primary Energy|Hard Coal|",var," (TWh/yr)")))

        #Electricity
        o_FuelCons_IndProc_elec <- o_FuelCons_IndProc[,,"peel"]
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_FuelCons_IndProc_elec[, , varList_steel[[var]]], dim = 3) / 1000, #from GWh to TWh
          paste0("Secondary Energy Input|Electricity|",var," (TWh/yr)")))

        #Hydrogen
        o_FuelCons_IndProc_H2 <- o_FuelCons_IndProc[,,"pehgen"]
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_FuelCons_IndProc_H2[, , varList_steel[[var]]], dim = 3) / 1000, #from GWh to TWh
          paste0("Secondary Energy Input|Hydrogen|",var," (TWh/yr)")))

        #Coke
        o_InputCons_IndProc_coke <- o_InputCons_IndProc[,,"coke"]
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_InputCons_IndProc_coke[, , varList_steel[[var]]], dim = 3) / 1000, #from GWh to TWh
          paste0("Primary Energy|Coke|",var," (TWh/yr)")))

        #Scrap
        o_InputCons_IndProc_scrap <- o_InputCons_IndProc[,,"scrap"]
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_InputCons_IndProc_scrap[, , varList_steel[[var]]], dim = 3), #already in million ton
          paste0("Material Input|Scrap|",var," (Million ton/yr)")))

        #levelised costs
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_LCOP_IndProc[, , varList_steel[[var]]] * v_Prod_Industry[, , varList_steel[[var]]], dim = 3) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Levelised cost plant built in t|",var," (Eur2010/ton)")))

      }

      #This could be moved up too, but it is easier to keep it here while we check it is correct
      #Marginal costs
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_ProdCostMarg_IndProc[, , varList_steel[[var]]] * v_Prod_Industry[, , varList_steel[[var]]], dim = 3) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Marginal production costs|",var," (Eur2010/ton)")))
      }
      #Production cost
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_ProdCost_IndProc[, , varList_steel[[var]]] * v_Prod_Industry[, , varList_steel[[var]]], dim = 3, na.rm = T) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Production costs|",var," (Eur2010/ton)")))
      }
      #Production cost: Component CAPEX
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_ProdCost_IndProc_CAPEX[, , varList_steel[[var]]] * v_Prod_Industry[, , varList_steel[[var]]], dim = 3, na.rm = T) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Production costs|Component CAPEX|",var," (Eur2010/ton)")))
      }
      #Production cost: Component Electricity
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_ProdCost_IndProc_Elec[, , varList_steel[[var]]] * v_Prod_Industry[, , varList_steel[[var]]], dim = 3, na.rm = T) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Production costs|Component Electricity|",var," (Eur2010/ton)")))
      }
      #Production cost: Component Emissions
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_ProdCost_IndProc_emi[, , varList_steel[[var]]] * v_Prod_Industry[, , varList_steel[[var]]], dim = 3, na.rm = T) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Production costs|Component Emissions|",var," (Eur2010/ton)")))
      }
      #Production cost: Component Fossil fuel
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_ProdCost_IndProc_FuelPECost[, , varList_steel[[var]]] * v_Prod_Industry[, , varList_steel[[var]]], dim = 3, na.rm = T) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Production costs|Component Fossil fuel|",var," (Eur2010/ton)")))
      }
      #Production cost: Component Hydrogen
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_ProdCost_IndProc_H2[, , varList_steel[[var]]] * v_Prod_Industry[, , varList_steel[[var]]], dim = 3, na.rm = T) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Production costs|Component Hydrogen|",var," (Eur2010/ton)")))
      }
      #Production cost: Component Input material
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_ProdCost_IndProc_InputMat[, , varList_steel[[var]]] * v_Prod_Industry[, , varList_steel[[var]]], dim = 3, na.rm = T) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Production costs|Component Material Input|",var," (Eur2010/ton)")))
      }
      #Production cost: Component OM Cost
      for (var in names(varList_steel)){
        .tmp2 <- mbind(.tmp2, setNames(
          dimSums(o_ProdCost_IndProc_OMCost[, , varList_steel[[var]]] * v_Prod_Industry[, , varList_steel[[var]]],
                  dim = 3, na.rm = T) /
            dimSums(v_Prod_Industry[, , varList_steel[[var]]], dim = 3),
          paste0("Production costs|Component OM Cost|",var," (Eur2010/ton)")))
      }

      ##Cost of keeping a certain capacity level (currently at 2030 level)
      m_ProtectLocal_Steel <- readGDX(gdx,name="q_ProtectLocal_Steel",field="m",
                                      format="first_found", react = 'silent')

      if(!is.null(m_ProtectLocal_Steel)) {

        # create MagPie object of v_cap with iso3 regions
        m_ProtectLocal_Steel <- limesMapping(m_ProtectLocal_Steel)

        #Clean marginal
        m_incentiveProtect_Steel <- m_ProtectLocal_Steel[, , "steel"]
        m_incentiveProtect_Steel <- collapseDim(m_incentiveProtect_Steel, dim  = 3)

        #Discount marginal
        o_incentiveProtect_Steel_disc <- new.magpie(cells_and_regions  =  getItems(v_Prod_Industry, dim  = 1),
                                               years  =  getYears(v_Prod_Industry),  names  =  NULL,
                                               fill  =  0,  sort  =  FALSE,  sets  =  NULL)
        #o_incentiveProtect_Steel_disc <- NULL
        for (t2 in getYears(m_incentiveProtect_Steel)) {
          t2_pos <- match(t2,getYears(o_incentiveProtect_Steel_disc))
          o_incentiveProtect_Steel_disc[,t2,] <- m_incentiveProtect_Steel[, t2, ]/f_npv[t2_pos] #[Geur 2010/Million-ton]
        }

        #Report values
        #Per capacity installed
        .tmp2 <- mbind(.tmp2, setNames(o_incentiveProtect_Steel_disc * 1000, #convert from Geur/Mt to eur/t
                                       "Cost|Incentive capacity|Industry|Steel (Eur2010/ton-cap)"))

        #per produced unit
        .tmp2 <- mbind(.tmp2, setNames(o_incentiveProtect_Steel_disc * 1000 /
                                         .tmp2[,,"Capacity factor|Industry|Steel (--)"], #convert from Geur/Mt to eur/t
                                       "Cost|Incentive production|Industry|Steel (Eur2010/ton)"))
      }



      # concatenate vars
      .tmp <- mbind(.tmp,.tmp2)


    }
  }



  return(.tmp)
}

