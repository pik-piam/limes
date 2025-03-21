#' Read in GDX and calculate capacities, used in convGDX2MIF.R for the reporting
#'
#' Read in Capacity Adequacy information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the Capacity Adequacy variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportCapacity Adequacy(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie getItems collapseDim
#' @export
#'

reportAdequacyContribution <- function(gdx) {

  # read sets and parameters
  te <- readGDX(gdx,name="te")
  tentra <- readGDX(gdx,name="tentra")
  tenr <- readGDX(gdx,name="tenr")
  ter <- readGDX(gdx,name="ter")
  ternofluc <- readGDX(gdx,name="ternofluc")
  teel <- readGDX(gdx,name="teel")
  tau <- readGDX(gdx,name="tau")
  testore <- readGDX(gdx,name="testore")
  tegas <- readGDX(gdx,name="tegas") #set of gas generation technologies
  telig <- readGDX(gdx,name="telig") #set of lignite generation technologies
  tecoal <- readGDX(gdx,name="tecoal") #set of hard coal generation technologies
  tengcc <- readGDX(gdx,name="tengcc") #set of NGCC generation technologies
  tehgen <- readGDX(gdx,name="tehgen") #set of hydrogen generation technologies
  tehydro <- readGDX(gdx,name="tehydro") #set of hydropower generation technologies
  tebio <- readGDX(gdx,name="tebio") #set of biomass generation technologies
  teoil <- readGDX(gdx,name="teoil") #set of oil generation technologies
  teothers <- readGDX(gdx,name="teothers") #set of other gases generation technologies
  tereserve <- readGDX(gdx,name="tereserve") #set of other gases generation technologies
  tegas_el <- intersect(tegas,teel)
  tengcc_el <- intersect(tengcc,teel)
  ter_el <- intersect(teel,ter)
  tewaste <- readGDX(gdx, name = "tewaste", format = "first_found", react = 'silent') # set of waste generation technologies
  if(is.null(tewaste)) {tewaste <- "waste"} #in old model versions this set was not defined and only the tech 'waste' existed

  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  p_tedata <- readGDX(gdx,name="p_tedata",field="l",format="first_found")
  p_adeq_te <- readGDX(gdx,name="p_adeq_te",field="l",format="first_found") #adequacy factor for each technology
  p_adeq_imp <- readGDX(gdx,name="p_adeq_imp",field="l",format="first_found") #adequacy factor for net imports

  # read variables and marginal values
  v_exdemand <- readGDX(gdx,name="v_exdemand",field="l",format="first_found",restore_zeros = FALSE) #demand
  v_cap <- readGDX(gdx,name=c("v_cap","vm_cap"),field="l",format="first_found",restore_zeros = FALSE)
  v_seprodmax <- readGDX(gdx,name="v_seprodmax",field="l",format="first_found",restore_zeros = FALSE)
  v_seprod <- readGDX(gdx,name="v_seprod",field="l",format="first_found",restore_zeros = FALSE)
  v_storeout <- readGDX(gdx,name="v_storeout",field="l",format="first_found",restore_zeros = FALSE)
  v_storein <- readGDX(gdx,name="v_storein",field="l",format="first_found",restore_zeros = FALSE)
  v_capreserve <- readGDX(gdx,name="v_capreserve",field="l",format="first_found",restore_zeros = FALSE)
  m_robuststrategy2 <- readGDX(gdx,name="q_robuststrategy2",field="m",format="first_found",restore_zeros = FALSE)

  #Take only certain parameters
  p_tedata_nu <- p_tedata[,,"nu"]

  # create MagPie object with iso3 regions
  v_exdemand <- limesMapping(v_exdemand)[,,tau]
  p_tedata_nu <- limesMapping(p_tedata_nu)
  v_cap <- limesMapping(v_cap)
  v_seprodmax <- limesMapping(v_seprodmax)[,,tau]
  v_seprodmax <- v_seprodmax[,,ter_el]
  m_robuststrategy2 <- limesMapping(m_robuststrategy2)[,,tau]
  v_seprod <- limesMapping(v_seprod)[,,tau]
  v_storeout <- limesMapping(v_storeout)[,,tau]
  v_storein <- limesMapping(v_storein)[,,tau]
  v_capreserve <- limesMapping(v_capreserve)

  #Check the version so to choose the electricity-related variables
  if(c_LIMESversion >= 2.28) {
    if(length(grep("seel",getNames(v_exdemand))) > 0) {
      p_eldemand <- v_exdemand[,,"seel"] #then filter by SE
    } else {
      p_eldemand <- v_exdemand
    }

    v_seprod <- v_seprod[,,"seel"]

   heating <- .readHeatingCfg(gdx)
    if(heating == "fullDH") {
      m_robuststrategy2 <- m_robuststrategy2[,,"seel"]

    }

  } else {
    p_eldemand <- v_exdemand
  }

  if(c_LIMESversion >= 2.37) { #First version with heat storage: Robert's version had it. The official does not
    if(length(grep("heat_sto",getNames(v_storein))) > 0) {
      v_storein_el <- v_storein[,,"seel"]
      v_storein_el <- v_storein_el[,,setdiff(testore,c("heat_sto"))]
      v_storein_el <- collapseDim(v_storein_el, dim = 3.2)
    } else {
      v_storein_el <- v_storein
    }

    if(length(grep("heat_sto",getNames(v_storeout))) > 0) {
      v_storeout_el <- v_storeout[,,"seel"]
      v_storeout_el <- v_storeout_el[,,setdiff(testore,c("heat_sto"))]
      v_storeout_el <- collapseDim(v_storeout_el, dim = 3.2)
    } else {
      v_storeout_el <- v_storeout
    }

    #Redefine testore set -> only electricity-related sets make sense in this function
    testore_el <- setdiff(testore,c("heat_sto"))
    testore_eladeq <- setdiff(testore_el,c("helec")) #Electrolysers do not contribute to capacity adequacy

  } else {
    v_storein_el <- v_storein
    v_storeout_el <- v_storeout

    #Redefine testore set -> only electricity-related sets make sense in this function
    testore_el <- setdiff(testore,c("heat_sto"))
    testore_eladeq <- setdiff(testore_el,c("helec")) #Electrolysers do not contribute to capacity adequacy
  }




  #Identify the 'tau' in which the marginal value for the robust constraint peaks and when demand peaks
  #Need a magpie variable with regi and year
  taumax <- new.magpie(cells_and_regions = getItems(v_exdemand, dim = 1), years = getYears(v_exdemand), names = NULL,
                                         fill = 0, sort = FALSE, sets = NULL)
  taumax <- setNames(taumax, NULL)
  taupeak <- taumax

  for (regi2 in getItems(m_robuststrategy2, dim = 1)) {
    for (year2 in getYears(m_robuststrategy2)) { #2010 and 2015 should remain as zeros
      taupeak[regi2,year2,] <- match(as.magpie(apply(p_eldemand[regi2,year2,],1:2,max)),p_eldemand[regi2,year2,])
      if (dimSums(m_robuststrategy2[regi2,year2,],dim=3) == 0) {taumax[regi2,year2,] <- 0}
      else
        taumax[regi2,year2,] <- match(as.magpie(apply(m_robuststrategy2[regi2,year2,],1:2,max)),m_robuststrategy2[regi2,year2,])
    }
  }

  #Capacity Adequacy FOR CONVENTIONAL TECHNOLOGIES AND STORAGE

  #Need a magpie variable with the same sets as v_cap
  capadeq <- new.magpie(cells_and_regions = getItems(v_exdemand, dim = 1), years = getYears(v_exdemand), names = tentra,
                        fill = 0, sort = FALSE, sets = NULL)

  for (tentra2 in tentra) {
    capadeq[,,tentra2] <- v_cap[,getYears(capadeq),tentra2]*p_tedata_nu[,,tentra2]*as.numeric(p_adeq_te[,,tentra2])
  }

  #Contribution of aggregate technologies (most challenging)
  tmp1 <- NULL
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c("tnr")],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Nuclear (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c(tehydro)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Hydro (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,intersect(teel,c(tecoal,telig))],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Coal (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,intersect(teel,c(tecoal))],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Hard Coal (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,intersect(teel,c(telig))],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Lignite (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,intersect(teel,c(tegas_el))],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Gas (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,intersect(teel,c(tebio))],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Biomass (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,intersect(teel,c(teoil))],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Oil (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,intersect(teel,c(tewaste))],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Waste (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,intersect(teel,c(tehgen))],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Hydrogen (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,intersect(teel,c(teothers))],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Other (GW)"))

  #Discard the capacity when there is no marginal value
  for (regi2 in getItems(p_eldemand, dim = 1)) {
    for (year2 in getYears(p_eldemand)) { #2010 and 2015 should remain as zeros
      if (taumax[regi2,year2,] == 0)
        tmp1[regi2,year2,] = 0
    }
  }

  #Contribution of aggregate technologies (peak demand)
  tmp2 <- NULL
  names_tmp <- getNames(tmp1)
  names_tmp <- sub("Most Challenging","Peak Demand",names_tmp)
  tmp2<-tmp1
  getNames(tmp2)<-names_tmp
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("tnr")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Nuclear (GW)"))
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("hydro","ror","hs")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Hydro (GW)"))
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("pc","pc80","pc95","pc10","pcc","lpc","lpc80","lpc95","lpc10","lpcc")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Coal (GW)"))
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("pc","pc80","pc95","pc10","pcc")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Hard Coal (GW)"))
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("lpc","lpc80","lpc95","lpc10","lpcc")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Lignite (GW)"))
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("ngcc","ngcc80","ngcc95","ngcc10","ngt","ngccc")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Gas (GW)"))
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("biolcigcc")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Biomass (GW)"))
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("oil")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Oil (GW)"))
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("waste")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Waste (GW)"))
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("hcc","hct","hfc")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Hydrogen (GW)"))
  #tmp2 <- mbind(tmp2,setNames(dimSums(capadeq[,,c("others")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Other (GW)"))

  #Discard the capacity when there is no marginal value
  for (regi2 in getItems(p_eldemand, dim = 1)) {
    for (year2 in getYears(p_eldemand)) { #2010 and 2015 should remain as zeros
      if (taupeak[regi2,year2,] == 0)
        tmp2[regi2,year2,] = 0
    }
  }


  #Capacity Adequacy FOR CONVENTIONAL vRES

  #create the variables according to the needed indexes
  capadeq_vres_marg <- new.magpie(cells_and_regions = getItems(v_exdemand, dim = 1), years = getYears(v_exdemand), names = ter_el,
                                                 fill = 0, sort = FALSE, sets = NULL)
  capadeq_vres_peak <- capadeq_vres_marg

  capadeq_stor_marg <- new.magpie(cells_and_regions = getItems(v_exdemand, dim = 1), years = getYears(v_exdemand), names = testore_eladeq,
                                                     fill = 0, sort = FALSE, sets = NULL)
  #capadeq_stor_marg <- setNames(capadeq_stor_marg,NULL)
  capadeq_stor_peak <- capadeq_stor_marg

  demand_marg <- new.magpie(cells_and_regions = getItems(p_eldemand, dim = 1), years = getYears(v_exdemand), names = NULL,
                                              fill = 0, sort = FALSE, sets = NULL)
  demand_peak <- demand_marg

  netimports_marg <- new.magpie(cells_and_regions = getItems(p_eldemand, dim = 1), years = getYears(v_exdemand), names = NULL,
                                fill = 0, sort = FALSE, sets = NULL)
  netimports_peak <- netimports_marg

  #In a newer version, contribution of storage to capacity adequacy is redefined
  v_StorAvailableForBackup <- readGDX(gdx,name="v_StorAvailableForBackup",field="l",
                                      format="first_found",restore_zeros = FALSE, react = "silent")
  if(is.null(v_StorAvailableForBackup)) {
    o_ContribStoAdeq <- v_storeout_el[,, testore_eladeq]
  } else {
    v_StorAvailableForBackup <- limesMapping(v_StorAvailableForBackup)[,,tau]
    o_ContribStoAdeq <- v_StorAvailableForBackup[,, testore_eladeq]
  }

  #consider ONLY vRES, imports and demand for the tau in which the marginal value for the robust constraint peaks and when demand peaks
  for (regi2 in getItems(p_eldemand, dim = 1)) {
    for (year2 in getYears(p_eldemand)) {
      if (taupeak[regi2,year2,] == 0) {
        capadeq_vres_peak[regi2,year2,] <- 0
        demand_peak[regi2,year2,] <- 0
        netimports_peak[regi2,year2,] <- 0
        capadeq_stor_peak[regi2,year2,] <- 0
      } else {
        capadeq_vres_peak[regi2,year2,] <-
          p_adeq_te[,,ter_el] * collapseDim(v_seprodmax[regi2,year2,as.character(taupeak[regi2,year2,])], dim = 3.1)
        demand_peak[regi2,year2,] <-
          p_eldemand[regi2,year2,as.character(taupeak[regi2,year2,])]
        netimports_peak[regi2,year2,] <-
          max(0,
              setNames(p_eldemand[regi2,year2,as.character(taupeak[regi2,year2,])],NULL) +
                dimSums(v_storein_el[regi2,year2,as.character(taupeak[regi2,year2,])], dim=3) -
                dimSums(v_storeout_el[regi2,year2,as.character(taupeak[regi2,year2,])], dim=3) -
                dimSums(v_seprod[regi2,year2,as.character(taupeak[regi2,year2,])], dim=3))
        capadeq_stor_peak[regi2,year2,] <-
          p_adeq_te[,,testore_eladeq] * collapseDim(
            o_ContribStoAdeq[regi2,year2,as.character(taupeak[regi2,year2,])]
            , dim = 3.1)
      }

      if (taumax[regi2,year2,] == 0) {
        capadeq_vres_marg[regi2,year2,] <- 0
        demand_marg[regi2,year2,] <- 0
        netimports_marg[regi2,year2,] <- 0
        capadeq_stor_marg[regi2,year2,] <- 0
        } else {
        capadeq_vres_marg[regi2,year2,] <-
          p_adeq_te[,,ter_el] * collapseDim(v_seprodmax[regi2,year2,as.character(taumax[regi2,year2,])], dim = 3.1)
        demand_marg[regi2,year2,] <-
          p_eldemand[regi2,year2,as.character(taumax[regi2,year2,])]
        netimports_marg[regi2,year2,] <-
          max(0,
              setNames(p_eldemand[regi2,year2,as.character(taumax[regi2,year2,])],NULL) +
                dimSums(v_storein_el[regi2,year2,as.character(taumax[regi2,year2,])], dim=3) -
                dimSums(v_storeout_el[regi2,year2,as.character(taumax[regi2,year2,])], dim=3) -
                dimSums(v_seprod[regi2,year2,as.character(taumax[regi2,year2,])], dim=3))
        capadeq_stor_marg[regi2,year2,] <-
          p_adeq_te[,,testore_eladeq] * collapseDim(
            o_ContribStoAdeq[regi2,year2,as.character(taumax[regi2,year2,])]
            , dim = 3.1)
      }
    }
  }

  #Contribution of vRES (most challenging)
  tmp3 <- NULL
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_vres_marg[,,c("spv","csp")],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Solar (GW)"))
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_vres_marg[,,c("windon","windoff")],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Wind (GW)"))

  #Contribution of vRES (peak demand)
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_vres_peak[,,c("spv","csp")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Solar (GW)"))
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_vres_peak[,,c("windon","windoff")],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Wind (GW)"))

  #Contribution of storage (most challenging)
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_stor_marg[,,c(testore_eladeq)],dim=3),
                              "Capacity Adequacy|Most Challenging|Contribution|Storage (GW)"))
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_stor_marg[,,"batteries"],dim=3),
                              "Capacity Adequacy|Most Challenging|Contribution|Storage|Stat Batteries (GW)"))
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_stor_marg[,,"psp"],dim=3),
                              "Capacity Adequacy|Most Challenging|Contribution|Storage|Pump Hydro (GW)"))

  #Contribution of storage (peak demand)
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_stor_peak[,,c(testore_eladeq)],dim=3),
                              "Capacity Adequacy|Peak Demand|Contribution|Storage (GW)"))
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_stor_peak[,,"batteries"],dim=3),
                              "Capacity Adequacy|Peak Demand|Contribution|Storage|Stat Batteries (GW)"))
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_stor_peak[,,"psp"],dim=3),
                              "Capacity Adequacy|Peak Demand|Contribution|Storage|Pump Hydro (GW)"))

  #Concatenating variables
  tmp4 <- mbind(tmp1,tmp2,tmp3)

  #DEMAND and NET IMPORTS WHEN MARGINAL VALUE OF ROBUST CONSTRAINT IS THE LARGEST
  tmp5 <- NULL
  tmp5 <- mbind(tmp5,setNames(p_adeq_imp*netimports_marg,"Capacity Adequacy|Most Challenging|Contribution|Imports (GW)"))
  tmp5 <- mbind(tmp5,setNames(p_adeq_imp*netimports_peak,"Capacity Adequacy|Peak Demand|Contribution|Imports (GW)"))
  tmp5 <- mbind(tmp5,setNames(demand_marg,"Capacity Adequacy|Most Challenging|Demand (GW)"))
  tmp5 <- mbind(tmp5,setNames(demand_peak,"Capacity Adequacy|Peak Demand|Demand (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve,dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve,dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tecoal)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Hard Coal (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tecoal)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Hard Coal (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(telig)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Lignite (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(telig)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Lignite (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tecoal,telig)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Coal (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tecoal,telig)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Coal (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tegas_el)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Gas (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tegas_el)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Gas (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tengcc_el)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Gas CC (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tengcc_el)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Gas CC (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,setdiff(tegas_el,tengcc_el)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Gas OC (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,setdiff(tegas_el,tengcc_el)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Gas OC (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tebio)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Biomass (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tebio)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Biomass (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(teoil)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Oil (GW)"))
  #tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(teoil)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Oil (GW)"))

  varList_el <- list(
    #Most challenging
    "Capacity Adequacy|Most Challenging|Contribution|Reserve Plants (GW)"                  =c(tereserve),
    "Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Hard Coal (GW)"        =intersect(tereserve,c(tecoal)),
    "Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Lignite (GW)"        =intersect(tereserve,c(telig)),
    "Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Coal (GW)"        =intersect(tereserve,c(tecoal,telig)),
    "Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Gas (GW)"        =intersect(tereserve,c(tegas)),
    "Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Gas CC (GW)"        =intersect(tereserve,c(tengcc)),
    "Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Gas OC (GW)"        =intersect(tereserve,setdiff(tegas,tengcc)),
    "Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Biomass (GW)"        =intersect(tereserve,c(tebio)),
    "Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Oil (GW)"        =intersect(tereserve,c(tebio)),

    #Peak demand
    "Capacity Adequacy|Peak Demand|Contribution|Reserve Plants (GW)"                  =c(tereserve),
    "Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Hard Coal (GW)"        =intersect(tereserve,c(tecoal)),
    "Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Lignite (GW)"        =intersect(tereserve,c(telig)),
    "Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Coal (GW)"        =intersect(tereserve,c(tecoal,telig)),
    "Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Gas (GW)"        =intersect(tereserve,c(tegas)),
    "Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Gas CC (GW)"        =intersect(tereserve,c(tengcc)),
    "Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Gas OC (GW)"        =intersect(tereserve,setdiff(tegas,tengcc)),
    "Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Biomass (GW)"        =intersect(tereserve,c(tebio)),
    "Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Oil (GW)"        =intersect(tereserve,c(tebio))
  )

  for (var in names(varList_el)){
    tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,varList_el[[var]]],dim=3),var))
  }


  #combine aggregated Capacity Adequacy with brake-down of technologies
  tmp <- mbind(tmp4,tmp5)

  return(tmp)
}

