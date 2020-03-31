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
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
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
  t <- readGDX(gdx,name="t")
  testore <- readGDX(gdx,name="testore")
  tegas <- readGDX(gdx,name="tegas") #set of gas generation technologies
  telig <- readGDX(gdx,name="telig") #set of lignite generation technologies
  tecoal <- readGDX(gdx,name="tecoal") #set of hard coal generation technologies
  tengcc <- readGDX(gdx,name="tengcc") #set of NGCC generation technologies
  tehgen <- readGDX(gdx,name="tehgen") #set of hydrogen generation technologies
  tehydro <- readGDX(gdx,name="tehydro") #set of hydropower generation technologies
  tebio <- readGDX(gdx,name="tebio") #set of biomass generation technologies
  teoil <- readGDX(gdx,name="teoil") #set of biomass generation technologies
  teothers <- readGDX(gdx,name="teothers") #set of biomass generation technologies
  tegas_el <- intersect(tegas,teel)
  tengcc_el <- intersect(tengcc,teel)
  
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  p_tedata <- readGDX(gdx,name="p_tedata",field="l",format="first_found")
  p_adeq_te <- readGDX(gdx,name="p_adeq_te",field="l",format="first_found") #adequacy factor for each technology
  p_adeq_imp <- readGDX(gdx,name="p_adeq_imp",field="l",format="first_found") #adequacy factor for net imports
  
  # read variables and marginal values
  v_exdemand <- readGDX(gdx,name="v_exdemand",field="l",format="first_found") #demand
  v_cap <- readGDX(gdx,name="v_cap",field="l",format="first_found")
  v_seprodmax <- readGDX(gdx,name="v_seprodmax",field="l",format="first_found")
  v_seprod <- readGDX(gdx,name="v_seprod",field="l",format="first_found")
  v_storeout <- readGDX(gdx,name="v_storeout",field="l",format="first_found")
  v_storein <- readGDX(gdx,name="v_storein",field="l",format="first_found")
  v_capreserve <- readGDX(gdx,name="v_capreserve",field="l",format="first_found")
  m_robuststrategy2 <- readGDX(gdx,name="q_robuststrategy2",field="m",format="first_found")
  
  #Take only certain parameters
  p_tedata_nu <- p_tedata[,,"nu"]
  
  # create MagPie object with iso3 regions
  v_exdemand <- limesMapping(v_exdemand)[,,tau]
  p_tedata_nu <- limesMapping(p_tedata_nu)
  v_cap <- limesMapping(v_cap)
  v_seprodmax <- limesMapping(v_seprodmax)[,,tau]
  m_robuststrategy2 <- limesMapping(m_robuststrategy2)[,,tau]
  v_seprod <- limesMapping(v_seprod)[,,tau]
  v_storeout <- limesMapping(v_storeout)[,,tau]
  v_storein <- limesMapping(v_storein)[,,tau]
  v_capreserve <- limesMapping(v_capreserve)
  
  #Check the version so to choose the electricity-related variables
  if(c_LIMESversion >= 2.28) {
    p_eldemand <- v_exdemand[,,"seel"]
    v_seprod <- v_seprod[,,"seel"]
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    if(c_heating == 1) {
      m_robuststrategy2 <- m_robuststrategy2[,,"seel"]
    }
    
  } else {
    p_eldemand <- v_exdemand
  }
  
  
  #Identify the 'tau' in which the marginal value for the robust constraint peaks and when demand peaks
  #Need a magpie variable with regi and year
  taumax <- new.magpie(cells_and_regions = getRegions(v_exdemand), years = t, names = NULL,
                                         fill = 0, sort = FALSE, sets = NULL, unit = "unknown")
  taumax <- setNames(taumax, NULL)
  taupeak <- taumax
  
  for (regi2 in getRegions(m_robuststrategy2)) {
    for (year2 in getYears(m_robuststrategy2)) { #2010 and 2015 should remain as zeros
      taupeak[regi2,year2,] <- match(as.magpie(apply(p_eldemand[regi2,year2,],1:2,max)),p_eldemand[regi2,year2,])
      if (dimSums(m_robuststrategy2[regi2,year2,],dim=3) == 0) {taumax[regi2,year2,] <- 0}
      else
        taumax[regi2,year2,] <- match(as.magpie(apply(m_robuststrategy2[regi2,year2,],1:2,max)),m_robuststrategy2[regi2,year2,])
    }
  }
  
  #Capacity Adequacy FOR CONVENTIONAL TECHNOLOGIES AND STORAGE
  
  #Need a magpie variable with the same sets as v_cap
  capadeq <- new.magpie(cells_and_regions = getRegions(v_exdemand), years = t, names = tentra,
                        fill = 0, sort = FALSE, sets = NULL, unit = "unknown")
  
  for (tentra2 in tentra) {
    capadeq[,as.numeric(t),tentra2] <- v_cap[,as.numeric(t),tentra2]*p_tedata_nu[,,tentra2]*as.numeric(p_adeq_te[,,tentra2])
  }
  
  #Contribution of aggregate technologies (most challenging)
  tmp1 <- NULL
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c("tnr")],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Nuclear (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c(tehydro)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Hydro (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c(tecoal,telig)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Coal (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c(tecoal)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Hard Coal (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c(telig)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Lignite (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c(tegas_el)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Gas (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c(tebio)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Biomass (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c(teoil)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Oil (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c("waste")],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Waste (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c(tehgen)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Hydrogen (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(capadeq[,,c(teothers)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Other (GW)"))
  
  #Discard the capacity when there is no marginal value
  for (regi2 in getRegions(p_eldemand)) {
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
  for (regi2 in getRegions(p_eldemand)) {
    for (year2 in getYears(p_eldemand)) { #2010 and 2015 should remain as zeros
      if (taupeak[regi2,year2,] == 0)
        tmp2[regi2,year2,] = 0
    }
  }
  
  
  #Capacity Adequacy FOR CONVENTIONAL vRES
  
  #create the variables according to the needed indexes
  capadeq_vres_marg <- new.magpie(cells_and_regions = getRegions(v_exdemand), years = t, names = ter,
                                                 fill = 0, sort = FALSE, sets = NULL, unit = "unknown")
  capadeq_vres_peak <- capadeq_vres_marg
  
  capadeq_stor_marg <- new.magpie(cells_and_regions = getRegions(v_exdemand), years = t, names = testore,
                                                     fill = 0, sort = FALSE, sets = NULL, unit = "unknown")
  #capadeq_stor_marg <- setNames(capadeq_stor_marg,NULL)
  capadeq_stor_peak <- capadeq_stor_marg
  
  demand_marg <- new.magpie(cells_and_regions = getRegions(p_eldemand), years = t, names = NULL,
                                              fill = 0, sort = FALSE, sets = NULL, unit = "unknown")
  demand_peak <- demand_marg
  
  netimports_marg <- new.magpie(cells_and_regions = getRegions(p_eldemand), years = t, names = NULL,
                                fill = 0, sort = FALSE, sets = NULL, unit = "unknown")
  netimports_peak <- netimports_marg
  
  #consider ONLY vRES, imports and demand for the tau in which the marginal value for the robust constraint peaks and when demand peaks
  for (regi2 in getRegions(p_eldemand)) {
    for (year2 in getYears(p_eldemand)) { 
      if (taupeak[regi2,year2,] == 0) {
        capadeq_vres_peak[regi2,year2,] <- 0
        demand_peak[regi2,year2,] <- 0
        netimports_peak[regi2,year2,] <- 0
        capadeq_stor_peak[regi2,year2,] <- 0
      }
      else {
        capadeq_vres_peak[regi2,year2,] <- p_adeq_te[,,ter]*collapseNames(v_seprodmax[regi2,year2,as.character(taupeak[regi2,year2,])])
        demand_peak[regi2,year2,] <- p_eldemand[regi2,year2,as.character(taupeak[regi2,year2,])]
        netimports_peak[regi2,year2,] <- max(0, setNames(p_eldemand[regi2,year2,as.character(taupeak[regi2,year2,])],NULL) + dimSums(v_storein[regi2,year2,as.character(taupeak[regi2,year2,])], dim=3) - dimSums(v_storeout[regi2,year2,as.character(taupeak[regi2,year2,])], dim=3) - dimSums(v_seprod[regi2,year2,as.character(taupeak[regi2,year2,])], dim=3))
        capadeq_stor_peak[regi2,year2,] <- p_adeq_te[,,testore]*collapseNames(v_storeout[regi2,year2,as.character(taupeak[regi2,year2,])])
      }
      
      if (taumax[regi2,year2,] == 0) {
        capadeq_vres_marg[regi2,year2,] <- 0
        demand_marg[regi2,year2,] <- 0
        netimports_marg[regi2,year2,] <- 0
        capadeq_stor_marg[regi2,year2,] <- 0
        }
      else {
        capadeq_vres_marg[regi2,year2,] <- p_adeq_te[,,ter]*collapseNames(v_seprodmax[regi2,year2,as.character(taumax[regi2,year2,])])
        demand_marg[regi2,year2,] <- p_eldemand[regi2,year2,as.character(taumax[regi2,year2,])]
        netimports_marg[regi2,year2,] <- max(0, setNames(p_eldemand[regi2,year2,as.character(taumax[regi2,year2,])],NULL) + dimSums(v_storein[regi2,year2,as.character(taumax[regi2,year2,])], dim=3) - dimSums(v_storeout[regi2,year2,as.character(taumax[regi2,year2,])], dim=3) - dimSums(v_seprod[regi2,year2,as.character(taumax[regi2,year2,])], dim=3))
        capadeq_stor_marg[regi2,year2,] <- p_adeq_te[,,testore]*collapseNames(v_storeout[regi2,year2,as.character(taumax[regi2,year2,])])
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
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_stor_marg[,,c(testore)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Storage (GW)"))
  
  #Contribution of storage (peak demand)
  tmp3 <- mbind(tmp3,setNames(dimSums(capadeq_stor_peak[,,c(testore)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Storage (GW)"))
  
  #Concatenating variables
  tmp4 <- mbind(tmp1,tmp2,tmp3)
  

  #DEMAND and NET IMPORTS WHEN MARGINAL VALUE OF ROBUST CONSTRAINT IS THE LARGEST
  tmp5 <- NULL
  tmp5 <- mbind(tmp5,setNames(p_adeq_imp*netimports_marg,"Capacity Adequacy|Most Challenging|Contribution|Imports (GW)"))
  tmp5 <- mbind(tmp5,setNames(p_adeq_imp*netimports_peak,"Capacity Adequacy|Peak Demand|Contribution|Imports (GW)"))
  tmp5 <- mbind(tmp5,setNames(demand_marg,"Capacity Adequacy|Most Challenging|Demand (GW)"))
  tmp5 <- mbind(tmp5,setNames(demand_peak,"Capacity Adequacy|Peak Demand|Demand (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve,dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve,dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tecoal)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Hard Coal (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tecoal)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Hard Coal (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(telig)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Lignite (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(telig)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Lignite (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tecoal,telig)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Coal (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tecoal,telig)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Coal (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tegas_el)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Gas (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tegas_el)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Gas (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tengcc_el)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Gas CC (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tengcc_el)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Gas CC (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,setdiff(tegas_el,tengcc_el)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Gas OC (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,setdiff(tegas_el,tengcc_el)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Gas OC (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tebio)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Biomass (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(tebio)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Biomass (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(teoil)],dim=3),"Capacity Adequacy|Most Challenging|Contribution|Reserve Plants|Oil (GW)"))
  tmp5 <- mbind(tmp5,setNames(dimSums(v_capreserve[,,c(teoil)],dim=3),"Capacity Adequacy|Peak Demand|Contribution|Reserve Plants|Oil (GW)"))
  
  
  #combine aggregated Capacity Adequacy with brake-down of technologies
  tmp <- mbind(tmp4[,as.numeric(c(t)),],tmp5)

  return(tmp)
}
  
