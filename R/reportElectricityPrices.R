#' Read in GDX and calculate electricity prices, used in convGDX2MIF.R for the reporting
#' 
#' Read in electricity prices information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportElectricityPrices(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie getYears
#' @export
#' 

reportElectricityPrices <- function(gdx) {
  
  # read sets and parameters
  p_taulength <- readGDX(gdx,name="p_taulength",field="l",format="first_found") #number of hours/year per tau
  t <- readGDX(gdx,name="t",field="l",format="first_found") #time set
  t0 <- readGDX(gdx,name="t0",field="l",format="first_found") #initial year
  c_esmdisrate <- readGDX(gdx,name="c_esmdisrate",field="l",format="first_found") #interest rate
  c_demandscale <- readGDX(gdx,name="c_demandscale",field="l",format="first_found") #electricity losses
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  p_ts <- readGDX(gdx,name="p_ts",field="l",format="first_found") #time step
  tau <- readGDX(gdx,name="tau") #set of time slices
  ter <- readGDX(gdx,name="ter") #set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx,name="ternofluc") #set of non-variable (non-fluctuating) renewable electricity generation technologies
  pety <- readGDX(gdx,name="pety") #set of primary energies
  
  # read variables and make sure only the "right" tau are taken -> to avoid info from gdx that might be stuck in the file
  v_exdemand <- readGDX(gdx,name="v_exdemand",field="l",format="first_found",restore_zeros = FALSE)[,,tau] #demand
  m_sebal <- readGDX(gdx,name="q_sebal",field="m",format="first_found")[,,tau]
  m_elecprices <- m_sebal[,,"seel"]
  m_robuststrategy2 <- readGDX(gdx,name="q_robuststrategy2",field="m",format="first_found")[,,tau]
  #v_seprod <- readGDX(gdx,name="v_seprod",field="l",format="first_found")[,,tau]
  #v_seprod <- v_seprod[,,pety]
  #v_storeout <- readGDX(gdx,name="v_storeout",field="l",format="first_found")[,,tau]
  #v_storein <- readGDX(gdx,name="v_storein",field="l",format="first_found")[,,tau]
  m_restarget <- readGDX(gdx,name="q_restarget",field="m",format="first_found")
  
  # create MagPie object of m_elecprices with iso3 regions
  m_elecprices <- limesMapping(m_elecprices) #[Geur/GWh]
  m_robuststrategy2 <- limesMapping(m_robuststrategy2) #[Geur/GWh]
  v_exdemand <- limesMapping(v_exdemand) #[GWh]
  #v_seprod <- limesMapping(v_seprod) #[GWh]
  #v_storeout <- limesMapping(v_storeout) #[GWh]
  #v_storein <- limesMapping(v_storein) #[GWh]
  m_restarget <- limesMapping(m_restarget)
  
  #Initialize heating price
  m_fullheprices <- new.magpie(cells_and_regions = getRegions(m_robuststrategy2), years = getYears(m_robuststrategy2), names = tau,
                                              fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  p_hedemand <- new.magpie(cells_and_regions = getRegions(v_exdemand), years = getYears(v_exdemand), names = tau,
                               fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  
  #Check the version so to load data and create MagPie object for variables that changed in that version and to choose the electricity-related variables
  if(c_LIMESversion >= 2.28) {
    #v_seprod_el <- v_seprod[,,"seel"]
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    
    if(c_heating == 1) {
      p_eldemand <- v_exdemand[,,"seel"]
      p_hedemand <- v_exdemand[,,"sehe"]
      #v_seprod_he <- v_seprod[,,"sehe"]
      m_fullelecprices <- m_robuststrategy2[,,"seel"]
      m_fullheprices <- m_robuststrategy2[,,"sehe"]
    } else {
      m_fullelecprices <- m_robuststrategy2
      p_eldemand <- v_exdemand
      }
    
    #m_restargetrelativegross_tech <- readGDX(gdx,name="q_restargetrelativegross_tech",field="m",format="first_found")
    #m_restargetrelativedem_tech <- readGDX(gdx,name="q_restargetrelativedem_tech",field="m",format="first_found")
    #m_restargetrelativegross_tech <- limesMapping(m_restargetrelativegross_tech) #[Geur/GWh]
    #m_restargetrelativedem_tech <- limesMapping(m_restargetrelativedem_tech) #[Geur/GWh]
  } else {
    #v_seprod_el <- v_seprod
    p_eldemand <- v_exdemand
    m_fullelecprices <- m_robuststrategy2
    
    #m_restargetrelative_DE <- readGDX(gdx,name="q_restargetrelative_DE",field="m",format="first_found")
    #m_restargetrelative <- readGDX(gdx,name="q_restargetrelative",field="m",format="first_found")
    #m_restargetrelative_DE <- limesMapping(m_restargetrelative_DE) #[Geur/GWh]
    #m_restargetrelative <- limesMapping(m_restargetrelative) #[Geur/GWh]
  }
  
  #Collapse names of demand (just in case)
  p_eldemand <- collapseNames(p_eldemand)
  p_hedemand <- collapseNames(p_hedemand)
  
  # calculate marginal value per tau
  m_elecprices <- m_elecprices/p_taulength
  m_fullelecprices <- collapseNames(m_fullelecprices)
  m_fullelecprices <- m_fullelecprices/p_taulength
  m_fullheprices <- collapseNames(m_fullheprices)
  m_fullheprices <- m_fullheprices/p_taulength
  
  #Create variables to allocate marginal values
  o_fullelecprices <- new.magpie(cells_and_regions = getRegions(m_robuststrategy2), years = getYears(m_robuststrategy2), names = tau,
                                 fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  o_fullheprices <- new.magpie(cells_and_regions = getRegions(m_robuststrategy2), years = getYears(m_robuststrategy2), names = tau,
                                 fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  
  for (t2 in getYears(m_fullelecprices)) {
    o_fullelecprices[,t2,] <- m_fullelecprices[,t2,]
    o_fullheprices[,t2,] <- m_fullheprices[,t2,]
  }
  
  #compute factor to discount average marginal values
  f_npv <- as.numeric(p_ts)*exp(-as.numeric(c_esmdisrate)*(as.numeric(t)-as.numeric(t0)))
  
  #discounting marginal values
  o_elecprices_disc <- NULL
  o_fullelecprices_disc <- NULL
  o_fullheprices_disc <- NULL
  
  #o_restargetrelativegross_tech_disc <- NULL
  #o_restargetrelativedem_tech_disc <- NULL
  #o_restargetrelative_DE_disc <- NULL #Only until version 2.26
  #o_restargetrelative_disc <- NULL #Only until version 2.26
  #o_restarget_disc <- NULL
  
  for (t2 in 1:length(t)) {
    o_elecprices_disc <- mbind(o_elecprices_disc,m_elecprices[,t2,]/f_npv[t2]) #[Geur 2010/GWh]
    o_fullelecprices_disc <- mbind(o_fullelecprices_disc,o_fullelecprices[,t2,]/f_npv[t2]) #[Geur 2010/GWh]
    o_fullheprices_disc <- mbind(o_fullheprices_disc,o_fullheprices[,t2,]/f_npv[t2]) #[Geur 2010/GWh]
    
    #o_restarget_disc <- mbind(o_restarget_disc,m_restarget[,t2,]/f_npv[t2]) #[Geur 2010/GWh-RES]
    
    ##Estimation for variables that changed in some versions
    #if(c_LIMESversion >= 2.28) {
    #  o_restargetrelativegross_tech_disc <- mbind(o_restargetrelativegross_tech_disc,m_restargetrelativegross_tech[,t2,]/f_npv[t2]) #[Geur 2010/GWh-RES]
    #  o_restargetrelativedem_tech_disc <- mbind(o_restargetrelativedem_tech_disc,m_restargetrelativedem_tech[,t2,]/f_npv[t2]) #[Geur 2010/GWh-RES]
    #} else {
    #  o_restargetrelative_DE_disc <- mbind(o_restargetrelative_DE_disc,m_restargetrelative_DE[,t2,]/f_npv[t2]) #[Geur 2010/GWh-RES]
    #  o_restargetrelative_disc <- mbind(o_restargetrelative_disc,m_restargetrelative[,t2,]/f_npv[t2]) #[Geur 2010/GWh-RES]
    #}
  }
  
  ##Total subsidies
  ##conversion from Geur/GWh -> eur/MWh
  #if(c_LIMESversion >= 2.28) {
  #  o_subsidRES_disc <- (o_restargetrelativegross_tech_disc + o_restargetrelativedem_tech_disc + o_restarget_disc)*1e6
  #} else {
  #  o_subsidRES_disc <- (o_restarget_disc + o_restargetrelative_DE_disc + o_restargetrelative_disc)*1e6
  #}
  #STILL NEED TO ADD ALL THE EQUATIONS RELATED TO SHARES OF RES!!!!!

  #weighted average marginal values per country (spot prices, capacity adequacy and RES subsidy)
  #conversion from Geur/GWh -> eur/MWh
  tmp1 <- NULL
  tmp1 <- mbind(tmp1,setNames(1e6*dimSums(o_elecprices_disc*p_taulength*p_eldemand,dim=3)/dimSums(p_taulength*p_eldemand,3),"Price|Secondary Energy|Electricity (Eur2010/MWh)"))
  #tmp1 <- mbind(tmp1,setNames(0*1e6*dimSums((o_fullelecprices_disc - o_elecprices_disc)*p_taulength*p_eldemand,dim=3)/dimSums(p_taulength*p_eldemand,3),"Price|Secondary Energy|Electricity|Other fees (Eur2010/MWh)"))
  #tmp1 <- mbind(tmp1,setNames(o_subsidRES_disc*dimSums(v_seprod_el[,,c(ter,ternofluc)]*p_taulength,dim=3)/(dimSums(p_taulength*p_eldemand,3)/as.numeric(c_demandscale)),"Price|Secondary Energy|Electricity|Other fees|RES subsidy (Eur2010/MWh)"))
  
  #weighted average marginal values per country (spot+capacity adequacy)
  #conversion from Geur/GWh -> eur/MWh
  tmp2 <- NULL
  tmp2 <- mbind(tmp2,setNames(1e6*dimSums(o_fullelecprices_disc*p_taulength*p_eldemand,dim=3)/dimSums(p_taulength*p_eldemand,3),"Price Full|Secondary Energy|Electricity (Eur2010/MWh)"))
  tmp2 <- mbind(tmp2,setNames(1e6*dimSums(o_fullheprices_disc*p_taulength*p_hedemand,dim=3)/dimSums(p_taulength*p_hedemand,3),"Price Full|Secondary Energy|Heat (Eur2010/MWh)"))
  
  
  # add global values
  tmp3 <- mbind(tmp1,tmp2)
  
  #CONSUMER AND PRODUCER SURPLUS CALCULATIONS
  tmp4 <- NULL
  ##Consumer
  #o_fullelecpricesyear_disc <- setNames(tmp2[,,"Price Full|Secondary Energy|Electricity (Eur2010/MWh)"],NULL) #[Eur 2010/MWh]
  ##Consumer costs: electricity price + subsidy
  #tmp4 <- mbind(tmp4,setNames((o_fullelecpricesyear_disc*dimSums(p_taulength*p_eldemand,dim=3))/1e6,"Consumer costs|Secondary Energy|Electricity (billion eur2010/yr)"))
  #
  ##Producer
  ##Initializing tau-dependent variables
  #v_seprod_el_tau<-p_eldemand*0
  #v_storeout_tau<-p_eldemand*0
  #v_storein_tau<-p_eldemand*0
  ##Sum for each tau
  #for (tau2 in tau) {
  #  v_seprod_el_tau[,,tau2] <- dimSums(v_seprod_el[,,tau2],3)
  #  v_storeout_tau[,,tau2] <- dimSums(v_storeout[,,tau2],3)
  #  v_storein_tau[,,tau2] <- dimSums(v_storein[,,tau2],3)
  #}
  #
  #tmp4 <- mbind(tmp4,setNames(dimSums(o_fullelecprices_disc*(v_seprod_el_tau+v_storeout_tau-v_storein_tau)*p_taulength,dim=3)/1e6,"Producer revenues|Secondary Energy|Electricity (billion eur2010/yr)"))
  #tmp4 <- mbind(tmp4,setNames(dimSums(o_fullelecprices_disc*(v_seprod_el_tau)*p_taulength,dim=3)/1e6,"Producer revenues|Secondary Energy|Electricity|Generation (billion eur2010/yr)"))
  #tmp4 <- mbind(tmp4,setNames(dimSums(o_fullelecprices_disc*(v_storeout_tau)*p_taulength,dim=3),"Producer revenues|Secondary Energy|Electricity|Storage (billion eur2010/yr)"))
  #tmp4 <- mbind(tmp4,setNames(dimSums(o_fullelecprices_disc*(v_storein_tau)*p_taulength,dim=3),"Energy costs|Secondary Energy|Electricity|Storage (billion eur2010/yr)"))
  
  
  # add global values
  tmp <- mbind(tmp3,tmp4)

  return(tmp)
}