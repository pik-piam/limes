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
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie getYears collapseDim
#' @export
#' 

reportElectricityPrices <- function(gdx) {
  
  # read sets and parameters
  p_taulength <- readGDX(gdx,name=c("p_taulength","pm_taulength"),field="l",format="first_found") #number of hours/year per tau
  tt <- readGDX(gdx,name="t",field="l",format="first_found") #time set
  t0 <- readGDX(gdx,name="t0",field="l",format="first_found") #initial year
  c_esmdisrate <- readGDX(gdx,name="c_esmdisrate",field="l",format="first_found") #interest rate
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  p_ts <- readGDX(gdx,name="p_ts",field="l",format="first_found") #time step
  tau <- readGDX(gdx,name="tau") #set of time slices
  ter <- readGDX(gdx,name="ter") #set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx,name="ternofluc") #set of non-variable (non-fluctuating) renewable electricity generation technologies
  
  # read variables and make sure only the "right" tau are taken -> to avoid info from gdx that might be stuck in the file
  v_exdemand <- readGDX(gdx,name="v_exdemand",field="l",format="first_found",restore_zeros = FALSE)[,,tau] #demand
  m_sebal <- readGDX(gdx,name="q_sebal",field="m",format="first_found", restore_zeros = FALSE)[,,tau]
  #m_elecprices <- m_sebal[,,"seel"]
  #m_heatprices <- m_sebal[,,"sehe"]
  m_robuststrategy2 <- readGDX(gdx,name="q_robuststrategy2",field="m",format="first_found", restore_zeros = FALSE)[,,tau]
  #v_seprod <- readGDX(gdx,name="v_seprod",field="l",format="first_found")[,,tau]
  #v_seprod <- v_seprod[,,pety]
  #v_storeout <- readGDX(gdx,name="v_storeout",field="l",format="first_found")[,,tau]
  #v_storein <- readGDX(gdx,name="v_storein",field="l",format="first_found")[,,tau]
  
  
  # create MagPie object of m_elecprices with iso3 regions
  m_sebal <- limesMapping(m_sebal) #[Geur/GWh]
  #m_elecprices <- limesMapping(m_elecprices) #[Geur/GWh]
  m_robuststrategy2 <- limesMapping(m_robuststrategy2) #[Geur/GWh]
  v_exdemand <- limesMapping(v_exdemand) #[GWh]
  #v_seprod <- limesMapping(v_seprod) #[GWh]
  #v_storeout <- limesMapping(v_storeout) #[GWh]
  #v_storein <- limesMapping(v_storein) #[GWh]
  
  
  #Initialize heating price
  m_fullheprices <- new.magpie(cells_and_regions = getItems(m_robuststrategy2, dim = 1), years = getYears(m_robuststrategy2), names = tau,
                                              fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  m_heatprices <- new.magpie(cells_and_regions = getItems(m_sebal, dim = 1), years = getYears(m_sebal), names = tau,
                               fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  p_hedemand <- new.magpie(cells_and_regions = getItems(v_exdemand, dim = 1), years = getYears(v_exdemand), names = tau,
                               fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  
  #Check the version so to load data and create MagPie object for variables that changed in that version and to choose the electricity-related variables
  if(c_LIMESversion >= 2.28) {
    #v_seprod_el <- v_seprod[,,"seel"]
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    
    if(c_heating == 1) {
      p_eldemand <- v_exdemand[,,"seel"]
      p_hedemand <- v_exdemand[,,"sehe"]
      p_hedemand <- collapseDim(p_hedemand, dim = 3.2)
      
      m_fullelecprices <- m_robuststrategy2[,,"seel"]
      m_fullheprices <- m_robuststrategy2[,,"sehe"]
      m_fullheprices <- collapseDim(m_fullheprices, dim = 3.2)/p_taulength
      
      m_elecprices <- m_sebal[,,"seel"]
      m_heatprices <- m_sebal[,,"sehe"]
      m_heatprices <- collapseDim(m_heatprices, dim = 3.2)/p_taulength
      
    } else {
      m_fullelecprices <- m_robuststrategy2
      m_elecprices <- m_sebal
      #Better to check that 'sehe' does not appear in v_exdemand
      if(length(grep("sehe",getNames(v_exdemand))) > 0) {
        p_eldemand <- v_exdemand[,,"seel"]
      } else {
        p_eldemand <- v_exdemand
      }
      
    }
    
    m_restargetrelativegross_tech <- readGDX(gdx,name="q_restargetrelativegross_tech",field="m",format="first_found")
    m_restargetrelativedem_tech <- readGDX(gdx,name="q_restargetrelativedem_tech",field="m",format="first_found")
    m_restargetrelativegross_tech <- limesMapping(m_restargetrelativegross_tech) #[Geur/GWh]
    m_restargetrelativedem_tech <- limesMapping(m_restargetrelativedem_tech) #[Geur/GWh]
    
    if(c_LIMESversion < 2.38) {
      m_restarget <- readGDX(gdx,name="q_restarget", field="m", format="first_found")
      m_restarget <- limesMapping(m_restarget)
    } else {
      m_restarget <- new.magpie(cells_and_regions = getItems(m_restargetrelativedem_tech, dim = 1), years = getYears(m_restargetrelativedem_tech), names = NULL,
                                fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
    }
    
    
  } else {
    #v_seprod_el <- v_seprod
    p_eldemand <- v_exdemand
    m_fullelecprices <- m_robuststrategy2
    
    m_restargetrelative_DE <- readGDX(gdx,name="q_restargetrelative_DE",field="m",format="first_found")
    m_restargetrelative <- readGDX(gdx,name="q_restargetrelative",field="m",format="first_found")
    m_restargetrelative_DE <- limesMapping(m_restargetrelative_DE) #[Geur/GWh]
    m_restargetrelative <- limesMapping(m_restargetrelative) #[Geur/GWh]
  }
  
  #Collapse names of demand (just in case)
  p_eldemand <- collapseDim(p_eldemand, dim = 3.2)
  
  # calculate marginal value per tau
  m_elecprices <- collapseDim(m_elecprices, dim = 3.2)/p_taulength
  m_fullelecprices <- collapseDim(m_fullelecprices, dim = 3.2)/p_taulength
  
  
  ##Create variables to allocate marginal values
  #o_fullelecprices <- new.magpie(cells_and_regions = getRegions(m_robuststrategy2), years = getYears(m_robuststrategy2), names = tau,
  #                               fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  #o_fullheprices <- new.magpie(cells_and_regions = getRegions(m_robuststrategy2), years = getYears(m_robuststrategy2), names = tau,
  #                               fill = NA, sort = FALSE, sets = NULL, unit = "unknown")
  #
  #for (t2 in getYears(m_fullelecprices)) {
  #  o_fullelecprices[,t2,] <- m_fullelecprices[,t2,]
  #  o_fullheprices[,t2,] <- m_fullheprices[,t2,]
  #}
  
  #compute factor to discount average marginal values
  f_npv <- as.numeric(p_ts)*exp(-as.numeric(c_esmdisrate)*(as.numeric(tt)-as.numeric(t0)))
  
  #discounting marginal values
  o_elecprices_disc <- NULL
  o_heatprices_disc <- NULL
  o_fullelecprices_disc <- NULL
  o_fullheprices_disc <- NULL
  
  o_restargetrelativegross_tech_disc <- NULL
  o_restargetrelativedem_tech_disc <- NULL
  o_restargetrelative_DE_disc <- NULL #Only until version 2.26
  o_restargetrelative_disc <- NULL #Only until version 2.26
  o_restarget_disc <- NULL
  
  for (t2 in 1:length(tt)) {
    o_elecprices_disc <- mbind(o_elecprices_disc, m_elecprices[,t2,]/f_npv[t2]) #[Geur 2010/GWh]
    o_heatprices_disc <- mbind(o_heatprices_disc, m_heatprices[,t2,]/f_npv[t2]) #[Geur 2010/GWh]
    o_fullelecprices_disc <- mbind(o_fullelecprices_disc, m_fullelecprices[,t2,]/f_npv[t2]) #[Geur 2010/GWh]
    o_fullheprices_disc <- mbind(o_fullheprices_disc, m_fullheprices[,t2,]/f_npv[t2]) #[Geur 2010/GWh]
    
    o_restarget_disc <- mbind(o_restarget_disc,m_restarget[,t2,]/f_npv[t2]) #[Geur 2010/GWh-RES]
    
    #Estimation for variables that changed in some versions
    if(c_LIMESversion >= 2.28) {
      o_restargetrelativegross_tech_disc <- mbind(o_restargetrelativegross_tech_disc,m_restargetrelativegross_tech[,t2,]/f_npv[t2]) #[Geur 2010/GWh-RES]
      o_restargetrelativedem_tech_disc <- mbind(o_restargetrelativedem_tech_disc,m_restargetrelativedem_tech[,t2,]/f_npv[t2]) #[Geur 2010/GWh-RES]
    } else {
      o_restargetrelative_DE_disc <- mbind(o_restargetrelative_DE_disc,m_restargetrelative_DE[,t2,]/f_npv[t2]) #[Geur 2010/GWh-RES]
      o_restargetrelative_disc <- mbind(o_restargetrelative_disc,m_restargetrelative[,t2,]/f_npv[t2]) #[Geur 2010/GWh-RES]
    }
  }
  
  #Subsidies
  
  if(c_LIMESversion >= 2.28) {
    o_subsidRES_disc <- (o_restargetrelativegross_tech_disc + o_restargetrelativedem_tech_disc + o_restarget_disc) #[Geur/GWh]
  } else {
    o_subsidRES_disc <- (o_restarget_disc + o_restargetrelative_DE_disc + o_restargetrelative_disc) #[Geur/GWh]
  }
  
  
  #weighted average marginal values per country (spot prices, capacity adequacy and RES subsidy)
  #conversion from Geur/GWh -> eur/MWh
  tmp1 <- NULL
  tmp1 <- mbind(tmp1,setNames(1e6*dimSums(o_elecprices_disc*p_taulength*p_eldemand,dim=3)/dimSums(p_taulength*p_eldemand,3),"Price|Secondary Energy|Electricity (Eur2010/MWh)"))
  tmp1 <- mbind(tmp1,setNames(1e6*dimSums(o_heatprices_disc*p_taulength*p_hedemand,dim=3)/dimSums(p_taulength*p_hedemand,3),"Price|Secondary Energy|Heat (Eur2010/MWh)"))
  tmp1 <- mbind(tmp1,setNames(1e6*dimSums((o_fullelecprices_disc - o_elecprices_disc)*p_taulength*p_eldemand,dim=3)/dimSums(p_taulength*p_eldemand,3),"Price|Secondary Energy|Electricity|Other fees (Eur2010/MWh)"))
  
  
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
  
  
  ##PRODUCER
  
  #Load sets
  te <- readGDX(gdx,name="te")
  tehe <- readGDX(gdx,name="tehe")
  teel <- readGDX(gdx,name="teel") #set of electricity generation technologies (non-storage)
  ter <- readGDX(gdx,name="ter") #set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx,name="ternofluc") #set of non-variable (non-fluctuating) renewable electricity generation technologies
  tefossil <- readGDX(gdx,name="tefossil") #set of fossil-based electricity generation technologies
  tenr <- readGDX(gdx,name="tenr") #set of non-renewable electricity generation technologies (includes storage)
  tegas <- readGDX(gdx,name="tegas") #set of gas generation technologies
  telig <- readGDX(gdx,name="telig") #set of lignite generation technologies
  tecoal <- readGDX(gdx,name="tecoal") #set of hard coal generation technologies
  tengcc <- readGDX(gdx,name="tengcc") #set of NGCC generation technologies
  tehydro <- readGDX(gdx,name="tehydro") #set of hydropower generation technologies
  tehgen <- readGDX(gdx,name="tehgen")
  tehydro <- readGDX(gdx,name="tehydro")
  tebio <- readGDX(gdx,name="tebio")
  teoil <- readGDX(gdx,name="teoil")
  techp <- readGDX(gdx,name="techp")
  teccs <- readGDX(gdx,name="teccs")
  teothers <- readGDX(gdx,name="teothers")
  tegas_el <- intersect(tegas,teel)
  tengcc_el <- intersect(tengcc,teel)
  
  #Load variables
  #v_seprod <- readGDX(gdx,name="v_seprod",field="l",format="first_found")[,,tau]
  #v_seprod <- v_seprod[,,pety]
  #v_storeout <- readGDX(gdx,name="v_storeout",field="l",format="first_found")[,,tau]
  #v_storein <- readGDX(gdx,name="v_storein",field="l",format="first_found")[,,tau]
  #v_deltacap <- readGDX(gdx,name="v_deltacap",field="l",format="first_found")
  #p_omeg <- readGDX(gdx,name="p_omeg",field="l",format="first_found")
  #v_disinvest <- readGDX(gdx,name="v_disinvest",field="l",format="first_found")
  if(c_LIMESversion == 2.36) {
    p_plantshortrunprofit <- readGDX(gdx,name="p_plantshortrunprofit",field="l",format="first_found") #Short run profits [eur]
    p_plantshortrunprofit_w_fix <- readGDX(gdx,name="p_plantshortrunprofit_w_fix",field="l",format="first_found") #Short run profits [including adequacy revenues and fix costs] [eur]
    p_plantprofit_t <- readGDX(gdx,name="p_plantprofit_t",field="l",format="first_found") #short-run profits for plants built in t [eur]
    p_prodsubsidy <- readGDX(gdx,name="p_prodsubsidy",field="l",format="first_found") #subsidy to RES [eur/GWh RES]
    p_prodsubsidycosts <- readGDX(gdx,name="p_prodsubsidycosts",field="l",format="first_found") #cost of subsidies to RES per country [eur]
    p_plantrevenues <- readGDX(gdx,name="p_plantrevenues",field="l",format="first_found") #plant revenues (sales and subsidies) [eur]
    
    
    #create MagPie object of m_elecprices with iso3 regions
    p_plantshortrunprofit <- limesMapping(p_plantshortrunprofit)
    p_plantshortrunprofit_w_fix <- limesMapping(p_plantshortrunprofit_w_fix)
    p_plantprofit_t <- limesMapping(p_plantprofit_t)
    p_prodsubsidy <- limesMapping(p_prodsubsidy)
    p_prodsubsidycosts <- limesMapping(p_prodsubsidycosts)
    p_plantrevenues <- limesMapping(p_plantrevenues)
    
    
    #List of technologies
    varList_el <- list(
      #Conventional
      " "                  =c(teel),
      "|Biomass "          		=intersect(teel,tebio),
      "|Biomass|w/o CCS "  		=intersect(teel,setdiff(tebio,teccs)),
      "|Coal "             		=intersect(teel,c(tecoal,telig)),
      "|Coal|w/o CCS "     		=intersect(teel,setdiff(c(tecoal,telig),teccs)),
      "|Coal|w/ CCS "      		=intersect(teel,intersect(c(tecoal,telig),teccs)),
      "|Hard Coal "        		=intersect(teel,c(tecoal)),
      "|Hard Coal|w/o CCS "		=intersect(teel,setdiff(c(tecoal),teccs)),
      "|Hard Coal|w/ CCS " 		=intersect(teel,intersect(c(tecoal),teccs)),
      "|Lignite "          		=intersect(teel,c(telig)),
      "|Lignite|w/o CCS "  		=intersect(teel,setdiff(c(telig),teccs)),
      "|Lignite|w/ CCS "   		=intersect(teel,intersect(c(telig),teccs)),
      "|Oil "              		=intersect(teel,c(teoil)),
      "|Gas "              		=intersect(teel,c(tegas)),
      "|Gas|w/o CCS "      		=intersect(teel,setdiff(tegas_el,teccs)),
      "|Gas|w/ CCS "       		=intersect(teel,intersect(tegas_el,teccs)),
      "|Gas CC|w/o CCS "   		=intersect(teel,setdiff(tengcc_el,teccs)),
      "|Gas CC|w/ CCS "    		=intersect(teel,intersect(tengcc_el,teccs)),
      "|Gas CC "           		=intersect(teel,c(tengcc_el)),
      "|Gas OC "           		=intersect(teel,setdiff(tegas_el,tengcc_el)),
      "|Other "            		=intersect(teel,c(teothers)),
      "|Hydrogen "         		=intersect(teel,c(tehgen)),
      "|Hydrogen FC "      		=intersect(teel,c("hfc")),
      "|Hydrogen OC "      		=intersect(teel,c("hct")),
      "|Hydrogen CC "      		=intersect(teel,c("hcc")),
      "|Nuclear "          		=intersect(teel,c("tnr")),
      "|Waste "            		=intersect(teel,c("waste")),
      "|Other Fossil "     		=intersect(teel,c(teothers,"waste",teoil)),
      
      #general aggregation
      "|Fossil "                 =intersect(teel,c(tefossil)),
      "|Fossil|w/o CCS "         =intersect(teel,setdiff(tefossil,teccs)),
      "|Fossil|w/ CCS "          =intersect(teel,intersect(tefossil,teccs)),
      "|Variable renewable "     =intersect(teel,c(ter)),
      "|Non-variable renewable " =intersect(teel,c(ternofluc)),
      "|Renewable "              =intersect(teel,c(ter,ternofluc)),
      "|Non-renewable "          =intersect(teel,tenr), #this does not include storage
      
      #Renewable
      "|Wind "         			=intersect(teel,c("windon","windoff")),       
      "|Wind|Onshore " 			=intersect(teel,c("windon")),   
      "|Wind|Offshore "			=intersect(teel,c("windoff")),      
      "|Solar "        			=intersect(teel,c("spv","csp")),
      "|Solar|PV "     			=intersect(teel,c("spv")), 
      "|Solar|CSP "    			=intersect(teel,c("csp")),  
      "|Hydro "        			=intersect(teel,c(tehydro))
    )
    
    tmp4 <- NULL
    for (var in names(varList_el)){
      #revenues <- mbind(revenues,dimSums(dimSums(v_seprod_el[,,varList_el[[var]]],dim=c(3.2,3.3))*p_taulength*o_fullelecprices_disc,dim=3))
      #tmp4 <- mbind(tmp4,setNames(dimSums(dimSums(v_seprod_el[,,varList_el[[var]]],dim=c(3.2,3.3,3.4))*p_taulength*o_fullelecprices_disc,dim=3),paste0("Revenues|Electricity",var,"(billion eur2010/yr)")))
      tmp4 <- mbind(tmp4,setNames(dimSums(p_plantrevenues[,,varList_el[[var]]],dim=3)/1e9,paste0("Revenues|Electricity",var,"(billion eur2010/yr)"))) #convert eur to billion eur
      tmp4 <- mbind(tmp4,setNames(dimSums(p_plantshortrunprofit[,,varList_el[[var]]],dim=3)/1e9,paste0("Short-run profits|Electricity",var,"(billion eur2010/yr)"))) #convert eur to billion eur
      tmp4 <- mbind(tmp4,setNames(dimSums(p_plantshortrunprofit_w_fix[,,varList_el[[var]]],dim=3)/1e9,paste0("Short-run profits [w adeq rev/fix costs]|Electricity",var,"(billion eur2010/yr)"))) #convert eur to billion eur
      #tmp4 <- mbind(tmp4,setNames(dimSums(p_plantprofit_t[,,varList_el[[var]]],dim=3)/1e9,paste0("Profits plants built in t|Electricity",var,"(billion eur2010/yr)"))) #convert eur to billion eur
    }
    
    #tmp4 <- mbind(tmp4,setNames(o_subsidRES_disc*1e6,"Subsidy for RES (Eur2010/MWh RES)")) #convert [eur/GWh RES] to [eur/MWh RES] - per unit of RES generated
    #tmp4 <- mbind(tmp4,setNames(p_prodsubsidycosts/(dimSums(p_taulength*p_eldemand,3)*1000),"Price|Secondary Energy|Electricity|Other fees|RES subsidy (Eur2010/MWh)")) #convert eur/GWh to eur/MWh - per unit of demand
    #tmp4 <- mbind(tmp4,setNames(p_prodsubsidycosts/1e9,"Subsidy costs|Electricity (billion eur2010/yr)")) #convert eur to billion eur
    
  }
  
  # create MagPie object of m_elecprices with iso3 regions
  #v_seprod <- limesMapping(v_seprod) #[GWh]
  #v_seprod_el <- v_seprod[,,"seel"]
  #v_storeout <- limesMapping(v_storeout) #[GWh]
  #v_storein <- limesMapping(v_storein) #[GWh]
  #v_deltacap <- limesMapping(v_deltacap) #[GW]
  #v_disinvest <- limesMapping(v_disinvest) #[GW]
  
  
  ##need to add the year 2010 to v_disinvest (depending on the scenario)
  #if (length(getYears(v_disinvest)) < length(t)) {
  #  for (t2 in setdiff(t,getYears(v_disinvest))) {
  #    tmp<- v_disinvest[,1,]*0
  #    getYears(tmp)<- t2
  #    v_disinvest<-mbind(tmp,v_disinvest)
  #  }
  #}
  
  
  
  
  #STILL NEED TO ADD ALL THE EQUATIONS RELATED TO SHARES OF RES!!!!!
  
  #Report subsidies -> conversion from Geur/GWh -> eur/MWh
  #tmp4 <- mbind(tmp4,setNames(o_subsidRES_disc*dimSums(dimSums(v_seprod_el[,,intersect(teel,c(ter,ternofluc))],dim=c(3.2,3.3,3.4))*p_taulength,dim=3)*1e6/(dimSums(p_taulength*p_eldemand,3)),"Price|Secondary Energy|Electricity|Other fees|RES subsidy (Eur2010/MWh)"))
  #tmp4 <- mbind(tmp4,setNames(o_subsidRES_disc*dimSums(dimSums(v_seprod_el[,,intersect(teel,c(ter,ternofluc))],dim=c(3.2,3.3,3.4))*p_taulength,dim=3),"Subsidy costs|Electricity (billion eur2010/yr)"))
   
  
  
  
  ##Create a set of combined sets (t,te), for the 
  #a <- expand.grid(getYears(v_seprod_el),te)
  #combset <- apply(a, 1, paste, collapse=".")
  ##capacity built in t (in 'name') that is still available in t ('year')
  #o_plantinst_t <- new.magpie(cells_and_regions = getRegions(v_seprod_el), years = getYears(v_seprod_el), names = combset,
  #                            fill = 0, sort = FALSE, sets = NULL, unit = "unknown")
  #
  #for (year_built in getYears(o_plantinst_t)) {
  #  for (year_avail in getYears(o_plantinst_t)) {
  #    years_operation <- which(getYears(o_plantinst_t) == year_avail) - which(getYears(o_plantinst_t) == year_built)
  #    if(years_operation >= 0) {
  #      o_plantinst_t[,year_avail,c(year_built)] <- (p_omeg[,,as.character(years_operation/p_ts+1)]*0+1)*(v_deltacap[,year_built,])
  #    }
  #  }
  #}
  
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
