#' Read in GDX and calculate electricity prices,  used in convGDX2MIF.R for the reporting
#'
#' Read in electricity prices information from GDX file,  information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX,  or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio,  Renato Rodrigues
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
  tt <- readGDX(gdx, name = "t", field = "l", format = "first_found") #time set
  t0 <- readGDX(gdx, name = "t0", field = "l", format = "first_found") #initial year
  p_ts <- readGDX(gdx, name = "p_ts", field = "l", format = "first_found") #time step
  tau <- readGDX(gdx, name = "tau") #set of time slices
  ter <- readGDX(gdx, name = "ter") #set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx, name = "ternofluc") #set of non-variable (non-fluctuating) renewable electricity generation technologies
  c_esmdisrate <- readGDX(gdx, name = "c_esmdisrate", field = "l", format = "first_found") #interest rate
  c_LIMESversion <- readGDX(gdx, name = "c_LIMESversion", field = "l", format = "first_found")
  p_taulength <- readGDX(gdx, name = c("p_taulength", "pm_taulength"), field = "l", format = "first_found")[, , tau] #number of hours/year per tau

  # read variables and make sure only the "right" tau are taken -> to avoid info from gdx that might be stuck in the file
  v_exdemand <- readGDX(gdx, name = "v_exdemand", field = "l", format = "first_found", restore_zeros  =  FALSE)[, , tau] #demand
  m_sebal <- readGDX(gdx, name = "q_sebal", field = "m", format = "first_found",  restore_zeros  =  FALSE)[, , tau]
  m_robuststrategy2 <- readGDX(gdx, name = "q_robuststrategy2", field = "m", format = "first_found",  restore_zeros  =  FALSE)[, , tau]

  # create MagPie object of m_elecprices with iso3 regions
  m_sebal <- limesMapping(m_sebal) #[Geur/GWh]
  m_robuststrategy2 <- limesMapping(m_robuststrategy2) #[Geur/GWh]
  v_exdemand <- limesMapping(v_exdemand) #[GWh]


  #Initialize heating price
  m_fullheprices <- new.magpie(cells_and_regions  =  getItems(m_robuststrategy2,  dim  =  1),  years  =  getYears(m_robuststrategy2),  names  =  tau,
                                              fill  =  NA,  sort  =  FALSE,  sets  =  NULL,  unit  =  "unknown")
  m_heatprices <- new.magpie(cells_and_regions  =  getItems(m_sebal,  dim  =  1),  years  =  getYears(m_sebal),  names  =  tau,
                               fill  =  NA,  sort  =  FALSE,  sets  =  NULL,  unit  =  "unknown")
  p_hedemand <- new.magpie(cells_and_regions  =  getItems(v_exdemand,  dim  =  1),  years  =  getYears(v_exdemand),  names  =  tau,
                               fill  =  NA,  sort  =  FALSE,  sets  =  NULL,  unit  =  "unknown")

  #Check the version so to load data and create MagPie object for variables that changed in that version and to choose the electricity-related variables
  if(c_LIMESversion >=  2.28) {
    #v_seprod_el <- v_seprod[, , "seel"]
    heating <- .readHeatingCfg(gdx)

    if(heating == "fullDH") {
      p_eldemand <- v_exdemand[, , "seel"]
      p_hedemand <- v_exdemand[, , "sehe"]
      p_hedemand <- collapseDim(p_hedemand,  dim  =  3.2)

      m_fullelecprices <- m_robuststrategy2[, , "seel"]
      m_fullheprices <- m_robuststrategy2[, , "sehe"]
      m_fullheprices <- collapseDim(m_fullheprices,  dim  =  3.2)/p_taulength

      m_elecprices <- m_sebal[, , "seel"]
      m_heatprices <- m_sebal[, , "sehe"]
      m_heatprices <- collapseDim(m_heatprices,  dim  =  3.2)/p_taulength

    } else {
      m_fullelecprices <- m_robuststrategy2
      m_elecprices <- m_sebal
      #Better to check that 'sehe' does not appear in v_exdemand
      if(length(grep("sehe", getNames(v_exdemand))) > 0) {
        p_eldemand <- v_exdemand[, , "seel"]
      } else {
        p_eldemand <- v_exdemand
      }

    }

    m_restargetrelativegross_tech <- readGDX(gdx, name = "q_restargetrelativegross_tech", field = "m", format = "first_found")
    m_restargetrelativedem_tech <- readGDX(gdx, name = "q_restargetrelativedem_tech", field = "m", format = "first_found")
    m_restargetrelativegross_tech <- limesMapping(m_restargetrelativegross_tech) #[Geur/GWh]
    m_restargetrelativedem_tech <- limesMapping(m_restargetrelativedem_tech) #[Geur/GWh]

    if(c_LIMESversion < 2.38) {
      m_restarget <- readGDX(gdx, name = "q_restarget",  field = "m",  format = "first_found")
      m_restarget <- limesMapping(m_restarget)
    } else {
      m_restarget <- new.magpie(cells_and_regions  =  getItems(m_restargetrelativedem_tech,  dim  =  1),  years  =  getYears(m_restargetrelativedem_tech),  names  =  NULL,
                                fill  =  NA,  sort  =  FALSE,  sets  =  NULL,  unit  =  "unknown")
    }


  } else {
    #v_seprod_el <- v_seprod
    p_eldemand <- v_exdemand
    m_fullelecprices <- m_robuststrategy2

    m_restargetrelative_DE <- readGDX(gdx, name = "q_restargetrelative_DE", field = "m", format = "first_found")
    m_restargetrelative <- readGDX(gdx, name = "q_restargetrelative", field = "m", format = "first_found")
    m_restargetrelative_DE <- limesMapping(m_restargetrelative_DE) #[Geur/GWh]
    m_restargetrelative <- limesMapping(m_restargetrelative) #[Geur/GWh]
  }

  #Collapse names of demand (just in case)
  p_eldemand <- collapseDim(p_eldemand, dim = 3.2)

  # calculate marginal value per tau
  m_elecprices <- collapseDim(m_elecprices, dim  = 3.2)/p_taulength
  m_fullelecprices <- collapseDim(m_fullelecprices, dim  = 3.2)/p_taulength

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
    o_elecprices_disc <- mbind(o_elecprices_disc,  m_elecprices[, t2, ]/f_npv[t2]) #[Geur 2010/GWh]
    o_heatprices_disc <- mbind(o_heatprices_disc,  m_heatprices[, t2, ]/f_npv[t2]) #[Geur 2010/GWh]
    o_fullelecprices_disc <- mbind(o_fullelecprices_disc,  m_fullelecprices[, t2, ]/f_npv[t2]) #[Geur 2010/GWh]
    o_fullheprices_disc <- mbind(o_fullheprices_disc,  m_fullheprices[, t2, ]/f_npv[t2]) #[Geur 2010/GWh]

    o_restarget_disc <- mbind(o_restarget_disc, m_restarget[, t2, ]/f_npv[t2]) #[Geur 2010/GWh-RES]

    #Estimation for variables that changed in some versions
    if(c_LIMESversion >=  2.28) {
      o_restargetrelativegross_tech_disc <- mbind(o_restargetrelativegross_tech_disc, m_restargetrelativegross_tech[, t2, ]/f_npv[t2]) #[Geur 2010/GWh-RES]
      o_restargetrelativedem_tech_disc <- mbind(o_restargetrelativedem_tech_disc, m_restargetrelativedem_tech[, t2, ]/f_npv[t2]) #[Geur 2010/GWh-RES]
    } else {
      o_restargetrelative_DE_disc <- mbind(o_restargetrelative_DE_disc, m_restargetrelative_DE[, t2, ]/f_npv[t2]) #[Geur 2010/GWh-RES]
      o_restargetrelative_disc <- mbind(o_restargetrelative_disc, m_restargetrelative[, t2, ]/f_npv[t2]) #[Geur 2010/GWh-RES]
    }
  }

  #Subsidies

  if(c_LIMESversion >=  2.28) {
    o_subsidRES_disc <- (o_restargetrelativegross_tech_disc + o_restargetrelativedem_tech_disc + o_restarget_disc) #[Geur/GWh]
  } else {
    o_subsidRES_disc <- (o_restarget_disc + o_restargetrelative_DE_disc + o_restargetrelative_disc) #[Geur/GWh]
  }


  #weighted average marginal values per country (spot prices,  capacity adequacy and RES subsidy)
  #conversion from Geur/GWh -> eur/MWh
  tmp1 <- NULL
  tmp1 <- mbind(tmp1, setNames(1e6 * dimSums(o_elecprices_disc * p_taulength * p_eldemand, dim = 3)
                               /dimSums(p_taulength * p_eldemand, 3),
                               "Price|Secondary Energy|Electricity (Eur2010/MWh)"))
  tmp1 <- mbind(tmp1, setNames(1e6 * dimSums(o_heatprices_disc * p_taulength * p_hedemand, dim = 3)
                               /dimSums(p_taulength * p_hedemand, 3),
                               "Price|Secondary Energy|Heat (Eur2010/MWh)"))
  tmp1 <- mbind(tmp1, setNames(1e6 * dimSums((o_fullelecprices_disc - o_elecprices_disc) * p_taulength * p_eldemand, dim = 3)
                               /dimSums(p_taulength * p_eldemand, 3),
                               "Price|Secondary Energy|Electricity|Other fees (Eur2010/MWh)"))


  #weighted average marginal values per country (spot+capacity adequacy)
  #conversion from Geur/GWh -> eur/MWh
  tmp2 <- NULL
  tmp2 <- mbind(tmp2, setNames(1e6 * dimSums(o_fullelecprices_disc * p_taulength * p_eldemand, dim = 3)
                               /dimSums(p_taulength * p_eldemand, 3),
                               "Price Full|Secondary Energy|Electricity (Eur2010/MWh)"))
  tmp2 <- mbind(tmp2, setNames(1e6 * dimSums(o_fullheprices_disc * p_taulength * p_hedemand, dim = 3)
                               /dimSums(p_taulength * p_hedemand, 3),
                               "Price Full|Secondary Energy|Heat (Eur2010/MWh)"))


  # add global values
  tmp3 <- mbind(tmp1, tmp2)

  #CONSUMER AND PRODUCER SURPLUS CALCULATIONS
  tmp4 <- NULL


  ##PRODUCER

  #Load sets
  te <- readGDX(gdx, name = "te")
  tehe <- readGDX(gdx, name = "tehe")
  teel <- readGDX(gdx, name = "teel") #set of electricity generation technologies (non-storage)
  ter <- readGDX(gdx, name = "ter") #set of variable renewable electricity generation technologies
  ternofluc <- readGDX(gdx, name = "ternofluc") #set of non-variable (non-fluctuating) renewable electricity generation technologies
  tefossil <- readGDX(gdx, name = "tefossil") #set of fossil-based electricity generation technologies
  tenr <- readGDX(gdx, name = "tenr") #set of non-renewable electricity generation technologies (includes storage)
  tegas <- readGDX(gdx, name = "tegas") #set of gas generation technologies
  telig <- readGDX(gdx, name = "telig") #set of lignite generation technologies
  tecoal <- readGDX(gdx, name = "tecoal") #set of hard coal generation technologies
  tengcc <- readGDX(gdx, name = "tengcc") #set of NGCC generation technologies
  tehydro <- readGDX(gdx, name = "tehydro") #set of hydropower generation technologies
  tehgen <- readGDX(gdx, name = "tehgen")
  tehydro <- readGDX(gdx, name = "tehydro")
  tebio <- readGDX(gdx, name = "tebio")
  teoil <- readGDX(gdx, name = "teoil")
  techp <- readGDX(gdx, name = "techp")
  teccs <- readGDX(gdx, name = "teccs")
  teothers <- readGDX(gdx, name = "teothers")
  tegas_el <- intersect(tegas, teel)
  tengcc_el <- intersect(tengcc, teel)

  #Load variables
  if(c_LIMESversion  ==  2.36) {
    p_plantshortrunprofit <- readGDX(gdx, name = "p_plantshortrunprofit", field = "l", format = "first_found") #Short run profits [eur]
    p_plantshortrunprofit_w_fix <- readGDX(gdx, name = "p_plantshortrunprofit_w_fix", field = "l", format = "first_found") #Short run profits [including adequacy revenues and fix costs] [eur]
    p_plantprofit_t <- readGDX(gdx, name = "p_plantprofit_t", field = "l", format = "first_found") #short-run profits for plants built in t [eur]
    p_prodsubsidy <- readGDX(gdx, name = "p_prodsubsidy", field = "l", format = "first_found") #subsidy to RES [eur/GWh RES]
    p_prodsubsidycosts <- readGDX(gdx, name = "p_prodsubsidycosts", field = "l", format = "first_found") #cost of subsidies to RES per country [eur]
    p_plantrevenues <- readGDX(gdx, name = "p_plantrevenues", field = "l", format = "first_found") #plant revenues (sales and subsidies) [eur]


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
      " "                   = c(teel),
      "|Biomass "          		 = intersect(teel, tebio),
      "|Biomass|w/o CCS "  		 = intersect(teel, setdiff(tebio, teccs)),
      "|Coal "             		 = intersect(teel, c(tecoal, telig)),
      "|Coal|w/o CCS "     		 = intersect(teel, setdiff(c(tecoal, telig), teccs)),
      "|Coal|w/ CCS "      		 = intersect(teel, intersect(c(tecoal, telig), teccs)),
      "|Hard Coal "        		 = intersect(teel, c(tecoal)),
      "|Hard Coal|w/o CCS "		 = intersect(teel, setdiff(c(tecoal), teccs)),
      "|Hard Coal|w/ CCS " 		 = intersect(teel, intersect(c(tecoal), teccs)),
      "|Lignite "          		 = intersect(teel, c(telig)),
      "|Lignite|w/o CCS "  		 = intersect(teel, setdiff(c(telig), teccs)),
      "|Lignite|w/ CCS "   		 = intersect(teel, intersect(c(telig), teccs)),
      "|Oil "              		 = intersect(teel, c(teoil)),
      "|Gas "              		 = intersect(teel, c(tegas)),
      "|Gas|w/o CCS "      		 = intersect(teel, setdiff(tegas_el, teccs)),
      "|Gas|w/ CCS "       		 = intersect(teel, intersect(tegas_el, teccs)),
      "|Gas CC|w/o CCS "   		 = intersect(teel, setdiff(tengcc_el, teccs)),
      "|Gas CC|w/ CCS "    		 = intersect(teel, intersect(tengcc_el, teccs)),
      "|Gas CC "           		 = intersect(teel, c(tengcc_el)),
      "|Gas OC "           		 = intersect(teel, setdiff(tegas_el, tengcc_el)),
      "|Other "            		 = intersect(teel, c(teothers)),
      "|Hydrogen "         		 = intersect(teel, c(tehgen)),
      "|Hydrogen FC "      		 = intersect(teel, c("hfc")),
      "|Hydrogen OC "      		 = intersect(teel, c("hct")),
      "|Hydrogen CC "      		 = intersect(teel, c("hcc")),
      "|Nuclear "          		 = intersect(teel, c("tnr")),
      "|Waste "            		 = intersect(teel, c("waste")),
      "|Other Fossil "     		 = intersect(teel, c(teothers, "waste", teoil)),

      #general aggregation
      "|Fossil "                  = intersect(teel, c(tefossil)),
      "|Fossil|w/o CCS "          = intersect(teel, setdiff(tefossil, teccs)),
      "|Fossil|w/ CCS "           = intersect(teel, intersect(tefossil, teccs)),
      "|Variable renewable "      = intersect(teel, c(ter)),
      "|Non-variable renewable "  = intersect(teel, c(ternofluc)),
      "|Renewable "               = intersect(teel, c(ter, ternofluc)),
      "|Non-renewable "           = intersect(teel, tenr),  #this does not include storage

      #Renewable
      "|Wind "         			 = intersect(teel, c("windon", "windoff")),
      "|Wind|Onshore " 			 = intersect(teel, c("windon")),
      "|Wind|Offshore "			 = intersect(teel, c("windoff")),
      "|Solar "        			 = intersect(teel, c("spv", "csp")),
      "|Solar|PV "     			 = intersect(teel, c("spv")),
      "|Solar|CSP "    			 = intersect(teel, c("csp")),
      "|Hydro "        			 = intersect(teel, c(tehydro))
    )

    tmp4 <- NULL
    for (var in names(varList_el)){
      tmp4 <- mbind(tmp4, setNames(dimSums(p_plantrevenues[, , varList_el[[var]]], dim = 3)/1e9, paste0("Revenues|Electricity", var, "(billion eur2010/yr)"))) #convert eur to billion eur
      tmp4 <- mbind(tmp4, setNames(dimSums(p_plantshortrunprofit[, , varList_el[[var]]], dim = 3)/1e9, paste0("Short-run profits|Electricity", var, "(billion eur2010/yr)"))) #convert eur to billion eur
      tmp4 <- mbind(tmp4, setNames(dimSums(p_plantshortrunprofit_w_fix[, , varList_el[[var]]], dim = 3)/1e9, paste0("Short-run profits [w adeq rev/fix costs]|Electricity", var, "(billion eur2010/yr)"))) #convert eur to billion eur

    }

  }

  # add global values
  tmp <- mbind(tmp3, tmp4)

  return(tmp)

}
