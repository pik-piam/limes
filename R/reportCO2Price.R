#' Read in GDX and calculate CO2 prices,  used in convGDX2MIF.R for the reporting
#'
#' Read in exogenous co2 prices and marginal values from GDX file,  information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX,  or the path to a gdx
#' @return MAgPIE object - contains the capacity variables
#' @author Sebastian Osorio,  Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportCO2Price(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#'

reportCO2Price <- function(gdx) {

  # read sets and parameters
  tt <- readGDX(gdx, name = "t", field = "l", format = "first_found") #time set
  t0 <- tt[1]
  c_esmdisrate <- readGDX(gdx, name = "c_esmdisrate", field = "l", format = "first_found") #interest rate
  p_ts <- readGDX(gdx, name = "p_ts", field = "l", format = "first_found") #time step
  c_emicappathscen <- readGDX(gdx, name = c("c_emicappathscen", "cm_emicappathscen"), field = "l", format = "first_found") #control variable for emission cap (per country)
  s_c2co2 <- readGDX(gdx, name = "s_c2co2", field = "l", format = "first_found") #time step
  p_co2price <- readGDX(gdx, name = "p_co2price", field = "l", format = "first_found") #exogenous co2 prices (for electricity)
  v_emi <- readGDX(gdx,name = c("v_emi", "vm_emi"),field = "l",format = "first_found",restore_zeros  =  FALSE)
  c_LIMESversion <- readGDX(gdx, name = "c_LIMESversion", field = "l", format = "first_found")

  # reduce parameters to years actually used (t in the model)
  y <- getYears(v_emi)
  p_co2price <- p_co2price[, y, ]

  if(c_LIMESversion >=  2.27) {
    c_industry_ETS <- readGDX(gdx, name = "c_industry_ETS", field = "l", format = "first_found")
  }

  # create MagPie object of v_emi with iso3 regions
  v_emi <- limesMapping(v_emi)

  #take only the co2 and convert from GtC to tCO2
  v_emi <- v_emi[, , "co2"]*as.numeric(s_c2co2)*1e9

  #LOADING LIST OF REGIONS TO AGGREGATE CERTAIN GROUPS (e.g.,  EU)
  # settings mapping path
  mappingregiPath <- system.file("extdata", "LIMES_country_ISO_3.csv", package = "limes")
  # reading mapping file
  mappingregi <- read.csv(mappingregiPath, sep = ";")

  #compute factor to discount average marginal values
  f_npv <- as.numeric(p_ts)*exp(-as.numeric(c_esmdisrate)*(as.numeric(tt)-as.numeric(t0)))


  #1) EXOGENOUS CO2 PRICE
  #a) Electricity sector
  p_co2price <- p_co2price/s_c2co2

  # create MagPie object of variables with iso3 regions
  p_co2price <- limesMapping(p_co2price)

  #b) Industry sector
  if(c_LIMESversion >=  2.29) {
  p_ind_co2price <- readGDX(gdx, name = "p_ind_co2price", field = "l", format = "first_found")[, y,] #exogenous co2 prices (for industry)
  p_ind_co2price <- p_ind_co2price/s_c2co2

  # create MagPie object of variables with iso3 regions
  p_ind_co2price <- limesMapping(p_ind_co2price)
  }


  #2) CO2 PRICE RESULTING FROM EMISSION CAP MARGINAL VALUES PER REGION

  # read marginal values
  #m_emicappath1 <- readGDX(gdx, name = c("q_emicappath1", "q30_emicappath1"), field = "m", format = "first_found")
  #m_emicappath2 <- readGDX(gdx, name = c("q_emicappath2", "q30_emicappath2"), field = "m", format = "first_found")
  m_emicappath_DE <- readGDX(gdx, name = "q_emicappath_DE", field = "m", format = "first_found")

  if(c_LIMESversion < 2.38) {
    m_emicappath3 <- readGDX(gdx, name = "q_emicappath3",  field = "m",  format = "first_found")
  } else {
    m_emicappath3 <- new.magpie(cells_and_regions  =  getItems(m_emicappath_DE,  dim  =  1),  years  =  getYears(m_emicappath_DE),  names  =  NULL,
                                fill  =  NA,  sort  =  FALSE,  sets  =  NULL)
  }


  #declaring the variable to save the information
  o_marg_emicap_regi <- NULL

  if (as.numeric(c_emicappathscen)  ==  0) {
    o_marg_emicap_regi  =  0
      } else if (c_emicappathscen  ==  1) {
        #o_marg_emicap_regi  =  (1/s_c2co2)*(-m_emicappath1)
      } else if (c_emicappathscen  ==  2) {
        #o_marg_emicap_regi  =  (1/s_c2co2)*(-m_emicappath2)
      } else if (c_emicappathscen  ==  3) {
        o_marg_emicap_regi  =  (1/s_c2co2)*(-m_emicappath3)
      } else {
        tmp<-m_emicappath_DE[, 2015, ]*0 #take just one column to ensure the lenght of the vector
        getYears(tmp) <- 2010
        m_emicappath_DE<-mbind(tmp, m_emicappath_DE)
        m_emicappath_DE <- limesMapping(m_emicappath_DE)
        o_marg_emicap_regi <- (1/s_c2co2)*(-m_emicappath_DE)
      }

  #Discounting the marginal values
  if (as.numeric(c_emicappathscen) !=  0) {
    o_marg_emicap_regi <- limesDiscount(o_marg_emicap_regi, gdx)
  }


  #3) CO2 PRICE RESULTING FROM EMISSION CAP MARGINAL VALUES FOR 'regipol'

  # read marginal values
  m_emicappath_EU <- readGDX(gdx, name = "q_emicappath_EU", field = "m", format = "first_found")

  #loading the countries belonging to 'regipol'
  regipol_iso2 <- readGDX(gdx, name = "regipol")
  regipol_iso3 <- mappingregi[match(regipol_iso2, mappingregi[, 1]), 2]
  regipol <- regipol_iso3 #Better to take it directly from the GDX file

  #allocating the ETS prices only to counties belonging to the EU ETS
  o_marg_emicap_EU <- p_co2price*0 #to create a vector with the same length and properties (magpie)
  for (regipol2 in regipol) {
    o_marg_emicap_EU[regipol2, , ] <- (1/s_c2co2)*(-m_emicappath_EU/f_npv)
  }


  #4) CO2 PRICE RESULTING FROM EMISSION BANKING

  #a) Banking constraint (EU ETS)
  # read marginal values
  m_bankemi_EU <- readGDX(gdx, name = "q_bankemi_EU", field = "m", format = "first_found")

  #loading the countries belonging to EU ETS
  EUETS_iso2 <- readGDX(gdx, name = "regeuets")
  EUETS_iso3 <- mappingregi[match(EUETS_iso2, mappingregi[, 1]), 2]
  EUETS <- EUETS_iso3 #Better to take it directly from the GDX file

  #allocating the ETS prices only to counties belonging to the EU ETS
  o_marg_bankemi_EU <- p_co2price*0 #to create a vector with the same lenght and properties (magpie)
  for (EUETS2 in EUETS) {
    o_marg_bankemi_EU[EUETS2, , ] <- (1/s_c2co2)*(-m_bankemi_EU*p_ts/f_npv)
  }

  #b) End of EU ETS (zero emissions after 2057) - only after v2.32
  o_marg_end_EUETS <- p_co2price*0 #to create a vector with the same lenght and properties (magpie)
  if(c_LIMESversion >=  2.32) { #temporary solution. This equation disappeared. Not sure if it was actually used in previous versions
    # read marginal values
    #m_end_EUETS <- readGDX(gdx, name = "q_end_EUETS", field = "m", format = "first_found")

    ##allocating the ETS prices only to counties belonging to the EU ETS
    #for (EUETS2 in EUETS) {
    #  o_marg_end_EUETS[EUETS2, , ] <- (1/s_c2co2)*(-m_end_EUETS/f_npv)
    #}
  }


  #5) CO2 PRICE RESULTING FROM EMISSION CUMULATIVE CAP (PER REGION)

  # read marginal values
  m_emiconstcum_regi <- readGDX(gdx, name = "q_emiconstcum_regi", field = "m", format = "first_found")
  c_emicapcumscen_regi <- readGDX(gdx, name = "c_emicapcumscen_regi", field = "l", format = "first_found")

  #declaring the variable to save the information
  o_marg_emicapcum_regi <- NULL

  if(c_emicapcumscen_regi > 0) {
    m_emiconstcum_regi <- limesMapping(m_emiconstcum_regi)
    m_emiconstcum_regi <- limesAllocateYears(m_emiconstcum_regi, gdx)
    for (t2 in c(1:length(tt))) {
    #the f_npv considers the parameter p_ts,  but emicapcumscen_regi is a constraint for the 5 years
    o_marg_emicapcum_regi  =  mbind(o_marg_emicapcum_regi,
                                    (1 / s_c2co2)
                                    * (- m_emiconstcum_regi[, t2, ]
                                       * p_ts/f_npv[t2]
                                    )
    )
    }
    o_marg_emicapcum_regi[, c(2010, 2015), ]<-0 #Ensuring the values for 2010 and 2015 are zero
    } else
      o_marg_emicapcum_regi  =  0


  #6) CO2 PRICE RESULTING FROM EMISSION CUMULATIVE CAP (FOR 'emicapcum_regi')

  # read marginal values
  m_emiconstcum <- readGDX(gdx, name = "q_emiconstcum", field = "m", format = "first_found")

  #loading the countries belonging to EU ETS
  emicapcum_regi_iso2 <- readGDX(gdx, name = "regeuets")
  emicapcum_regi_iso3 <- mappingregi[match(emicapcum_regi_iso2, mappingregi[, 1]), 2]
  emicapcum_regi <- emicapcum_regi_iso3 #Better to take it directly from the GDX file

  #allocating the ETS prices only to counties belonging to the EU ETS
  o_marg_emicapcum_EU <- p_co2price*0 #to create a vector with the same lenght and properties (magpie)
  m_emiconstcum <- limesAllocateYears(m_emiconstcum, gdx) #to allocate the value to each year
  for (emicapcum_regi2 in emicapcum_regi) {
    o_marg_emicapcum_EU[emicapcum_regi2, , ] <- (1/s_c2co2)*(-m_emiconstcum*p_ts/f_npv) #the f_npv considers the parameter p_ts,  but this is a constraint for the 5 years
    o_marg_emicapcum_EU[, c(2010, 2015), ]<-0 #Ensuring the values for 2010 and 2015 are zero
  }


  #--------------------------------------------------------------------------------------------------------------------
  # AGGREGATING ALL CO2 PRICE COMPONENTS
  #CO2 price for single regions resulting from the different constraints (exogen co2 price,  emission cap,  emission budget)
  o_co2price <- p_co2price + o_marg_emicap_regi + o_marg_emicap_EU + o_marg_bankemi_EU + o_marg_end_EUETS + o_marg_emicapcum_regi + o_marg_emicapcum_EU
  if(c_LIMESversion >=  2.29) {
  o_ind_co2price <- p_ind_co2price + o_marg_bankemi_EU + o_marg_end_EUETS
  }

  # writing the co2 price
  tmp1<-setNames(o_co2price, "Price|Carbon|Net|Electricity (Eur2010/t CO2)")

  #DIFFERENT PARTS OF CARBON PRICES
  tmp2 <- NULL

  #CO2 price for single regions resulting from national policies (exogen co2 price,  emission cap,  emission budget)
  o_co2price_nat <- p_co2price + o_marg_emicap_regi + o_marg_emicapcum_regi + o_marg_end_EUETS
  tmp2<-setNames(o_co2price_nat, "Price|Carbon|National Climate Target|Power Sector (Eur2010/t CO2)")

  #CO2 price for single regions resulting from aggregated policies - ETS (banking,  emission cap,  emission budget)
  o_co2price_ETS <- o_marg_emicap_EU + o_marg_bankemi_EU + o_marg_emicapcum_EU
  tmp2<-mbind(tmp2,  setNames(o_co2price_ETS, "Price|Carbon|ETS (Eur2010/t CO2)"))

  #CO2 price for industry (region's taxes and the EU ETS price)
  if(c_LIMESversion >=  2.29) {
    #Only report these variables if industry is included
    if(c_industry_ETS  ==  1) {
      tmp2<-mbind(tmp2, setNames(o_ind_co2price, "Price|Carbon|Net|Industry (Eur2010/t CO2)"))
      tmp2<-mbind(tmp2, setNames(p_ind_co2price, "Price|Carbon|National Climate Target|Industry (Eur2010/t CO2)"))
    }
  }

  #concatenate reporting data
  tmp3 <- mbind(tmp1, tmp2)

  #--------------------------------------------------------------------------------------------------------------------
  #COST OF EMISSIONS and REVENUES FROM EUA AUCTIONS
  #Read additional parameters and variables
  p_emicappath_EUETS <- readGDX(gdx, name = "p_emicappath_EUETS", field = "l", format = "first_found")
  if(c_LIMESversion >=  2.28) {
    p_auction_EUETS <- readGDX(gdx, name = "p_auction_EUETS", field = "l", format = "first_found")
    p_intakeMSR <- readGDX(gdx, name = c("p_intakeMSR", "p_withholdEUA"), field = "l", format = "first_found")
    p_outtakeMSR <- readGDX(gdx, name = c("p_outtakeMSR", "p_backloadEUA"), field = "l", format = "first_found")
  }

  tmp4 <- NULL
  tmp4<-mbind(tmp4,  setNames(o_co2price*dimSums(v_emi[, , "seel"],  dim = 3,  na.rm  =  T)/(1e9), "Total Energy System Cost|Power Sector|CO2 costs (billion eur2010/yr)"))

  c_bankemi_EU <- readGDX(gdx, name = "c_bankemi_EU", field = "l", format = "first_found") #banking constraint... many of the variables should not be reported if EU ETS is not modelled at least partially
  if(c_LIMESversion >=  2.28 & c_bankemi_EU  ==  1) {
    # read variables from gdx
    p_shareEUA_auct <- readGDX(gdx, name = "p_shareEUA_auct", field = "l", format = "first_found")
    p_shareEUA_auct <- limesMapping(p_shareEUA_auct)
    p_certificates_cancelled <- readGDX(gdx, name = "p_certificates_cancelled", field = "l", format = "first_found")
    p_sharefreeEUA <- readGDX(gdx, name = "p_sharefreeEUA", field = "l", format = "first_found")
    #set related to countries cancelling
    regi_cancEUA_iso2 <- readGDX(gdx, name = "regi_cancEUA") #set of countries cancelling EUA (unilaterally)
    regi_cancEUA_iso3 <- mappingregi[match(regi_cancEUA_iso2, mappingregi[, 1]), 2]
    regi_cancEUA <- regi_cancEUA_iso3

    #Unilateral cancellation
    o_selfcancelEUA_regi <- setNames(o_co2price_ETS*0,  NULL)
    if(length(regi_cancEUA) > 0) {
      o_selfcancelEUA_regi[regi_cancEUA, , ] <- p_certificates_cancelled * p_shareEUA_auct[regi_cancEUA, , ] / dimSums(p_shareEUA_auct[regi_cancEUA, , ], dim = 1)
    }
    if(c_LIMESversion <=  2.30) {
      tmp4 <- mbind(tmp4, setNames(o_selfcancelEUA_regi, "Emissions|CO2|Unilateral EUA cancellation (Mt CO2/yr)"))
    } else {
      tmp4 <- mbind(tmp4, setNames(o_selfcancelEUA_regi*s_c2co2*1000, "Emissions|CO2|Unilateral EUA cancellation (Mt CO2/yr)"))
    }

    #Report the auction and revenues from auction only if industry is modelled
    if(c_industry_ETS  ==  1) {

      if(c_LIMESversion <=  2.30) {
        #Preliminary auction of EUA
        o_prelauction_EUETS_regi <- p_emicappath_EUETS*p_shareEUA_auct*(1-p_sharefreeEUA)
        tmp4 <- mbind(tmp4, setNames(o_prelauction_EUETS_regi*s_c2co2*1000, "Emissions|CO2|Preliminary auction ETS (Mt CO2/yr)"))

        #o_auction_preunicanc is indeed what is allocated to different countries,  and then they could decide to cancel unilaterally
        #i.e.,  they do not auction these EUA and delete them from the EU log
        o_auction_preunicanc <- p_emicappath_EUETS*(1-p_sharefreeEUA) - p_intakeMSR + p_outtakeMSR

        #EUA cancelled need to be subtracted from EUA preliminary auctions
        o_auctionEUA_regi <- o_auction_preunicanc*p_shareEUA_auct - 0*o_selfcancelEUA_regi/(s_c2co2*1000)
        tmp4 <- mbind(tmp4, setNames(o_auctionEUA_regi*s_c2co2*1000, "Emissions|CO2|Certificates auctioned ETS (Mt CO2/yr)"))

      } else {

        #Preliminary auction of EUA
        p_prelauction_EUETS <- readGDX(gdx, name = "p_prelauction_EUETS", field = "l", format = "first_found")[, y, ]
        o_prelauction_EUETS_regi <- p_prelauction_EUETS*p_shareEUA_auct
        tmp4 <- mbind(tmp4, setNames(o_prelauction_EUETS_regi*s_c2co2*1000, "Emissions|CO2|Preliminary auction ETS (Mt CO2/yr)"))
        #Available for auction before unilateral cancellation
        #o_auction_preunicanc is indeed what is allocated to different countries,  and then they could decide to cancel unilaterally
        #i.e.,  they do not auction these EUA and delete them from the EU log
        o_auction_preunicanc <- p_auction_EUETS[, y, ] + dimSums(o_selfcancelEUA_regi, dim = 1)
        o_auction_preunicanc_regi <- o_auction_preunicanc*p_shareEUA_auct
        tmp4 <- mbind(tmp4, setNames(o_auction_preunicanc_regi*s_c2co2*1000, "Emissions|CO2|Certificates for auction - before unilateral cancellation (Mt CO2/yr)"))
        o_auctionEUA_regi <- o_auction_preunicanc_regi - o_selfcancelEUA_regi
        tmp4 <- mbind(tmp4, setNames(o_auctionEUA_regi*s_c2co2*1000, "Emissions|CO2|Certificates auctioned ETS (Mt CO2/yr)"))

      }

      #Revenues from EUA auction
      tmp4 <- mbind(tmp4, setNames(o_co2price*o_auctionEUA_regi*s_c2co2, "Revenues EUA auction (billion eur2010/yr)"))

    }

  }


  #concatenate reporting data
  tmp <- mbind(tmp3, tmp4)


  var_names <- c(
    "Emissions|CO2|Certificates for auction - before unilateral cancellation (Mt CO2/yr)",
    "Emissions|CO2|Certificates auctioned ETS (Mt CO2/yr)",
    "Emissions|CO2|Preliminary auction ETS (Mt CO2/yr)",
    "Revenues EUA auction (billion eur2010/yr)"
  )

  for(var in var_names) {
    if(var %in% getNames(tmp)) {
      tmp[,  c(2010, 2015),  var] <- NA
    }
  }



  return(tmp)
}

