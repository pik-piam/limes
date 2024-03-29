#' Read in GDX and calculate emissions, used in convGDX2MIF.R for the reporting
#'
#' Read in data that only exists at EU ETS level, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param output a magpie object containing all needed variables generated by other report*.R functions
#' @return MAgPIE object - contains the emission variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportEUETSvars(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie getItems<-
#' @export
#'
reportEUETSvars <- function(gdx,output=NULL) {

  if(is.null(output)){
    stop("please provide a file containing all needed information")
  }

  # read parameters
  s_c2co2 <- readGDX(gdx,name="s_c2co2",field="l",format="first_found") #conversion factor C -> CO2
  c_bankemi_EU <- readGDX(gdx,name="c_bankemi_EU",field="l",format="first_found") #banking constraint... many of the variables should not be reported if EU ETS is not modelled at least partially

  #read variables
  v_bankemi <- readGDX(gdx,name="v_bankemi",field="l",format="first_found", react = 'silent')
  if(!is.null(v_bankemi)) { #This variable only existed until 20230831
    o_bankemi_EUETS <- v_bankemi
  }
  v_bankemi_EUETS <- readGDX(gdx,name="v_bankemi_EUETS",field="l",format="first_found", react = 'silent')
  if(!is.null(v_bankemi_EUETS)) { #This variable exists since 20230831
    o_bankemi_EUETS <- v_bankemi_EUETS
  }
  y <- getYears(output)
  p_emicappath_EUETS <- readGDX(gdx,name="p_emicappath_EUETS",field="l",format="first_found")[, y, ]


  #If variables should not exist (equations are off), write NA
  if(c_bankemi_EU == 0) {
    o_bankemi_EUETS[] <- NA
    p_emicappath_EUETS[] <- NA
  }

  #Initialize this variable
  o_aviation_demandEUA <- 0

  #report the variables
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #(DO NOT FORGET TO INCLUDE THESE VARIABLES IN mappingvars and aggvars EXCEL FILES, and to include in the fictitious file)
  tmp1 <- NULL

  tmp2 <- NULL
  #Check the version so one can check if the industry is already included
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  #until version 2.26 LIMES only included electricity
  if(c_LIMESversion <= 2.26) {
    tmp2 <- mbind(tmp2,setNames(o_bankemi_EUETS*s_c2co2*1000,"Emissions level in ETS|CO2|Energy|Supply|Electricity (Mt CO2)"))
    tmp2 <- mbind(tmp2,setNames(p_emicappath_EUETS*s_c2co2*1000,"EU ETS cap|CO2|Energy|Supply|Electricity (Mt CO2/yr)"))
  } else {

    #check if industry is included in the run
    c_industry_ETS <- readGDX(gdx,name="c_industry_ETS",field="l",format="first_found")
    #distinguish the cap (for the whole EU ETS (stationary), electricity and industry or only electricity)
    if(c_industry_ETS == 0) {
      tmp2 <- mbind(tmp2,setNames(o_bankemi_EUETS*s_c2co2*1000,"Emissions level in ETS|CO2|Energy|Supply|Electricity (Mt CO2)"))
      tmp2 <- mbind(tmp2,setNames(p_emicappath_EUETS*s_c2co2*1000,"EU ETS cap|CO2|Energy|Supply|Electricity (Mt CO2/yr)"))
    } else {
      tmp2 <- mbind(tmp2,setNames(o_bankemi_EUETS[,,]*s_c2co2*1000,"Emissions|CO2|Total number of allowances in circulation [TNAC] (Mt CO2)"))

      #include the aviation variables (only available from 2.28)
      if (c_LIMESversion >= 2.28 & c_bankemi_EU == 1) { #aviation-related variables are only required when modelling the EU ETS
        c_aviation <- readGDX(gdx,name="c_aviation",field="l",format="first_found")
        if(c_aviation > 0){
          p_aviation_cap <- readGDX(gdx,name="p_aviation_cap",field="l",format="first_found")[, y ,]
          p_aviation_emi <- readGDX(gdx,name="p_aviation_emi",field="l",format="first_found")[, y ,]
          o_aviation_demandEUA <- as.magpie(apply(mbind(p_aviation_emi*0,p_aviation_emi-p_aviation_cap),1:2,max))
          if (c_LIMESversion >= 2.31) {
            o_aviation_demandEUA <- readGDX(gdx,name="p_demaviationEUA",field="l",format="first_found")[, y ,]
          }
          #include historical emissions in 2015
          o_aviation_demandEUA[,2015,] <- 20/(s_c2co2*1000)

          tmp2 <- mbind(tmp2,setNames(p_aviation_cap*s_c2co2*1000,"Emissions|CO2|Cap|Aviation (Mt CO2/yr)"))
          tmp2 <- mbind(tmp2,setNames(p_aviation_emi*s_c2co2*1000,"Emissions|CO2|Aviation (Mt CO2/yr)"))
          tmp2 <- mbind(tmp2,setNames(o_aviation_demandEUA*s_c2co2*1000,"Emissions|CO2|Certificates from Stationary|Aviation (Mt CO2/yr)"))
        }
      }

      #LOADING LIST OF REGIONS TO AGGREGATE CERTAIN GROUPS (e.g., EU)
      # settings mapping path
      mappingregiPath <- system.file("extdata","LIMES_country_ISO_3.csv",package="limes")
      # reading mapping file
      mappingregi <- read.csv(mappingregiPath,sep=";")
      #loading the countries belonging to 'regeuets'
      regeuets_iso2 <- readGDX(gdx,name="regeuets")
      regeuets_iso3 <- mappingregi[match(regeuets_iso2,mappingregi[,1]),2]
      regeuets <- regeuets_iso3 #Better to take it directly from the GDX file
      o_emi_elec_ind <- NULL
      if("Emissions|CO2|Electricity and Industry (Mt CO2/yr)" %in% getNames(output)) {
        #Include UK until 2020 (only when the link has been implemented and there is no link)
        o_emi_elec_ind <- setNames(dimSums(output[regeuets,,"Emissions|CO2|Electricity and Industry (Mt CO2/yr)"], dim=1), NULL)

        c_linkEUETS_UK <- readGDX(gdx, name = c("c_linkEUETS_UK"), field = "l", format = "first_found", react = 'silent') #link between EU ETS and UK ETS, and thus when B
        if(!is.null(c_linkEUETS_UK)) {
          if(c_linkEUETS_UK == 0) {
            #Take UK off after 2020
            o_emi_elec_ind[,setdiff(getYears(output),paste0("y",seq(2010,2020,5))),] <-
              setNames(dimSums(output[setdiff(regeuets,"GBR"),setdiff(y,paste0("y",seq(2010,2020,5))),"Emissions|CO2|Electricity and Industry (Mt CO2/yr)"], dim=1), NULL)
          }
        }
        #Add region "GlO" to avoid errors in forecoming sums
        getItems(o_emi_elec_ind, dim = 1) <- "GLO"
      }


      #Load share from heating
      p_shareheating_EUETS <- readGDX(gdx,name="p_shareheating_EUETS",field="l",format="first_found")[, y, ]
      #In v2.27 there is no heating. Thus, if industry was included, the share for heating had to be subtracted
      if(c_LIMESversion == 2.27) {
        tmp2 <- mbind(tmp2,setNames(p_emicappath_EUETS[,,]*s_c2co2*1000,"Emissions|CO2|Cap|Stationary|Electricity and Industry (Mt CO2/yr)"))
        tmp2 <- mbind(tmp2,setNames(p_emicappath_EUETS[,,]*s_c2co2*1000/(1-p_shareheating_EUETS),"Emissions|CO2|Cap|Stationary (Mt CO2/yr)"))
      } else { #i.e., c_LIMESversion > 2.27

        p_certificates_cancelled <- readGDX(gdx,name="p_certificates_cancelled",field="l",format="first_found")[, y, ]

        #From this version, we estimate differently the auction, free allocation, unilateral cancellation, and thus the cap
        if(c_LIMESversion <= 2.30) {
          tmp2 <- mbind(tmp2,setNames(p_emicappath_EUETS[,,]*s_c2co2*1000,"Emissions|CO2|Cap|Stationary|Electricity and Industry (Mt CO2/yr)"))
          o_prelcap <- ((p_emicappath_EUETS[,,]+o_aviation_demandEUA)*s_c2co2*1000+p_certificates_cancelled)/(1-p_shareheating_EUETS)
          tmp2 <- mbind(tmp2,setNames(o_prelcap,"Emissions|CO2|Cap|Stationary (Mt CO2/yr)"))
          o_exoemiheat <- o_prelcap*p_shareheating_EUETS
          #o_exoemiheat[,c(2010,2015),] <- c(317,272)  #include historical heating emissions from 2010 and 2015
          tmp2 <- mbind(tmp2,setNames(o_exoemiheat,"Emissions|CO2|Energy|Supply|Heat|District Heating (Mt CO2/yr)"))
          if(!is.null(o_emi_elec_ind)) {
            tmp2 <- mbind(tmp2,setNames(o_emi_elec_ind + o_exoemiheat,"Emissions|CO2|EU ETS (Mt CO2/yr)")) #this does not include aviation demand
            tmp2 <- mbind(tmp2,setNames(o_emi_elec_ind + o_exoemiheat + o_aviation_demandEUA*s_c2co2*1000,"Emissions|CO2|EU ETS|w/ aviation (Mt CO2/yr)")) #this includes aviation demand
          }

        } else {#c_LIMESversion > 2.30

          if(c_LIMESversion <= 2.33) {
            tmp2 <- mbind(tmp2,setNames(p_emicappath_EUETS[,,]*s_c2co2*1000,"Emissions|CO2|Cap|Stationary (Mt CO2/yr)"))
          } else {
            p_emicap_EUETS <- readGDX(gdx,name="p_emicap_EUETS",field="l",format="first_found")[, y, ]
            tmp2 <- mbind(tmp2,setNames(p_emicap_EUETS[,,]*s_c2co2*1000,"Emissions|CO2|Cap|Stationary (Mt CO2/yr)"))

            #Include some historical values
            tmp2[, c(2010), "Emissions|CO2|Cap|Stationary (Mt CO2/yr)"] <- 2049 #Cap in phase 2 (2008-2012)
            tmp2[, c(2015), "Emissions|CO2|Cap|Stationary (Mt CO2/yr)"] <- 2008 #average of cap between 2013 and 2017

            p_unsoldEUA <- readGDX(gdx,name="p_unsoldEUA",field="l",format="first_found")[, y, ]
            tmp2 <- mbind(tmp2,setNames(p_unsoldEUA[,,]*s_c2co2*1000,"Emissions|CO2|Unallocated certificates (Mt CO2/yr)"))
          }


          ##Report depending on the heat representation
          modules <- readGDX(gdx,name="modules",field="l",format="first_found", react = 'silent')

          #identify if the model version is modular. If not, create the equivalence for the old switch c_heating
          if(is.null(modules)) {
            c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found", react = 'silent')
            #equivalence of heating scenarios
            tmp <- list("0" = "off",
                        "1" = "fullDH",
                        "2" = "mac")
            heating <- tmp[[which(names(tmp) == as.character(c_heating))]]
          } else {
            #Load switch for heating
            heating <- .readHeatingCfg(gdx)
          }

          #Link with UK ETS (needed to make some adjustments in how some variables are aggregated)
          c_linkEUETS_UK <- readGDX(gdx, name = c("c_linkEUETS_UK"), field = "l", format = "first_found", react = 'silent') #link between EU ETS and UK ETS

          #Check if share was included already
          p_share_EmiHeat_UK <- readGDX(gdx,name="p_share_EmiHeat_UK",field="l",format="first_found", react = 'silent')
          if(is.null(p_share_EmiHeat_UK)) {
            p_share_EmiHeat_UK <- 0
          }

          #Previous version did not have endogenous heating
          #-> with endogenous this variable will be calculated from the region-based heating
          if(heating == "off") {
            p_exoemiheat <- readGDX(gdx,name="p_exoemiheat",field="l",format="first_found")[, y, ] #exogenous emissions from heating (share of cap)
            o_DH_emi <- p_exoemiheat * s_c2co2 * 1000

            #Adjust if Brexit implemented
            if(!is.null(c_linkEUETS_UK)) { #When there is switch for the link, this parameter does exist, and we need to adjust the EU ETS value
              o_DH_emi[,setdiff(y,paste0("y",seq(2010,2020,5))),] <- o_DH_emi[,setdiff(y,paste0("y",seq(2010,2020,5))),] * (1 - p_share_EmiHeat_UK)
            }

            tmp2 <- mbind(tmp2,setNames(o_DH_emi, "Emissions|CO2|Energy|Supply|Heat|District Heating (Mt CO2/yr)"))

          } else if(heating == "mac") {
            #Read additional parameters
            p_DH_emiabat <- readGDX(gdx,name="p_DH_emiabat",field="l",format="first_found")[, y, ]
            v_DH_emiabatproc <- readGDX(gdx,name="v_DH_emiabatproc",field="l",format="first_found")

            #Estimate DH emissions (baselines - abated)
            o_DH_emi <- p_DH_emiabat-v_DH_emiabatproc
            o_DH_emi <- dimSums(o_DH_emi,3)
            o_DH_emi <- o_DH_emi * s_c2co2 * 1000

            #Adjust if Brexit implemented
            if(!is.null(c_linkEUETS_UK)) { #When there is switch for the link, this parameter does exist, and we need to adjust the EU ETS value
              o_DH_emi[,setdiff(y,paste0("y",seq(2010,2020,5))),] <- o_DH_emi[,setdiff(y,paste0("y",seq(2010,2020,5))),] * (1 - p_share_EmiHeat_UK)
            }

            #Add NA to 2010 (no value in parameter) and name
            o_DH_emi[,c(2010),] <- NA
            tmp2 <- mbind(tmp2,setNames(o_DH_emi,"Emissions|CO2|Energy|Supply|Heat|District Heating (Mt CO2/yr)"))

          } else { #fullDH

            #Include UK until 2020 (only when the link has been implemented and there is no link)
            o_DH_emi <- setNames(dimSums(output[regeuets,,"Emissions|CO2|Energy|Supply|Heat|District Heating (Mt CO2/yr)"], dim=1), NULL)

            if(!is.null(c_linkEUETS_UK)) {
              if(c_linkEUETS_UK == 0) {
                #Take UK off after 2020
                o_DH_emi[,setdiff(getYears(output),paste0("y",seq(2010,2020,5))),] <-
                  setNames(dimSums(output[setdiff(regeuets,"GBR"),setdiff(y,paste0("y",seq(2010,2020,5))),
                                          "Emissions|CO2|Energy|Supply|Heat|District Heating (Mt CO2/yr)"], dim=1), NULL)
              }
            }
            #Add region "GLO" to avoid errors in forecoming sums
            getItems(o_DH_emi, dim = 1) <- "GLO"

          } #end if regarding heating representation

        } #end else c_LIMESversion > 2.30

      } ##end else c_LIMESversion > 2.27

    } #end of industry representation

  }

  # concatenate data
  tmp <- mbind(tmp1,tmp2)

  #Additional variables to represent linkage of EU ETS with other systems
  tmp3 <- NULL
  c_linkEUETS_UK <- readGDX(gdx, name = "c_linkEUETS_UK", field="l", format = "first_found", react = 'silent')
  if(!is.null(c_linkEUETS_UK)) {
    if(c_linkEUETS_UK == 1) {
      v_bankemi_EUETSlinked <- readGDX(gdx, name = "v_bankemi_EUETSlinked", field="l", format = "first_found", react = 'silent')
      o_bankemi_EUETSlinked <- new.magpie(cells_and_regions = getItems(v_bankemi_EUETSlinked, dim = 1), years = y, names = NA,
                                          fill = NA, sort = FALSE, sets = NULL)
      o_bankemi_EUETSlinked[,getYears(v_bankemi_EUETSlinked),] <- v_bankemi_EUETSlinked[,,"l"]
      tmp3 <- mbind(tmp3,setNames(o_bankemi_EUETSlinked*s_c2co2*1000,"Emissions|CO2|Joint TNAC with linked systems (Mt CO2)"))
    }
  }

  # concatenate data
  tmp <- mbind(tmp,tmp3)

  #Maritime
  tmp4 <- NULL
  c_maritime <- readGDX(gdx, name = "c_maritime", format = "first_found", react = 'silent')
  o_EmiEUETS_Maritime <- 0 #Assume 0 for when not included/represented in model
  if(!is.null(c_maritime)) {
    if(c_maritime >= 1) {

      #Load parameters
      p_EmiEUETS_Maritime <- readGDX(gdx, name = c("p_EmiEUETS_Maritime","p_maritime_emi"), format = "first_found", react = 'silent')[, y, ]
      p_EmiCapEUETS_Maritime <- readGDX(gdx, name = c("p_EmiCapEUETS_Maritime","p_maritime_cap"), format = "first_found", react = 'silent')[, y, ]

      tmp4 <- mbind(tmp4, setNames(p_EmiCapEUETS_Maritime * s_c2co2 * 1000,
                                   "Emissions|CO2|Cap|Maritime (Mt CO2/yr)"))
      tmp4 <- mbind(tmp4, setNames(tmp[,, "Emissions|CO2|Cap|Stationary (Mt CO2/yr)"] + p_EmiCapEUETS_Maritime * s_c2co2 * 1000,
                                   "Emissions|CO2|Cap|Stationary|w/ Maritime (Mt CO2/yr)"))

      #Representation of emissions have changed
      #Some model version has fixed emissions and only considered until 2030
      #This was replaced by a MAC for maritime
      if(!is.null(p_EmiEUETS_Maritime)) {
        o_EmiEUETS_Maritime <- p_EmiEUETS_Maritime * s_c2co2 * 1000
        tmp4 <- mbind(tmp4, setNames(o_EmiEUETS_Maritime, "Emissions|CO2|Maritime (Mt CO2/yr)"))
      }

      #Load parameters
      p_MACC_AbatPotEUETS_Maritime <- readGDX(gdx, name = c("p_MACC_AbatPotEUETS_Maritime","p_MACC_AbatPot_Maritime"), format = "first_found", react = 'silent')
      v_EmiAbatProcEUETS_Maritime <- readGDX(gdx, name = c("v_EmiAbatProcEUETS_Maritime","v_EmiAbatProc_Maritime"), field="l", format = "first_found", react = 'silent')

      if(!is.null(p_MACC_AbatPotEUETS_Maritime)) {
        #Estimate Maritime emissions (baselines - abated)
        o_EmiEUETS_Maritime <- dimSums(p_MACC_AbatPotEUETS_Maritime - v_EmiAbatProcEUETS_Maritime, 3) * s_c2co2 * 1000

        tmp4 <- mbind(tmp4, setNames(dimSums(v_EmiAbatProcEUETS_Maritime, 3) * s_c2co2 * 1000, "Emissions abated|CO2|Maritime (Mt CO2/yr)"))
        tmp4 <- mbind(tmp4, setNames(o_EmiEUETS_Maritime, "Emissions|CO2|Maritime (Mt CO2/yr)"))
      }

    }
  }

  #Aggregated emissions EU ETS
  if(c_bankemi_EU == 1) {
    #Emissions from other sectors
    p_emiothersec <- readGDX(gdx,name="p_emiothersec",field="l",format="first_found")[, y, ] #exogenous emissions (from other sectors if introduced into the EU ETS)
    tmp4 <- mbind(tmp4,setNames(p_emiothersec*s_c2co2*1000,"Emissions|CO2|Additional sectors in EU ETS (Mt CO2/yr)"))
    tmp4 <- mbind(tmp4,setNames(o_emi_elec_ind + o_DH_emi + o_EmiEUETS_Maritime + (p_emiothersec + o_aviation_demandEUA)*s_c2co2*1000,
                                "Emissions|CO2|EU ETS|w/ aviation (Mt CO2/yr)")) #this includes aviation demand
    tmp4 <- mbind(tmp4,setNames(o_emi_elec_ind + o_DH_emi + o_EmiEUETS_Maritime + (p_emiothersec)*s_c2co2*1000,
                                "Emissions|CO2|EU ETS (Mt CO2/yr)")) #this includes aviation demand

  } #end if c_bankemi_EU == 1

  # concatenate data
  tmp <- mbind(tmp,tmp4)


  #Add NAs to avoid inconsistencies: There are no industry emissions values for 2010 and 2015
  var_names <- c(
    "Emissions|CO2|Certificates from Stationary|Aviation (Mt CO2/yr)"
  )

  for(var in var_names) {
    if(var %in% getNames(tmp)) {
      tmp[, c(2010), var] <- NA
    }
  }

  var_names <- c(
    "Emissions|CO2|Cap|Stationary|Electricity and Industry (Mt CO2/yr)",
    "Emissions|CO2|EU ETS|w/ aviation (Mt CO2/yr)",
    "Emissions|CO2|Cap|Aviation (Mt CO2/yr)",
    "Emissions|CO2|Aviation (Mt CO2/yr)"
  )

  for(var in var_names) {
    if(var %in% getNames(tmp)) {
      tmp[, c(2010,2015), var] <- NA
    }
  }

  var_names <- c(
    "Emissions|CO2|Cap|Maritime (Mt CO2/yr)",
    "Emissions abated|CO2|Maritime (Mt CO2/yr)",
    "Emissions|CO2|Maritime (Mt CO2/yr)"
  )

  for(var in var_names) {
    if(var %in% getNames(tmp)) {
      tmp[, c(2010,2015,2020), var] <- NA
    }
  }











  return(tmp)
}

