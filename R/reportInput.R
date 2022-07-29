#' Read in GDX and calculate availability factors,  used in convGDX2MIF.R for the reporting
#'
#' Read in availability factors information from GDX file,  information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX,  or the path to a gdx
#' @param mappingPath path to mapping file
#' @return MAgPIE object - contains the availability factors
#' @author Sebastian Osorio,  Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportNuren(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets
#' @importFrom magclass new.magpie getYears getNames getItems collapseDim
#' @importFrom luscale speed_aggregate
#' @importFrom utils read.csv
#' @export
#'
reportInput <- function(gdx, mappingPath = NULL) {

  #0) Load technology mapping
  # settings mapping path
  if (is.null(mappingPath))
    mappingPath <- system.file("extdata", "MappingTech.csv", package = "limes")
  # reading mapping file
  mapping_tech <- read.csv(mappingPath, sep = ";")

  c_reportheating <- 0


  #1) Availability and expansion potential technologies per grade
  # read parameters and sets
  p_nurenannual_adj <- readGDX(gdx, name = "p_nurenannual_adj", field = "l", format = "first_found") #annual availability factor for RES (per grade)
  p_nurenannual_adj2 <- readGDX(gdx, name = "p_nurenannual_adj2", field = "l", format = "first_found") #annual availability factor for RES
  f_capmax <- readGDX(gdx, name = "f_capmax", field = "l", format = "first_found") #capacity potential (per grade)
  ter <- readGDX(gdx, name = "ter") #set of variable renewable generation technologies
  grade <- readGDX(gdx, name = "grade") #set of grades (quality of RES potential)
  p_taulength <- readGDX(gdx, name = c("p_taulength", "pm_taulength"), field = "l", format = "first_found") #number of hours/year per tau
  #p_nuren_adj <- readGDX(gdx, name = "p_nuren_adj", field = "l", format = "first_found") #availability factor for RES
  #tau <- readGDX(gdx, name = "tau") #set of time slices

  #Make sure only the "right" tau are taken -> to avoid info from gdx that might be stuck in the file
  #p_nuren <- p_nuren[, , tau]
  #p_taulength <- p_taulength[, , tau]

  # create MagPie object of demand with iso3 regions
  #p_nuren_adj <- limesMapping(p_nuren_adj[, , c(ter)])
  p_nurenannual_adj <- limesMapping(p_nurenannual_adj[, , c(ter)])
  p_nurenannual_adj2 <- limesMapping(p_nurenannual_adj2[, , c(ter)])
  f_capmax <- limesMapping(f_capmax[, , c(ter)])

  #Allocate years for expansion potential
  o_capmax <- limesAllocateYears(f_capmax, gdx)

  #REPORT ONLY ELECTRICITY-RELATED FACTORS
  ter <- setdiff(ter, "sol_heat")

  tmp1 <- NULL
  for (ter2 in ter) {
    tech_name <- as.character(mapping_tech[mapping_tech$LIMES_tech  ==  ter2, ]$Report_tech)

    #tmp1 <- mbind(tmp1, setNames(p_nurenannual_adj2[, , ter2], as.character(paste0("Annual availability factor|Electricity|", tech_name, " (--)")))) #Now it is below
    #tmp1 <- mbind(tmp1, setNames(dimSums(o_capmax[, , ter2], dim = 3), as.character(paste0("Expansion Potential||Electricity|", tech_name, " (GW)"))))

    for (grade2 in grade) {
      tmp1 <- mbind(tmp1, setNames(p_nurenannual_adj[, , paste0(ter2, ".", grade2)], paste0("Annual availability factor|Electricity|", tech_name, "|Grade|", grade2, " (--)")))
      #tmp1 <- mbind(tmp1, setNames(o_capmax[, , paste0(ter2, ".", grade2)], paste0("Expansion Potential|Electricity|", tech_name, "|Grade|", grade2, " (GW)")))
    }
  }

  #2) Technologies' parameters

  # read sets
  te <- readGDX(gdx, name = "te") #set of technologies
  tehe <- readGDX(gdx, name = "tehe") #set of technologies
  testore <- readGDX(gdx, name = "testore") #set of technologies
  #Do not report some technologies for the moment
  tech_out <- c("ror", "hs", "hvacline")
  #Do not include heating technologies if switch is off
  if(c_reportheating  ==  0) {
    tech_out <- c(tech_out, tehe)
  }
  te <- setdiff(te, tech_out)

  # read parameters
  p_incoall <- readGDX(gdx, name = "p_incoall", field = "l", format = "first_found") #investment costs for power capacity
  p_tedata <- readGDX(gdx, name = "p_tedata", field = "l", format = "first_found") #technology data
  p_emifac <- readGDX(gdx, name = c("p30_emifac","p_emifac"), field = "l", format = "first_found") #emissions factors
  p_incostall <- readGDX(gdx, name = "p_incostall", field = "l", format = "first_found") #investment costs for reservoir capacity

  # create MagPie object of demand with iso3 regions
  p_incoall <- limesMapping(p_incoall)
  p_tedata <- limesMapping(p_tedata)
  p_incostall <- limesMapping(p_incostall[, , testore])

  #Allocate years to p_tedata
  o_omv <- limesAllocateYears(p_tedata[, , "omv"], gdx)
  o_eta <- limesAllocateYears(p_tedata[, , "eta"], gdx)
  o_lifetime <- limesAllocateYears(p_tedata[, , "lifetime"], gdx)
  o_nu2 <- limesAllocateYears(p_tedata[, , "nu2"], gdx)
  o_buildtime <- limesAllocateYears(p_tedata[, , "buildtime"], gdx)
  o_autocons <- limesAllocateYears(p_tedata[, , "autocons"], gdx)

  #Emission factor for each country
  o_emifac_tmp <- limesAllocateYears(p_emifac[, , "co2"], gdx)
  o_emifac_tmp <- collapseDim(o_emifac_tmp,  dim  =  3.2)
  o_emifac <- new.magpie(cells_and_regions  =  getItems(p_incoall,  dim  =  1),  years  =  getYears(p_incoall),  names  =  getNames(o_emifac_tmp),
                         fill  =  NA,  sort  =  FALSE,  sets  =  NULL,  unit  =  "unknown")
  for (regi in getItems(p_incoall,  dim  =  1)) {
    o_emifac[regi, , ] <- o_emifac_tmp["GLO", , ]
  }

  #single technologies
  tmp2 <- NULL

  #adding the name of the variable and the technology
  for (te2 in te) { #ignoring the transmission data
    tech_name <- as.character(mapping_tech[mapping_tech$LIMES_tech  ==  te2, ]$Report_tech)
    tmp2 <- mbind(tmp2, setNames(p_incoall[, , te2]*1000, paste0("Investment costs|", tech_name, " (eur/kW)")))
    tmp2 <- mbind(tmp2, setNames(p_incoall[, , te2]*1000*p_tedata[, , paste0("omf.", te2)], paste0("Fixed O&M costs|", tech_name, " (eur/kW-yr)")))
    tmp2 <- mbind(tmp2, setNames(o_omv[, , te2]*1e6, paste0("Variable O&M costs|", tech_name, " (eur/MWh)")))
    tmp2 <- mbind(tmp2, setNames(o_eta[, , te2], paste0("Electrical efficiency|", tech_name, " (--)")))
    tmp2 <- mbind(tmp2, setNames(o_lifetime[, , te2], paste0("Lifetime|", tech_name, " (yr)")))
    tmp2 <- mbind(tmp2, setNames(o_buildtime[, , te2], paste0("Buildtime|", tech_name, " (yr)")))
    tmp2 <- mbind(tmp2, setNames(o_autocons[, , te2], paste0("Autoconsumption|", tech_name, " (--)")))

    if(te2 %in% ter) {
      tmp2 <- mbind(tmp2, setNames(p_nurenannual_adj2[, , te2], as.character(paste0("Annual availability factor|Electricity|", tech_name, " (--)"))))
    } else {
      tmp2 <- mbind(tmp2, setNames(o_nu2[, , te2]*1e6, paste0("Annual availability factor|", tech_name, " (--)")))
    }

    if(te2 %in% testore) {
      tmp2 <- mbind(tmp2, setNames(p_incostall[, , te2]*1000, as.character(paste0("Investment costs|Storage|", tech_name, " (eur/kWh)"))))
    }

    if(sum(o_emifac[, , te2]) !=  0) { #Convert from [Gt/GWh] to [eur/kWh]
      tmp2 <- mbind(tmp2, setNames(o_emifac[, , te2]*(44/12)*1e9/(o_eta[, , te2]*(1-o_autocons[, , te2])), as.character(paste0("Emission factor|", tech_name, " (eur/kWh)"))))
    }

  }

  #3) Demand

  # read parameters
  p_exdemand <- readGDX(gdx, name = "p_exdemand", field = "l", format = "first_found") #electricity and heat demand
  c_demandscale <- readGDX(gdx, name = "c_demandscale", field = "l", format = "first_found") #factor for scaling electricity demand
  p_losses_heat <- readGDX(gdx, name = c("f_losses_heat", "p_losses_DH"), field = "l", format = "first_found")

  # create MagPie object of demand with iso3 regions
  p_exdemand <- limesMapping(p_exdemand)
  p_losses_heat <- limesMapping(p_losses_heat)

  #Split electricity and heat demand
  o_eldemand <- p_exdemand[, , "seel"]
  o_hedemand <- p_exdemand[, , "sehe"]

  tmp3 <- NULL
  tmp3 <- mbind(tmp3, setNames((dimSums(o_eldemand*p_taulength, dim = 3)/(c_demandscale))/1000, "Final energy|Electricity [exogenous] (TWh)"))
  tmp3 <- mbind(tmp3, setNames((dimSums(o_eldemand*p_taulength, dim = 3))/1000, "Final energy|Electricity|w/ losses [exogenous] (TWh)"))

  if(c_reportheating  ==  1) {
    tmp3 <- mbind(tmp3, setNames((dimSums(o_hedemand*p_taulength, dim = 3)/(1+p_losses_heat))/1000, "Final energy|Heat [exogenous] (TWh)"))
    tmp3 <- mbind(tmp3, setNames((dimSums(o_hedemand*p_taulength, dim = 3))/1000, "Final energy|Heat|w/ losses [exogenous] (TWh)"))
  }#

  # add global values
  tmp <- mbind(tmp1, tmp2, tmp3)

  return(tmp)
}

