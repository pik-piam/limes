#' Read in GDX and write *.mif time slice reporting
#'
#' Read in all information from GDX file and create
#' the *.mif reporting for the power generation at the time slice resolution
#'
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param file name of the mif file which will be written, if no name is
#' provided a magpie object containing all the reporting information is
#' returned
#' @param scenario scenario name that is used in the *.mif reporting
#' @param time temporal resolution of the reporting, default: c(seq(2010,2050,5))
#' @author Antoine Levesque
#' @examples
#' \dontrun{
#' convGDX2MIF_tau(gdx, file = "LIMES_tau_default.csv", scenario = "default")
#' }
#'
#' @export
#' @importFrom magclass mbind write.report getNames getItems<- getItems


convGDX2MIF_tau <- function(gdx, file = NULL, scenario = "default", time = as.numeric(readGDX(gdx, name = "t"))) {
  # generate the report

  # initialize report variable
  output <- NULL

  # Reporting at the tau level only for Generation output

  # adding electricity generation info to report output
  output <- mbind(output, reportGeneration(gdx,
                                           output,
                                           reporting_tau = TRUE)[, time, ]
  )
  # adding demand
  output <- mbind(output, reportDemand(gdx,
                                       output,
                                       reporting_tau = TRUE)[, time, ]
  )

  # adding electricity prices
  output <- mbind(output, reportElectricityPrices(gdx,
                                                  reporting_tau = TRUE)[, time, ]
  )

  #Save file before aggregation
  output_beforeagg <- output
  #output <-  output_beforeagg

  #AGGREGATE (WEIGHTED AVERAGE OF) SOME INTENSIVE VARIABLES (e.g., electricity price)
  output_RegAgg <- limesInt2Ext(output,gdx)

  #Grouping countries
  #aggregating all countries
  output_tot <- dimSums(output,dim=1, na.rm = T)
  getItems(output_tot, dim = 1) <- "GLO"
  #Replacing the aggregated for intensive variables (a sum that makes no sense) by the weighted average calculated above
  output_tot[,,getNames(output_RegAgg)] <- output_RegAgg["GLO",,]

  #aggregating only EU-28
  EU<-which(getItems(output, dim = 1) != "NOR" & getItems(output, dim = 1) != "CHE" & getItems(output, dim = 1) != "BAL" & getItems(output, dim = 1) != "GLO")
  output_EU<-NULL
  output_EU<-dimSums(output[EU,,],dim=1, na.rm = T)
  getItems(output_EU, dim = 1) <- "EU28"
  #Replacing the aggregated for intensive variables (a sum that makes no sense) by the weighted average calculated above
  output_EU[,,getNames(output_RegAgg)] <- output_RegAgg["EU28",,]

  #aggregating only EU-27
  EU27<-which(getItems(output, dim = 1) != "GBR" & getItems(output, dim = 1) != "NOR" & getItems(output, dim = 1) != "CHE" & getItems(output, dim = 1) != "BAL" & getItems(output, dim = 1) != "GLO")
  output_EU27<-NULL
  output_EU27<-dimSums(output[EU27,,],dim=1, na.rm = T)
  getItems(output_EU27, dim = 1) <- "EU27"
  #Replacing the aggregated for intensive variables (a sum that makes no sense) by the weighted average calculated above
  output_EU[,,getNames(output_RegAgg)] <- output_RegAgg["EU27",,]

  #aggregating only EU-ETS
  EUETS_pre2020<-which(getItems(output, dim = 1) != "CHE" & getItems(output, dim = 1) != "BAL" & getItems(output, dim = 1) != "GLO")
  EUETS_post2020<-which(getItems(output, dim = 1) != "CHE" & getItems(output, dim = 1) != "BAL" & getItems(output, dim = 1) != "GLO" & getItems(output, dim = 1) != "GBR")
  output_EUETS<-NULL
  #ETS until 2020 contains UK
  output_EUETS<-dimSums(output[EUETS_pre2020,,],dim=1, na.rm = T)
  #ETS after 2020 does not contain UK
  output_EUETS[,setdiff(time,c(2010:2020)),]<-dimSums(output[EUETS_post2020,setdiff(time,c(2010:2020)),],dim=1, na.rm = T)
  getItems(output_EUETS, dim = 1)<-"EUETS"
  #Replacing the aggregated for intensive variables (a sum that makes no sense) by the weighted average calculated above
  output_EUETS[,,getNames(output_RegAgg)] <- output_RegAgg["EUETS",,] #Add intensive variables

  #Concatenate options
  output <- mbind(output,output_tot,output_EU,output_EU27,output_EUETS)

  # WRITE REPORT
  # load the model version
  c_LIMESversion <- readGDX(gdx, name = "c_LIMESversion", field = "l", format = "first_found") # model version
  if (!is.null(file)) {
    write.report(output, model = paste0("LIMES_EU_v", c_LIMESversion), scenario = scenario, file = file, ndigit = 7)
  } else {
    return(output)
  }
}
