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

  # WRITE REPORT
  # load the model version
  c_LIMESversion <- readGDX(gdx, name = "c_LIMESversion", field = "l", format = "first_found") # model version
  if (!is.null(file)) {
    write.report(output, model = paste0("LIMES_EU_v", c_LIMESversion), scenario = scenario, file = file, ndigit = 7)
  } else {
    return(output)
  }
}
