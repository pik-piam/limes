#' Read in GDX and calculate industry emissions, used in convGDX2MIF.R for the reporting
#'
#' Read in emissions data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the emission variables
#' @author Sebastian Osorio
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportOtherEUETSsector(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#'
reportOtherEUETSsector <- function(gdx) {

  .tmp <- NULL

  ##Aviation




  #"Emissions abated|CO2|Aviation (Mt CO2/yr)"
  #"Emissions|CO2|Aviation (Mt CO2/yr)"

  # read switch for new industry module
  o_AviationEmi_regi <- readGDX(gdx,name="o_AviationEmi_regi",field="l",format="first_found", react = 'silent')

  #Check if the parameter has been defined
  if(!is.null(o_AviationEmi_regi)) {
    if(o_AviationEmi_regi >= 1) {


      }
  }



  return(.tmp)
}

