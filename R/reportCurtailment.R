#' Read in GDX and calculate electricity curtailment, used in convGDX2MIF.R for the reporting
#'
#' Read in electricity curtailment data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the curtailment
#' @author Sebastian Osorio
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#'
#' \dontrun{reportCurtailment(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums
#' @export
#'
reportCurtailment <- function(gdx) {

  # read parameters and sets
  tau <- readGDX(gdx,name="tau") #set of time slices
  ter <- readGDX(gdx,name="ter") #set of variable renewable generation technologies
  teel <- readGDX(gdx,name="teel") #set of variable renewable generation technologies
  pety <- readGDX(gdx,name="pety") #set of primary energies
  p_taulength <- readGDX(gdx,name=c("p_taulength","pm_taulength"),field="l",format="first_found")[,,tau] #number of hours/year per tau

  # read variables
  v_seprod <- readGDX(gdx, name = "v_seprod", field = "l", format = "first_found", restore_zeros = FALSE)[, , tau]
  v_seprodmax <- readGDX(gdx,name="v_seprodmax", field="l", format="first_found", restore_zeros = FALSE)[,, tau]

  #Make sure only the "right" dimensions are taken -> to avoid info from gdx that might be stuck in the file
  v_seprod_ter <- v_seprod[,,ter]

  # create MagPie object of variables with iso3 regions
  v_seprod_ter <- limesMapping(v_seprod_ter[,,"seel"])
  v_seprodmax <- limesMapping(v_seprodmax)

  #Collapse some dimensions to avoid problems
  v_seprod_ter <- collapseDim(v_seprod_ter, dim = c(3.2,3.3))

  #curtailment of VRES ('ter')
  # generation per aggregated technology per country

  varList_el <- list(
    # Renewable
    "Secondary Energy|Electricity|Curtailment (TWh/yr)"               = intersect(teel, ter),
    "Secondary Energy|Electricity|Curtailment|Wind (TWh/yr)"          = intersect(teel, c("windon", "windoff")),
    "Secondary Energy|Electricity|Curtailment|Wind|Onshore (TWh/yr)"  = intersect(teel, c("windon")),
    "Secondary Energy|Electricity|Curtailment|Wind|Offshore (TWh/yr)" = intersect(teel, c("windoff")),
    "Secondary Energy|Electricity|Curtailment|Solar (TWh/yr)"         = intersect(teel, c("spv", "csp")),
    "Secondary Energy|Electricity|Curtailment|Solar|PV (TWh/yr)"      = intersect(teel, c("spv")),
    "Secondary Energy|Electricity|Curtailment|Solar|CSP (TWh/yr)"     = intersect(teel, c("csp"))
  )


  #and converting from GWh to TWh
  tmp1 <- NULL
  for (var in names(varList_el)) {
    tmp1 <- mbind(tmp1, setNames(dimSums(
      (dimSums(v_seprodmax[, , varList_el[[var]]] - v_seprod_ter[, , varList_el[[var]]], dim = c(3.2)) )
                                         * p_taulength, dim = 3.1)
                                 / 1000,
                                 var))
  }

  tmp2 <- NULL

  #concatenating global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}

