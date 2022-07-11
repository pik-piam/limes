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
  pety <- readGDX(gdx,name="pety") #set of primary energies

  p_taulength <- readGDX(gdx,name=c("p_taulength","pm_taulength"),field="l",format="first_found")[,,tau] #number of hours/year per tau

  # read variables
  v_seprod <- readGDX(gdx,name="v_seprod",field="l",format="first_found")[,,tau]
  v_seprodmax <- readGDX(gdx,name="v_seprodmax",field="l",format="first_found")[,,tau]

  #Make sure only the "right" tau are taken -> to avoid info from gdx that might be stuck in the file
  v_seprod <- v_seprod[,,pety]
  v_seprod <- v_seprod[,,"seel"]

  # create MagPie object of variables with iso3 regions
  v_seprod <- limesMapping(v_seprod)
  v_seprodmax <- limesMapping(v_seprodmax)

  #curtailment of VRES ('ter')
  #and converting from GWh to TWh
  tmp1 <- NULL
  tmp1 <- mbind(tmp1,setNames((dimSums(v_seprodmax[,,ter]*p_taulength,dim=3)-dimSums(v_seprod[,,ter]*p_taulength,dim=3))/1000,"Secondary Energy|Electricity|Curtailment (TWh/yr)"))


  tmp2 <- NULL

  #concatenating global values
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}

