#' Read in GDX and calculate emissions, used in convGDX2MIF.R for the reporting
#' 
#' Read in emissions data from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains the emission variables
#' @author Sebastian Osorio, Renato Rodrigues
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportEmissions(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportFictitiousVars <- function(gdx) {
  
  # read sets and parameters
  t <- readGDX(gdx,name="t")
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  
  # read a variable with the minimum needed subindexes (t,regi)
  v_costfu <- readGDX(gdx,name="v_costfu",field="l",format="first_found")
  #c_emicapcum_regi <- readGDX(gdx,name="c_emicapcum_regi",field="l",format="first_found")
  
  # create MagPie object of v_costfu with iso3 regions
  v_costfu <- limesMapping(v_costfu)
  #c_emicapcum_regi <- limesMapping(c_emicapcum_regi)
  
  o_fictitious <- new.magpie(cells_and_regions = getRegions(v_costfu), years = t, names = NULL,
                             fill = 0, sort = FALSE, sets = NULL, unit = "unknown")
  
  tmp1 <- NULL
  
  AggVarPath <- system.file("extdata","AggregateVariables.csv",package="limes")
  # reading mapping file
  AggVarfile <- read.csv(AggVarPath,sep=";")
  #  write the *.mif or give back the magpie opject output
  AggVars <- paste0(as.vector(AggVarfile$LIMES)," (",as.vector(AggVarfile$UnitLIMES) , ")")
  
  #Check the version: When there is endogenous heating, related emissions should not appear here (to avoid duplicates)
  if(c_LIMESversion >= 2.28) {
    c_heating <- readGDX(gdx,name="c_heating",field="l",format="first_found")
    if(c_heating == 1) {
      AggVars <- AggVars[is.na(match(AggVars,"Emissions|CO2|Energy|Supply|Heat (Mt CO2/yr)"))]
    }
  } 
  
  for (name_var in AggVars) {
    tmp1 <- mbind(tmp1,setNames(o_fictitious,name_var))
  }
  
  tmp2 <- NULL
  
  # concatenate data
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
