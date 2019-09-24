#' Read in GDX and calculate MSR-related variables, used in convGDX2MIF.R for the reporting
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
#' \dontrun{reportEmissions(gdx)}
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @export
#' 
reportMSR <- function(gdx) {
  
  #Read LIMES version (industry was included in version 2.27)
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  tmp <-NULL
  
  if(c_LIMESversion > 2.26) {
    # read parameters
    s_c2co2 <- readGDX(gdx,name="s_c2co2",field="l",format="first_found") #conversion factor C -> CO2
    c_industry_ETS <- readGDX(gdx,name="c_industry_ETS",field="l",format="first_found")
    c_MSR <- readGDX(gdx,name="c_MSR",field="l",format="first_found")
    
    #read variables with default time step (5 years)
    p_auction_EUETS <- readGDX(gdx,name="p_auction_EUETS",field="l",format="first_found")
    p_freealloc_EUETS <- readGDX(gdx,name="p_freealloc_EUETS",field="l",format="first_found")
    p_withholdEUA <- readGDX(gdx,name="p_withholdEUA",field="l",format="first_found")
    p_backloadEUA <- readGDX(gdx,name="p_backloadEUA",field="l",format="first_found")
    p_cancelEUA <- readGDX(gdx,name="p_cancelEUA",field="l",format="first_found")
    p_MSR <- readGDX(gdx,name="p_MSR",field="l",format="first_found")
    p_extraintakeMSR <- readGDX(gdx,name="p_extraintakeMSR",field="l",format="first_found")
    p_emicappath_EUETS <- readGDX(gdx,name="p_emicappath_EUETS",field="l",format="first_found")
    p_sharefreeEUA <- readGDX(gdx,name="p_sharefreeEUA",field="l",format="first_found")
    
    
    #If variables should not exist (equations are off), write NA
    if(c_MSR == 0) {
      p_withholdEUA[] <- NA
      p_backloadEUA[] <- NA
      p_cancelEUA[] <- NA
      p_MSR[] <- NA
      p_extraintakeMSR[] <- NA
      p_auction_EUETS[] <- NA
      p_freealloc_EUETS[] <- NA
      p_emicappath_EUETS <- NA
      p_sharefreeEUA <- NA
      
    }
    
    #report the variables 
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #(DO NOT FORGET TO INCLUDE THESE VARIABLES IN MappingVars*.csv and AggregateVariables.csv EXCEL FILES)
    tmp1 <- NULL
    tmp1 <- mbind(tmp1,setNames(p_freealloc_EUETS*s_c2co2*1000,"Emissions|CO2|Free-allocated certificates ETS (Mt CO2/yr)"))
    tmp1 <- mbind(tmp1,setNames(p_withholdEUA*s_c2co2*1000,"Emissions|CO2|Intake to MSR (Mt CO2/yr)"))
    tmp1 <- mbind(tmp1,setNames(p_backloadEUA*s_c2co2*1000,"Emissions|CO2|Outtake from MSR (Mt CO2/yr)"))
    tmp1 <- mbind(tmp1,setNames(p_cancelEUA*s_c2co2*1000,"Emissions|CO2|Cancellation from MSR (Mt CO2/yr)"))
    tmp1 <- mbind(tmp1,setNames(p_extraintakeMSR*s_c2co2*1000,"Emissions|CO2|Additional intake to the MSR (Mt CO2)"))
    tmp1 <- mbind(tmp1,setNames(p_MSR*s_c2co2*1000,"Emissions|CO2|MSR level (Mt CO2)"))
    
    tmp2 <- NULL
    
    
    # concatenate data
    tmp <- mbind(tmp1,tmp2)
  }
  
  return(tmp)
}
  
