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
  
  # read a variable with the minimum needed subindexes (t,regi)
  v_costfu <- readGDX(gdx,name="v_costfu",field="l",format="first_found")
  
  # create MagPie object of v_costfu with iso3 regions
  v_costfu <- limesMapping(v_costfu)
  
  o_fictitious <- v_costfu*0
  
  ##create fictitious variables of those that only exist for an aggregated region, e.g., the EU ETS
  #v_bankemi <- v_costfu*0
  #p_emicappath_EUETS <- v_costfu*0
  #p_withholdEUA <- v_costfu*0
  #p_backloadEUA <- v_costfu*0
  #p_cancelEUA <- v_costfu*0
  #p_MSR <- v_costfu*0
  #p_extraintakeMSR <- v_costfu*0
  #p_auction_EUETS <- v_costfu*0
  #p_freealloc_EUETS <- v_costfu*0
  #p_aviation_cap <- v_costfu*0
  #p_aviation_emi <- v_costfu*0
  #
  ##report the fictitious variables 
  ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ##(DO NOT FORGET TO INCLUDE THESE VARIABLES IN mappingvars and aggvars EXCEL FILES) 
  #tmp1 <- NULL
  #
  #tmp2 <- NULL
  ##Check the version so one can check if the industry is already included
  #c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")
  #if(c_LIMESversion <= 2.26) {
  #  tmp2 <- mbind(tmp2,setNames(v_bankemi[,,],"Emissions level in ETS|CO2|Energy|Supply|Electricity (Mt CO2)"))
  #  tmp2 <- mbind(tmp2,setNames(p_emicappath_EUETS[,,],"EU ETS cap|CO2|Energy|Supply|Electricity (Mt CO2/yr)"))
  #} else {
  #  c_industry_ETS <- readGDX(gdx,name="c_industry_ETS",field="l",format="first_found")
  #  if(c_industry_ETS == 0) {
  #    tmp2 <- mbind(tmp2,setNames(v_bankemi[,,],"Emissions level in ETS|CO2|Energy|Supply|Electricity (Mt CO2)"))
  #    tmp2 <- mbind(tmp2,setNames(p_emicappath_EUETS[,,],"EU ETS cap|CO2|Energy|Supply|Electricity (Mt CO2/yr)"))
  #  } else {
  #    tmp2 <- mbind(tmp2,setNames(v_bankemi[,,],"Emissions level in ETS|CO2 (Mt CO2)"))
  #  }
  #  if (c_LIMESversion >= 2.28) {
  #    c_aviation <- readGDX(gdx,name="c_aviation",field="l",format="first_found")
  #    if(c_aviation == 1){
  #      tmp2 <- mbind(tmp2,setNames(p_aviation_cap,"Emissions|CO2|Cap|Aviation (Mt CO2/yr)"))
  #      tmp2 <- mbind(tmp2,setNames(p_aviation_emi,"Emissions|CO2|Aviation (Mt CO2/yr)"))
  #      tmp2 <- mbind(tmp2,setNames(p_aviation_emi,"Emissions|CO2|Certificates from Stationary|Aviation (Mt CO2/yr)"))
  #    }
  #  }
  #  
  #  #in any case, create the variables for the MSR
  #  tmp2 <- mbind(tmp2,setNames(p_auction_EUETS,"Emissions|CO2|Certificates auctioned ETS (Mt CO2/yr)"))
  #  tmp2 <- mbind(tmp2,setNames(p_freealloc_EUETS,"Emissions|CO2|Free-allocated certificates ETS (Mt CO2/yr)"))
  #  tmp2 <- mbind(tmp2,setNames(p_withholdEUA,"Emissions|CO2|Intake to MSR (Mt CO2/yr)"))
  #  tmp2 <- mbind(tmp2,setNames(p_backloadEUA,"Emissions|CO2|Outtake from MSR (Mt CO2/yr)"))
  #  tmp2 <- mbind(tmp2,setNames(p_cancelEUA,"Emissions|CO2|Cancellation from MSR (Mt CO2/yr)"))
  #  tmp2 <- mbind(tmp2,setNames(p_extraintakeMSR,"Emissions|CO2|Additional intake to the MSR (Mt CO2)"))
  #  tmp2 <- mbind(tmp2,setNames(p_MSR,"Emissions|CO2|MSR level (Mt CO2)"))
  #  
  #  tmp2 <- mbind(tmp2,setNames(p_MSR,"Emissions|CO2|Cap|Stationary|Electricity and Industry (Mt CO2/yr)"))
  #  tmp2 <- mbind(tmp2,setNames(p_MSR,"Emissions|CO2|Cap|Stationary (Mt CO2/yr)"))
  #  tmp2 <- mbind(tmp2,setNames(p_MSR,"Emissions|CO2|Energy|Supply|Heating (Mt CO2/yr)"))
  #  
  #  
  #} 
  
  tmp1 <- NULL
  
  AggVarPath <- system.file("extdata","AggregateVariables.csv",package="limes")
  # reading mapping file
  AggVarfile <- read.csv(AggVarPath,sep=";")
  #  write the *.mif or give back the magpie opject output
  AggVars <- paste0(as.vector(AggVarfile$LIMES)," (",as.vector(AggVarfile$UnitLIMES) , ")")
  
  for (name_var in AggVars) {
    tmp1 <- mbind(tmp1,setNames(o_fictitious,name_var))
  }
  
  tmp2 <- NULL
  
  # concatenate data
  tmp <- mbind(tmp1,tmp2)

  return(tmp)
}
  
