#' mapping of weights for the variables for global aggregation
#'
#' @param output variable to be changed
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param mappingPath path to mapping file
#' @author Sebastian Osorio and Renato Rodrigues
#'
#' @export
#' @importFrom magclass getItems new.magpie getYears getNames getItems<-
#' @importFrom luscale speed_aggregate
#' @importFrom utils read.csv
#' @importFrom gdx readGDX
#'
#'

limesInt2Ext <- function(output,gdx,mappingPath=NULL){

  #Read LIMES version (industry was included in version 2.27)
  c_LIMESversion <- readGDX(gdx,name="c_LIMESversion",field="l",format="first_found")

  # settings mapping path
  if (is.null(mappingPath))
    mappingPath <- system.file("extdata","WeightedAverageVars.csv",package="limes")
  # reading mapping file
  mapping_int2ext <- read.csv(mappingPath,sep=";")
  mapping_int2ext <- mapping_int2ext[mapping_int2ext$ext != 0,]

  #If industry is included, change some weights (before, only electricity)
  if(c_LIMESversion > 2.26) {
    c_industry_ETS <- readGDX(gdx,name="c_industry_ETS",field="l",format="first_found")
    if(c_industry_ETS == 1) {
      levels(mapping_int2ext$ext) <- c(levels(mapping_int2ext$ext), "Emissions|CO2|Electricity and Industry")
      mapping_int2ext[mapping_int2ext$int == "Price|Carbon|ETS",]$ext <- "Emissions|CO2|Electricity and Industry"
    }
  }

  if(c_LIMESversion >=  2.33) { #if heating module is included, DH emissions are endogenous and per country, so use this to weight ETS prices
    heating <- .readHeatingCfg(gdx)
    if(heating == "fullDH") {
      levels(mapping_int2ext$ext) <- c(levels(mapping_int2ext$ext), "Emissions|CO2|EU ETS")
      mapping_int2ext[mapping_int2ext$int == "Price|Carbon|ETS",]$ext <- "Emissions|CO2|EU ETS"
    }
  }


  #Load the data (output)
  var<-output
  var_names <- getNames(var)

  #Keep only names in var_names, i.e., delete units
  var_names_tmp <- gsub(" [(].*", "", var_names)

  #Check if the variables (intensive and corresponding extensive) were previously calculated (output)
  pos_int <- match(mapping_int2ext$int, var_names_tmp)
  pos_ext <- match(mapping_int2ext$ext, var_names_tmp)

  #Make sure only the intensive variables for which the corresponding variable is available, are weighted (avoid errors)
  pos_tmp2 <- intersect(which(!is.na(pos_int)),which(!is.na(pos_ext)))
  int_tmp2 <- mapping_int2ext$int[pos_tmp2]
  ext_tmp2 <- mapping_int2ext$ext[pos_tmp2]

  #Retrieve variables with their corresponding units
  int <- var_names[match(int_tmp2, var_names_tmp)]
  ext <- var_names[match(ext_tmp2, var_names_tmp)]

  #Allocate very low values for extensive variables where values are 0
  var[, , ext] <- pmax(var[, , ext], 1e-20)

  #Create a magpie object for each intensive variable
  regionSubsetList <- list("GLO" = getItems(var, dim = 1),
                           "EU28" = setdiff(getItems(var, dim = 1), c("CHE","NOR","BAL")),
                           "EU27" = setdiff(getItems(var, dim = 1), c("GBR","CHE","NOR","BAL")),
                           "EUETS" = setdiff(getItems(var, dim = 1), c("CHE","BAL")))

  tmp_RegAgg <- new.magpie(cells_and_regions = names(regionSubsetList),
                           years = getYears(var),
                           names = int,
                           fill=0)

  #Initialize the weighted intensive variables in case they cannot be calculated later (lack of the extensive variable used to weight it)
  tmp_RegAgg[,,int] <- NA

  tmp_RegAgg_ie2 <-NULL

  if(length(int) > 0) {

    for(region in names(regionSubsetList)){
      tmp_RegAgg_ie2 <- do.call("mbind",
                                lapply(int, function(i2e) {
                                  map <- data.frame(region=regionSubsetList[[region]], parentRegion=region, stringsAsFactors=FALSE)
                                  result <- speed_aggregate(var[regionSubsetList[[region]],,i2e],map,weight=var[regionSubsetList[[region]],, ext[match(i2e,int)]]/dimSums(var[regionSubsetList[[region]],, ext[match(i2e,int)]], dim=1))
                                  getItems(result, dim = 1) <- region
                                  return(result)
                                })
                                )
      tmp_RegAgg[region,,int] <- tmp_RegAgg_ie2[region,,int]
    }

  }

  return(tmp_RegAgg)
}
