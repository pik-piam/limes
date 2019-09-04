#' Mapping limes iso 2 codes to iso 3 codes
#' 
#' @param var variable to be changed
#' @param mappingPath path to mapping file
#' @author Sebastian Osorio and Renato Rodrigues
#' @examples
#' 
#' \dontrun{LIMESMapping(var,mappingPath="R/mapping/mapping.csv")}
#' 
#' @export
#' @importFrom magclass collapseNames getRegions<-
#' @importFrom utils read.csv
#' 
#' 

limesMapping <- function(var,mappingPath=NULL){
  # settings mapping path
  if (is.null(mappingPath))
    mappingPath <- system.file("extdata","LIMES_country_ISO_3.csv",package="limes")
  # reading mapping file
  mapping <- read.csv(mappingPath,sep=";")
  # initializing output var
  output <- NULL
  
  #extract the regions of one variable (in some cases, a var only exists for some regions)
  #find the correct position of the region in the subindex (it should normally be in the first position)
  pos <- 0
  region_var <- NULL
  for (k in 1:lengths(strsplit(getNames(var),".",1)[1])) {
    a <- unique(sapply(strsplit(getNames(var),".",1), '[[', k))
    if (length(intersect(a,mapping$LIMES_ISO2)) > 0)
      region_var <- a
      pos <- k
  }
  
  #region_var <- unique(sapply(strsplit(getNames(var),".",1), '[[', 1))
  
  # looping old region names
  for (oldRegion in mapping$LIMES_ISO2){
    # newRegion
    newRegion <- as.vector(mapping[mapping$LIMES_ISO2==oldRegion,]$LIMES_ISO3)
    
    #check if the oldregion exist in the variable (to avoid 'subscript out of bounds)
    if (oldRegion %in% region_var) {
      # creating per country margpie object 
      partMagPie <- var[,,oldRegion]
      getRegions(partMagPie) <- newRegion
      partMagPie <- collapseNames(partMagPie)
    } else {
      partMagPie <- var[,,region_var[1]]*0
      #partMagPie <- NA
      getRegions(partMagPie) <- newRegion
      partMagPie <- collapseNames(partMagPie)
    }
    
    ## creating per country margpie object 
    #partMagPie <- var[,,oldRegion]
    #getRegions(partMagPie) <- newRegion
    #partMagPie <- collapseNames(partMagPie)
    
    # merging per country objects with output
    output <- mbind(output,partMagPie)
  } 
  return(output)
}