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
#' @importFrom magclass collapseDim getItems<-
#' @importFrom utils read.csv
#' @importFrom stringr str_detect str_remove
#'

limesMapping <- function(var,mappingPath=NULL){


# Internal function -------------------------------------------------------
  namesWOregi <- function(m_obj, reg){

    tmp <- str_remove(getNames(var[,,regi]),
                      reg)
    tmp <- str_remove(tmp, "^\\.")
    tmp <- str_remove(tmp, "\\.$")
    tmp <- str_replace(tmp, "\\.{2}", ".")
  }

# End internal function ---------------------------------------------------

  # settings mapping path
  if (is.null(mappingPath))
    mappingPath <- system.file("extdata","LIMES_country_ISO_3.csv",package="limes")
  # reading mapping file
  mapping <- read.csv(mappingPath,sep=";")

  #extract the regions of one variable (in some cases, a var only exists for some regions)
  #find the correct position of the region in the subindex (it should normally be in the first position)
  region_var <- NULL
  for (k in 1:lengths(strsplit(getNames(var),"[.]")[1])) {
    a <- unique(sapply(strsplit(getNames(var),"[.]"), '[[', k))
    if (length(intersect(a,mapping$LIMES_ISO2)) > 0) {
      region_var <- a
      dimregi <- 3 + k/10 #Save the dimension in which the region is embedded. Need to specify this later and this can vary from variable to variable
    }
  }

  ##Find the common names (i.e. variable mapping or technologies)
  var_names <- NULL
  k <- 0
  for (regi in region_var) {

    tmp <- namesWOregi(var, regi)

    #Aggregate the new elements
    var_names <- union(tmp,var_names)

    #Find if names indeed differ across variables
    k <- k + length(setdiff(var_names,tmp))
  }

  ##Check if the variables are symetrical, i.e., they have the same names after removing the region
  #If k = 0, then variable is symetrical and we can keep the initial formulation: just concatenating the variable for each region
  #If k>0, then variable is asymetrical and we need to create an array containing all potential name combinations and allocate the values for each
  #This ensures that old model versions, where all variably were symetrical as no expost calculations were needed (e.g., on v_emi for CHP),
  #still work without risking to have errors due to differences in the name sets (when variables are not declared with their respective subsets)
  if(k == 0) {
    # initializing output var
    output <- NULL
  } else {
    #Create an array with all the possible names (i.e., tech combinations)
    output <- new.magpie(cells_and_regions = mapping$LIMES_ISO3[match(region_var,mapping$LIMES_ISO2)], years = getYears(var), names = c(var_names),
                         fill = 0, sort = FALSE, sets = NULL)
  }


  # looping old region names
  for (oldRegion in mapping$LIMES_ISO2) {
    # newRegion
    newRegion <- as.vector(mapping[mapping$LIMES_ISO2==oldRegion,]$LIMES_ISO3)

    #check if the oldregion exist in the variable (to avoid 'subscript out of bounds)
    if (oldRegion %in% region_var) {
      # creating per country margpie object
      partMagPie <- var[,,oldRegion]
      getItems(partMagPie, dim = 1) <- newRegion
      partMagPie <- collapseDim(partMagPie, dim = dimregi)
    } else {
      partMagPie <- var[,,region_var[1]]*0
      #partMagPie <- NA
      getItems(partMagPie, dim = 1) <- newRegion
      partMagPie <- collapseDim(partMagPie, dim = dimregi)
    }

    # merging per country objects with output/allocating objects in output
    if(k == 0) {
      output <- mbind(output,partMagPie)
    } else {
      output[newRegion,,getNames(partMagPie)] <- partMagPie
    }


  }

  return(output)
}
