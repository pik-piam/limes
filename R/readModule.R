#' Helper functions to retrieve module realisations
#'
#'.find_real_module returns the realisation for a given module
#'@param module_set gives the tuples (module, realisations)
#'@param module_name gives the specific module for which we want to get the
#'                   realisation
#'
#'@noRd



.find_real_module <- function(module_set, module_name){
  return(module_set[module_set$modules == module_name,2])
}

#' Helper functions to retrieve module realisation
#'
#'.readHeatingCfg returns the realisation for the heating module
#' and translates the old c_heating parameter into the new realisations
#' if the model does not contain the heating module
#'
#'@param gdx the gdx name from which to retrieve the value
#'
#'@noRd
#'
.readHeatingCfg <- function(gdx){

  #c_heating describes the heating assumption before heating was modularised
  c_heating <-  readGDX(gdx,name="c_heating",format="first_found", react = "silent")

  if (!is.null(c_heating)){
    heating <- switch(as.character(c_heating),
                      "0" = "off",
                      "1" = "fullDH",
                      "2" = "mac")
  } else {
    module2realisation <- readGDX(gdx, "module2realisation")
    heating <- .find_real_module(module2realisation, "heating")
  }

  return(heating)
}
