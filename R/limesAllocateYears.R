#' Allocating years to variables that do not change over time
#' 
#' @param var variable to be changed
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @author Sebastian Osorio
#' @examples
#' 
#' \dontrun{limesAllocateYears(var)}
#' 
#' @export
#' @importFrom magclass getYears<-
#' @importFrom gdx readGDX
#' 
#' 
limesAllocateYears <- function(var,gdx){
  
  #reading the years from the gdx
  tt <- readGDX(gdx,name="t") #set of years
  
  # initializing output var
  output<-NULL
  
  # looping years
  for (t2 in tt) {
    # creating per year margpie object
    getYears(var)<-t2
    # merging per country objects with output
    output<-mbind(output,var)
  }
  return(output)
  
}