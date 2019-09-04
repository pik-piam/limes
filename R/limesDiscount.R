#' Discounting variables (matrix) 
#' 
#' @param var variable to be changed
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @author Sebastian Osorio
#' @examples
#' 
#' \dontrun{limesDiscount(var)}
#' 
#' @export
#' @importFrom magclass mbind
#' @importFrom gdx readGDX
#' 
#' 
limesDiscount <- function(var,gdx){
  
  #reading the parameters from the gdx
  t <- readGDX(gdx,name="t") #set of years
  t0 <- readGDX(gdx,name="t0",field="l",format="first_found") #initial year
  p_ts <- readGDX(gdx,name="p_ts",field="l",format="first_found") #time step
  c_esmdisrate <- readGDX(gdx,name="c_esmdisrate",field="l",format="first_found") #interest rate
  
  #compute factor to discount average marginal values
  f_npv <- as.numeric(p_ts)*exp(-as.numeric(c_esmdisrate)*(as.numeric(t)-as.numeric(t0)))
  
  # initializing output var
  output<-NULL
  
  # looping years
  for (t2 in 1:length(t)) {
    
    #Dividing each vector by a constant (when using 'apply' for this calculation, the magpie proporties are lost)
    output = mbind(output,(var[,t2,]/f_npv[t2]))
    
  }
  return(output)
  
}