checkBounds <-
function(lower, upper){
  if (length(lower) != length(upper)) 
        stop("'lower' and 'upper' are not of same length")
  if (!is.vector(lower)) 
        lower <- as.vector(lower)
  if (!is.vector(upper)) 
        upper <- as.vector(upper)
  if (any(lower > upper)) 
        stop("'lower' > 'upper'")
  if (any(lower == "Inf")) 
        warning("you set a component of 'lower' to 'Inf'. May imply 'NaN' results", 
            immediate. = TRUE)
  if (any(lower == "-Inf")) 
        warning("you set a component of 'lower' to '-Inf'. May imply 'NaN' results", 
            immediate. = TRUE)
  if (any(upper == "Inf")) 
        warning("you set a component of 'upper' to 'Inf'. May imply 'NaN' results", 
            immediate. = TRUE)
  if (any(upper == "-Inf")) 
        warning("you set a component of 'upper' to '-Inf'. May imply 'NaN' results", 
            immediate. = TRUE)
  return(TRUE)
}
