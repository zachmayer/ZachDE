parDEoptim <- function (fn, lower, upper, n=1, control = DEoptim.control(), .packages=NULL, ...) {
  library("DEoptim")
  library("foreach")
  checkBounds(lower, upper)
  segSizes <- (upper-lower)/n  
  segments <- lapply(1:n, function(x){
    lower+segSizes*(x-1)
  })

  out <- foreach(L=segments, .packages=c(.packages,'DEoptim')) %dopar% {
    U <- L + segSizes
    opt <- DEoptim (fn, L, U, control = control)
    new('parDEoptim',optim=opt$optim,member=opt$member)
  }
  best <- which.min(unlist(lapply(out, function(x) x@optim$bestval)))
  out[[best]]
}
