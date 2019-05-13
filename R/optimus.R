
optimus_settings <- function(prime, inv, maxid){
  if (!requireNamespace("gmp", quietly = TRUE)) {
    stop("Requires packages `gmp`")
  }
  structure(list(prime = gmp::as.bigz(prime, mod = maxid),
                 inv = gmp::as.bigz(inv, mod = maxid),
                 maxid = gmp::as.bigz(maxid)),
            class = "Optimus")
}
