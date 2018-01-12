#' @export
to.binary <- function(num,max.dig=9){
  res <- rep(0,max.dig)
  for(i in max.dig:0){
    mod <- num %% 2^i
    if(mod < num){
      res[max.dig-i] <- 1
      num <- num-2^i
    } else {
      if(num == 2^i){
        res[max.dig-i] <- 1
        break
      }
    }
  }
  return(res)
}
