mm <- function() {
  library(combinat)

  combs <- t(combn(LETTERS[1:6], 4))
  m <- matrix(unlist(apply(combs, 1, permn)), nrow=360, byrow=TRUE)
  mred <- m
  truth <- m[sample(1:360,1),]

  answer <- function(truth, guess, quiet=FALSE) {
    whites <- 0
    blacks <- 0
    for (i in 1:4) {
      if ( guess[i]==truth[i] ) {
        blacks <- blacks + 1
      } else if (guess[i] %in% truth) {
        whites <- whites + 1
      }
    }
    if ( quiet==FALSE ) {
      cat("blacks:", blacks, "whites:", whites, "\n")
    }
    return(c(blacks, whites))
  }

  trials <- 0
  ans <- c(0,0)
  while(!identical(ans,c(4,0))) {
    cat("\nwrite four letters from A to F with nothing in between\n")
    trials <- trials + 1
    x <- readline()
    guess <- unlist(strsplit(x, ""))
    cat("trial", trials, "\n")
    ans <- answer(truth, guess)
    if ( !identical(ans,c(4,0)) ) {
      if ( trials > 1 ) {
        if(sum(apply(mred, 1, identical, guess))==0) {
          cat("think twice, it's my only advice\n")
        }
      }
      a <- list()
      for (i in 1:nrow(mred)) {
        a[[i]] <- answer(mred[i,],guess,quiet=TRUE)
      }
      red <- unlist(lapply(a, identical, ans))
      if ( sum(red) > 1 ) {
        mred <- mred[red,]
      }
    }
  }
  cat("you're a master mind\n")
}

