set.seed(1234)

drive_sim <- function(goforit = FALSE){
  state <- list()
  state$down <- 1
  state$togo <- 10
  state$topaydirt <- 75
  while(TRUE){
    y <- rgamma(1,9/100,3/100) 
    
    if (state$down <= 3){
      if (y >= state$topaydirt){
        return(7)
      } else if (y >= state$togo){
        state$down <- 1
        state$topaydirt <- state$topaydirt - y
        state$togo <- min(10,state$topaydirt)
      } else {
        state$down <- state$down + 1
        state$togo <- state$togo  - y
        state$topaydirt <- state$topaydirt - y
      }
    } else if (!goforit){
      if (state$topaydirt > 40){
        return(0)
      }
      else {
        return(3*rbinom(1,1,.7))
      } 
    } else if (y >= state$topaydirt){
      return(7)
    } else if (y >= state$togo) {
      state$down <- 1
      state$topaydirt <- state$topaydirt - y
      state$togo <- min(10,state$topaydirt)
    } else { 
      return(0)
    }
  }
}
  
table(replicate(10000,drive_sim(goforit = TRUE)))/10000
table(replicate(10000,drive_sim(goforit = FALSE)))/10000

x <- 0
while(TRUE){
  print(x)
  x <- x+1
}

drive_sim_MS <- function(goforit = FALSE,deficit = 0){
  browser()
  state <- list()
  state$down <- 1
  state$togo <- 10
  state$topaydirt <- 75
  while(TRUE){
    y <- rgamma(1,9/100,3/100) 
    
    if (state$down <= 3){
      if (y >= state$topaydirt){
        return(7)
      } else if (y >= state$togo){
        state$down <- 1
        state$topaydirt <- state$topaydirt - y
        state$togo <- min(10,state$topaydirt)
      } else {
        state$down <- state$down + 1
        state$togo <- state$togo  - y
        state$topaydirt <- state$topaydirt - y
      }
    } else if (deficit == 7) {
      if (y >= state$topaydirt){
        return(7)
      } else if (y >= state$togo) {
        state$down <- 1
        state$topaydirt <- state$topaydirt - y
        state$togo <- min(10,state$topaydirt)
      } else { 
        return(0)
      }
    } else if (deficit == 3) {
      if (goforit | state$topaydirt > 40){
        if (y >= state$topaydirt){
          return(7)
        } else if (y >= state$togo) {
          state$down <- 1
          state$topaydirt <- state$topaydirt - y
          state$togo <- min(10,state$topaydirt)
        } else { 
          return(0)
        }
      } else {
        return(3*rbinom(1,1,.7))
      }
    } else {
      if (goforit){
        if (y >= state$topaydirt){
          return(7)
        } else if (y >= state$togo) {
          state$down <- 1
          state$topaydirt <- state$topaydirt - y
          state$togo <- min(10,state$topaydirt)
        } else { 
          return(0)
        }
      } else if (state$topaydirt > 40) {
        return(0)
      } else {
        return(3*rbinom(1,1,.7))
      }
    }
  }
}
