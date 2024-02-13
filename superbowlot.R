set.seed(1234)

source("./fgpct.R")

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
        return(3*rbinom(1,1,fgpct(state$topaydirt + 17)))
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
  #browser()
  state <- list()
  state$down <- 1
  state$togo <- 10
  state$topaydirt <- 75
  while(TRUE){
    #y <- rgamma(1,9/100,3/100) 
    y <- sample(yards,1)
    
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
        return(3*rbinom(1,1,fgpct(state$topaydirt + 17)))
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
        return(3*rbinom(1,1,fgpct(state$topaydirt + 17)))
      }
    }
  }
}

drive_sim_MS(goforit = TRUE, deficit = 0)

table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 0)))/10000
table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 3)))/10000
table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 7)))/10000
table(replicate(10000,drive_sim_MS(goforit = FALSE, deficit = 0)))/10000
table(replicate(10000,drive_sim_MS(goforit = FALSE, deficit = 3)))/10000
table(replicate(10000,drive_sim_MS(goforit = FALSE, deficit = 7)))/10000


simOT <- function(){
  a <- drive_sim_MS(goforit = FALSE, deficit = 0) 
  b <- drive_sim_MS(goforit = FALSE, deficit = a) 
  if (a > b){return("A")} else if (a < b) {return("B")} else {
    a <- 0
    b <- 0
    i <- 0
      while (TRUE){
        i <- i + 1
        score <- drive_sim_MS(goforit = FALSE, deficit = 0)
        if (score > 0){
          if (i %% 2 == 1){return("A")} else {return("B")}
        }
          }  
  }
}

table(replicate(50000,simOT()))/50000




