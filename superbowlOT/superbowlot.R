set.seed(1234)

source("./fgpct.R")

# drive_sim <- function(goforit = FALSE){
#   state <- list()
#   state$down <- 1
#   state$togo <- 10
#   state$topaydirt <- 75
#   while(TRUE){
#     y <- rgamma(1,9/100,3/100) 
#     
#     if (state$down <= 3){
#       if (y >= state$topaydirt){
#         return(7)
#       } else if (y >= state$togo){
#         state$down <- 1
#         state$topaydirt <- state$topaydirt - y
#         state$togo <- min(10,state$topaydirt)
#       } else {
#         state$down <- state$down + 1
#         state$togo <- state$togo  - y
#         state$topaydirt <- state$topaydirt - y
#       }
#     } else if (!goforit){
#       if (state$topaydirt > 40){
#         return(0)
#       }
#       else {
#         return(3*rbinom(1,1,fgpct(state$topaydirt + 17)))
#       } 
#     } else if (y >= state$topaydirt){
#       return(7)
#     } else if (y >= state$togo) {
#       state$down <- 1
#       state$topaydirt <- state$topaydirt - y
#       state$togo <- min(10,state$topaydirt)
#     } else { 
#       return(0)
#     }
#   }
# }
  
# table(replicate(10000,drive_sim(goforit = TRUE)))/10000
# table(replicate(10000,drive_sim(goforit = FALSE)))/10000

# x <- 0
# while(TRUE){
#   print(x)
#   x <- x+1
# }

drive_sim_MS <- function(goforit = FALSE,deficit = 0, yards_to_ez = 75, gofor2 = FALSE){
  #browser()
  state <- list()
  state$down <- 1
  state$togo <- 10
  state$topaydirt <- yards_to_ez
  while(TRUE){
    #y <- rgamma(1,9/100,3/100) 
    y <- sample(yards,1)
    
    if (state$down <= 3){
      if (y >= state$topaydirt){
        if (gofor2) {
          return(list(score = 6 + 2*rbinom(1,1,0.4735),
                      dist = yards_to_ez))
        } else {
          return(list(score = 6 + rbinom(1,1,fgpct(33)),
                      dist = yards_to_ez))
        }
      } else if (state$topaydirt - y >= 100) {
        return(list(score = -2,dist = yards_to_ez))
      } else if (state$topaydirt - y >= 100) {
        return(list(score = -2,dist = 75))
      } else if (y >= state$togo){
        state$down <- 1
        state$topaydirt <- state$topaydirt - y
        state$togo <- min(10,state$topaydirt)
      } else {
        state$down <- state$down + 1
        state$togo <- state$togo  - y
        state$topaydirt <- state$topaydirt - y
      }
    } else if (deficit >= 6) {
      if (y >= state$topaydirt){
        if (gofor2) {
          return(list(score = 6 + 2*rbinom(1,1,0.4735),
                      dist = yards_to_ez))
        } else {
          return(list(score = 6 + rbinom(1,1,fgpct(33)),
                      dist = yards_to_ez))
        }
      } else if (state$topaydirt - y >= 100) {
        return(list(score = -2,dist = yards_to_ez))
      } else if (state$topaydirt - y >= 100) {
        return(list(score = -2,dist = 75))
      } else if (y >= state$togo) {
        state$down <- 1
        state$topaydirt <- state$topaydirt - y
        state$togo <- min(10,state$topaydirt)
      } else { 
        return(list(score = 0,
                    dist = 100 - (state$topaydirt - y)))
      }
    } else if (deficit == 3) {
      if (goforit | state$topaydirt > 40){
        if (y >= state$topaydirt){
          if (gofor2) {
            return(list(score = 6 + 2*rbinom(1,1,0.4735),
                        dist = yards_to_ez))
          } else {
            return(list(score = 6 + rbinom(1,1,fgpct(33)),
                        dist = yards_to_ez))
          }
        } else if (state$topaydirt - y >= 100) {
          return(list(score = -2,dist = yards_to_ez))
        } else if (state$topaydirt - y >= 100) {
          return(list(score = -2,dist = 75))
        } else if (y >= state$togo) {
          state$down <- 1
          state$topaydirt <- state$topaydirt - y
          state$togo <- min(10,state$topaydirt)
        } else { 
          return(list(score = 0,
                      dist = 100 - (state$topaydirt - y)))
        }
      } else {
        kick <- rbinom(1,1,fgpct(state$topaydirt + 17))
        return(list(score = 3*kick,
                    dist = ifelse(kick==1,yards_to_ez,100-(state$topaydirt - 7))))
        return(list(score = 3*rbinom(1,1,fgpct(state$topaydirt + 17)),
                    dist = 75))
      }
    } else {
      if (goforit){
        if (y >= state$topaydirt){
          if (gofor2) {
            return(list(score = 6 + 2*rbinom(1,1,0.4735),
                        dist = yards_to_ez))
          } else {
            return(list(score = 6 + rbinom(1,1,fgpct(33)),
                        dist = yards_to_ez))
          }
        } else if (state$topaydirt - y >= 100) {
          return(list(score = -2,dist = yards_to_ez))
        } else if (y >= state$togo) {
          state$down <- 1
          state$topaydirt <- state$topaydirt - y
          state$togo <- min(10,state$topaydirt)
        } else { 
          return(list(score = 0,
                      dist = 100 - (state$topaydirt - y)))
        }
      } else if (state$topaydirt > 40) {
        p <- sample(punt,1)
        if (state$topaydirt - p < 0){
          dist <- 20
        } else {
          dist <- state$topaydirt - p
        }
        return(list(score = 0,dist = 100 - dist))
      } else {
        kick <- rbinom(1,1,fgpct(state$topaydirt + 17))
        return(list(score = 3*kick,
                    dist = ifelse(kick==1,yards_to_ez,100-(state$topaydirt - 7))))
      }
    }
  }
}

drive_sim_MS(goforit = TRUE, deficit = 0)

table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 0, gofor2 = TRUE)$score))/10000
table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 3, gofor2 = TRUE)$score))/10000
table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 7, gofor2 = TRUE)$score))/10000
table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 0)$score))/10000
table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 3)$score))/10000
table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 7)$score))/10000
table(replicate(10000,drive_sim_MS(goforit = FALSE, deficit = 0, gofor2 = TRUE)$score))/10000
table(replicate(10000,drive_sim_MS(goforit = FALSE, deficit = 3, gofor2 = TRUE)$score))/10000
table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 7, gofor2 = TRUE)$score))/10000


simOT <- function(goforit = FALSE, gofor2 = FALSE){
  a <- drive_sim_MS(goforit = goforit, gofor2 = gofor2) 
  if (a$score == -2){return("B1")} else {
  b <- drive_sim_MS(goforit = goforit, deficit = a$score,  yards_to_ez = a$dist, gofor2 = case_when(a$score == 8 ~ TRUE, a$score == 6 ~ FALSE, TRUE ~ gofor2)) 
    if (a$score > b$score){return("A1")} else if (a$score < b$score) {return("B2")} else {
      this_drive <- b
      i <- 2
      while (TRUE){
        i <- i + 1 
        last_drive <- this_drive
        this_drive <- drive_sim_MS(goforit = goforit, deficit = 0, yards_to_ez = last_drive$dist, gofor2 = FALSE)
        if (this_drive$score > 0){
          if (i %% 2 == 1){
            return(paste0("A",i))
          } else {
            return(paste0("B",i))
          }
        } else if (this_drive$score == -2){
          if (i %% 2 == 1){
            return(paste0("B",i))
          } else {
            return(paste0("A",i))
          }
        }
      }  
    }
  }
}

nsim<- 10000
res <- replicate(nsim,simOT())

table((substring(res,1, 1)))/nsim
plot(table(as.numeric(substring(res,2, nchar(res)))), col = c("red","blue"))

