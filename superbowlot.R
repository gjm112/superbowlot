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

drive_sim_MS <- function(goforit = FALSE,deficit = 0,yards_to_ez = 75,gofor2 = FALSE, SV = FALSE){
  # SV stands for sudden victory 
  # (if you score a touchdown, game over, don't kick PAT/go for 2)
  #browser()
  state <- list()
  state$down <- 1
  state$togo <- 10
  state$topaydirt <- yards_to_ez
  while(TRUE){
    #y <- rgamma(1,9/100,3/100) 
    y <- sample(yards,1)
    
    if (state$down <= 3){ # Simulating 1st, 2nd, or 3rd Down
      if (y >= state$topaydirt){ # Touchdown!!
        if (gofor2) {
          return(list(score = 6 + ifelse(SV,0,2*rbinom(1,1,0.4735)),
                      dist = 75))
        } else {
          return(list(score = 6 + ifelse(SV,0,rbinom(1,1,fgpct(33))),
                      dist = 75))
        }
      } else if (state$topaydirt - y >= 100) { # Safety!!
        return(list(score = -2,dist = 75))
      } else if (y >= state$togo){ # First Down!!
        state$down <- 1
        state$topaydirt <- state$topaydirt - y
        state$togo <- min(10,state$topaydirt)
      } else { # Other
        state$down <- state$down + 1
        state$togo <- state$togo  - y
        state$topaydirt <- state$topaydirt - y
      }
    } else if (deficit >= 6) { # Have to Go For it on 4th Down
      if (y >= state$topaydirt){ # Touchdown!!
        if (gofor2) {
          return(list(score = 6 + 2*rbinom(1,1,0.4735),
                      dist = 75))
        } else {
          return(list(score = 6 + rbinom(1,1,fgpct(33)),
                      dist = 75))
        }
      } else if (state$topaydirt - y >= 100) { # Safety!!
        return(list(score = -2,dist = 75))
      } else if (y >= state$togo) { # First Down!!
        state$down <- 1
        state$topaydirt <- state$topaydirt - y
        state$togo <- min(10,state$topaydirt)
      } else { # Turnover on Downs!!
        return(list(score = 0,
                    dist = 100 - (state$topaydirt - y)))
      }
    } else if (deficit == 3) {
      if (goforit | (fgpct(state$topaydirt + 17) < 0.5)){ # Choose to go for it or too far away for field goal
        if (y >= state$topaydirt){ # Touchdown!!
          return(list(score = 6,
                      dist = 75)) # Doesn't matter if coach will go for 2
          # Touchdown wins the game
        } else if (state$topaydirt - y >= 100) { # Safety!!
          return(list(score = -2,dist = 75))
        } else if (y >= state$togo) { # First Down!!
          state$down <- 1
          state$topaydirt <- state$topaydirt - y
          state$togo <- min(10,state$topaydirt)
        } else { # Turnover on Downs!!
          return(list(score = 0,
                      dist = 100 - (state$topaydirt - y)))
        }
      } else { # Kick the Field Goal
        kick <- rbinom(1,1,fgpct(state$topaydirt + 17)) # Simulate the kick
        return(list(score = 3*kick,
                    dist = ifelse(kick == 1,75,100 - (state$topaydirt - 7)))) 
        # If made, ball at 75, otherwise, ball at the spot of the kick
      }
    } else {
      if (goforit){ # Choosing to go for it on 4th down
        if (y >= state$topaydirt){ # Touchdown!!
          if (gofor2) {
            return(list(score = 6 + ifelse(SV,0,2*rbinom(1,1,0.4735)),
                        dist = 75))
          } else {
            return(list(score = 6 + ifelse(SV,0,rbinom(1,1,fgpct(33))),
                        dist = 75))
          }
        } else if (state$topaydirt - y >= 100) { # Safety!!
          return(list(score = -2,dist = 75))
        } else if (y >= state$togo) { # First Down!!
          state$down <- 1
          state$topaydirt <- state$topaydirt - y
          state$togo <- min(10,state$topaydirt)
        } else { # Turnover on Downs!!
          return(list(score = 0,
                      dist = 100 - (state$topaydirt - y)))
        }
      } else if (fgpct(state$topaydirt + 17) < 0.5) { # Punt
        p <- sample(punt,1)
        if (state$topaydirt - p < 0){
          dist <- 20
        } else {
          dist <- state$topaydirt - p
        }
        return(list(score = 0,dist = 100 - dist))
      } else { # Kick the Field Goal
        kick <- rbinom(1,1,fgpct(state$topaydirt + 17)) # Simulate the kick
        return(list(score = 3*kick,
                    dist = ifelse(kick == 1,75,100 - (state$topaydirt - 7))))
        # If made, ball at 75, otherwise, ball at the spot of the kick
      }
    }
  }
}

drive_sim_MS(goforit = FALSE, deficit = 0)
# 
# table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 0, gofor2 = TRUE)$score))/10000
# table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 3, gofor2 = TRUE)$score))/10000
# table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 7, gofor2 = TRUE)$score))/10000
# table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 0)$score))/10000
# table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 3)$score))/10000
# table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 7)$score))/10000
# table(replicate(10000,drive_sim_MS(goforit = FALSE, deficit = 0, gofor2 = TRUE)$score))/10000
# table(replicate(10000,drive_sim_MS(goforit = FALSE, deficit = 3, gofor2 = TRUE)$score))/10000
# table(replicate(10000,drive_sim_MS(goforit = TRUE, deficit = 7, gofor2 = TRUE)$score))/10000


simOT <- function(goforit = FALSE, gofor2 = FALSE){ # Coach's decision to Go For it/Go For 2
  a <- drive_sim_MS(goforit = goforit, gofor2 = gofor2) 
  if (a$score == -2){return("B1")} else { # Team B scored a safety, Game Over!
  b <- drive_sim_MS(goforit = goforit, deficit = a$score,  yards_to_ez = a$dist, gofor2 = case_when(a$score == 8 ~ TRUE, a$score == 6 ~ FALSE, TRUE ~ gofor2), SV=TRUE) 
    # case_when() logic
    # If Team A has 8 points, Team B MUST go for 2
    # If Team A has 6 points, Team B will in all likelihood kick the PAT to win
    # If Team A has 7 points, Team B will make a coach's decision
    # Otherwise, PAT does not matter, a touchdown will win
    if (a$score > b$score){return("A1")} else if (a$score < b$score) {return("B2")} else {
      this_drive <- b
      i <- 2
      while (TRUE){
        i <- i + 1 
        last_drive <- this_drive
        # Go For 2 no longer matters, next team to score wins.
        # Assuming that, if a team is in field goal range, they will kick
        this_drive <- drive_sim_MS(goforit = FALSE, deficit = 0, yards_to_ez = last_drive$dist, SV = TRUE)
        if (this_drive$score > 0){ # Team with the ball scores a TD
          if (i %% 2 == 1){
            return(paste0("A",i))
          } else {
            return(paste0("B",i))
          }
        } else if (this_drive$score < 0){ # Team without the ball scores a safety
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

