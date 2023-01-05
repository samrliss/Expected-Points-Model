#=================================
#===Expected Points Model=========
#=================================

library(tukeyGH)

Scenario <- function(D, FP, YTG, POS){ 
  Points = 0
  Dnew = D
  FPnew = FP
  YTGnew = YTG
  POSnew = POS
  
  #Sample which play scenario will happen
  PlayDist = c(rep("Pass",60393),rep("Run",68171),rep("Sack",3830))
  Play = sample(PlayDist,1)
  #Determine if Complete or Incomplete pass
  PassResultDist = c(rep("Complete",36281),rep("Incomplete",24112))
  PassResult = sample(PassResultDist,1)
  #Determine what type of pass is thrown
  PassDist = c(rep("Short",25468),rep("Medium",5996),rep("Long",2912))
  PassType = sample(PassDist,1)
  
  #Determine distance of yards gained 
  #Distance of yards gained if run play
  if (Play == "Run"){
    x = seq(-21,96,length = 1000)
    Yards = sample(x, 1, prob =dg(x, a =  3.0000, b = 4.6280, g = 0.1912) , replace = T)
  }
  #Distance of yards lossed if sack
  if (Play == "Sack"){
    x = seq(1,27,length = 1000)
    Yards = ((sample(x, 1, prob = dweibull(x, shape=2.115261, scale = 7.950869) , replace = T)) - 1)*-1
  }
  #Distance of yards gained if pass is incomplete will be 0
  if (Play == "Pass" & PassResult == "Incomplete"){
    Yards = 0
  }
  #Distance of yards gained if short pass is complete
  if (Play == "Pass" & PassResult == "Complete" & PassType == "Short"){
    x = seq(2,121,length = 1000)
    Yards = ((sample(x, 1, prob = dgamma(x, shape=23.1653280, rate = 0.7019524) , replace = T)) - 25)
  }
  #Distance of yards gained if medium pass is complete
  if (Play == "Pass" & PassResult == "Complete" & PassType == "Medium"){
    x = seq(-3,88,length = 1000)
    Yards = sample(x, 1, prob = dg(x, a = 16.000000, b = 6.443094, g = 0.325172 ) , replace = T)
  }
  #Distance of yards gained if long pass is complete
  if (Play == "Pass" & PassResult == "Complete" & PassType == "Long"){
    x = seq(12,98,length = 1000)
    Yards = sample(x, 1, prob = dg(x, a = 33.000000, b = 12.1665961, g = 0.4823053) , replace = T)
  }
  
  #Field goal probability 
  FGprob = (exp(-5.85362 + (0.08635*FPnew)))/(1 + exp(-5.85362 + (0.08635*FPnew)))
  
   #Model begins on 4th down
   if (Dnew == 4){
     #Team A begins with ball
     if (POSnew == 1){
       #Punts
       #From 0 to 15 yardline
       if (FPnew > 0 & FPnew <= 15){
        outcome = sample(c(rep('BLOCKED',3),rep('KICKED',820),rep('TOUCHBACK',0)),1)
        #Punt is blocked
        if (outcome == 'BLOCKED'){
          YTGnew = 10
          Dnew = 1
          POSnew = POSnew * -1
          FPnew = 100 - FPnew
        }
        #Punt is kicked
        else{
          x = seq(3,100,length = 1000)
          Yards = 100 - sample(x, 1, prob = dnorm(x, mean =  49.47805, sd = 14.48829) , replace = T)
          #Punt returned for a touchdown
          if (Yards == 0){
            Points = Points - 7
            return(Points)
          }
          #Punt fair caught or returned 
          else {
            YTGnew = 10
            Dnew = 1
            POSnew = POSnew * -1
            FPnew = Yards
          }
        }
       }
       #From 15 to 30 yardline
       else if (FPnew > 15 & FPnew <= 30){
         outcome = sample(c(rep('BLOCKED',35),rep('KICKED',2912),rep('TOUCHBACK',19)),1)
         #Punt is blocked
         if (outcome == 'BLOCKED'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 100 - FPnew
         }
         #Touchback
         else if (outcome == 'TOUCHDOWN'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 20
         }
         #Punt is kicked
         else{
           x = seq(3,100,length = 1000)
           Yards = 100 - sample(x, 1, prob = dnorm(x, mean =  64.75309, sd = 13.21342) , replace = T)
           #Punt returned for a touchdown
           if (Yards == 0){
             Points = Points - 7
             return(Points)
           }
           #Punt fair caught or returned 
           else {
             YTGnew = 10
             Dnew = 1
             POSnew = POSnew * -1
             FPnew = Yards
           }
         }
       }
       #From 30 to 45 yardline
       else if (FPnew > 30 & FPnew <= 45){
         outcome = sample(c(rep('BLOCKED',18),rep('KICKED',2955),rep('TOUCHBACK',148)),1)
         #Punt is blocked
         if (outcome == 'BLOCKED'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 100 - FPnew
         }
         #Touchback
         else if (outcome == 'TOUCHDOWN'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 20
         }
         #Punt is kicked
         else{
           x = seq(3,100,length = 1000)
           Yards = 100 - sample(x, 1, prob = dnorm(x, mean =  77.27817, sd = 11.67367) , replace = T)
           #Punt returned for a touchdown
           if (Yards == 0){
             Points = Points - 7
             return(Points)
           }
           #Punt fair caught or returned 
           else {
             YTGnew = 10
             Dnew = 1
             POSnew = POSnew * -1
             FPnew = Yards
           }
         }
       }
       #From 45 to 60 yardline
       else if (FPnew > 45 & FPnew <= 60){
         outcome = sample(c(rep('BLOCKED',7),rep('KICKED',1537),rep('TOUCHBACK',329)),1)
         #Punt is blocked
         if (outcome == 'BLOCKED'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 100 - FPnew
         }
         #Touchback
         else if (outcome == 'TOUCHDOWN'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 20
         }
         #Punt is kicked
         else{
           x = seq(3,100,length = 1000)
           Yards = 100 - sample(x, 1, prob = dweibull(x, shape =  14.33961, scale = 90.81987) , replace = T)
           #Punt returned for a touchdown
           if (Yards == 0){
             Points = Points - 7
             return(Points)
           }
           #Punt fair caught or returned 
           else {
             YTGnew = 10
             Dnew = 1
             POSnew = POSnew * -1
             FPnew = Yards
           }
         }
       }
       #Kicks a field goal
       else{
         Kick = sample(c(0,1), size = 1, prob = c(1-FGprob, FGprob), replace = TRUE)
         #Makes a field goal
         if (Kick == 1){
           Points = Points + 3
           return(Points)
         }
         #Misses a field goal
         else {
           Points = Points
           POSnew = POSnew * -1
           FPnew = 100 - FPnew
           Dnew = 1
           YTGnew = 10
         }
       }
     }
     #Team B begins with ball
     else{
       #Punts
       #From 0 to 15 yardline
       if (FPnew > 0 & FPnew <= 15){
         outcome = sample(c(rep('BLOCKED',3),rep('KICKED',820),rep('TOUCHBACK',0)),1)
         #Punt is blocked
         if (outcome == 'BLOCKED'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 100 - FPnew
         }
         #Punt is kicked
         else{
           x = seq(3,100,length = 1000)
           Yards = 100 - sample(x, 1, prob = dnorm(x, mean =  49.47805, sd = 14.48829) , replace = T)
           #Punt returned for a touchdown
           if (Yards == 0){
             Points = Points + 7
             return(Points)
           }
           #Punt fair caught or returned
           else {
             YTGnew = 10
             Dnew = 1
             POSnew = POSnew * -1
             FPnew = Yards
           }
         }
       }
       #From 15 to 30 yardline
       else if (FPnew > 15 & FPnew <= 30){
         outcome = sample(c(rep('BLOCKED',35),rep('KICKED',2912),rep('TOUCHBACK',19)),1)
         #Punt is blocked
         if (outcome == 'BLOCKED'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 100 - FPnew
         }
         #Touchback
         else if (outcome == 'TOUCHDOWN'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 20
         }
         #Punt is kicked
         else{
           x = seq(3,100,length = 1000)
           Yards = 100 - sample(x, 1, prob = dnorm(x, mean =  64.75309, sd = 13.21342) , replace = T)
           #Punt returned for a touchdown
           if (Yards == 0){
             Points = Points - 7
             return(Points)
           }
           #Punt fair caught or returned 
           else {
             YTGnew = 10
             Dnew = 1
             POSnew = POSnew * -1
             FPnew = Yards
           }
         }
       }
       #From 30 to 45 yardline
       else if (FPnew > 30 & FPnew <= 45){
         outcome = sample(c(rep('BLOCKED',18),rep('KICKED',2955),rep('TOUCHBACK',148)),1)
         #Punt is blocked
         if (outcome == 'BLOCKED'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 100 - FPnew
         }
         #Touchback
         else if (outcome == 'TOUCHDOWN'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 20
         }
         #Punt is kicked
         else{
           x = seq(3,100,length = 1000)
           Yards = 100 - sample(x, 1, prob = dnorm(x, mean =  77.27817, sd = 11.67367) , replace = T)
           #Punt returned for a touchdown
           if (Yards == 0){
             Points = Points - 7
             return(Points)
           }
           #Punt fair caught or returned 
           else {
             YTGnew = 10
             Dnew = 1
             POSnew = POSnew * -1
             FPnew = Yards
           }
         }
       }
       #From 45 to 60 yardline
       else if (FPnew > 45 & FPnew <= 60){
         outcome = sample(c(rep('BLOCKED',7),rep('KICKED',1537),rep('TOUCHBACK',329)),1)
         #Punt is blocked
         if (outcome == 'BLOCKED'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 100 - FPnew
         }
         #Touchback
         else if (outcome == 'TOUCHDOWN'){
           YTGnew = 10
           Dnew = 1
           POSnew = POSnew * -1
           FPnew = 20
         }
         #Punt is kicked
         else{
           x = seq(3,100,length = 1000)
           Yards = 100 - sample(x, 1, prob = dweibull(x, shape =  14.33961, scale = 90.81987) , replace = T)
           #Punt returned for a touchdown
           if (Yards == 0){
             Points = Points - 7
             return(Points)
           }
           #Punt fair caught or returned 
           else {
             YTGnew = 10
             Dnew = 1
             POSnew = POSnew * -1
             FPnew = Yards
           }
         }
       }
       #Kicks a field goal
       else{
         Kick = sample(c(0,1), size = 1, prob = c(1-FGprob, FGprob), replace = TRUE)
         #Makes a field goal
         if (Kick == 1){
           Points = Points - 3
           return(Points)
         }
         #Misses a field goal
         else {
           Points = Points
           POSnew = POSnew * -1
           FPnew = 100 - FPnew
           Dnew = 1
           YTGnew = 10
         }
       }
     }
   }
   #Model begins on a down less than 4
   else{
    #Play runs
    FPnew = FP + Yards
    #Play leads to touchdown
    if (FPnew >= 100){
      #Team A TD
      if (POSnew == 1){
        Points = Points + 7
        return(Points)
      }
      #Team B TD
      else {
        Points = Points - 7
        return(Points)
      }
    }
    else if (FPnew <= 0){
      #Team B safety
      if (POSnew == 1){
        Points = Points - 2
        return(Points)
      }
      #Team A safety
      else {
        Points = Points + 2
        return(Points)
      }
    }
    else {
      #Gains a first down
      if (YTG - Yards <= 0){
        YTGnew = 10
        Dnew = 1
      }
      #Does not gain a first down
      else {
        YTGnew = YTG - Yards
        Dnew = D + 1 
      }
    }
   }
  print(paste0("Down: ", Dnew, ", FP: ", ceiling(FPnew), ", YTG: ", ceiling(YTGnew), ", POS: ", POSnew))
  return(Scenario(Dnew, FPnew, YTGnew, POSnew))
}

Scenario(2, 60, 7, 1)
#Inputs into function: Down, Field Position, Yards to Go, Who has possesion



#===========================================================================
#Yard increments: 30, 35, 40, 45, 50, 55, 60

#Each time it is a 4th and 1 situation

#Model 3 situations: go for it and fail, go for it and succeed, and punt

#Find weighted combination of go for it situations 
#Equation to solve for: pS + (p-1)F = Punt

Situation <- function (D1, FP1, YTG1, POS1){ 
  print(paste0("Convert on 30: ", mean(replicate(10000, Scenario(D1, FP1 + 1, YTG1, POS1)))))
  print(paste0("Fail on 30: ", mean(replicate(10000, Scenario(D1, 100 - FP1, YTG1, POS1*-1)))))
  print(paste0("Punt on 30: ", mean(replicate(10000, Scenario(D1+3, FP1, YTG1-9, POS1)))))
  print(paste0("Convert on 35: ", mean(replicate(10000, Scenario(D1, FP1 + 6, YTG1, POS1)))))
  print(paste0("Fail on 35: ", mean(replicate(10000, Scenario(D1, 100 - (FP1 + 5), YTG1, POS1*-1)))))
  print(paste0("Punt on 35: ", mean(replicate(10000, Scenario(D1+3, FP1 + 5, YTG1-9, POS1)))))
  print(paste0("Convert on 40: ", mean(replicate(10000, Scenario(D1, FP1 + 11, YTG1, POS1)))))
  print(paste0("Fail on 40: ", mean(replicate(10000, Scenario(D1, 100 - (FP1 + 10), YTG1, POS1*-1)))))
  print(paste0("Punt on 40: ", mean(replicate(10000, Scenario(D1+3, FP1 + 10, YTG1-9, POS1)))))
  print(paste0("Convert on 45: ", mean(replicate(10000, Scenario(D1, FP1 + 16, YTG1, POS1)))))
  print(paste0("Fail on 45: ", mean(replicate(10000, Scenario(D1, 100 - (FP1 + 15), YTG1, POS1*-1)))))
  print(paste0("Punt on 45: ", mean(replicate(10000, Scenario(D1+3, FP1 + 15, YTG1-9, POS1)))))
  print(paste0("Convert on 50: ", mean(replicate(10000, Scenario(D1, FP1 + 21, YTG1, POS1)))))
  print(paste0("Fail on 50: ", mean(replicate(10000, Scenario(D1, 100 - (FP1 + 20), YTG1, POS1*-1)))))
  print(paste0("Punt on 50: ", mean(replicate(10000, Scenario(D1+3, FP1 + 20, YTG1-9, POS1)))))
  print(paste0("Convert on 55: ", mean(replicate(10000, Scenario(D1, FP1 + 26, YTG1, POS1)))))
  print(paste0("Fail on 55: ", mean(replicate(10000, Scenario(D1, 100 - (FP1 + 25), YTG1, POS1*-1)))))
  print(paste0("Punt on 55: ", mean(replicate(10000, Scenario(D1+3, FP1 + 25, YTG1-9, POS1)))))
  print(paste0("Convert on 60: ", mean(replicate(10000, Scenario(D1, FP1 + 31, YTG1, POS1)))))
  print(paste0("Fail on 60: ", mean(replicate(10000, Scenario(D1, 100 - (FP1 + 30), YTG1, POS1*-1)))))
  print(paste0("Punt on 60: ", mean(replicate(10000, Scenario(D1+3, FP1 + 30, YTG1-9, POS1)))))
}
Situation(1, 30, 10, 1)

yards = 
probS = c(0.5139, 0.6349, 0.5246, 0.4475, 0.4955, 0.4277, 0.3764)
#If you think you have a greater than 0.37 chance of makiung it
  