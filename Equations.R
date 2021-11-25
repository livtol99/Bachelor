library(R2jags)
library(dplyr)
library(knitr)

# -------- Omega ----------------
### Interpreting the equation defining omega (number of cards considered optimal)
# B.omega[k,t,s] <- -B.g_ref[s]/(log(1-B.p[k,t,s]))

plot(-2/(log(1-seq(0,1,.01))))


#Nicer plot
plot(seq(0,1,.03),-2/(log(1-seq(0,1,.03))),type='l',lwd=2, col = "blue",xlab="Probability of a loss",ylab = expression(paste(omega, " - Number of pumps considered optimal")))
lines(seq(0,1,.03),-1/(log(1-seq(0,1,.03))),type='l',lwd=2, col = "orange")
lines(seq(0,1,.03),-0.5/(log(1-seq(0,1,.03))),type='l',lwd=2, col = "purple")

attach(mtcars)

legend("right", inset=.02, title= expression(paste(gamma, " - Risk propensity")),
       c("2","1","0.5"), fill = c("blue", "orange", "purple"), horiz=TRUE, cex=0.9, bty = "n")

#This plot tells us that, as the probability of getting a loss increase, the number of cards considered optimal (omega) decreases.
# The influence of gamma on omega is illustrated through the three different graps.
# When an individual's Gamme/risk propensity is high, so will their omega/number os cards considered optimal be. 

# bruk h eller l for type =


# ---------------------------------------------------------------------------------------------------------------

# -------Theta------------
# Interpreting the equation defining theta (probability of stopping the game/banking)

# theta[k,t,s] <- 1-(1/(1+exp(tau[s]*k[t,s]-omega[k,t,s])))
plot(1-(1/(1+exp(2*seq(1,10,1)-10))))

#Nicer plot
plot(seq(1,15,.1),1-(1/(1+exp(2*seq(1,15,.1)-10))),type='l',lwd=2,col = 'blue',xlab="Number of card flips",ylab = expression(paste(theta," - Probability of banking")))
lines(seq(1,15,.1),1-(1/(1+exp(1.5*seq(1,15,.1)-10))), col = "orange",lwd =2)
lines(seq(1,15,.1),1-(1/(1+exp(1*seq(1,15,.1)-10))), col = "purple",lwd =2)

# Adding a legend
attach(mtcars)

legend("right", inset=.02, title= expression(paste(tau, " - Consistency")),
       c("2","1.5","1"), fill = c("blue", "orange", "purple"), horiz=TRUE, cex=0.8, bty = "n")

# The closer we get to our optimal number of cards (omega) (which is set to be 10 for all graphs),
# the higher is the probability that a person will bank/stop flipping cards and thereby saving the money they have earned.
# A person with low consistency (tau), will have a lower chance of ending the game close to the optimal number of cards
# when compared to a person with higher consistency. A person with t = 2 has about 80% chance of ending the game
# at close to six card flips when the optimal number of cards is considered to be 10. A person with t = 1, will 
# have the same probability of ending the game at close to 12 flips, so after the optimal number of card flips. 

