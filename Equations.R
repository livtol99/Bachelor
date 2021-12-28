library(R2jags)
library(dplyr)
library(knitr)
attach(mtcars)

# -------- Omega ----------------
### Interpreting the equation defining omega (number of cards considered optimal)
# B.omega[k,t,s] <- -B.g_ref[s]/(log(1-B.p[k,t,s]))
#  B.omega[t,s] <- -B.g_ref[s]/(log(1-B.p[t,s]))


#Nicer plot
plot(seq(0,1,.03),-2/(log(1-seq(0,1,.03))),type='l',lwd=2, col = "blue",xlab="Probability of a loss",ylab = expression(paste(omega, " - Number of card flips considered optimal")))
lines(seq(0,1,.03),-1/(log(1-seq(0,1,.03))),type='l',lwd=2, col = "orange")
lines(seq(0,1,.03),-0.5/(log(1-seq(0,1,.03))),type='l',lwd=2, col = "purple")


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
plot(seq(1,20,.01),1-(1/(1+exp(10*(seq(1,20,.01)-10)))))

# nicer plot no bug
plot(seq(5,15,.01),1-(1/(1+exp(15*(seq(5,15,.01)-10)))),type = 'l', lwd = 2, col = 'blue', xlab = "Number of card flips", ylab = expression(paste(theta," - Probability of ending the game")))
lines(seq(5,15,.01),1-(1/(1+exp(2*(seq(5,15,.01)-10)))), col = "orange", lwd=2)
lines(seq(5,15,.01),1-(1/(1+exp(1*(seq(5,15,.01)-10)))), col = "purple", lwd=2)

# Adding a legend
legend("right", inset=.02, title= expression(paste(tau, " - Consistency")),
       c("15","2","1"), fill = c("blue", "orange", "purple"), horiz=TRUE, cex=0.8, bty = "n")

legend("bottom", inset = .05, title = expression(paste(omega, " = ")), c("10       "), horiz = TRUE,cex = 0.7, bty = "s")


########-------------------------------------------
##### Plots with actual values 

# Median gamma crack 2.11
# Median control gamma 0.62

# Omega
#  B.omega[t,s] <- -B.g_ref[s]/(log(1-B.p[t,s]))
plot(seq(0,1,.03),-2.11/(log(1-seq(0,1,.03))),type='l',lwd=2, col = "blue",xlab="Probability of a loss",ylab = expression(paste(omega, " - Number of card flips considered optimal")))
lines(seq(0,1,.03),-0.62/(log(1-seq(0,1,.03))),type='l',lwd=2, col = "orange")

legend("right", inset=.02, title= expression(paste(gamma, " - Risk propensity mean value")),
       c("Crack Users (2.11)","Controls (0.62)"), fill = c("blue", "orange"), horiz=F, cex=0.8, bty = "n")


##### Theta 
# Mean crack tau 0.04
# Mean control tau 0.12


# theta[k,t,s] <- 1-(1/(1+exp(tau[s]*k[t,s]-omega[k,t,s])))

# nicer plot no bug
plot(seq(5,15,.01),1-(1/(1+exp(0.04*(seq(5,15,.01)-10)))),type = 'l', lwd = 2, col = 'blue', xlab = "Number of card flips", ylab = expression(paste(theta," - Probability of ending the game")))
lines(seq(5,15,.01),1-(1/(1+exp(0.12*(seq(5,15,.01)-10)))), col = "orange", lwd=2)

# Adding a legend
legend("right", inset=.02, title= expression(paste(tau, " - Consistency mean value")),
       c("Crack Users (0.04)", "Controls (0.12"), fill = c("blue", "orange"), horiz=F, cex=0.8, bty = "n")

legend("bottom", inset = .05, title = expression(paste(omega, " = ")), c("10       "), horiz = T,cex = 0.7, bty = "s")

controlsvscrack.ttest.samples
