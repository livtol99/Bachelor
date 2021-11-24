library(R2jags)
library(dplyr)


### Interpreting the equation defining omega (number of cards considered optimal)
# B.omega[k,t,s] <- -B.g_ref[s]/(log(1-B.p[k,t,s]))

plot(-2/(log(1-seq(0,1,0.01))))

#This plot tells us that, as the probability of getting a loss increase, the optimal number of cards decreases.
# If gamma is high, the number of cards considered optimal is also higher. 
# Make a plot comparing high and low gamma to illustrate what gamma indicates. 





### Interpreting the equation defining theta (probability of stopping the game/banking)

# theta[k,t,s] <- 1-(1/(1+exp(tau[s]*k[t,s]-omega[k,t,s])))
plot(1-(1/(1+exp(2*seq(1,17,0.5)-15))))

# The closer we get to our optimal number of cards (omega), the higher is the probabvility that a person will bank/stop the game.

