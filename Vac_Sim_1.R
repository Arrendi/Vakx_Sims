setwd("C:/Users/Ahegar.HEALTH/OneDrive/CovMoh/Spreadsheets/Spreadsheets/Data")
.libPaths( c( "C:/Users/Ahegar.HEALTH/Box/R/Packages" , .libPaths() ) )

# LOAD THE PACKAGES:
library(deSolve)
library(reshape2)
library(ggplot2)

# MODEL INPUTS:

# Specify the total population size
N <- 419200

# Specify the vaccination coverage
p <- 0.3                                    # 30% coverage

# Initial number of people in each compartment
initial_state_values <- c(S = (1-p)*N,  # the unvaccinated proportion of the
                          # population is susceptible
                          I = 10938,        # the epidemic starts with a single
                          # infected person
                          R = 10095,        # there is no prior immunity in
                          # the population
                          V = p*N,      # a proportion p of the population is
                          # vaccinated (vaccination coverage) 
                          Iv = 0)       # no vaccinated individual has been
# infected at the beginning of the simulation

# Parameters
parameters <- c(beta = 0.045,     # the infection rate in units of days^-1
                gamma = 0.0625,     # the rate of recovery in units of days^-1
                c_s = 0.9,       # the reduction in the force of infection
                # acting on those vaccinated
                c_i = 0.9)       # the reduction in the infectivity of
# vaccinated infected people       

# TIMESTEPS:

# Sequence of timesteps to solve the model at
times <- seq(from = 0, to = 1095, by = 0.1)   # from 0 to 3 years, daily intervals

# MODEL FUNCTION: 

vaccine_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {    
    
    # Defining lambda as a function of beta and I:
    lambda <- beta * I/N + c_i * beta * Iv/N 
    # the Iv compartment is c_i times less infectious than the I compartment
    
    # The differential equations
    dS <- -lambda * S            
    dI <- lambda * S - gamma * I   
    dR <- gamma * I + gamma * Iv            # infected and vaccinated infected
    # individuals recover at the same rate   
    dV <- -c_s * lambda * V                 # vaccinated people become infected at
    # a rate c_s * lambda
    dIv <- c_s * lambda * V - gamma * Iv    # vaccinated people who become infected
    # move into the Iv compartment
    
    # Return the number of people in each compartment at each timestep 
    # (in the same order as the input state variables)
    return(list(c(dS, dI, dR, dV, dIv))) 
  })
  
}

# MODEL OUTPUT:

# Solving the differential equations using the ode integration algorithm
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = vaccine_model,
                            parms = parameters))

# PLOT THE OUTPUT

# Plot the number of infected people over time
ggplot(data = output,                                               
       aes(x = time, y = I+Iv)) +   # infected people are in the I and Iv compartment
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Number of infected people") +
  labs(title = paste("Combined leaky vaccine with coverage of", p*100, "%"))