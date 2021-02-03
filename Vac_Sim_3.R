setwd("C:/Users/Ahegar.HEALTH/OneDrive/CovMoh/Spreadsheets/Spreadsheets/Data")
.libPaths( c( "C:/Users/Ahegar.HEALTH/Box/R/Packages" , .libPaths() ) )

# Loading and exploring the dataset
data <- read.csv("../Graphics_and_Data/m3_nb3_data.csv")

plot(data$time, data$number_infected)

# PACKAGES
require(deSolve)
require(ggplot2)

# INPUT
initial_state_values <- c(S = 499200-(10938+10095),        # the total population size is 500
                          I = 10938,       
                          R = 10095)

times <- seq(from = 0, to = 1095, by = 0.1)  # the outbreak lasts 200 days

# SIR MODEL FUNCTION
sir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {
    
    N <- S+I+R
    
    lambda <- beta * I/N
    
    # The differential equations
    dS <- -lambda * S               
    dI <- lambda * S - gamma * I
    dR <- gamma * I             
    
    # Output
    return(list(c(dS, dI, dR))) 
  })
}

## DISTANCE FUNCTION 
SIR_SSQ <- function(parameters, dat) {  # takes as inputs the parameter values and dataset
  
  beta <- parameters[1]    # extract and save the 1st value in the "parameters" 
  # input argument as beta
  gamma <- parameters[2]   # and 2nd value in the "parameters" input argument as gamma
  
  # Simulate the model with initial conditions and timesteps defined above,
  # and parameter values from function call
  output <- as.data.frame(ode(y = initial_state_values, 
                              times = times, 
                              func = sir_model,
                              parms = c(beta = beta,      # ode() takes the values for
                                        # beta and gamma extracted from
                                        gamma = gamma)))  # the "parameters" input argument
  # of the SIR_SSQ() function
  
  # Calculate the sum of squares by comparing the model output with the matching
  # datapoints: This involves, for each timepoint with available data, 
  # calculating the difference between the number of infections
  # predicted by the model and the observed number of infections, squaring all these 
  # differences,and taking the sum of all squared differences
  SSQ <- sum((output$I[output$time %in% dat$time]-dat$number_infected)^2)
  
  return(SSQ)
  
}

optim(par = c(0.045, 0.0625),         # chose sensible starting values for beta and gamma
      fn = SIR_SSQ,              # the distance function to optimise
      dat = data)                # the dataset to fit to 
# ("dat" argument is passed to the 
# function specified in fn)

# Simulate the model with the estimated best-fitting parameter values
parameters <- c(beta = 0.045,
                gamma = 0.0625)

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))

# PLOT OF THE MODEL FIT
ggplot() +
  geom_line(data = output, aes(x = time, y = I)) +                              
  geom_point(data = data, aes(x = time, y = number_infected), col = "red") +  
  xlab("Time (days)")+                                              
  ylab("Prevalence of infection") +                                 
  labs(title = paste("Model fit to the epidemic curve with beta =", parameters["beta"], 
                     "and gamma =", parameters["gamma"]))

initial_state_values <- c(S = 0.8*499200,    # only 80% of the population are susceptible
                          I = 1,       
                          R = 0.3*499200)    # 30% of the population are vaccinated/immune

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))

# PLOT OF THE MODEL FIT

ggplot() +
  geom_line(data = output, aes(x = time, y = I)) +                              
  xlab("Time (days)")+                                              
  ylab("Prevalence of infection") + 
  labs(title = "Vaccine coverage of 92% with an all-or-nothing vaccine with 75% efficacy") +
  ylim(c(0,400000))                         