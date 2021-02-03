setwd("C:/Users/Ahegar.HEALTH/OneDrive/CovMoh/Spreadsheets/Spreadsheets/Data")
.libPaths( c( "C:/Users/Ahegar.HEALTH/Box/R/Packages" , .libPaths() ) )

# Specify the vaccination coverage
p <- 0.30                                  # 30% coverage 

# Also need to update this in the initial conditions:
# Initial number of people in each compartment
initial_state_values <- c(S = (1-p)*N,    # the unvaccinated proportion of the
                          # population is susceptible
                          I = 10938,          # the epidemic starts with a
                          # single infected person
                          R = 10095,          # there is no prior immunity in 
                          # the population
                          V = p*N,        # a proportion p of the population
                          # is vaccinated (vaccination coverage) 
                          Iv = 0)         # no vaccinated individual has been
# infected at the beginning of the simulation

# MODEL OUTPUT:

# Solving the differential equations using the ode integration algorithm
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = vaccine_model,
                            parms = parameters))

# Plot the number of infected people over time
ggplot(data = output,                                               
       aes(x = time, y = I+Iv)) +    # infected people are in the I and Iv compartment
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Number of infected people") +
  labs(title = paste("Combined leaky vaccine with coverage of", p*100, "%")) + 
  ylim(c(0,N))
