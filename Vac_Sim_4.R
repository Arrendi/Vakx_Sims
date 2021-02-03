# 1) Vaccinating only children
# There are more doses of vaccine than children in the population, so the vaccine coverage will be 100% in children
# and 0% in the other groups as per the question

vacc_cov1 <- 0                  # vaccine coverage in children
vacc_cov2 <- 0.10               # vaccine coverage in adults
vacc_cov3 <- 1                  # vaccine coverage in the elderly

vacc_eff3 <- 0.9                # vaccine efficacy in the elderly (100% in the other age groups)

# Effective vaccine coverage for each age group:
p1 <- vacc_cov1
p2 <- vacc_cov2 
p3 <- vacc_cov3 * vacc_eff3

# Population size in total and for each age group:
N <- 419200
N1 <- 0.46*N
N2 <- 0.48*N
N3 <- 0.06*N

# Fill in initial state values for a naive population based on effective vaccine coverage:
initial_state_values <- c(S1 = N1-p1*N1,
                          S2 = N2-p2*N2,  
                          S3 = N3-p3*N3,
                          I1 = 10938, # the outbreak starts with 1 infected person (can be of either age) 
                          I2 = 0,
                          I3 = 0,
                          R1 = p1*N1,
                          R2 = p2*N2,   
                          R3 = p3*N3)

# Run model output
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_age_model,
                            parms = parameters))

# Calculate cumulative incidence in each age group:
results1 <- data.frame(child_cum_inc = output$S1[1]-output$S1[nrow(output)],
                       adult_cum_inc = output$S2[1]-output$S2[nrow(output)], 
                       elderly_cum_inc =  output$S3[1]-output$S3[nrow(output)],
                       total_cum_inc = sum(output[1,c("S1", "S2", "S3")])-sum(output[nrow(output),c("S1", "S2", "S3")]))
print(results1)

#60% Vaccine Efficacy 
CovLambda <- 1/5
Vef0.6 <- 0.4 * CovLambda
VacImp1 <- 0.3*(Vef0.6 * 326976) #Estimated number of people susceptible