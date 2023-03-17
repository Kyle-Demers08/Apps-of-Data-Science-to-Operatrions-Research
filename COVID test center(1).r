setwd("C:/Users/yyuan11/OneDrive - William & Mary/Documents/DATA440/Lecture 06 Simulation of a COVID test center")

install.packages("arulesViz")

library(simmer)
library(simmer.plot)
library(simmer.bricks) # simplify some syntax
library(data.table)

# Input variables, time unit: hour
Arrival = function() {return(rexp(1, 1/2.5))}          # Inter Arrival Time - Exponential Distribution
RegTime = function() {return(runif(1, 0.5, 1.5))}      # Registration Time - Uniform Distribution
SwabTime = function() {return(rnorm(1, 2, 1/3))}       # Swab Taking Time - Normal Distribution
CleanTime = function() {return(rnorm(1, 2, 0.33))}     # Bay Cleaning Time - Normal Distribution
TestTime = function() {return(rnorm(1, 1.5, 0.25))}    # Test Process Time - Normal Distribution
SimTime = 240 

##############################################################################
######### Model 1: basis: no wait for test result ############################
##############################################################################
t = trajectory("a visit") %>%
  # Registration Step
  seize("Registrar", 1) %>%  
  timeout(RegTime) %>%    
  release("Registrar",1) %>%
  
  # Pickup Test Kit
  seize("Test Staff", 1) %>%   
  timeout(0.5) %>%
  release("Test Staff", 1) %>% 
  
  # Take Swab, Return Swab & Clean Bay Steps
  seize("Bay", 1) %>%  
  timeout(SwabTime) %>%
  timeout(0.5) %>%       
  timeout(CleanTime) %>%
  release("Bay",1) 

plot(t, verbose = TRUE)

env = simmer()

# Add resources
env %>% 
  add_resource("Registrar", 1) %>%
  add_resource("Bay", 4) %>%
  add_resource("Test Staff", 1)

# Add arrivals
env %>% add_generator("Student", t, Arrival)

env %>% run(until = SimTime)  

# Check results for this sample path
resources = get_mon_resources(env)
arrivals = get_mon_arrivals(env, per_resource = TRUE)

subset(arrivals, name == "Student8")  

# check the total time in system
arrivals = get_mon_arrivals(env, per_resource = FALSE)
subset(arrivals, name == "Student8")  

hist(arrivals$end_time - arrivals$start_time) # flow time in system

  
########################################################################################
######### Model 2: monitor number of students in system ################################
########################################################################################
t = trajectory("a visit") %>%
  # Registration Step
  seize("Registrar", 1) %>%  
  timeout(RegTime) %>%    
  release("Registrar",1) %>%
  
  # Pickup Test Kit
  seize("Test Staff", 1) %>%   
  timeout(0.5) %>%
  release("Test Staff", 1) %>% 
  
  # Take Swab, Return Swab & Clean Bay Steps
  seize("Bay", 1) %>%  
  timeout(SwabTime) %>%
  timeout(0.5) %>%       
  timeout(CleanTime) %>%
  release("Bay",1) 

t_dummy = trajectory("dummy for num of students") %>%
  set_attribute("number at Registrar",function() {get_queue_count(env,"Registrar")+
      get_server_count(env,"Registrar")}) %>%
  set_attribute("number at Bay",function() {get_queue_count(env,"Bay")+
      get_server_count(env,"Bay")}) %>%
  set_attribute("number at Test Staff",function() {get_queue_count(env,"Test Staff")+
      get_server_count(env,"Test Staff")}) %>%
  seize("Dummy server", 1) %>%  
  timeout(0.001) %>%    
  release("Dummy server",1) 
  
env = simmer()

# Add resources
env %>% 
  add_resource("Registrar", 1) %>%
  add_resource("Bay", 4) %>%
  add_resource("Test Staff", 1) %>%
  add_resource("Dummy server",1000)

# Add arrivals
env %>% 
  add_generator("Student", t, Arrival) %>%
  add_generator("Dummies", t_dummy, function() {return(1)},mon=2) # interarrival time is 1

env %>% run(until = SimTime)  

# Check results for this sample path
df_arr=get_mon_arrivals(env, per_resource=TRUE)
df_att=get_mon_attributes(env)

df_reg=df_arr[df_arr$resource=='Registrar',]

#plot(df_att$time,df_att$value,type='l', main='Number of students at Registrar')

unique(df_att$key)

check=df_att[df_att$key=='number at Registrar',]
plot(check$time,check$value,type='l', main='Number of students',ylim=c(0,4))

# Add a red line for Number of students at Bay
check=df_att[df_att$key=='number at Bay',]
lines(check$value,lty=2, col='red')

# Add a blue line for Number of students at Test staff
check=df_att[df_att$key=='number at Test Staff',]
lines(check$value,lty=3, col='blue')

legend(0, 4, legend=c("Registrar", "Bay", "Test Staff"),
       col=c("black","red", "blue"), lty=1:3, cex=0.8)

# Does the plot make sense?

######################################################################
# We have not modeled the flow of "perform test": ####################
# Assume student waits when staff performs test. #####################
# It becomes complicated because we can only #########################
# move One student along One arrow, when defining his trajectory. ####
# Use parallel paths to clone the student (get another copy of him)###

# visit() combines seize-timeout-release pipeline, a function in simmer.brick
t1 = trajectory() %>%
  
  # Registration Step
  visit("Registrar", RegTime, 1) %>%
  
  # Pickup Test Kit
  visit("Test Staff", 0.5, 1) %>%
  
  # Take Swab (Note - Bay Not Released)
  seize("Bay", 1) %>%  
  timeout(SwabTime) %>%
  
  # Return Swab
  visit("Test Staff", 0.5, 1) 

# Here the path splits into parallel routes

t2 = trajectory() %>%
  
  # Clean Bay
  timeout(CleanTime) %>%
  
  # Release Bay (seized in t1)
  release("Bay",1) 

t3 = trajectory() %>%
  # Perform Test
  visit("Test Staff", TestTime, 1) 


# Join Trajectories
t = trajectory() %>%
  
  # path before the split
  join(t1) %>%
  
  # split into parallel paths t2, t3
  clone(n = 2, t2, t3) %>%
  
  # join parallel paths, check Help to see the meaning of wait=TRUE 
  synchronize(wait = TRUE) 

plot(t, verbose = TRUE)

env <- simmer() %>%
  add_resource("Registrar", 1)  %>%
  add_resource("Bay", 4)   %>%
  add_resource("Test Staff", 1) %>%
  add_generator("Student", t, Arrival)

# Run Simulation
env %>% run(until = SimTime)

# Check Results
resources = get_mon_resources(env)
arrivals = get_mon_arrivals(env, per_resource = TRUE)
arrivals2 = get_mon_arrivals(env)
student0_traj = subset(arrivals, name == "Student0")
student0_traj[order(student0_traj$start_time),]

# Plot Flow Time
plot(arrivals2, metric="flow_time")



##########################################################################
########## You operate the COVID ED #############################
########## Decision: How many Test Staff per week ########################
########## Understand how this number affects average flow time ##########
##########################################################################

# We will change the arrival rate input according to the prediction of COVID cases
# Check data: https://www.nytimes.com/interactive/2021/us/williamsburg-virginia-covid-cases.html

# Time unit: 10 minutes
Arrival = function() {return(rexp(1, 2))} 

t1 = trajectory() %>%
  
  # Registration Step
  visit("Registrar", RegTime, 1) %>%
  
  # Pickup Test Kit
  visit("Test Staff", 0.5, 1) %>%
  
  # Take Swab (Note - Bay Not Released)
  seize("Bay", 1) %>%  
  timeout(SwabTime) %>%
  
  # Return Swab
  visit("Test Staff", 0.5, 1) 

# Here the path splits into parallel routes

t2 = trajectory() %>%
  
  # Clean Bay
  timeout(CleanTime) %>%
  
  # Release Bay (seized in t1)
  release("Bay",1) 

t3 = trajectory() %>%
  # Perform Test
  visit("Test Staff", TestTime, 1) 

# Join Trajectories
t = trajectory() %>%
  
  # path before the split
  join(t1) %>%
  
  # split into parallel paths t2, t3
  clone(n = 2, t2, t3) %>%
  
  # join parallel paths, check Help to see the meaning of wait=TRUE 
  synchronize(wait = TRUE) 

env <- simmer() %>%
  add_resource("Registrar", 2)  %>%
  add_resource("Bay", 8)   %>%
  add_resource("Test Staff",6) %>%
  add_generator("Student", t, Arrival)

# Run Simulation
env %>% run(until = SimTime)

# Check Results
resources = get_mon_resources(env)
arrivals = get_mon_arrivals(env, per_resource = TRUE)
arrivals2 = get_mon_arrivals(env)

arrivals2$flow_time = arrivals2$end_time-arrivals2$start_time

mean(arrivals2[30:nrow(arrivals2),'flow_time'])
plot(arrivals2,metric = 'flow_time')

#63 #1159
#2256 #12.322
#3.7 #1964
## Try to increase the number of Test Staff=4, what happened to the flow time?
## Why is it even bigger than the case with 1 Test staff?
## How many arrivals on average per unit time?
## How long in total to process one student on average?
## Be careful about the stability of queueing models.

plot(resources,metric = 'usage')
# Do you think the system is currently stable? What resources we need to add more?
# Write code to automatically iterate over possible number of each resource


