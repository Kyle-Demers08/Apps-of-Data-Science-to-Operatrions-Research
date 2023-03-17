#imports
library(magrittr)
library(simmer) 
library(simmer.plot)
# time unit 1 minute
simTime = 10000
take_order = function() {return(rexp(1, 0.8))}
order_inside = function() {return(rexp(1, 1/1.8))} #an order every 1.8 minutes
order_drivethrough = function() {return(rexp(1, 1/1.8))} #an order every 1.8 minutes
cook_time = rexp(1,1/5)

inside = trajectory('inside') %>%
  #order
  seize("cashier_inside",amount=1) %>%
  timeout(take_order) %>%
  release("cashier_inside",amount=1) %>%
  
  seize('cook_inside',1) %>%
  timeout(cook_time) %>%
  release('cook_inside',1)

outside = trajectory('outside') %>%
  #order
  seize("cashier_outside",amount=1) %>%
  timeout(take_order) %>%
  release("cashier_outside",amount=1) %>%
  
  seize('cook_outside',1) %>%
  timeout(cook_time) %>%
  release('cook_outside',1)

dummy = trajectory() %>%
  set_attribute("inside_queue",function() {get_queue_count(env,"cook_inside")}) %>%
  set_attribute("outside_queue",function() {get_queue_count(env,"cook_outside")})
  
env = simmer()
env %>%
  add_resource("cook_inside", 2) %>%
  add_resource("cashier_inside", 2) %>%
  add_resource("cashier_outside", 2) %>%
  add_resource("cook_outside", 1) %>% #since there are only 3
  add_generator("customer_inside", inside, order_inside) %>%
  add_generator("customer_outside", outside, order_drivethrough) %>%
  add_generator("Dummies", dummy, function() {return(1)},mon=2) %>% # interarrival time is 1
  run(simTime)  

df_att = get_mon_attributes(env)
df_arr = get_mon_arrivals(env, per_resource=TRUE)
df_arr = df_arr[order(df_arr$start_time),]
df_res = get_mon_resources(env)  

View(df_att)
#outside(drivethrough) can't keep up with the orders

env = simmer()
env %>%
  add_resource("cook_inside", 1) %>%
  add_resource("cashier_inside", 2) %>%
  add_resource("cashier_outside", 2) %>%
  add_resource("cook_outside", 2) %>% #since there are only 3
  add_generator("customer_inside", inside, order_inside) %>%
  add_generator("customer_outside", outside, order_drivethrough) %>%
  add_generator("Dummies", dummy, function() {return(1)},mon=2) %>% # interarrival time is 1
  run(simTime)  

df_att = get_mon_attributes(env)
df_arr = get_mon_arrivals(env, per_resource=TRUE)
df_arr = df_arr[order(df_arr$start_time),]
df_res = get_mon_resources(env)  
  
View(df_att)
#inside can't keep up with the orders

env = simmer()
env %>%
  add_resource("cook_inside", 2) %>%
  add_resource("cashier_inside", 2) %>%
  add_resource("cashier_outside", 2) %>%
  add_resource("cook_outside", 2) %>% #since there are only 3
  add_generator("customer_inside", inside, order_inside) %>%
  add_generator("customer_outside", outside, order_drivethrough) %>%
  add_generator("Dummies", dummy, function() {return(1)},mon=2) %>% # interarrival time is 1
  run(simTime)  

df_att = get_mon_attributes(env)
df_arr = get_mon_arrivals(env, per_resource=TRUE)
df_arr = df_arr[order(df_arr$start_time),]
df_res = get_mon_resources(env)  

View(df_att)
#both keep up with the orders