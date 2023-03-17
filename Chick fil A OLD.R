setwd("/Users/yutingyuan/Documents/William Mary/DATA440AppToOR2023spring/Midterm exam Chick fil A")

# package for discrete event simulation (DES)
library(simmer) 
library(simmer.plot)

# package for pipe operator %>%, which is used to call a series of functions
library(magrittr)

################################################################################
########### Model 1: Old Chick fil A ###########################################
################################################################################
# time unit 1 minute
simTime = 10000

# lane1, lane2 are ordering lanes
# lane_pickup is the lane after lane1 and lane2 merge
customer = trajectory("Customer's path") %>%
  set_attribute("lane", function() {
    (get_server_count(env, "lane1")>=get_server_count(env, "lane2"))*1+1}
  ) %>% # choose the shortest lane
  select(function() {
    paste0("lane",get_attribute(env, "lane"))
  }) %>% 
  seize_selected(1) %>%
  
  seize("cashier",amount=1) %>%
  timeout(function() {rexp(1, 0.8)}) %>%
  release("cashier",amount=1) %>%
  
  seize("lane_pickup",amount=1) %>% # try to seize a car space in pickup lane
  release_selected(1) %>% # once going to the pickup lane, release the ordering lane
  
  seize("cook",amount=1) %>%
  timeout(function() {rexp(1, 1/5)}) %>%
  release("cook",amount=1) %>%
  release("lane_pickup") # assume after the order is cooked, the car immediately leaves the system

dummy = trajectory() %>%
  set_attribute("carline_ordering",function() {
    carline1 = max(get_server_count(env,"lane1"),get_server_count(env,"lane2"))}) %>%
  set_attribute("carline_pickup",function() {
    carline2 = get_server_count(env,"lane_pickup")})  

env = simmer()
env %>%
  add_resource("cashier", 2) %>%
  add_resource("lane1", capacity=200) %>% # ample car spaces in ordering lane1
  add_resource("lane2", capacity=200) %>% # ample car spaces in ordering lane2
  add_resource("lane_pickup", capacity=6) %>% # 6 car spaces in pick up lane
  add_resource("cook", 3) %>%
  add_generator("Customer", customer, function() rexp(1, 1/1.8), mon=2) %>%
  add_generator("Dummy recorder", dummy, function() 1, mon=2) %>%
  run(simTime)

## investigate to see if the code is correct
df_att = get_mon_attributes(env)
df_arr = get_mon_arrivals(env, per_resource=TRUE)
df_arr = df_arr[order(df_arr$start_time),]

df_res = get_mon_resources(env)

## average car line length
df_att = df_att[substr(df_att$name,1,1)=='D',] # only look at dummy recorder
ordering = df_att[df_att$key=="carline_ordering",'value']
plot(ordering, type='l')

pickup = df_att[df_att$key=="carline_pickup",'value']
plot(pickup, type='l')

total_carline = ordering + pickup
plot(total_carline, type='l')

mean(ordering) 
mean(pickup) 
mean(total_carline) 

## average flow time
df_arr = get_mon_arrivals(env)
df_arr = df_arr[substr(df_arr$name,1,1)=='C',] # picking customer's data only

df_arr$flow_time = df_arr$end_time - df_arr$start_time

mean(df_arr$flow_time) 

#####################################################################
### Monte carlo experiment ##########################################
dat_old=data.frame(rep=1:500,carline=1:500,flow=1:500)

for(ii in 1:500){
  print(paste0("Repetition ",ii))
  env = simmer()
  env %>%
    add_resource("cashier", 2) %>%
    add_resource("lane1", capacity=200) %>% # ample car spaces in ordering lane1
    add_resource("lane2", capacity=200) %>% # ample car spaces in ordering lane2
    add_resource("lane_pickup", capacity=6) %>% # 6 car spaces in pick up lane
    add_resource("cook", 3) %>%
    add_generator("Customer", customer, function() rexp(1, 1/1.8), mon=2) %>%
    add_generator("Dummy recorder", dummy, function() 1, mon=2) %>%
    run(simTime)
  
  ## average car line length
  df_att = get_mon_attributes(env)
  df_att = df_att[substr(df_att$name,1,1)=='D',] # only look at dummy recorder
  ordering = df_att[df_att$key=="carline_ordering",'value']
  
  pickup = df_att[df_att$key=="carline_pickup",'value']
  
  total_carline = ordering + pickup
  
  dat_old[ii,'carline']=mean(total_carline)
  
  ## average flow time
  df_arr = get_mon_arrivals(env)
  df_arr = df_arr[substr(df_arr$name,1,1)=='C',] # picking customer's data only
  
  df_arr$flow_time = df_arr$end_time - df_arr$start_time
  
  dat_old[ii,'flow']=mean(df_arr$flow_time) 
}

write.csv(dat_old, "Old chickfila.csv",row.names = FALSE)

hist(dat_old$carline)
mean(dat_old$carline) # 

hist(dat_old$flow)
mean(dat_old$flow) # 
