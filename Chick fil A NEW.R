setwd("/Users/yutingyuan/Documents/William Mary/DATA440AppToOR2023spring/Midterm exam Chick fil A")

# package for discrete event simulation (DES)
library(simmer) 
library(simmer.plot)

# package for pipe operator %>%, which is used to call a series of functions
library(magrittr)

###############################################################################
########### Model 2: New Chick fil A ###########################################
################################################################################
# time unit 1 minute
simTime = 10000

# lane1, lane2 are ordering lanes
# stick to the lane after joining the pick up lane
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
  
  select(function() {ifelse(get_attribute(env, "lane")==1,"lane1pickup","lane2pickup")}, id=2) %>% 
  seize_selected(amount=1, id=2) %>% # try to go on to the corresponding pickup lane
  release_selected(1) %>% # once going to the pickup lane, release the ordering lane
  
  seize("cook",amount=1) %>%
  timeout(function() {rexp(1, 1/5)}) %>%
  release("cook",amount=1)  %>%
  release_selected(amount=1, id=2) # leave the pickup lane once the order is ready

dummy = trajectory() %>%
  set_attribute("carline_lane1",function() {
    carline1 = get_server_count(env,"lane1")+get_server_count(env,"lane1pickup")}) %>%
  set_attribute("carline_lane2",function() {
    carline2 = get_server_count(env,"lane2")+get_server_count(env,"lane2pickup")})  

env = simmer()
env %>%
  add_resource("cashier", 2) %>%
  add_resource("lane1", capacity=200) %>% # ample car spaces in ordering lane1
  add_resource("lane2", capacity=200) %>% # ample car spaces in ordering lane2
  add_resource("lane1pickup", capacity=10) %>% # 10 car spaces in lane1 pick up 
  add_resource("lane2pickup", capacity=10) %>% # 10 car spaces in lane2 pick up 
  add_resource("cook", 3) %>%
  add_generator("Customer", customer, function() rexp(1, 1/1.9), mon=2) %>%
  add_generator("Dummy recorder", dummy, function() 1, mon=2) %>%
  run(simTime)

## investigate to see if the code is correct
df_att = get_mon_attributes(env)
df_arr = get_mon_arrivals(env, per_resource=TRUE)
df_arr = df_arr[order(df_arr$start_time),]

df_res = get_mon_resources(env)

## average car line length
df_att = df_att[substr(df_att$name,1,1)=='D',] # only look at dummy recorder
carline1 = df_att[df_att$key=='carline_lane1',] 
carline2 = df_att[df_att$key=='carline_lane2',] 
plot(carline1$value, type='l')
lines(carline2$value,col='red')

mean(pmax(carline1$value,carline2$value)) 

## average flow time
df_arr = get_mon_arrivals(env)
df_arr = df_arr[order(df_arr$start_time),]
df_arr = df_arr[substr(df_arr$name,1,1)=='C',] # picking customer's data only

df_arr$flow_time = df_arr$end_time - df_arr$start_time

mean(df_arr$flow_time) 

### Monte carlo experiment
dat_new=data.frame(rep=1:500,carline=1:500,flow=1:500)
for(ii in 1:500){
  print(paste0("Repetition ",ii))
  
  env = simmer()
  env %>%
    add_resource("cashier", 2) %>%
    add_resource("lane1", capacity=200) %>% # ample car spaces in ordering lane1
    add_resource("lane2", capacity=200) %>% # ample car spaces in ordering lane2
    add_resource("lane1pickup", capacity=10) %>% # 10 car spaces in lane1 pick up 
    add_resource("lane2pickup", capacity=10) %>% # 10 car spaces in lane2 pick up 
    add_resource("cook", 3) %>%
    add_generator("Customer", customer, function() rexp(1, 1/1.8), mon=2) %>%
    add_generator("Dummy recorder", dummy, function() 1, mon=2) %>%
    run(simTime)
  
  df_att = get_mon_attributes(env)
  df_att = df_att[substr(df_att$name,1,1)=='D',] # only look at dummy recorder
  carline1 = df_att[df_att$key=='carline_lane1',] 
  carline2 = df_att[df_att$key=='carline_lane2',] 
  
  dat_new[ii,'carline']=mean(pmax(carline1$value,carline2$value)) 
  
  df_arr = get_mon_arrivals(env)
  df_arr = df_arr[order(df_arr$start_time),]
  df_arr = df_arr[substr(df_arr$name,1,1)=='C',] # picking customer's data only
  
  df_arr$flow_time = df_arr$end_time - df_arr$start_time
  
  dat_new[ii,'flow']=mean(df_arr$flow_time) 
}

write.csv(dat_new,"New chickfila.csv",row.names = FALSE)

hist(dat_new$carline)
hist(dat_new$flow)
mean(dat_new$carline) # 
mean(dat_new$flow) # 
