library(simmer)
library(simmer.plot)

##########################################################################################
################ One Kid example #########################################################
##########################################################################################

p=0.7 # probability of getting infected from school
lambda=1/14 # one virus every 14 days
simTime=365*10 # run for ten years, just try to be long enough to get steady state

# dummy trajectory for recording if the kid is sick on that day
dummy = trajectory() %>%
  set_attribute("busy server",function() {get_server_count(env,"kid")})

# the trajectory for virus to go through
virus = trajectory() %>%
  log_("virus is here hahahaha") %>%
  branch(function() sample(c(1, 2), 1, prob=c(p, 1-p)), c(T,T), 
         trajectory("infected") %>%
           seize("kid", amount=1) %>%
           log_("kid got sick") %>%
           timeout(function() runif(1,5,7)) %>%
           release("kid", amount=1),
         trajectory("ok")  %>%
           log_("fine")) 

env=simmer() # initialize environment first; otherwise, attribute won't work
env %>%
  add_resource("kid", capacity=1, queue_size=0) %>%
  add_generator("VIRUS", virus, function() rexp(1, lambda)) %>%
  add_generator("Dummy_recorder", dummy, function() {return(1)}, mon=2) %>%
  run(until=simTime) 

df_res=get_mon_resources(env)
df_arr=get_mon_arrivals(env)
df_att=get_mon_attributes(env)

##### Only look at arrivals of VIRUS
arrivals=df_arr[substr(df_arr$name,1,1)=='V',]
#this is the proportion of virus that successfully infected the kid
length(which(arrivals$activity_time>0))/nrow(arrivals)

##### Check the time the kid is sick
length(which(df_att$value==1)) # days the kid is sick
length(which(df_att$value==1))/simTime # percentage of the time the kid is sick


##########################################################################################
################ Two Kids example Winter Time########################################################
##########################################################################################
p1=0.7 # probability of kid 1 (4 yrs old) getting infected from school
p2=0.3 # probability of kid 2 (7 yrs old) getting infected from school

lambda=1/14 # one virus every 14 days

simTime=365*10 # run for ten years, just try to be long enough to get steady state

# dummy trajectory for recording if the kid is sick on that day

dummy = trajectory() %>%
  set_attribute('busy server1',function() {get_server_count(env,"kid1")}) %>%
  set_attribute('busy server2',function() {get_server_count(env,"kid2")})

# trajectory of virus at the school of Kid 1
virus1 = trajectory() %>%
  log_('virus is at gca') %>%
  branch(function() sample(c(1,2), 1, prob = c(p1, 1-p1)), c(T,T),
         trajectory('infected') %>%
           seize('kid1', amount =1) %>%
           log_('kid1 got sick') %>%
           timeout(function() runif(1,5,7)) %>% #sick for 5-7 days
           release("kid1", amount=1) %>% #after 1 recovers the virus gets to the other
           seize('kid2', amount =1) %>% 
           log_('kid2 was given disease from kid1') %>%
           timeout(function() runif(1,2,4)) %>% #sick for 2-4 days
           release('kid2', amount = 1),
          trajectory('ok') %>%
           log_('fine'))
          
         
  
# trajectory of virus at the school of Kid 2
virus2 = trajectory() %>%
  log_('virus is at matoaka') %>%
  branch(function() sample(c(1,2), 1, prob = c(p2, 1-p2)), c(T,T),
         trajectory('infected') %>%
           seize('kid2', amount =1) %>%
           log_('kid2 got sick') %>%
           timeout(function() runif(1,3,5)) %>% #sick for 3-5 days
           release("kid2", amount=1) %>% #after 1 recovers the virus gets to the other
           seize('kid1', amount =1) %>% 
           log_('kid1 was given disease from kid2') %>%
           timeout(function() runif(1,4,6)) %>% #sick for 4-6 days
           release('kid1', amount = 1),
         trajectory('ok') %>%
           log_('fine'))

env=simmer()
env %>%
  add_resource("kid1", capacity=1, queue_size=0) %>%
  add_resource("kid2", capacity=1, queue_size=0) %>%
  add_generator("VIRUS at GCA", virus1, function() rexp(1, lambda)) %>%
  add_generator("VIRUS at Matoaka", virus2, function() rexp(1, lambda)) %>%
  add_generator("Dummy_recorder", dummy, function() {return(1)}, mon=2) %>%
  run(until=simTime)

df_res=get_mon_resources(env)
df_arr=get_mon_arrivals(env)
df_att=get_mon_attributes(env)
##### Only look at arrivals of VIRUS
arrivals=df_arr[substr(df_arr$name,1,1)=='V',]
#this is the proportion of virus that successfully infected the kid
length(which(arrivals$activity_time>0))/nrow(arrivals)

##### Check the time the kid is sick
length(which(df_att$value==1 & df_att$key == 'busy server1')) # days kid1 is sick
length(which(df_att$value==1 & df_att$key == 'busy server2')) # days kid2 is sick

length(which(df_att$value==1 & df_att$key == 'busy server1'))/simTime # percentage of the time the kid1 is sick
length(which(df_att$value==1 & df_att$key == 'busy server2'))/simTime # percentage of the time the kid2 is sick

#### When either kid is sick
length(which(df_att$value==1)) 
length(which(df_att$value==1))/simTime 
#note that these (below and above) should be equal since it immediately always infects the next kid
length(which(df_att$value==1 & df_att$key == 'busy server1'))/simTime + # percentage of the time the kid1 is sick
length(which(df_att$value==1 & df_att$key == 'busy server2'))/simTime #When kid 2 is sick

#answer: about 46% of the time using winter rates the kids are sick

##########################################################################################
################ Two Kids example Summer Time########################################################
##########################################################################################
p1=0.7 # probability of kid 1 (4 yrs old) getting infected from school
p2=0.3 # probability of kid 2 (7 yrs old) getting infected from school

lambda=1/21 # one virus every 21 days

simTime=365*10 # run for ten years, just try to be long enough to get steady state

# dummy trajectory for recording if the kid is sick on that day

dummy = trajectory() %>%
  set_attribute('busy server1',function() {get_server_count(env,"kid1")}) %>%
  set_attribute('busy server2',function() {get_server_count(env,"kid2")})

# trajectory of virus at the school of Kid 1
virus1 = trajectory() %>%
  log_('virus is at gca') %>%
  branch(function() sample(c(1,2), 1, prob = c(p1, 1-p1)), c(T,T),
         trajectory('infected') %>%
           seize('kid1', amount =1) %>%
           log_('kid1 got sick') %>%
           timeout(function() runif(1,5,7)) %>% #sick for 5-7 days
           release("kid1", amount=1) %>% #after 1 recovers the virus gets to the other
           seize('kid2', amount =1) %>% 
           log_('kid2 was given disease from kid1') %>%
           timeout(function() runif(1,2,4)) %>% #sick for 2-4 days
           release('kid2', amount = 1),
         trajectory('ok') %>%
           log_('fine'))



# trajectory of virus at the school of Kid 2
virus2 = trajectory() %>%
  log_('virus is at matoaka') %>%
  branch(function() sample(c(1,2), 1, prob = c(p2, 1-p2)), c(T,T),
         trajectory('infected') %>%
           seize('kid2', amount =1) %>%
           log_('kid2 got sick') %>%
           timeout(function() runif(1,3,5)) %>% #sick for 3-5 days
           release("kid2", amount=1) %>% #after 1 recovers the virus gets to the other
           seize('kid1', amount =1) %>% 
           log_('kid1 was given disease from kid2') %>%
           timeout(function() runif(1,4,6)) %>% #sick for 4-6 days
           release('kid1', amount = 1),
         trajectory('ok') %>%
           log_('fine'))

env=simmer()
env %>%
  add_resource("kid1", capacity=1, queue_size=0) %>%
  add_resource("kid2", capacity=1, queue_size=0) %>%
  add_generator("VIRUS at GCA", virus1, function() rexp(1, lambda)) %>%
  add_generator("VIRUS at Matoaka", virus2, function() rexp(1, lambda)) %>%
  add_generator("Dummy_recorder", dummy, function() {return(1)}, mon=2) %>%
  run(until=simTime)

df_res=get_mon_resources(env)
df_arr=get_mon_arrivals(env)
df_att=get_mon_attributes(env)
##### Only look at arrivals of VIRUS
arrivals=df_arr[substr(df_arr$name,1,1)=='V',]
#this is the proportion of virus that successfully infected the kid
length(which(arrivals$activity_time>0))/nrow(arrivals)

##### Check the time the kid is sick
length(which(df_att$value==1 & df_att$key == 'busy server1')) # days kid1 is sick
length(which(df_att$value==1 & df_att$key == 'busy server2')) # days kid2 is sick

length(which(df_att$value==1 & df_att$key == 'busy server1'))/simTime # percentage of the time the kid1 is sick
length(which(df_att$value==1 & df_att$key == 'busy server2'))/simTime # percentage of the time the kid1 is sick

#### When either kid is sick
length(which(df_att$value==1)) 
length(which(df_att$value==1))/simTime
#note that these (below and above) should be equal since it immediately always infects the next kid
length(which(df_att$value==1 & df_att$key == 'busy server1'))/simTime + # percentage of the time the kid1 is sick
  length(which(df_att$value==1 & df_att$key == 'busy server2'))/simTime #When kid 2 is sick

#Answer: about 35% of the time the kids are sick using summer time rates
