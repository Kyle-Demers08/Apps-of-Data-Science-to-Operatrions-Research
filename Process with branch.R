setwd("C:/Users/yyuan11/OneDrive - William & Mary/Documents/DATA440/Lecture 06 Simulation of a COVID test center")

library(simmer)
library(simmer.plot)

remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)

env=simmer()

p=0.8
lambda=1/14

virus = trajectory() %>%
  log_("virus is here hahahaha") %>%
  seize("daughter", amount=1) %>%
  branch(function() sample(c(1, 2), 1, prob=c(p, 1-p)), c(T,T), 
         trajectory("infected") %>%
           log_("got sick") %>%
           timeout(function() runif(1,3,7)),
         trajectory("ok")  %>%
           log_("fine")%>%
           timeout(0.01)) %>%
  release("daughter", amount=1)

env=simmer() %>%
  add_resource("daughter", capacity=1, queue_size=0) %>%
  add_generator("VIRUS", virus, function() rexp(1, lambda)) %>%
  run(until=3000)

df_res=get_mon_resources(env)
df_arr=get_mon_arrivals(env)

df_arr=df_arr[df_arr$finished==TRUE,]
length(which(df_arr$activity_time==0.01))/nrow(df_arr)
