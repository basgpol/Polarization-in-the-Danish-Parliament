#################################################################
########################## SNA PROJECT ##########################
########### Voting behaviour in the Danish Parliament ###########
######################### DATA ANALYSIS #########################
#################################################################

################
### Contents ###
################

# 1) Loading packages
# 2) Loading data
# 3) Creating functions
# 4) Data Handling
# 5) Correcting mistakes in API Data 
# 6) Data Saving

# =====================================================================================================

###########################
### 1) LOADING PACKAGES ###
###########################

library(dplyr)
library(lubridate)
library(igraph)
library(ggplot2)
library(zoo)
library(data.table)

# =====================================================================================================

###########################
##### 2) LOADING DATA #####
###########################

#Setting working directory
setwd("C:/Users/BjørnAugust/OneDrive for Business/Dokumenter/Studie/11. semester/Social Network Analysis/scripts")
#loading dataset creating in script "DATA IMPORT"
df = read.csv("voting_data.csv", header = TRUE)

# =====================================================================================================

##################################
##### 3 ) CREATING FUNCTIONS #####
##################################

##### CREATING FUNCTION TO CALCULATE GRAPH DENSITY #######

graph_density_calculator <- function(df, voteid){
  ### The following function calculates the graph density from a social network created by voting data
  ### The function takes two inputs:
  ###     1) A dataframe of voting data (equal to "voting_data_csv").
  ###     2) A id number for the voting of which it should calculate the graph density. 
  
  # Creating a affiliation matrix for the specific vote with the selected id
  df_affilliation = df %>% 
    filter(afstemningid == voteid) %>% #removs all other observations
    select(aktorid, type) %>% #keep only variables of actorid and votetype
    filter(type != "Fravær") # removing all the members that didn't vote
  
  # Creating undirected igraph object
  g = graph.edgelist(as.matrix(df_affilliation), directed = F) # formating as a igraph object
  
  # Changing to one mode igraph object
  V(g)$type = bipartite.mapping(g)$type
  one_mode = bipartite_projection(g)$proj1 # change to one mode
  
  # Calculating graph density for the network
  graph_density = graph.density(one_mode)
  
  # The function returns the calculated graph density
  return(graph_density)
}

### Testing the function ###
graph_density_calculator(df, 1)
graph_density_calculator(df, 300)

##### CREATING FUNCTION TO CALCULATE MEAN DEGREE #######

mean_degree_calculator <- function(df, voteid){
  ### The following function calculates the graph density from a social network created by voting data
  ### The function takes two inputs:
  ###     1) A dataframe of voting data (equal to "voting_data_csv").
  ###     2) A id number for the voting of which it should calculate the graph density. 
  
  # Creating a affiliation matrix for the specific vote with the selected id
  df_affilliation = df %>% 
    filter(afstemningid == voteid) %>% #removs all other observations
    select(aktorid, type) %>% #keep only variables of actorid and votetype
    filter(type != "Fravær") # removing all the members that didn't vote
  
  # Creating undirected igraph object
  g = graph.edgelist(as.matrix(df_affilliation), directed = F) # formating as a igraph object
  
  # Changing to one mode igraph object
  V(g)$type = bipartite.mapping(g)$type
  one_mode = bipartite_projection(g)$proj1 # change to one mode
  
  # Calculating graph density for the network
  mean_degree = mean(degree(one_mode, mode = "total", normalized = F))
  
  # The function returns the calculated graph density
  return(mean_degree)
}

### Testing the function ###
mean_degree_calculator(df, 1)
mean_degree_calculator(df, 300)


##### CREATING FUNCTION TO CALCULATE MODULARITY #######

modularity_calculator <- function(df, voteid){
  ### The following function calculates the louvian modularity of  a social network created by voting data
  ### The function takes two inputs:
  ###     1) A dataframe of voting data (equal to "voting_data_csv").
  ###     2) A id number for the voting of which it should calculate the graph density. 
  
  # Creating a affiliation matrix for the specific vote with the selected id
  df_affilliation = df %>% 
    filter(afstemningid == voteid) %>% #removs all other observations
    select(aktorid, type) %>% #keep only variables of actorid and votetype
    filter(type != "Fravær") # removing all the members that didn't vote
  
  # Creating undirected igraph object
  g = graph.edgelist(as.matrix(df_affilliation), directed = F) # formating as a igraph object
  
  # Changing to one mode igraph object
  V(g)$type = bipartite.mapping(g)$type
  one_mode = bipartite_projection(g)$proj1 # change to one mode
  
  # Calculating modularity for the network
  modularity_measure = cluster_louvain(one_mode)$modularity
  
  
  # The function returns the calculated modularity
  return(modularity_measure)
}

### Testing the function ###
modularity_calculator(df, 1)
modularity_calculator(df, 300)

##### CREATING FUNCTION TO CALCULATE GRAPH DENSITY (EXCLUDING NEITHER VOTES) #####

graph_density_calculator_no_neither <- function(df, voteid){
  ### The following function calculates the graph density from a social network created by voting data
  ### The function takes two inputs:
  ###     1) A dataframe of voting data (equal to "voting_data_csv").
  ###     2) A id number for the voting of which it should calculate the graph density. 
  
  # Creating a affiliation matrix for the specific vote with the selected id
  df_affilliation = df %>% 
    filter(afstemningid == voteid) %>% #removs all other observations
    select(aktorid, type) %>% #keep only variables of actorid and votetype
    filter(type != "Fravær" & type != "Hverken for eller imod") # removing all the members that didn't vote
  
  # Creating undirected igraph object
  g = graph.edgelist(as.matrix(df_affilliation), directed = F) # formating as a igraph object
  
  # Changing to one mode igraph object
  V(g)$type = bipartite.mapping(g)$type
  one_mode = bipartite_projection(g)$proj1 # change to one mode
  
  # Calculating graph density for the network
  graph_density = graph.density(one_mode)
  
  # The function returns the calculated graph density
  return(graph_density)
}

### Testing the function ###
graph_density_calculator_no_neither(df, 1)
graph_density_calculator_no_neither(df, 300)

##### CREATING FUNCTION TO CALCULATE MEAN DEGREE (EXCLUDING NEITHER VOTES) #######

mean_degree_calculator_no_neither <- function(df, voteid){
  ### The following function calculates the graph density from a social network created by voting data
  ### The function takes two inputs:
  ###     1) A dataframe of voting data (equal to "voting_data_csv").
  ###     2) A id number for the voting of which it should calculate the graph density. 
  
  # Creating a affiliation matrix for the specific vote with the selected id
  df_affilliation = df %>% 
    filter(afstemningid == voteid) %>% #removs all other observations
    select(aktorid, type) %>% #keep only variables of actorid and votetype
    filter(type != "Fravær" & type != "Hverken for eller imod") # removing all the members that didn't vote
  
  # Creating undirected igraph object
  g = graph.edgelist(as.matrix(df_affilliation), directed = F) # formating as a igraph object
  
  # Changing to one mode igraph object
  V(g)$type = bipartite.mapping(g)$type
  one_mode = bipartite_projection(g)$proj1 # change to one mode
  
  # Calculating graph density for the network
  mean_degree = mean(degree(one_mode, mode = "total", normalized = F))
  
  # The function returns the calculated graph density
  return(mean_degree)
}

### Testing the function ###
mean_degree_calculator_no_neither(df, 1)
mean_degree_calculator_no_neither(df, 300)

##### CREATING FUNCTION TO CALCULATE MODULARITY  (EXCLUDING NEITHER VOTES) #######

modularity_calculator_no_neither <- function(df, voteid){
  ### The following function calculates the louvian modularity of  a social network created by voting data
  ### The function takes two inputs:
  ###     1) A dataframe of voting data (equal to "voting_data_csv").
  ###     2) A id number for the voting of which it should calculate the graph density. 
  
  # Creating a affiliation matrix for the specific vote with the selected id
  df_affilliation = df %>% 
    filter(afstemningid == voteid) %>% #removs all other observations
    select(aktorid, type) %>% #keep only variables of actorid and votetype
    filter(type != "Fravær" & type != "Hverken for eller imod") # removing all the members that didn't vote
  
  # Creating undirected igraph object
  g = graph.edgelist(as.matrix(df_affilliation), directed = F) # formating as a igraph object
  
  # Changing to one mode igraph object
  V(g)$type = bipartite.mapping(g)$type
  one_mode = bipartite_projection(g)$proj1 # change to one mode
  
  # Calculating modularity for the network
  modularity_measure = cluster_louvain(one_mode)$modularity
  
  
  # The function returns the calculated modularity
  return(modularity_measure)
}

### Testing the function ###
modularity_calculator_no_neither(df, 1)
modularity_calculator_no_neither(df, 300)

# =====================================================================================================

############################
##### 4) DATA HANDLING #####
############################

##### CALCULATING GRAPH DENSITY, MEAN DEGREE & MODULARITY FOR ALL VOTINGS #####
df_calculated = df %>% 
  group_by(afstemningid) %>% 
  summarise(gd = graph_density_calculator(df, afstemningid), # calculating graph density for each voting
            mean_degree = mean_degree_calculator(df, afstemningid), # calculating mean degree for each voting
            modularity = modularity_calculator(df,afstemningid), # calculating modularity for each voting
            gd_no_neither = graph_density_calculator_no_neither(df, afstemningid), # calculating graph density for each voting excluding neithers
            mean_degree_no_neither = mean_degree_calculator_no_neither(df, afstemningid), # calculating mean degree for each voting excluding neithers
            modularity_no_neither = modularity_calculator_no_neither(df,afstemningid),# calculating modularity for each voting excluding neithers
            dag = first(dato), # variable showing the date of earch voting
            periode = first(periodeid), # variable describing the type of the vote
            beskrivelse = first(afstemningsbeskrivelse)) # variable describing the type of the vote

# Changing variable format
df_polarization = df_calculated %>% 
  mutate(dato = as.Date(dag)) %>% # converting format from factor to Date
  mutate(afstemningsbeskrivelse = as.character(beskrivelse))# converting format from factor to character string

# Creating dataframe with number and proportion of different votes
df_prop = df %>% 
  group_by(afstemningid) %>% 
  summarise(yes_n = sum(type == "For"), # calculate the number of yes-votes for each voting 
            no_n = sum(type == "Imod"), # calculate the number of no-votes for each voting
            neither_n = sum(type == "Hverken for eller imod"),  # calculate the number of neither-votes for each voting
            absent_n = sum(type == "Fravær"),  # calculate the number of absent MPs for each voting
            dato = as.Date(first(dato)), # gets the date for each voting
            beskrivelse = first(afstemningsbeskrivelse), # gets the description for each voting
            periode = first(periodeid)) # gets the period id for each voting

# =====================================================================================================

####################################################
######## 5) CORRECTING MISTAKES IN THE API #########
####################################################

# creating a new variable with the true voting data
#period <= 31 --> FALSE
#period > 31 --> TRUE
df_correct_var = df %>% 
  filter(periodeid < 32) %>% # selecting all obs with mistakes
  mutate(type = as.factor(ifelse(type == "For", "For", # yes-votes are correct
                                 ifelse(type == "Imod", "Imod", # no-votes are correct
                                        ifelse(type == "Fravær", "Hverken for eller imod", "Fravær"))))) %>% #substituting absent and neither-votes
  bind_rows(filter(df, periodeid > 31)) # add with new obs that is correct

# Creating the final data frame with the corrected variables
df_corrected = df_correct_var %>% 
  group_by(afstemningid) %>% 
  summarise(yes_n = sum(type == "For"), # calculate the number of yes-votes for each voting 
            no_n = sum(type == "Imod"), # calculate the number of no-votes for each voting
            neither_n = sum(type == "Hverken for eller imod"),  # calculate the number of neither-votes for each voting
            absent_n = sum(type == "Fravær"),  # calculate the number of absent MPs for each voting
            dato = as.Date(first(dato)), # gets the date for each voting
            beskrivelse = first(afstemningsbeskrivelse), # gets the description for each voting
            periode = first(periodeid),
            gd = graph_density_calculator(df_correct_var, afstemningid), # calculating graph density for each voting
            mean_degree = mean_degree_calculator(df_correct_var, afstemningid), # calculating mean degree for each voting
            modularity = modularity_calculator(df_correct_var,afstemningid), # calculating modularity for each voting
            gd_no_neither = graph_density_calculator_no_neither(df_correct_var, afstemningid), # calculating graph density for each voting excluding neithers
            mean_degree_no_neither = mean_degree_calculator_no_neither(df_correct_var, afstemningid), # calculating mean degree for each voting excluding neithers
            modularity_no_neither = modularity_calculator_no_neither(df_correct_var,afstemningid),# calculating modularity for each voting excluding neithers
            dag = first(dato), # variable showing the date of earch voting
            periode = first(periodeid), # variable describing the type of the vote
            beskrivelse = first(afstemningsbeskrivelse)) # variable describing the type of the vote



# =====================================================================================================

#################################
######## 6) DATA SAVING #########
#################################

# Setting wd
setwd("C:/Users/BjørnAugust/OneDrive for Business/Dokumenter/Studie/11. semester/Social Network Analysis/scripts")
# Saving dfs as csv
write.table(df_polarization, "voting_polarization.csv", sep=",")
write.table(df_prop, "voting_votetypes.csv", sep=",")
write.table(df_corrected, "voting_data_corrected.csv", sep=",")

# Clearing enviroment
#rm(list=ls())
