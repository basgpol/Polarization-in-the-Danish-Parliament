#################################################################
########################## SNA PROJECT ##########################
########### Voting behaviour in the Danish Parliament ###########
########################## DATA IMPORT ##########################
#################################################################

################
### Contents ###
################

# 1) Loading packages
# 2) Creating scraper-function
# 3) Importing data
# 3) Data manipulation
# 5) Data saving

# =====================================================================================================


###########################
### 1) Loading packages ###
###########################

library(httr)
library(ggplot2)
library(dplyr)
library(lubridate)


####################################
### 2) Creating scraper function ###
####################################

collect_ft_data <- function(url_basic, parameter_vector, max_obs, filter_equal=NULL, filter_contain=NULL){
  ### The following function collects data from the API of the Danish Parliament
  ### The function needs three inputs and returns a dataframe:
  ###     1) A URL provided by the API (http://oda.ft.dk/) with the data of interest (e.g.www.http://oda.ft.dk/api/Afstemning?$inlinecount=allpages).
  ###     2) A character vetor with the names of parameters you are interested in (e.g. c("id", "vedtage", "typeid")). The maximum number of variables is 15.
  ###     3) The number of observations you want to collect. Default is all observations.
  ###     4) A FILTER FOR THE SEARCH, CONTAINS VECTOR WITH c("attribute", "search.term")
  
  #Making the filter into url, if one of them is specified
  if (length(filter_equal)>1){
    chr.filter <- c("&$filter=", URLencode(filter_equal[1], reserved=T),"%20eq%20",URLencode(filter_equal[2], reserved=T))
  } else if (length(filter_contain)>1){
    chr.filter <- c("&$filter=substringof('", URLencode(filter_contain[2], reserved=T),"',", URLencode(filter_contain[1], reserved=T),")")
  } else {
    chr.filter <- NULL
  }
  
  #Make the new filter URL
  filter_url <- paste(chr.filter, collapse="")
  #combine with entire URL
  full_url <- paste0(url_basic, filter_url, collapse="")
  
  # Making a GET-request
  data_page <- GET(full_url)
  data_content <- content(data_page)
  num_obs <- data_content$odata.count
  print(paste0("The dataset has ", num_obs, " observations"))
  
  # Creating empty dataframe
  df <- data.frame(NA) #Make dataset full of NA
  for (i in parameter_vector){df[,i] <- NA} #Make a collumn for each parameter
  df <- df[-1] #Removes first variable from empty df
  
  # Setting the number of data observations that the function collects 
  if(missing(max_obs)) {
    obs_collect <- num_obs #Default is all observation
  } else {
    obs_collect <- max_obs #Else the specified amount
  }
  
  # Collecting data with the selected variables and number of observations
  seqnr <- seq(from=0, to=obs_collect - 20, by=20) #How many searches must be made. Each page returns 20 obs
  
  for(i in seqnr) {
    link <- paste0(full_url,"&$skip=",i) #Skip = antal obs already taken
    data.temp <- GET(link)
    data.temp.content <- content(data.temp)
    values = data.temp.content$value #Take only the values-element of the content-list
    
    for(j in 1:20) { #For every observation of the current page
      df[i + j,] <- c(values[[j]][[parameter_vector[1]]], #Take this entire vector, with all the parameter values, and store it as the first obs in the data
                      values[[j]][[parameter_vector[2]]],
                      values[[j]][[parameter_vector[3]]],
                      values[[j]][[parameter_vector[4]]],
                      values[[j]][[parameter_vector[5]]],
                      values[[j]][[parameter_vector[6]]],
                      values[[j]][[parameter_vector[7]]],
                      values[[j]][[parameter_vector[8]]],
                      values[[j]][[parameter_vector[9]]],
                      values[[j]][[parameter_vector[10]]],
                      values[[j]][[parameter_vector[11]]],
                      values[[j]][[parameter_vector[12]]],
                      values[[j]][[parameter_vector[13]]],
                      values[[j]][[parameter_vector[14]]],
                      values[[j]][[parameter_vector[15]]])
    }}
  return(df)
}

#### Testing the function ####
test_df = collect_ft_data("http://oda.ft.dk/api/Stemme?$inlinecount=allpages",c("afstemningid", "id", "opdateringsdato"), max_obs = 200)
collect_ft_data("http://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages",c("id", "navn", "typeid"), max_obs = 100, filter_equal = c("typeid", "5"))

# =====================================================================================================

#################################
####### 3) IMPORTING DATA #######
#################################

### Collecting data on members of parliament
members_df = collect_ft_data("http://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages", # url collected from 
                             c("id", "navn", "fornavn", "efternavn","typeid"), # variables of interest
                             max_obs = 2680, filter_equal = c("typeid", "5")) # only selecting persons

head(members_df)

members_df = members_df %>% 
  rename(aktorid = id) %>%  ## renaming variable name from "id" to "aktorid"
  mutate(aktorid = as.integer(aktorid)) #chaning class to integrer


### loading voting data scraped using Python
voting_data_df = read.csv("df_voting_data.csv", header = TRUE)
  
  
  #voting_data_df_200000 = collect_ft_data("http://oda.ft.dk/api/Stemme?$inlinecount=allpages",
                                 #c("id", "typeid", "afstemningid", "akt\u00f8rid"),
                                 #max_obs = 200000)

#tail(voting_data_df_100000)

### Colecting data on the meaning of voting type (for each vote)
voting_type_df = data.frame(typeid = c(1,2,3,4), 
                            type = c("For", "Imod", "Fravær", "Hverken for eller imod"))

### Colecting data on the meaning of voting type (for the entire debate)
voting_type_entire_df = data.frame(afstemningstype = c(1,2,3,4), 
                            afstemningsbeskrivelse = c("Endelig vedtagelse", 
                                     "Udvalgsindstilling", 
                                     "Forslag til vedtagelse", 
                                     "Ændringsforslag"))

### Collecting on parliamentary meeting dates
# collecting all voting id's and meetingids
voting_id_df = collect_ft_data("http://oda.ft.dk/api/Afstemning?$inlinecount=allpages", # url collected from 
                                c("id", "vedtaget", "m\u00f8deid","typeid"), # variables of interest
                                max_obs = 6640)

voting_id_df = voting_id_df %>% 
  rename(afstemningid ="id") %>%  # renaming variable
  rename(afstemningstype = "typeid")   # renaming variable

# collecting dates related to meeting id
meeting_id_df = collect_ft_data("http://oda.ft.dk/api/M%C3%B8de?$inlinecount=allpages", # url collected from 
                                c("id", "titel", "dato","periodeid"), # variables of interest
                                max_obs = 6860)

meeting_id_df = meeting_id_df %>% 
  rename(mødeid ="id") %>%   # renaming variable
  rename(mødetitel = "titel") %>%    # renaming variable
  mutate(mødeid = as.integer(mødeid)) #chaning class to integrer

##############################
######## MERGING DATA ########
##############################

df = left_join(voting_data_df, voting_type_df, by="typeid") #merging voting data with voting labes

df = df %>% #renaming aktørid to aktorid
  rename(aktorid = "aktørid")

df = left_join(df, members_df, by="aktorid") # merging votingdata with names for politicians
df = left_join(df, voting_id_df, by="afstemningid" ) #merging on meeting id
df = left_join(df, meeting_id_df, by="mødeid" ) #merging on meeting dates and period id
df = left_join(df, voting_type_entire_df, by="afstemningstype") # merging on voting descriptions

# =====================================================================================================


#################################
##### 4) DATA MANIPULATION ######
#################################

# selcting only relevant variables
df_selected = df %>% 
  select(id, afstemningid, aktorid, type, navn, vedtaget, "mødeid", dato, periodeid, afstemningsbeskrivelse)

# formating variables
df_selected = df_selected %>% 
  mutate(dato = as.Date(df_selected$dato,format = "%Y-%m-%d")) %>% # turning variable into date format
  mutate(periodeid = as.integer(periodeid)) %>% # turning variable into integer
  mutate(year = year(dato)) %>% 
  mutate(month = month(dato)) %>% 
  mutate(day = day(dato))

# =====================================================================================================

##############################
######## DATA SAVING #########
##############################

# Setting wd
setwd("C:/Users/BjørnAugust/OneDrive for Business/Dokumenter/Studie/11. semester/Social Network Analysis/scripts")
# Saving df as csv
write.table(df_selected, "voting_data.csv", sep=",")

# Clearing enviroment
rm(list=ls())

