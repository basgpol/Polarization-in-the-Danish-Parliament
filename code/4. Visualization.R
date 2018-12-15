#################################################################
########################## SNA PROJECT ##########################
########### Voting behaviour in the Danish Parliament ###########
####################### DATA VISUALIZATION ######################
#################################################################

################
### Contents ###
################

# 1) Loading packages
# 2) Loading data
# 3) Data manipulation
# 4) Visualization: Plotting voting networks
# 5) Visualization: Plotting polarization


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
library(scales)

# =====================================================================================================

###########################
##### 2) LOADING DATA #####
###########################

#Setting working directory
setwd("C:/Users/BjørnAugust/OneDrive for Business/Dokumenter/Studie/11. semester/Social Network Analysis/scripts")

#loading datasets from github
df_voting_types = read.csv(url("https://raw.githubusercontent.com/basgpol/Polarization-in-the-Danish-Parliament/master/voting_votetypes.csv"), header = TRUE)
df_polarization = read.csv(url("https://raw.githubusercontent.com/basgpol/Polarization-in-the-Danish-Parliament/master/voting_polarization.csv"), header = TRUE)
df_corrected = read.csv(url("https://raw.githubusercontent.com/basgpol/Polarization-in-the-Danish-Parliament/master/voting_data_corrected.csv"), header = TRUE)

#loading datasets from server on personal computer (fileto large to upload on github)
df = read.csv("voting_data.csv", header = TRUE)


# =====================================================================================================

################################
##### 3) DATA MANIPULATION #####
################################

df_merge = left_join(df_polarization, 
                    select(df_voting_types, afstemningid, yes_n, no_n, neither_n, absent_n),
                    by = "afstemningid") 

# Changing format for date-variable and save as new dfs          
df_final = df_merge %>% 
  mutate(dato = as_date(dato))

df_correct = df_corrected %>% 
  mutate(dato = as_date(dato))

# Creating list with election dates
election_dates= c(as.Date("2015-06-18"), as.Date("2011-09-15"), as.Date("2007-11-13"), as.Date("2005-02-08"))


# =====================================================================================================

#####################################################
##### 4) VISUALIZATION: PLOTING VOTING NETWORKS #####
#####################################################

### Creating plot of single vote ###
df_single1 = filter(df, periodeid == 30)
id_single = first(df_single1$afstemningid)

df_single = df %>% # creating edge liste
  filter(afstemningid == id_single) %>% #removs all other observations
  select(navn, type) %>% #keep only variables of actorid and votetype
  filter(type != "Fravær") # removing all the members that didn't vote
g_single = graph.edgelist(as.matrix(df_single), directed = F) # formating as a igraph object
V(g_single)$type = bipartite.mapping(g_single)$type
one_mode_single = bipartite_projection(g_single)$proj1 # turn into one-mode network

# Creating single vote plot
plot_single = plot(one_mode_single,
                   main = "Network for single vote",
                   vertex.size = 5,
                   #vertex.label.color = NA,
                   vertex.label = NA,
                   vertex.label.cex = 1,
                   vertex.color = "grey",
                   ylim = c(-1, 1),
                   edge.lty = "dashed",
                   edge.arrow.size= 0.1,
                   edge.arrow.width= 0.1,
                   edge.color= "light grey",
                   width = 0.01)# adjust focus)

### Creating plot of a parliamentary year ###

# Creating a df with votes from a single period
df_period = df %>% 
  filter(periodeid == 28) %>% 
  mutate(voting_vote = paste(as.character(afstemningid), as.character(type), sep = ":")) %>%
  filter(type != "Fravær") %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>% 
  select(navn, voting_vote) 

# Plotting Period Voting Network
g_period = graph.data.frame(as.matrix(df_period), directed = F) # formating as a igraph object
V(g_period)$type = bipartite.mapping(g_period)$type
one_mode_period = bipartite_projection(g_period)$proj1

plot_period = plot.igraph(one_mode_period,
                          main = "Network for parliament year",
                          vertex.size = 5,
                          #vertex.label.color = NA,
                          vertex.label = NA,
                          vertex.label.cex = 1,
                          vertex.color = "grey",
                          ylim = c(-1, 1),
                          edge.lty = "dashed",
                          edge.arrow.size= 0.1,
                          edge.arrow.width= 0.1,
                          edge.color= "light grey",
                          width = 0.01)# adjust focus)

# =====================================================================================================

##################################################
##### 5) VISUALIZATION: PLOTING POLARIZATION #####
##################################################

##### DATA VISUALIZATION: MEAN DEGREE #####

# Visualizing overall trend in mean degree
overall_trend_endelig_vedtagelse_MD = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>% 
  ggplot(aes(x = dato, y = mean_degree)) +
  geom_point() + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Mean Degree")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5)) + # center plot title
  ggtitle("Overall time trend of mean degree in Parliamentary votes") # name title

overall_trend_endelig_vedtagelse_MD

# Visualizing overall trend in mean degree as rolling average
overall_trend_endelig_vedtagelse_rolling_MD = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>% 
  arrange(dato) %>% 
  mutate(rolling_mean_degree = rollmean(mean_degree, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_degree)) +
  geom_point() + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Mean Degree")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5)) + # center plot title
  ggtitle("Overall time trend of mean degree (as rolling average) in Parliamentary votes") # name title
overall_trend_endelig_vedtagelse_rolling_MD

# Visualizing overall trend in mean degree as rolling average and period id
overall_trend_endelig_vedtagelse_rolling_period_id_MD = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>% 
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(rolling_mean_degree = rollmean(mean_degree, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_degree)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Mean Degree")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # center plot title
  labs(title = "Development in mean degree in Parliamentary votes", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
overall_trend_endelig_vedtagelse_rolling_period_id_MD

# Histogram of average mean degree for each parliament period
hist_gd_period_endelig_vedtagelse_MD = df_final %>% 
  group_by(periode) %>% # grouping by period
  summarise(MD_mean = mean(mean_degree)) %>%  # calculate mean graph density for each period
  mutate(periode_id = as.factor(periode)) %>%  # change formant from integer to factor
  ggplot(aes(x=periode_id, y=MD_mean)) +
  geom_bar(stat = "identity", fill = "blue", na.rm = T, alpha = 0.5 ) +
  theme_minimal() + # set theme
  xlab("Parliament period")+ # name label for x-axis
  ylab("Average mean degree for the parliament period")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5)) + # center plot title
  ggtitle("Average Graph Density for each parliament period") # name title
hist_gd_period_endelig_vedtagelse_MD


##### DATA VISUALIZATION: GRAPH DENSITY #####

# Visualizing overall trend in graph density
overall_trend_endelig_vedtagelse = df_final %>% 
  ggplot(aes(x = dato, y = gd)) + # sæt jeres variable navne her
  geom_point() + # create a plot
  geom_smooth() + # create a smooth line
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Graph Density")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5)) + # center plot title
  ggtitle("Overall time trend of graph density in Parliamentary votes") # name title
overall_trend_endelig_vedtagelse

# Visualizing overall trend in graph density
overall_trend_endelig_vedtagelse = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>% 
  ggplot(aes(x = dato, y = gd)) +
  geom_point() + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Graph Density")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5)) + # center plot title
  ggtitle("Overall time trend of graph density in Parliamentary votes") # name title
overall_trend_endelig_vedtagelse

# Visualizing overall trend in graph density as rolling average
overall_trend_endelig_vedtagelse_rolling = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>% 
  arrange(dato) %>% 
  mutate(rolling_mean_gd = rollmean(gd, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_gd)) +
  geom_point() + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Graph Density")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5)) + # center plot title
  ggtitle("Overall time trend of graph density (as rolling mean) in Parliamentary votes") # name title
overall_trend_endelig_vedtagelse_rolling

# Visualizing overall trend in graph density as rolling average and period id
overall_trend_endelig_vedtagelse_rolling_period_id = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>% 
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(rolling_mean_gd = rollmean(gd, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_gd)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[2]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[3]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[4]), colour="black", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ # formatting x-axis
  scale_y_continuous(breaks = seq(0.3,1,0.1)) +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Graph Density")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # center plot title
  labs(title = "Development in graph density in Parliamentary votes", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
overall_trend_endelig_vedtagelse_rolling_period_id

# Visualizing overall trend in graph density as rolling average and period id for votes where all MP's attended
overall_trend_endelig_vedtagelse_rolling_period_id_all = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>%
  filter(absent_n <= 0) %>% 
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(rolling_mean_gd = rollmean(gd, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_gd)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ # formatting x-axis
  scale_y_continuous(breaks = seq(0.3,0.6,0.05)) +
  ylab("Graph Density")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # center plot title
  labs(title = "Development in graph density in Parliamentary votes", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
overall_trend_endelig_vedtagelse_rolling_period_id_all

# Visualizing overall trend in graph density since 2015 as rolling average and period id for votes where all MP's attended
overall_trend_endelig_vedtagelse_rolling_period_id_all_2015 = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>%
  filter(dato >= "2015-01-01") %>% 
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(rolling_mean_gd = rollmean(gd, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_gd)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Graph Density")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # center plot title
  labs(title = "Development in graph density in Parliamentary votes", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
overall_trend_endelig_vedtagelse_rolling_period_id_all_2015

# Histogram of average graph density for head parliament period
hist_gd_period_endelig_vedtagelse = df_final %>% 
  group_by(periode) %>% # grouping by period
  summarise(gd_mean = mean(gd)) %>%  # calculate mean graph density for each period
  mutate(periode_id = as.factor(periode)) %>%  # change formant from integer to factor
  ggplot(aes(x=periode_id, y=gd_mean)) +
  geom_bar(stat = "identity", fill = "blue", na.rm = T, alpha = 0.5 ) +
  theme_minimal() + # set theme
  xlab("Parliament period")+ # name label for x-axis
  ylab("Average Graph Density")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5)) + # center plot title
  ggtitle("Average Graph Density for each parliament period") # name title
hist_gd_period_endelig_vedtagelse




##### DATA VISUALIZATION: CHANGES IN THE VOTING TYPES #####

# Visualizing the proportion of neither votes
overall_trend_neither = df_final %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(neither_prop = neither_n / (179-absent_n)) %>% 
  mutate(rolling_mean_neither_prop = rollmean(neither_prop, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_neither_prop)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Proportion of neither votes")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # center plot title
  labs(title = "Development in neither votes in Parliamentary votes", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
overall_trend_neither

# Visualizing the proportion of neither votes
overall_trend_neither = df_final %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(neither_prop = neither_n / (179-absent_n)) %>% 
  mutate(rolling_mean_neither_prop = rollmean(neither_prop, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_neither_prop)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Proportion of neither votes")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # center plot title
  labs(title = "Development in neither votes in Parliamentary votes", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
overall_trend_neither

# Visualizing the proportion of neither votes
overall_trend_neither = df_final %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  ggplot(aes(x = dato, y = neither_n)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Sum of neither votes")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # center plot title
  labs(title = "Development in neither votes in Parliamentary votes", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
overall_trend_neither

# Visualizing the number of politicians attending each vote
overall_trend_attending = df_final %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  #mutate(rolling_mean_neither_prop = rollmean(neither_prop, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = 179 - absent_n)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("MPs attending vote")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # center plot title
  labs(title = "Development in MP attending parliamentary votes", # give title
       caption = "[Dots are rolling means] and colours indicate different parliamentary years") # give note
overall_trend_attending

# Plotting developments in votes given
trend_vote_types = df_final %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  arrange(dato) %>% 
  ggplot(aes(x = dato)) +
  geom_smooth(aes(y = yes_n, colour = "Yes votes"), se = F, linetype = 2) +
  geom_smooth(aes(y = no_n, colour = "No votes"), se = F, linetype = 5) +
  geom_smooth(aes(y = neither_n, colour = "Neither votes"), se = F, linetype = 1) +
  geom_smooth(aes(y = absent_n, colour = "Absent MPs"), se = F, linetype = 4) +
  geom_vline(aes(xintercept = election_dates[1]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[2]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[3]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[4]), colour="black", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ # formatting x-axis
  scale_y_continuous(breaks = seq(0,100,20)) +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Number of different votes")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Development of vote types ", # give title
       caption = "Dashed lines indicate parliamentary elections", # inserting note
       colour = "Vote types") # change legend title

trend_vote_types 


df_test = filter(df_final, dato > "2014-06-01" & dato < "2015-01-01")

##### DATA VISUALIZATION: MODULARITY #####

# Visualizing overall trend in modularity
overall_trend_endelig_vedtagelse_modularity = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>% 
  ggplot(aes(x = dato, y = modularity)) +
  geom_point() + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="red") +
  geom_vline(aes(xintercept = election_dates[2]), colour="red") +
  geom_vline(aes(xintercept = election_dates[3]), colour="red") +
  geom_vline(aes(xintercept = election_dates[4]), colour="red") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Modularity")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5)) + # center plot title
  ggtitle("Overall time trend of modularity in Parliamentary votes") # name title
overall_trend_endelig_vedtagelse_modularity

# Visualizing overall trend in modularity as rolling average and period id
overall_trend_endelig_vedtagelse_rolling_period_id_modularity = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>% 
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(rolling_mean_modularity = rollmean(modularity, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_modularity)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[2]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[3]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[4]), colour="black", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ # formatting x-axis
  scale_y_continuous(breaks = seq(0,0.7,0.1)) +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Modularity")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5), # center plot title
        legend.position = "none") +   # remobel legend
  labs(title = "Development in modularity in Parliamentary votes", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
overall_trend_endelig_vedtagelse_rolling_period_id_modularity

# Visualizing overall trend in graph density as rolling average and period id
overall_trend_endelig_vedtagelse_rolling_period_id_modularity = df_final %>% 
  filter(afstemningsbeskrivelse == "Endelig vedtagelse") %>%
  filter(absent_n <= 0) %>% 
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(rolling_mean_modularity = rollmean(modularity, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_modularity)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[2]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[3]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[4]), colour="black", linetype = "dashed") +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Modularity")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5), # center plot title
        legend.position = "none") +   # remobel legend
  labs(title = "Development in modularity in Parliamentary votes", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
overall_trend_endelig_vedtagelse_rolling_period_id_modularity

##### DATA VISUALIZATION: WITH CORRECTED DATA #####
# Plotting developments in votes given
trend_vote_types_correct = df_correct %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  filter(gd >= 0.5 ) %>% # removing left-overs from  wrong registration 
  arrange(dato) %>% 
  ggplot(aes(x = dato)) +
  geom_smooth(aes(y = yes_n, colour = "Yes votes"), se = F, linetype = 2) +
  geom_smooth(aes(y = no_n, colour = "No votes"), se = F, linetype = 5) +
  geom_smooth(aes(y = neither_n, colour = "Neither votes"), se = F, linetype = 1) +
  geom_smooth(aes(y = absent_n, colour = "Absent MPs"), se = F, linetype = 4) +
  geom_vline(aes(xintercept = election_dates[1]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[2]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[3]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[4]), colour="black", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ # formatting x-axis
  scale_y_continuous(breaks = seq(0,100,20)) +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Number of different votes")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Development of vote types (corrected data)", # give title
       caption = "Dashed lines indicate parliamentary elections", # inserting note
       colour = "Vote types") # change legend title
trend_vote_types_correct 

# Visualizing overall trend in graph density as rolling average and period id
trend_graph_density_correct = df_correct %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  filter(gd >= 0.5 ) %>% # removing left-overs from  wrong registration 
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(rolling_mean_gd = rollmean(gd, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_gd)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[2]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[3]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[4]), colour="black", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ # formatting x-axis
  scale_y_continuous(breaks = seq(0.3,1,0.1)) +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Graph Density")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # center plot title
  labs(title = "Development in graph density in Parliamentary votes (corrected data)", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
trend_graph_density_correct

# Visualizing overall trend in modularity as rolling average and period id
trend_modularity_correct = df_correct %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  filter(gd >= 0.5 ) %>% # removing left-overs from  wrong registration
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(rolling_mean_modularity = rollmean(modularity, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_modularity)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[2]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[3]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[4]), colour="black", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ # formatting x-axis
  scale_y_continuous(breaks = seq(0,0.7,0.1)) +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Modularity")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5), # center plot title
        legend.position = "none") +   # remobel legend
  labs(title = "Development in modularity in Parliamentary votes (corrected data)", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
trend_modularity_correct

# Scatterplot showing relationship between MPs attending and Graph Density
scatter_attending_gd = df_correct %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  mutate(attending = 179-absent_n) %>% # creating variable with the number of MPs attending
  filter(attending >=90) %>% # removes votes that wasn't able to make a authoritative decision
  ggplot(aes(x = attending, y = gd)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal() + # set theme
  xlab("Number of MP atteding a vote ")+ # name label for x-axis
  ylab("Graph Density")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5), # center plot title
        legend.position = "none") +   # remobel legend
  labs(title = "Relationship between number of MPs and graph density", # give title
       caption = "Dots represent votes. Line illustrate a linear regression. Data is corrected") # give note
scatter_attending_gd

# Scatterplot showing relationship between MPs attending and Graph Density
scatter_attending_modularity = df_correct %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  mutate(attending = 179-absent_n) %>% # creating variable with the number of MPs attending
  filter(attending >=90) %>% # removes votes that wasn't able to make a authoritative decision
  ggplot(aes(x = attending, y = modularity)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal() + # set theme
  xlab("Number of MP atteding a vote ")+ # name label for x-axis
  ylab("Modularity")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5), # center plot title
        legend.position = "none") +   # remobel legend
  labs(title = "Relationship between number of MPs and modularity", # give title
       caption = "Dots represent votes. Line illustrate a linear regression. Data is corrected") # give note
scatter_attending_modularity

# Visualizing overall trend in graph density as rolling average and period id
trend_graph_density_correct_105_115 = df_correct %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  filter(gd >= 0.5 ) %>% # removing left-overs from  wrong registration
  mutate(attending = 179-absent_n) %>% # creating variable with the number of MPs attending
  filter(attending >= 105 & attending >= 115) %>%  # only looking a votes with 105-115 MPs attending
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(rolling_mean_gd = rollmean(gd, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_gd)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[2]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[3]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[4]), colour="black", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ # formatting x-axis
  scale_y_continuous(breaks = seq(0.3,1,0.1)) +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Graph Density")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") + # center plot title
  labs(title = "Development in graph density in Parliamentary votes (corrected data)", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
trend_graph_density_correct_105_115

# Visualizing overall trend in modularity as rolling average and period id
trend_modularity_correct_105_115 = df_correct %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  filter(gd >= 0.5 ) %>% # removing left-overs from  wrong registration
  mutate(attending = 179-absent_n) %>% # creating variable with the number of MPs attending
  filter(attending >= 105 & attending >= 115) %>%  # only looking a votes with 105-115 MPs attending
  arrange(dato) %>% 
  mutate(periodeid = as.factor(periode)) %>% 
  mutate(rolling_mean_modularity = rollmean(modularity, k = 5, na.pad = TRUE)) %>% 
  ggplot(aes(x = dato, y = rolling_mean_modularity)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[2]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[3]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[4]), colour="black", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ # formatting x-axis
  scale_y_continuous(breaks = seq(0,0.7,0.1)) +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Modularity")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5), # center plot title
        legend.position = "none") +   # remobel legend
  labs(title = "Development in modularity in Parliamentary votes (corrected data)", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
trend_modularity_correct_105_115

# Scatter plot of attending over time
trend_attending = df_correct %>% 
  filter(beskrivelse == "Endelig vedtagelse") %>%
  filter(gd >= 0.5 ) %>% # removing left-overs from  wrong registration
  mutate(attending = 179-absent_n) %>% # creating variable with the number of MPs attending
  arrange(dato) %>%
  mutate(periodeid = as.factor(periode)) %>% 
  ggplot(aes(x = dato, y = attending)) +
  geom_point(aes(colour = periodeid)) + # create a plot
  geom_smooth() + # create a smooth line
  geom_vline(aes(xintercept = election_dates[1]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[2]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[3]), colour="black", linetype = "dashed") +
  geom_vline(aes(xintercept = election_dates[4]), colour="black", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ # formatting x-axis
  #scale_y_continuous(breaks = seq(0,0.7,0.1)) +
  theme_minimal() + # set theme
  xlab("Date")+ # name label for x-axis
  ylab("Number of attending MPs")+ # name label for y-axis
  theme(plot.title = element_text(hjust = 0.5), # center plot title
        legend.position = "none") +   # remobel legend
  labs(title = "Development of attending MPs over time", # give title
       caption = "Dots are rolling means and colours indicate different parliamentary years") # give note
trend_attending
