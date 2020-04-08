## COVID-2019 interactive mapping tool
## Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk), February 2020

## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

#setwd("~/Dropbox/glab/cv/R/nCoV_tracker_USstates_UScouties")


# load required packages ####
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")

#install.packages("sf", type = "source")
#if(!require("sf")) install.packages("/Users/tgraeber/Dropbox/glab/cv/R/local/packages/sf_0.8-1.tar.gz", repos = NULL) #, type="source")

#if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
#if(!require(ggiraph)) install.packages("http://cran.us.r-project.org/bin/macosx/el-capitan/contrib/3.6/ggiraph_0.7.0.tgz")

if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")


if(!require(lubridate)) install.packages("lubridate")
if(!require(RCurl)) install.packages("RCurl")





############################### #
############################### #
############################### #
#######    read data    #######
############################### #
############################### #
############################### #
# update data with automated script
source("jhu_data_full+1.R")

# set mapping colour for each outbreak
covid_col = "#cc4c02"
covid_other_col = "#662506"
sars_col = "#045a8d"
h1n1_col = "#4d004b"
ebola_col = "#016c59"

# import data
cv_cases = read.csv("input_data/coronavirus.csv")
sars_cases = read.csv("input_data/sars.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
ebola_cases = read.csv("input_data/ebola.csv")
h1n1_cases = read.csv("input_data/h1n1.csv")
worldcountry = geojson_read("input_data/countries.geo.json", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")

#LOAD STATE DATA ####

#library(RCurl)
x <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
cv_cases_USstates <- read.csv(text = x)
saveRDS(cv_cases_USstates, "input_data/us-states.rds")
#cv_cases_USstates <- readRDS("input_data/us-states.rds")
write.csv(cv_cases_USstates,'input_data/us-states.csv')

states = read.csv("input_data/USstates.csv")
state_populations_orig = read.csv("input_data/US-state-population_2019.csv")
#length(state_populations_orig$state)

#hash - foo
state_populations <- state_populations_orig$population
names(state_populations) <- state_populations_orig$state
#state_populations["California"]
#state_populations["New York"]



#LOAD COUNTY DATA ####

#library(RCurl)
x <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
cv_cases_UScounties <- read.csv(text = x)

saveRDS(cv_cases_UScounties, "input_data/us-counties.rds")
#cv_cases_UScounties <- readRDS("input_data/us-counties.rds")
write.csv(cv_cases_UScounties,'input_data/us-counties.csv')

cv_cases_UScounties$state.county = paste(cv_cases_UScounties$state, cv_cases_UScounties$county, sep = "-")
cv_cases_UScounties$state.county = sub("Doña Ana", "Dona Ana", cv_cases_UScounties$state.county)

#could be informative to include cases with an unlknown county
#cv_cases_UScounties <- subset(cv_cases_UScounties, cv_cases_UScounties$county!="Unknown")

counties_affected = unique(cv_cases_UScounties$state.county)
#length(counties_affected) #2387 (2004-04-03)
#length(unique(counties_affected)) #2387 (2004-04-03)

county_populations_orig = read.csv("input_data/co-est2019-alldata.csv")
county_state_populations.df <- data.frame("state" = county_populations_orig$STNAME, "county" = county_populations_orig$CTYNAME, "population" = county_populations_orig$POPESTIMATE2019, "county_number" = county_populations_orig$COUNTY, 
                                          "state.county" = paste(county_populations_orig$STNAME, county_populations_orig$CTYNAME, sep = "-"))
county_state_populations.df$state.county = sub(" County$", "", county_state_populations.df$state.county)
county_state_populations.df$state.county = sub(" Parish$", "", county_state_populations.df$state.county)
county_state_populations.df$state.county = sub(" Municipality$", "", county_state_populations.df$state.county)
county_state_populations.df$state.county = sub("Do\xfc\xbe\x8c\x96\x98\xbca Ana", "Dona Ana", county_state_populations.df$state.county)
#county_state_populations.df$state.county[1835]

#county_populations.df <- county_state_populations.df %>% filter(as.character(state) != as.character(county))
#state_populations.df <- county_state_populations.df %>% filter(as.character(state) == as.character(county))

#avoids complications like D.C. being listed as a state and a county
county_populations.df <- county_state_populations.df %>% filter(county_number != 0)
state_populations.df  <- county_state_populations.df %>% filter(county_number == 0)

# add populations for the cities that are included in the cv case reports
#https://worldpopulationreview.com/us-cities/new-york-city-population/
#New York City, New York Population 2020        8,398,748
#https://worldpopulationreview.com/us-cities/kansas-city-population/ (Apr 4 2020)
#  Kansas City, Missouri Population 2020       491,918   
##county_populations["New York-New York City"] = 8398748
##county_populations["Missouri-Kansas City"] = 491918
#county_populations.df[nrow(county_populations.df) + 1,] = 
#  list("New York","New York City","8398748","-2","New York-New York City")
#county_populations.df[nrow(county_populations.df) + 1,] = 
#  list("Missouri","Kansas City","491918","-2","Missouri-Kansas City")
####method above gave error:
#Warning message:
#  In `[<-.factor`(`*tmp*`, iseq, value = "New York City") :
#  invalid factor level, NA generated

df1 = tibble("New York","New York City","8398748","-2","New York-New York City")
df2 = tibble("Missouri","Kansas City","491918","-2","Missouri-Kansas City")
names(df1) <- names(county_populations.df)
names(df2) <- names(county_populations.df)

county_populations.df <- rbind(county_populations.df, df1)
county_populations.df <- rbind(county_populations.df, df2)

         #make per capita for unknowns very low
         #county_populations[as.character(paste(states$state, "Unknown", sep = "-"))] = 1000000000
### make per capita for unknowns based on state populations
#county_populations[as.character(paste(states$state, "Unknown", sep = "-"))] = state_populations[as.character(states$state)]
df = tibble(states$state, "Unknown", state_populations[as.character(states$state)], "-1", as.character(paste(states$state, "Unknown", sep = "-")))
names(df) <- names(county_populations.df)
county_populations.df <- rbind(county_populations.df, df)


#hash - foo
county_populations <- county_populations.df$population
names(county_populations) <- county_populations.df$state.county
counties <- county_populations.df$state.county
#length(county_populations) #3142
#length(counties) #3142
#length(unique(counties) #3142

#county_populations["Los Angeles County"]
#county_populations["Ventura County"]
#county_populations["California-Unknown"] 
#county_populations["New York-New York City"] 



### MAP FUNCTIONS ###
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative cases") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new cases") + theme_bw() + 
    scale_fill_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# test function
#cumulative_plot(cv_aggregated, current_date)
#new_cases_plot(cv_aggregated, current_date)

# function to plot cumulative sars cases by date
sars_cumulative_plot = function(sars_aggregated, sars_date) {
  plot_df = subset(sars_aggregated, date<=as.Date(sars_date, format="%Y-%m-%d"))
  ggplot(plot_df, aes(x = date, y = cases)) + geom_line(colour = sars_col) + geom_point(size = 1, alpha = 0.8, colour = sars_col) +
    ylab("cumulative cases") + theme_bw() + 
    scale_colour_manual(values=c(sars_col)) + scale_x_date(date_labels = "%b", limits=c(sars_min_date,sars_max_date)) +
    scale_y_continuous(limits=c(0,10000), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 5, 5, 5))
}

# function to plot new cases by date
sars_new_cases_plot = function(sars_aggregated, plot_date) {
  plot_df_new = subset(sars_aggregated, date<=plot_date)
  ggplot(plot_df_new, aes(x = date, y = new)) + 
    geom_bar(position="stack", stat="identity", fill = sars_col) + 
    ylab("new cases") + theme_bw() + ylim(0,2000) + 
    scale_fill_manual(values=c(sars_col)) +
    xlim(c(sars_min_date,sars_max_date)) + scale_x_date(date_labels = "%b", limits=c(sars_min_date,sars_max_date)) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 5, 5, 5))
}

# function to plot new cases by region
country_cases_plot = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, 
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(cv_min_date,current_date+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab("new") + theme_bw() + 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(cv_min_date,current_date+1)) + xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_case100,"\n", region, ": ",outcome))) +
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_death10,"\n", region, ": ",outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death"))  {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(cv_min_date,current_date+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",outcome))) +
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative (log10)") + theme_bw() +
    scale_y_continuous(trans="log10") +
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}



#added fuinctions

# function to plot new cases by state
state_cases_plot = function(cv_cases_USstates, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
  if (start_point=="Date") {
    g = ggplot(cv_cases_USstates, aes(x = date, y = new_outcome, fill = region, #order = region,
                                      text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(cv_min_date_states,current_date_states+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases_USstates = subset(cv_cases_USstates, days_since_case100>0)
    #region = region[1:length(cv_cases_USstates$new_outcome)]
    g = ggplot(cv_cases_USstates, aes(x = days_since_case100, y = new_outcome, fill = region, 
                                      text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases_USstates = subset(cv_cases_USstates, days_since_death10>0)
    g = ggplot(cv_cases_USstates, aes(x = days_since_death10, y = new_outcome, fill = region, 
                                      text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab("new") + theme_bw() + 
    #    scale_y_continuous(trans="log10") +
    scale_fill_manual(values=state_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}



# function to plot new cases by county
county_cases_plot = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, #order = region,
                                      text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(cv_min_date_counties,current_date_counties+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = new_outcome, fill = region, 
                                      text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = new_outcome, fill = region, 
                                      text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab("new") + theme_bw() + 
    #    scale_y_continuous(trans="log10") +
    scale_fill_manual(values=county_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}


# function to line plot new_outcome by region
region_cases_bar_plot = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death"), graph_start_date, log_flag = FALSE, ylabel = "Outcome") {
  
  #to align x-axis labeles and x-axis data
  #something related to: https://stackoverflow.com/questions/42973629/ggplot-date-scales-shifts-one-month-forward
  #another lead could be vjust, but not as likely: #theme(axis.text.x.top = element_text(vjust = 0.5))
 # offset = 7 #may need to be a different number if x-axis ticks are different from 1 week
  offset = 0 #may need to be a different number if x-axis ticks are different from 1 week
  
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date+offset, y = new_outcome, fill = region, #order = region,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(graph_start_date+offset,current_date+1+offset)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset( cv_cases, days_since_case100>0)
    #region = region[1:length( cv_cases$new_outcome)]
    g = ggplot(cv_cases, aes(x = days_since_case100, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset( cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Days since 10th death")
  }
  
  cols = cv_cases$color.hex
  names(cols) <- cv_cases$region
  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab(ylabel) + theme_bw() + 
    scale_fill_manual(values=cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  
  if (log_flag == TRUE || log_flag == "On") {g1 = g1 + scale_y_continuous(trans="log10")}
  
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}


# function to line plot outcome by region
region_cases_line_plot = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death"), graph_start_date, log_flag = FALSE, ylabel = "Outcome") {

    #to align x-axis labeles and x-axis data
  #something related to: https://stackoverflow.com/questions/42973629/ggplot-date-scales-shifts-one-month-forward
  #another lead could be vjust, but not as likely: #theme(axis.text.x.top = element_text(vjust = 0.5))
  offset = 7 #may need to be a different number if x-axis ticks are different from 1 week
  
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date+offset, y = outcome, group = region, fill = region, #order = region,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(graph_start_date+offset,current_date+1+offset)) +
      xlab("Date")
  }
  #print(start_point)
  #print(cv_min_date)
  #print(current_date+1)
    
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset( cv_cases, days_since_case100>0)
    #region = region[1:length( cv_cases$outcome)]
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, fill = region, 
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",outcome)))+
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset( cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, fill = region, 
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",outcome))) +
      xlab("Days since 10th death")
  }
  
  cols = cv_cases$color.hex
  names(cols) <- cv_cases$region
    
  g1 = g +
    #geom_bar(position="stack", stat="identity") + 
    geom_line(aes(color=region)) +
    geom_point(size=0.5, shape=20, aes(color=region)) +
    
    ylab(ylabel) + theme_bw() + 
    scale_fill_manual(values=cols) +
    scale_color_manual(values=cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))

  if (log_flag == TRUE || log_flag == "On") {g1 = g1 + scale_y_continuous(trans="log10")}
  
    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}



# function to render plotly of epidemic comparison depending on selected outcome
comparison_plot = function(epi_comp, comparison) {
  epi_comp$outcome = epi_comp[,comparison] 
  epi_comp = epi_comp[order(epi_comp$outcome),]
  epi_comp$outbreak = factor(epi_comp$outbreak, levels=epi_comp$outbreak)
  
  p1 <- ggplot(epi_comp, aes(x = outbreak, y = outcome, fill=outbreak, text = paste0(outbreak, ": ",outcome))) + geom_bar(alpha = 0.8, stat="identity") +
    ylab("N") + xlab("") + theme_bw() + 
    scale_fill_manual(values=c("2019-COVID"=covid_col, "2003-SARS"=sars_col, "2014-Ebola"=ebola_col,"2009-H1N1 (swine flu)"=h1n1_col)) +
    theme(legend.position = "")
  
  if(comparison == "cfr") { p1 = p1 + ylab("%") }
  if(comparison == "deaths") { p1 = p1 + scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  if(comparison == "cases") { p1 = p1 + scale_y_continuous(trans='log10', limits = c(1,1e8), breaks=c(1,1000,1e6,1e9), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  ggplotly(p1 + coord_flip(), tooltip = c("text")) %>% layout(showlegend = FALSE)
}





### DATA PROCESSING: COVID-19 ####

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 
#update_countries = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)

# add variable for days since 100th case and 10th death
cv_cases$days_since_case100 = cv_cases$days_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$days_since_case100[country_db$cases>=100] = 1:sum(country_db$cases>=100)
  country_db$days_since_death10[country_db$deaths>=10] = 1:sum(country_db$deaths>=10)
  cv_cases$days_since_case100[cv_cases$country==country_name] = country_db$days_since_case100
  cv_cases$days_since_death10[cv_cases$country==country_name] = country_db$days_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset for countries with at least 100 cases
cv_today_100 = subset(cv_today, cases>=100)
cv_today_100 <- cv_today_100[order(-cv_today_100$cases),]

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                recovered, new_recovered, active_cases, 
                                per100k, newper100k, activeper100k,
                                days_since_case100, days_since_death10)), "input_data/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for days since 100th case and 10th death
cv_cases_continent$days_since_case100 = cv_cases_continent$days_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
  continent_name = as.character(unique(cv_cases_continent$continent))[i]
  continent_db = subset(cv_cases_continent, continent==continent_name)
  continent_db$days_since_case100[continent_db$cases>=100] = 1:sum(continent_db$cases>=100)
  continent_db$days_since_death10[continent_db$deaths>=10] = 1:sum(continent_db$deaths>=10)
  cv_cases_continent$days_since_case100[cv_cases_continent$continent==continent_name] = continent_db$days_since_case100
  cv_cases_continent$days_since_death10[cv_cases_continent$continent==continent_name] = continent_db$days_since_death10
}
write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$days_since_case100 = cv_cases_global$days_since_death10 = 1:nrow(cv_cases_global)
write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$id)
if (all(cv_large_countries$alpha3 %in% worldcountry$id)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,1,10,50,100,500)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (active)", "2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
            title = "<small>Active cases per 100,000</small>") #%>%
#fitBounds(0,-25,90,65) # alternative coordinates for closer zoom

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = 0 }
  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names

cv_cases$color.hex = country_cols[as.character(cv_cases$country)]






### DATA PROCESSING: COVID-19 ###
### STATE DATA PROCESSING ####

# extract time stamp from cv_cases
#library(lubridate)
#library(dplyr)
dates <- data.frame(
  loco = c(as.character(cv_cases_USstates$date))
)

minmax <- dates %>% 
  # transform to date format with lubridate
  mutate(loco = ymd(loco)) %>% 
  # find min and max
  summarise(min = min(loco),
            max = max(loco))

update_states <- minmax[1,2]

# check consistency of state names across datasets
if (all(unique(cv_cases_USstates$state) %in% unique(states$state))==FALSE) { print("Error: inconsistent state names")}
#a <- unique(cv_cases_USstates$state)
#write.csv(a, "temp.csv")

# extract dates from cv data
if (any(grepl("/", cv_cases_USstates$date))) { 
  cv_cases_USstates$date = format(as.Date(cv_cases_USstates$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases_USstates$date = as.Date(cv_cases_USstates$date, format="%Y-%m-%d") }
cv_cases_USstates$date = as.Date(cv_cases_USstates$date)
cv_min_date_states = as.Date(min(cv_cases_USstates$date),"%Y-%m-%d")
current_date_states = as.Date(max(cv_cases_USstates$date),"%Y-%m-%d")
#cv_max_date_clean = format(as.POSIXct(current_date_states),"%d %B %Y")

# merge cv data with state data and extract key summary variables
cv_cases_USstates = merge(cv_cases_USstates, states, by = "state")
cv_cases_USstates = cv_cases_USstates[order(cv_cases_USstates$date),]
#cv_cases_USstates$per1k = as.numeric(format(round(cv_cases_USstates$cases/((state_populations[as.character(cv_cases_USstates$state)])/1000),1),nsmall=1))
#moved.down cv_cases_USstates$per1k = as.numeric(cv_cases_USstates$cases * 1000/(state_populations[as.character(cv_cases_USstates$state)]))
#moved.down cv_cases_USstates$deaths_per1k = as.numeric(cv_cases_USstates$deaths * 1000/(state_populations[as.character(cv_cases_USstates$state)]))
#cv_cases_USstates$newper100k = as.numeric(format(round(cv_cases_USstates$new_cases/(state_populations[cv_cases_USstates$state]/100000),1),nsmall=1))
#cv_cases_USstates$activeper100k = as.numeric(format(round(cv_cases_USstates$active_cases/(state_populations[cv_cases_USstates$state]/100000),1),nsmall=1))
cv_cases_USstates$million_pop = as.numeric(state_populations[cv_cases_USstates$state]>1e6)

cv_cases_USstates_orig <- cv_cases_USstates
  
cv_cases_USstates <- cv_cases_USstates_orig
cv_cases_USstates <- cv_cases_USstates %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(new_cases = cases - lag(cases, default = first(cases))) %>%
  ungroup()

cv_cases_USstates <- cv_cases_USstates %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(new_deaths = deaths - lag(deaths, default = first(deaths))) %>%
  ungroup()

#cv_cases_USstates <- cv_cases_USstates %>%
#  group_by(state) %>%
#  arrange(date) %>%
#  mutate(new_per1k = per1k - lag(per1k, default = first(per1k))) %>%
#  ungroup()

#cv_cases_USstates <- cv_cases_USstates %>%
#  group_by(state) %>%
#  arrange(date) %>%
#  mutate(new_deaths_per1k = deaths_per1k - lag(deaths_per1k, default = first(deaths_per1k))) %>%
#  ungroup()

cv_cases_USstates$per1k = as.numeric(cv_cases_USstates$cases * 1000/(state_populations[as.character(cv_cases_USstates$state)]))
cv_cases_USstates$deaths_per1k = as.numeric(cv_cases_USstates$deaths * 1000/(state_populations[as.character(cv_cases_USstates$state)]))

cv_cases_USstates$new_per1k = as.numeric(cv_cases_USstates$new_cases * 1000/(state_populations[as.character(cv_cases_USstates$state)]))
cv_cases_USstates$new_deaths_per1k = as.numeric(cv_cases_USstates$new_deaths * 1000/(state_populations[as.character(cv_cases_USstates$state)]))

cv_cases_USstates_subset <- cv_cases_USstates

#as.numeric(format(round(5.12345, digits=2),nsmall=3))
#as.numeric(5.12345)

# add variable for days since 100th case and 10th death
cv_cases_USstates$days_since_case100 = cv_cases_USstates$days_since_death10 = 0
for (i in 1:length(unique(cv_cases_USstates$state))) {
  state_name = as.character(unique(cv_cases_USstates$state))[i]
  state_db = subset(cv_cases_USstates, state==state_name)
  state_db$days_since_case100[state_db$cases>=100] = 1:sum(state_db$cases>=100)
  state_db$days_since_death10[state_db$deaths>=10] = 1:sum(state_db$deaths>=10)
  cv_cases_USstates$days_since_case100[cv_cases_USstates$state==state_name] = state_db$days_since_case100
  cv_cases_USstates$days_since_death10[cv_cases_USstates$state==state_name] = state_db$days_since_death10
}

# creat variable for today's data
cv_today_USstates = subset(cv_cases_USstates, date==current_date_states) 
current_case_count_USstates = sum(cv_today_USstates$cases)
current_case_count_USstates_NewYork = sum(cv_today_USstates$cases[cv_today_USstates$state=="New York"])
current_case_count_USstates_other = sum(cv_today_USstates$cases[cv_today_USstates$state!="New York"])
current_death_count_USstates = sum(cv_today_USstates$deaths)

# create subset for states with at least N cases
cv_today_USstates_N = subset(cv_today_USstates, cases>=20)
cv_today_USstates_N <- cv_today_USstates_N[order(-cv_today_USstates_N$cases),]

# write current day's data
write.csv(cv_today_USstates %>% select(c(state, date, cases, deaths,
                                per1k,
                                days_since_case100, days_since_death10)), "input_data/coronavirus_today_state.csv")

## aggregate at continent level
#cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

## add variable for days since 100th case and 10th death
#cv_cases_continent$days_since_case100 = cv_cases_continent$days_since_death10 = 0
#cv_cases_continent$continent = cv_cases_continent$continent_level
#for (i in 1:length(unique(cv_cases_continent$continent))) {
#  continent_name = as.character(unique(cv_cases_continent$continent))[i]
#  continent_db = subset(cv_cases_continent, continent==continent_name)
#  continent_db$days_since_case100[continent_db$cases>=100] = 1:sum(continent_db$cases>=100)
#  continent_db$days_since_death10[continent_db$deaths>=10] = 1:sum(continent_db$deaths>=10)
#  cv_cases_continent$days_since_case100[cv_cases_continent$continent==continent_name] = continent_db$days_since_case100
#  cv_cases_continent$days_since_death10[cv_cases_continent$continent==continent_name] = continent_db$days_since_death10
#}
#write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

## aggregate at global level
#cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
#cv_cases_global$days_since_case100 = cv_cases_global$days_since_death10 = 1:nrow(cv_cases_global)
#write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

## select large countries for mapping polygons
#cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$id)
#if (all(cv_large_countries$alpha3 %in% worldcountry$id)==FALSE) { print("Error: inconsistent country names")}
#cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

## create plotting parameters for map
#bins = c(0,1,10,50,100,500)
#cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
#plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]

## creat cv base map 
#basemap = leaflet(plot_map) %>% 
#  addTiles() %>% 
#  addLayersControl(
#    position = "bottomright",
#    overlayGroups = c("2019-COVID (active)", "2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"),
#    options = layersControlOptions(collapsed = FALSE)) %>% 
#  hideGroup(c("2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
#  addProviderTiles(providers$CartoDB.Positron) %>%
#  fitBounds(~-100,-50,~80,80) %>%
#  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
#            title = "<small>Active cases per 100,000</small>") #%>%
##fitBounds(0,-25,90,65) # alternative coordinates for closer zoom

## sum cv case counts by date
#cv_aggregated = aggregate(cv_cases_USstates$cases, by=list(Category=cv_cases_USstates$date), FUN=sum)
#names(cv_aggregated) = c("date", "cases")

## add variable for new cases in last 24 hours
#for (i in 1:nrow(cv_aggregated)) { 
#  if (i==1) { cv_aggregated$new[i] = 0 }
#  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
#}

## add plotting region
#cv_aggregated$region = "Global"
#cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to states to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
cls_names1 = c(as.character(unique(cv_cases_USstates$state))) #, as.character(unique(cv_cases_continent$continent)),"Global")
state_cols1 = cls[1:length(cls_names1)]
names(state_cols1) = cls_names1
state_cols <- state_cols1

cv_cases_USstates$color.hex = state_cols[as.character(cv_cases_USstates$state)]



# ------- order states by current number of cases -------------
cv_cases_USstates_current <- subset(cv_cases_USstates, date == update_states)
case_order_USstates <- order(cv_cases_USstates_current$cases, decreasing = FALSE)

#> order(cv_cases_USstates_current$cases, decreasing = FALSE)
#[1] 37 50 45 55  2 12 36 53 28 29 13 42 49 21 33  8 31 18 17 43  9  4 14 19 25 39 40
#[28] 48 26  1 30 44  3 51 54 27 35 22 46 38 16  6  7 47 11 52 41 20 15 23 10 24  5 32
#[55] 34
#cv_cases_USstates_current$state[5]
#cv_cases_USstates_current$cases[5]

case_order_USstates_string <- str_pad(order(cv_cases_USstates_current$cases, decreasing = FALSE), 2, pad = "0")
#hash - foo
state2order = c(1:length(state_populations_orig$state))
state2order_string <- str_pad(state2order, 2, pad = "0")

names(state2order_string) <- cv_cases_USstates_current$state[case_order_USstates]

#old case_order_USstates[as.character(cv_cases_USstates$state[22])]
#old cv_cases_USstates$state[22]

cv_cases_USstates$state.ordered = paste(state2order_string[as.character(cv_cases_USstates$state)], cv_cases_USstates$state, sep = " ")

state.ordered2state = cv_cases_USstates_current$state
names(state.ordered2state) <- paste(state2order_string[as.character(state.ordered2state)], state.ordered2state, sep = " ")

state2state.ordered = names(state.ordered2state)
names(state2state.ordered) <- cv_cases_USstates_current$state

# assign colours to states to ensure consistency between plots 
#cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)

cls_names2 = c(state2state.ordered[as.character(unique(cv_cases_USstates$state))]) #, as.character(unique(cv_cases_continent$continent)),"Global")
state_cols2 = cls[1:length(cls_names2)]
names(state_cols2) = cls_names2


# ------- order states by current number of cases per capita -------------
#cv_cases_USstates_current <- subset(cv_cases_USstates, date == update_states)
case_order_USstates.per1k <- order(cv_cases_USstates_current$per1k, decreasing = FALSE)

case_order_USstates.per1k_string <- str_pad(order(cv_cases_USstates_current$per1k, decreasing = FALSE), 2, pad = "0")
#hash - foo
state2order.per1k = c(1:55)
state2order.per1k_string <- str_pad(state2order.per1k, 2, pad = "0")

names(state2order.per1k_string) <- cv_cases_USstates_current$state[case_order_USstates.per1k]

cv_cases_USstates$state.per1k.ordered = paste(state2order.per1k_string[as.character(cv_cases_USstates$state)], cv_cases_USstates$state, sep = " ")

state.ordered.per1k2state = cv_cases_USstates_current$state
names(state.ordered.per1k2state) <- paste(state2order.per1k_string[as.character(state.ordered.per1k2state)], state.ordered.per1k2state, sep = " ")

state2state.ordered.per1k = names(state.ordered.per1k2state)
names(state2state.ordered.per1k) <- cv_cases_USstates_current$state

# assign colours to states to ensure consistency between plots 
#cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)

cls_names3 = c(state2state.ordered.per1k[as.character(unique(cv_cases_USstates$state))]) #, as.character(unique(cv_cases_continent$continent)),"Global")
state_cols3 = cls[1:length(cls_names3)]
names(state_cols3) = cls_names3








### DATA PROCESSING: COVID-19 ###
### COUNTY DATA PROCESSING ####

# extract time stamp from cv_cases
#library(lubridate)
#library(dplyr)
dates <- data.frame(
  loco = c(as.character(cv_cases_UScounties$date))
)

minmax <- dates %>% 
  # transform to date format with lubridate
  mutate(loco = ymd(loco)) %>% 
  # find min and max
  summarise(min = min(loco),
            max = max(loco))

update_counties <- minmax[1,2]

# check consistency of county names across datasets

counties_affected = unique(cv_cases_UScounties$state.county)
counties <- unique(county_populations.df$state.county)

overlap = counties_affected[counties_affected %in% counties]
#length(overlap) #2347, 2387 when include "state-unknown" (n=55) and NYC & KC as counties (2020-04-03)

missing = counties_affected[!(counties_affected %in% counties)]
#length(missing) #40, e.g. "New York-New York City" , "Rhode Island-Unknown", 0 after including these
#write.csv(missing, "missing.csv")

overlap2 = counties[counties %in% counties_affected]
#length(overlap) #2347, 2387 when include "state-unknown" (n=55) and NYC & KC as counties (2020-04-03)

missing2 = counties[!(counties %in% counties_affected)]
#length(missing2) #795, 812 after adding "state-unknown" as 55 counties. 
#write.csv(missing2, "missing2.csv")
#length(counties) #3199 when include "state-unknown" (n=55) and NYC & KC as counties (2020-04-03) 
#length(counties_affected) #2387 (2020-04-03)

#if (all(unique(cv_cases_UScounties$state.county) %in% unique(county_populations.df$state.county))==FALSE) { print("Error: inconsistent county names")}
#a <- unique(cv_cases_UScounties$state.county)
#write.csv(a, "temp.csv")

# extract dates from cv data
if (any(grepl("/", cv_cases_UScounties$date))) { 
  cv_cases_UScounties$date = format(as.Date(cv_cases_UScounties$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases_UScounties$date = as.Date(cv_cases_UScounties$date, format="%Y-%m-%d") }
cv_cases_UScounties$date = as.Date(cv_cases_UScounties$date)
cv_min_date_counties = as.Date(min(cv_cases_UScounties$date),"%Y-%m-%d")
current_date_counties = as.Date(max(cv_cases_UScounties$date),"%Y-%m-%d")
#cv_max_date_clean = format(as.POSIXct(current_date_counties),"%d %B %Y")

# merge cv data with county data and extract key summary variables
###cv_cases_UScounties = merge(cv_cases_UScounties, counties_affected, by = "county")
cv_cases_UScounties = cv_cases_UScounties[order(cv_cases_UScounties$date),]
#cv_cases_UScounties$per1k = as.numeric(format(round(cv_cases_UScounties$cases/((county_populations[as.character(cv_cases_UScounties$state.county)])/1000),1),nsmall=1))
#moved.down cv_cases_UScounties$per1k = as.numeric(cv_cases_UScounties$cases * 1000/(as.numeric(county_populations[as.character(cv_cases_UScounties$state.county)])))
#moved.down cv_cases_UScounties$deaths_per1k = as.numeric(cv_cases_UScounties$deaths * 1000/(county_populations[as.character(cv_cases_UScounties$state.county)]))
#cv_cases_UScounties$newper100k = as.numeric(format(round(cv_cases_UScounties$new_cases/(county_populations[cv_cases_UScounties$state.county]/100000),1),nsmall=1))
#cv_cases_UScounties$activeper100k = as.numeric(format(round(cv_cases_UScounties$active_cases/(county_populations[cv_cases_UScounties$state.county]/100000),1),nsmall=1))
cv_cases_UScounties$million_pop = as.numeric(county_populations[cv_cases_UScounties$state.county]>1e6)

cv_cases_UScounties_orig <- cv_cases_UScounties

cv_cases_UScounties <- cv_cases_UScounties_orig
cv_cases_UScounties <- cv_cases_UScounties %>%
  group_by(state.county) %>%
  arrange(date) %>%
  mutate(new_cases = cases - lag(cases, default = first(cases))) %>%
  ungroup()

cv_cases_UScounties <- cv_cases_UScounties %>%
  group_by(state.county) %>%
  arrange(date) %>%
  mutate(new_deaths = deaths - lag(deaths, default = first(deaths))) %>%
  ungroup()

#cv_cases_UScounties <- cv_cases_UScounties %>%
#  group_by(county) %>%
#  arrange(date) %>%
#  mutate(new_per1k = per1k - lag(per1k, default = first(per1k))) %>%
#  ungroup()

#cv_cases_UScounties <- cv_cases_UScounties %>%
#  group_by(county) %>%
#  arrange(date) %>%
#  mutate(new_deaths_per1k = deaths_per1k - lag(deaths_per1k, default = first(deaths_per1k))) %>%
#  ungroup()

cv_cases_UScounties$per1k = as.numeric(cv_cases_UScounties$cases * 1000/(as.numeric(county_populations[as.character(cv_cases_UScounties$state.county)])))
cv_cases_UScounties$deaths_per1k = as.numeric(cv_cases_UScounties$deaths * 1000/(as.numeric(county_populations[as.character(cv_cases_UScounties$state.county)])))
#ETHOW
cv_cases_UScounties$new_per1k = as.numeric(cv_cases_UScounties$new_cases * 1000/(as.numeric(county_populations[as.character(cv_cases_UScounties$state.county)])))
cv_cases_UScounties$new_deaths_per1k = as.numeric(cv_cases_UScounties$new_deaths * 1000/(as.numeric(county_populations[as.character(cv_cases_UScounties$state.county)])))


cv_cases_UScounties_subset <- cv_cases_UScounties


# add variable for days since 100th case and 10th death
cv_cases_UScounties$days_since_case100 = cv_cases_UScounties$days_since_death10 = 0
for (i in 1:length(unique(cv_cases_UScounties$state.county))) {
  state.county_name = as.character(unique(cv_cases_UScounties$state.county))[i]
  state.county_db = subset(cv_cases_UScounties, state.county==state.county_name)
  state.county_db$days_since_case100[state.county_db$cases>=100] = 1:sum(state.county_db$cases>=100)
  state.county_db$days_since_death10[state.county_db$deaths>=10] = 1:sum(state.county_db$deaths>=10)
  cv_cases_UScounties$days_since_case100[cv_cases_UScounties$state.county==state.county_name] = state.county_db$days_since_case100
  cv_cases_UScounties$days_since_death10[cv_cases_UScounties$state.county==state.county_name] = state.county_db$days_since_death10
}

# creat variable for today's data
cv_today_UScounties = subset(cv_cases_UScounties, date==current_date_counties) 
current_case_count_UScounties = sum(cv_today_UScounties$cases)
current_case_count_UScounties_NewYork = sum(cv_today_UScounties$cases[cv_today_UScounties$state.county=="New York-New York City"])
current_case_count_UScounties_other = sum(cv_today_UScounties$cases[cv_today_UScounties$state.county!="New York-New York City"])
current_death_count_UScounties = sum(cv_today_UScounties$deaths)

# create subset for countries with at least N cases
cv_today_UScounties_N = subset(cv_today_UScounties, cases>=20)
cv_today_UScounties_N <- cv_today_UScounties_N[order(-cv_today_UScounties_N$cases),]

# write current day's data
write.csv(cv_today_UScounties %>% select(c(state.county, date, cases, deaths,
                                per1k,
                                days_since_case100, days_since_death10)), "input_data/coronavirus_today_state.county.csv")

## aggregate at continent level
#cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

## add variable for days since 100th case and 10th death
#cv_cases_continent$days_since_case100 = cv_cases_continent$days_since_death10 = 0
#cv_cases_continent$continent = cv_cases_continent$continent_level
#for (i in 1:length(unique(cv_cases_continent$continent))) {
#  continent_name = as.character(unique(cv_cases_continent$continent))[i]
#  continent_db = subset(cv_cases_continent, continent==continent_name)
#  continent_db$days_since_case100[continent_db$cases>=100] = 1:sum(continent_db$cases>=100)
#  continent_db$days_since_death10[continent_db$deaths>=10] = 1:sum(continent_db$deaths>=10)
#  cv_cases_continent$days_since_case100[cv_cases_continent$continent==continent_name] = continent_db$days_since_case100
#  cv_cases_continent$days_since_death10[cv_cases_continent$continent==continent_name] = continent_db$days_since_death10
#}
#write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

## aggregate at global level
#cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
#cv_cases_global$days_since_case100 = cv_cases_global$days_since_death10 = 1:nrow(cv_cases_global)
#write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

## select large countries for mapping polygons
#cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$id)
#if (all(cv_large_countries$alpha3 %in% worldcountry$id)==FALSE) { print("Error: inconsistent country names")}
#cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

## create plotting parameters for map
#bins = c(0,1,10,50,100,500)
#cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
#plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]

## creat cv base map 
#basemap = leaflet(plot_map) %>% 
#  addTiles() %>% 
#  addLayersControl(
#    position = "bottomright",
#    overlayGroups = c("2019-COVID (active)", "2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"),
#    options = layersControlOptions(collapsed = FALSE)) %>% 
#  hideGroup(c("2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
#  addProviderTiles(providers$CartoDB.Positron) %>%
#  fitBounds(~-100,-50,~80,80) %>%
#  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
#            title = "<small>Active cases per 100,000</small>") #%>%
##fitBounds(0,-25,90,65) # alternative coordinates for closer zoom

## sum cv case counts by date
#cv_aggregated = aggregate(cv_cases_UScounties$cases, by=list(Category=cv_cases_UScounties$date), FUN=sum)
#names(cv_aggregated) = c("date", "cases")

## add variable for new cases in last 24 hours
#for (i in 1:nrow(cv_aggregated)) { 
#  if (i==1) { cv_aggregated$new[i] = 0 }
#  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
#}

## add plotting region
#cv_aggregated$region = "Global"
#cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to counties to ensure consistency between plots 
cls_counties = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),40)
cls_names1 = c(as.character(unique(cv_cases_UScounties$state.county))) #, as.character(unique(cv_cases_continent$continent)),"Global")
county_cols1 = cls_counties[1:length(cls_names1)]
names(county_cols1) = cls_names1
county_cols <- county_cols1

cv_cases_UScounties$color.hex = county_cols1[as.character(cv_cases_UScounties$state.county)]

# ------- order counties by current number of cases -------------
cv_cases_UScounties_current <- subset(cv_cases_UScounties, date == update_counties)
case_order_UScounties <- order(cv_cases_UScounties_current$cases, decreasing = FALSE)

#> order(cv_cases_UScounties_current$cases, decreasing = FALSE)
#cv_cases_UScounties_current$state.county[5]
#cv_cases_UScounties_current$cases[5]

case_order_UScounties_string <- str_pad(order(cv_cases_UScounties_current$cases, decreasing = FALSE), 2, pad = "0")
#hash - foo
county2order = c(1:length(counties))
county2order_string <- str_pad(county2order, 4, pad = "0")

names(county2order_string) <- cv_cases_UScounties_current$state.county[case_order_UScounties]

#old case_order_UScounties[as.character(cv_cases_UScounties$state.county[22])]
#old cv_cases_UScounties$state.county[22]

cv_cases_UScounties$state.county.ordered = paste(county2order_string[as.character(cv_cases_UScounties$state.county)], cv_cases_UScounties$state.county, sep = " ")

county.ordered2county = cv_cases_UScounties_current$state.county
names(county.ordered2county) <- paste(county2order_string[as.character(county.ordered2county)], county.ordered2county, sep = " ")

county2county.ordered = names(county.ordered2county)
names(county2county.ordered) <- cv_cases_UScounties_current$state.county
#length(county2county.ordered)

# assign colours to counties to ensure consistency between plots 
#cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
cls_counties = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),40)

cls_names4 = c(county2county.ordered[as.character(unique(cv_cases_UScounties$state.county))]) #, as.character(unique(cv_cases_continent$continent)),"Global")

county_cols4 = cls_counties[1:length(cls_names4)]
names(county_cols4) = cls_names4

#length(cls_counties)
#length(cls_names4)
#length(county_cols4)



# ------- order counties by current number of cases per capita -------------
cv_cases_UScounties_current <- subset(cv_cases_UScounties, as.Date(date) == as.Date(update_counties))
case_order_UScounties.per1k <- order(cv_cases_UScounties_current$per1k, decreasing = FALSE)
case_order_UScounties.per1k.rev <- order(cv_cases_UScounties_current$per1k, decreasing = TRUE)

case_order_UScounties.per1k_string <- str_pad(case_order_UScounties.per1k, 2, pad = "0")
case_order_UScounties.per1k.rev_string <- str_pad(case_order_UScounties.per1k.rev, 2, pad = "0")
#case_order_UScounties.per1k_string <- str_pad(order(cv_cases_UScounties_current$per1k, decreasing = FALSE), 2, pad = "0")
#case_order_UScounties.per1k.rev_string <- str_pad(order(cv_cases_UScounties_current$per1k, decreasing = TRUE), 2, pad = "0")
#hash - foo
county2order.per1k = c(1:length(counties))
county2order.per1k_string <- str_pad(county2order.per1k, 4, pad = "0")
county2order.per1k.rev = c(1:length(counties))
county2order.per1k.rev_string <- str_pad(county2order.per1k.rev, 4, pad = "0")

names(county2order.per1k_string) <- cv_cases_UScounties_current$state.county[case_order_UScounties.per1k]
names(county2order.per1k.rev_string) <- cv_cases_UScounties_current$state.county[case_order_UScounties.per1k.rev]

cv_cases_UScounties$state.county.per1k.ordered = paste(county2order.per1k_string[as.character(cv_cases_UScounties$state.county)], cv_cases_UScounties$state.county, sep = " ")
cv_cases_UScounties$state.county.per1k.rev.ordered = paste(county2order.per1k.rev_string[as.character(cv_cases_UScounties$state.county)], cv_cases_UScounties$state.county, sep = " ")

county.ordered.per1k2county = cv_cases_UScounties_current$state.county
names(county.ordered.per1k2county) <- paste(county2order.per1k_string[as.character(county.ordered.per1k2county)], county.ordered.per1k2county, sep = " ")

county2county.ordered.per1k = names(county.ordered.per1k2county)
names(county2county.ordered.per1k) <- cv_cases_UScounties_current$state.county

county.ordered.per1k.rev2county = cv_cases_UScounties_current$state.county
names(county.ordered.per1k.rev2county) <- paste(county2order.per1k.rev_string[as.character(county.ordered.per1k.rev2county)], county.ordered.per1k.rev2county, sep = " ")

county2county.ordered.per1k.rev = names(county.ordered.per1k.rev2county)
names(county2county.ordered.per1k.rev) <- cv_cases_UScounties_current$state.county

# assign colours to counties to ensure consistency between plots 
#cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
cls_names5 = c(county2county.ordered.per1k[as.character(unique(cv_cases_UScounties$state.county))]) #, as.character(unique(cv_cases_continent$continent)),"Global")
county_cols5 = cls_counties[1:length(cls_names5)]
names(county_cols5) = cls_names5
#county_cols5[1:10]
#county_cols5["2093 California-San Mateo"]





### DATA PROCESSING: SARS ###

# extract dates from sars data
sars_cases$date = as.Date(sars_cases$date, format="%d/%m/%Y")
sars_min_date = min(sars_cases$date)
sars_max_date = max(sars_cases$date)
sars_max_date_clean = format(as.POSIXct(sars_max_date),"%d %B %Y")

# merge sars data with country data and extract key summary variables
sars_cases = merge(sars_cases, countries, by = "country")
sars_cases = sars_cases[order(sars_cases$date),]
sars_cases$per100k = as.numeric(format(round(sars_cases$cases/(sars_cases$population/100000),1),nsmall=1))
sars_final = subset(sars_cases, date==sars_max_date) 
sars_final_case_count = sum(sars_final$cases)

# select polygons for sars base map
sars_large_countries = sars_final %>% filter(country %in% country_geoms$countries_present)
sars_large_countries = sars_large_countries[order(sars_large_countries$alpha3),]
sars_plot_map <- worldcountry[worldcountry$id %in% sars_large_countries$alpha3, ]

# create plotting parameters for sars map
sars_pal <- colorBin("Blues", domain = sars_large_countries$per100k, bins = bins)

# creat sars interactive map (needs to include polygons and circles as slider input not recognised upon initial loading)
sars_basemap = leaflet(sars_plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2003-SARS (cumulative)", "2019-COVID", "2009-H1N1 (swine flu)", "2014-Ebola"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.1, fillColor = ~sars_pal(sars_large_countries$per100k), group = "2003-SARS (cumulative)",
              label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_large_countries$country, sars_large_countries$cases, sars_large_countries$deaths, sars_large_countries$per100k) %>% lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                textsize = "15px", direction = "auto")) %>%
  
  addCircleMarkers(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                   fillOpacity = 0.2, color = sars_col, group = "2003-SARS (cumulative)",
                   label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$per100k) %>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                     textsize = "15px", direction = "auto")) %>%
  
  addCircleMarkers(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5),
                   fillOpacity = 0.2, color = covid_col, group = "2019-COVID",
                   label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", cv_today$country, cv_today$cases, cv_today$deaths, cv_today$recovered, cv_today$per100k) %>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                     textsize = "15px", direction = "auto"))  %>%
  
  addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4),
                   fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                   label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                     textsize = "15px", direction = "auto")) %>%
  
  addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4),
                   fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
                   label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
                     textsize = "15px", direction = "auto")) 

# sum sars case counts by date
sars_aggregated = aggregate(sars_cases$cases, by=list(Category=sars_cases$date), FUN=sum)
names(sars_aggregated) = c("date", "cases")

# add variable for new sars cases in last 7 days
for (i in 1:nrow(sars_aggregated)) { 
  if (i==1) { sars_aggregated$new[i] = NA }
  if (i>1) { 
    sars_aggregated$new[i] = sars_aggregated$cases[i] - sars_aggregated$cases[i-1] 
  }
}
sars_aggregated$new[sars_aggregated$new<0] = 0





### OUTBREAK COMPARISON DATA ###

# load epidemic comparison data
epi_comp = as.data.frame(data.table::fread("input_data/epi_comp.csv"))
epi_comp$outbreak = factor(epi_comp$outbreak, levels = epi_comp$outbreak)
epi_comp$cases[1] = current_case_count
epi_comp$deaths[1] = current_death_count
epi_comp$countries[1] = nrow(subset(cv_today, country!="Diamond Princess Cruise Ship"))
epi_comp$cfr[1] = round(epi_comp$deaths[1]/epi_comp$cases[1]*100,1)
epi_comp$cfr = round(epi_comp$cfr,2)



### SHINY PARAMETERS ###
initial.state.set = c("California")

### SHINY UI ###
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 "COVID-19 tracker", id="nav",
                 
                 tabPanel("COVID-19 mapper",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 20, width = 250, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            
                                            h3(textOutput("reactive_case_count"), align = "right"),
                                            h4(textOutput("reactive_death_count"), align = "right"),
                                            span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                            span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                            h6(textOutput("clean_date_reactive"), align = "right"),
                                            h6(textOutput("reactive_country_count"), align = "right"),
                                            tags$i(h6("Updated once daily. For more regular updates, refer to: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."))),
                                            tags$i(h6("Reported cases are subject to significant variation in testing capacity between countries.")),
                                            plotOutput("epi_curve", height="130px", width="100%"),
                                            plotOutput("cumulative_plot", height="130px", width="100%"),
                                            
                                            sliderInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                        max = as.Date(current_date,"%Y-%m-%d"),
                                                        value = as.Date(current_date),
                                                        timeFormat = "%d %b", 
                                                        animate=animationOptions(interval = 2000, loop = FALSE))
                              ),
                              
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                            tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                              
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                            actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                         onclick = sprintf("window.open('%s')", 
                                                                           "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                              
                              
                          )
                 ),
                 
                 #ethow_orig
                 tabPanel("Region plots",
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              pickerInput("level_select", "Level:",   
                                          choices = c("Global", "Continent", "Country"), 
                                          selected = c("Country"),
                                          multiple = FALSE),
                              
                              pickerInput("region_select", "Country/Region:",   
                                          choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
                                          options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                          selected = cv_today_100$country,
                                          multiple = TRUE), 
                              
                              pickerInput("outcome_select", "Outcome:",   
                                          choices = c("Cases", "Deaths"), 
                                          selected = c("Cases"),
                                          multiple = FALSE),
                              
                              pickerInput("start_date", "Plotting start date:",   
                                          choices = c("Date", "Day of 100th confirmed case", "Day of 10th death"), 
                                          options = list(`actions-box` = TRUE),
                                          selected = "Date",
                                          multiple = FALSE), 
                              "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 100 confirmed cases are included."
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("New", plotlyOutput("country_plot")),
                                tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
                                tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
                              )
                            )
                          )
                 ),
                 
                 #ethow_new
                 tabPanel("US State plots",
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              pickerInput("level_select2", "Level:",   
                                          #choices = c("Global", "Continent", "Country"), 
                                          choices = c("State"), #, "foo"), 
                                          selected = c("State"),
                                          multiple = FALSE),
                              
                              pickerInput("region_select2", "States:",   
                                          choices = as.character(cv_today_USstates_N[order(-cv_today_USstates_N$cases),]$state), 
                                          options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                          selected = cv_today_USstates_N$state,
                                          multiple = TRUE), 
                              
                              pickerInput("outcome_select2", "Outcome:",   
                                          choices = c("Cases", "Deaths", "Cases per capita", "Deaths per capita"), 
                                          selected = c("Cases"),
                                          multiple = FALSE),
                              
                              pickerInput("start_date2", "Type of graph start date:",   
                                          choices = c("Date", "Day of 100th confirmed case", "Day of 10th death"), 
                                          options = list(`actions-box` = TRUE),
                                          selected = "Date",
                                          multiple = FALSE), 
                              
                              pickerInput("log_flag2", "Log scale:",   
                                          choices = c("Off", "On"), 
                                          options = list(`actions-box` = TRUE),
                                          selected = "Off",
                                          multiple = FALSE), 
                              
                              sliderInput("graph_start_date2",
                                          "Graphing start date:",
                                          min = as.Date(cv_min_date,"%Y-%m-%d"),
                                          max = as.Date(current_date,"%Y-%m-%d"),
                                          value=as.Date(cv_min_date),
                                          timeFormat="%Y-%m-%d"),
                              #"Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 100 confirmed cases are included."
                              "Select outcome, regions, and plotting start date from drop-down menues to update plots."
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("New", plotlyOutput("region_cases_bar_plot_state")),
                                tabPanel("Cumulative", plotlyOutput("region_cases_line_plot_state"))
                                #tabPanel("Cumulative (log10)", log_flag = TRUE, plotlyOutput("region_cases_line_plot"))
                              )
                            )
                          )
                 ),

#ethow_new2
tabPanel("US County plots",
         
         sidebarLayout(
           sidebarPanel(
             
             pickerInput("level_select3", "Level:",   
                         #choices = c("Global", "Continent", "Country"), 
                         choices = c("County"), #, "foo"), 
                         selected = c("County"),
                         multiple = FALSE),
             
             pickerInput("level_select3b", "Limit states included:",   
                         choices = as.character(cv_today_USstates_N[order(-cv_today_USstates_N$cases),]$state), 
                         options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                         #selected = cv_today_USstates_N$state,
                         selected = "California",
                         multiple = TRUE), 
             
             pickerInput("region_select3", "Counties:",   
                         #choices = as.character(cv_today_UScounties_N[order(-cv_today_UScounties_N$cases),]$state.county), 
                         choices = subset(as.character(cv_today_UScounties_N[order(-cv_today_UScounties_N$cases),]$state.county), cv_today_UScounties_N$state %in% initial.state.set),
                         options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                         #selected = cv_today_UScounties_N$state.county, 
                         selected = subset(cv_today_UScounties_N$state.county, cv_today_UScounties_N$state %in% initial.state.set),
                         multiple = TRUE), 
             
             pickerInput("outcome_select3", "Outcome:",   
                         choices = c("Cases", "Deaths", "Cases per capita", "Deaths per capita"), 
                         selected = c("Cases"),
                         multiple = FALSE),
             
             pickerInput("start_date3", "Type of graph start date:",   
                         choices = c("Date", "Day of 100th confirmed case", "Day of 10th death"), 
                         options = list(`actions-box` = TRUE),
                         selected = "Date",
                         multiple = FALSE), 
             
             pickerInput("log_flag3", "Log scale:",   
                         choices = c("Off", "On"), 
                         options = list(`actions-box` = TRUE),
                         selected = "Off",
                         multiple = FALSE), 
             
             sliderInput("graph_start_date3",
                         "Graphing start date:",
                         min = as.Date(cv_min_date,"%Y-%m-%d"),
                         max = as.Date(current_date,"%Y-%m-%d"),
                         value=as.Date(cv_min_date),
                         timeFormat="%Y-%m-%d"),
             #"Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 100 confirmed cases are included."
             "Select outcome, regions, and plotting start date from drop-down menues to update plots."
           ),
           
           mainPanel(
             tabsetPanel(
               tabPanel("New", plotlyOutput("region_cases_bar_plot_county")),
               tabPanel("Cumulative", plotlyOutput("region_cases_line_plot_county"))
               #tabPanel("Cumulative (log10)", log_flag = TRUE, plotlyOutput("region_cases_line_plot"))
             )
           )
         )
),



                 
                 
                                  
                 tabPanel("SARS mapper",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("sars_map", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 20, width = 250, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            
                                            h3(textOutput("sars_reactive_case_count"), align = "right"),
                                            h4(textOutput("sars_reactive_death_count"), align = "right"),
                                            h6(textOutput("sars_clean_date_reactive"), align = "right"),
                                            h6(textOutput("sars_reactive_country_count"), align = "right"),
                                            plotOutput("sars_epi_curve", height="130px", width="100%"),
                                            plotOutput("sars_cumulative_plot", height="130px", width="100%"),
                                            span(("The final count appears to decrease as several cases initially classified as SARS were later re-assigned."),align = "left", style = "font-size:80%"),#tags$br(),
                                            span(("Circles show confirmed cases for COVID, SARS, and Ebola, and estimated deaths for H1N1."),align = "left", style = "font-size:80%"),
                                            
                                            sliderTextInput("sars_plot_date",
                                                            label = h5("Select mapping date"),
                                                            choices = format(unique(sars_cases$date), "%d %b %y"),
                                                            selected = format(sars_max_date, "%d %b %y"),
                                                            grid = TRUE,
                                                            animate=animationOptions(interval = 2000, loop = FALSE))
                              ),
                              
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                            tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                              
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                            actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                         onclick = sprintf("window.open('%s')", 
                                                                           "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                          )
                 ),
                 
                 tabPanel("Outbreak comparisons",
                          
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("comparison_metric", h3("Select comparison:"),
                                           c("Cases" = "cases",
                                             "Deaths" = "deaths",
                                             "Countries/regions affected" = "countries",
                                             "Case fatality rate" = "cfr")),
                              textOutput("epi_notes_1"),
                              textOutput("epi_notes_2"),
                              textOutput("epi_notes_3")
                            ),
                            
                            mainPanel(plotlyOutput("comparison_plot"), width = 6)
                          )
                 ),
                 
                 tabPanel("Data",
                          numericInput("maxrows", "Rows to show", 25),
                          verbatimTextOutput("rawtable"),
                          downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                          "Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                                             "Johns Hopkins Center for Systems Science and Engineering.")
                 ),
                 
                 tabPanel("About this site",
                          tags$div(
                            tags$h4("Last update"), 
                            h6(paste0(update)),
                            "This site is updated once daily. At this time of rapid escalation of the COVID-19 pandemic, the following resources offer the latest numbers of known cases:",tags$br(),
                            tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),tags$br(),
                            tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),tags$br(),
                            "The aim of this site is to complement the above resources by providing several interactive features not currently available elsewhere, including the timeline function, 
                            the ability to overlay past outbreaks, and an emphasis on normalised counts (per 100,000 individuals).",tags$br(),
                            tags$br(),tags$h4("Background"), 
                            "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                            These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                            The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                            This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
                            tags$br(),tags$br(),
                            "In isolation, these headlines can be hard to interpret. 
                            How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                            This site is updated daily based on data published by Johns Hopkins University. 
                            By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
                            tags$br(),tags$br(),
                            "An article discussing this site was published in ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
                            "The map was also featured on the BBC World Service program",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                            tags$br(),tags$br(),tags$h4("Code"),
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                            tags$br(),tags$br(),tags$h4("Sources"),
                            tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
                            " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",tags$br(),
                            tags$b("2003-SARS cases: "), tags$a(href="https://www.who.int/csr/sars/country/en/", "WHO situation reports"),tags$br(),
                            tags$b("2009-H1N1 confirmed deaths: "), tags$a(href="https://www.who.int/csr/disease/swineflu/updates/en/", "WHO situation reports"),tags$br(),
                            tags$b("2009-H1N1 projected deaths: "), "Model estimates from ", tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001558", "GLaMOR Project"),tags$br(),
                            tags$b("2009-H1N1 cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                            tags$b("2009-H1N1 case fatality rate: "), "a systematic review by ", tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/24045719", "Wong et al (2009)"), "identified 
                            substantial variation in case fatality rate estimates for the H1N1 pandemic. However, most were in the range of 10 to 100 per 100,000 symptomatic cases (0.01 to 0.1%).
                            The upper limit of this range is used for illustrative purposes in the Outbreak comarisons tab.",tags$br(),
                            tags$b("2014-Ebola cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                            tags$b("Country mapping coordinates: "), tags$a(href="https://gist.github.com/tadast/8827699", "Github"),tags$br(),
                            tags$br(),tags$br(),tags$h4("Authors"),
                            "Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
                            "Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
                            tags$br(),tags$br(),tags$h4("Contact"),
                            "edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
                            tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                          )
                 )
                 
                 
)





### SHINY SERVER ###

server = function(input, output, session) {
  
  # covid tab 
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == input$plot_date)
  })
  
  reactive_db_last24h = reactive({
    cv_cases %>% filter(date == input$plot_date & new_cases>0)
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$id)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_last24h = reactive({
    large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$id)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$id %in% reactive_db_large()$alpha3, ]
  })
  
  reactive_polygons_last24h = reactive({
    worldcountry[worldcountry$id %in% reactive_db_large_last24h()$alpha3, ]
  })
  
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$death), big.mark=","), " deaths")
  })
  
  output$reactive_recovered_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$recovered), big.mark=","), " recovered")
  })
  
  output$reactive_active_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$active_cases), big.mark=","), " active cases")
  })
  
  output$reactive_case_count_China <- renderText({
    paste0("Mainland China: ", prettyNum(sum(subset(reactive_db(), country=="Mainland China")$cases), big.mark=",")," (",
           prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new, big.mark=",")," new)")
  })
  
  output$reactive_case_count_row <- renderText({
    paste0("Other: ", prettyNum(sum(subset(reactive_db(), country!="Mainland China")$cases), big.mark=",")," (",
           prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new, big.mark=",")," new)")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
  })
  
  output$reactive_new_cases_24h <- renderText({
    paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$activeper100k)) %>% #group = "2019-COVID (cumulative)",
      #label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed COVID cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_large()$country, reactive_db_large()$cases, reactive_db_large()$deaths, reactive_db_large()$recovered, reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
      #labelOptions = labelOptions(
      #             style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
      #             textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                       label = sprintf("<strong>%s (past 24h)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                       label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (active)",
                       label = sprintf("<strong>%s (active)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto"))  %>%
      
      addCircleMarkers(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                       fillOpacity = 0.2, color = sars_col, group = "2003-SARS",
                       label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$per100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4), 
                       fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                       label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                       fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
                       label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
                         textsize = "15px", direction = "auto"))
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(cv_aggregated, input$plot_date)
  })
  
  output$epi_curve <- renderPlot({
    new_cases_plot(cv_aggregated, input$plot_date)
  })
  
  # sars tab 
  sars_mod_date = reactive({
    format(as.Date(input$sars_plot_date, format="%d %b %y"), "%Y-%m-%d")
  })
  
  output$sars_clean_date_reactive <- renderText({
    format(as.POSIXct(sars_mod_date()),"%d %B %Y")
  })
  
  sars_reactive_db = reactive({
    sars_cases %>% filter(date == sars_mod_date())
  })
  
  sars_reactive_db_large = reactive({
    large_countries = sars_reactive_db() %>% filter(country!="Singapore" & country!="Diamond Princess Cruise Ship" & country!="Hong Kong" & country!="Macao")
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  sars_reactive_polygons = reactive({
    worldcountry[worldcountry$id %in% sars_reactive_db_large()$alpha3, ]
  })
  
  output$sars_reactive_case_count <- renderText({
    paste0(sum(sars_reactive_db()$cases), " cases")
  })
  
  output$sars_reactive_death_count <- renderText({
    paste0(sum(sars_reactive_db()$death), " deaths")
  })
  
  
  output$sars_reactive_country_count <- renderText({
    paste0(length(unique(sars_reactive_db()$country_group)), " countries/territories affected")
  })
  
  output$sars_map <- renderLeaflet({
    sars_basemap
  })
  
  observeEvent(input$sars_plot_date, {
    leafletProxy("sars_map") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = sars_reactive_polygons(), stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.1, fillColor = ~sars_pal(sars_reactive_db_large()$per100k), group = "2003-SARS (cumulative)",
                  label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_reactive_db_large()$country, sars_reactive_db_large()$cases, sars_reactive_db_large()$deaths, sars_reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                    textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = sars_reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                       fillOpacity = 0.2, color = sars_col, group = "2003-SARS (cumulative)",
                       label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_reactive_db()$country, sars_reactive_db()$cases, sars_reactive_db()$deaths, sars_reactive_db()$per100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5),
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID",
                       label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", cv_today$country, cv_today$cases, cv_today$deaths, cv_today$recovered, cv_today$per100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto"))  %>%
      
      addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4),
                       fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                       label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4),
                       fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
                       label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
                         textsize = "15px", direction = "auto")) 
  })
  
  output$sars_cumulative_plot <- renderPlot({
    sars_cumulative_plot(sars_aggregated, sars_mod_date())
  })
  
  output$sars_epi_curve <- renderPlot({
    sars_new_cases_plot(sars_aggregated, sars_mod_date())
  })
  
  # comparison plot
  output$comparison_plot <- renderPlotly({
    comparison_plot(epi_comp, input$comparison_metric)
  })
  
  # add footnote for cases
  output$epi_notes_1 <- renderText({
    if(input$comparison_metric=="cases") { paste0("Note that the axis is on a log10 scale so moves in 10-fold increments.
                                                  The 60.8 million estimated cases of H1N1 dwarf all other outbreaks of plotted on a standard linear scale.") }
  })
  
  # add footnote for deaths
  output$epi_notes_2 <- renderText({
    if(input$comparison_metric=="deaths") { 
      paste0("For H1N1, the number of laboratory-confirmed deaths reported by the WHO is displayed. Subsequent modelling studies have estimated the actual number to be in the range of 123,000 to 203,000.")
    }
  })
  
  # add note for cfr
  output$epi_notes_3 <- renderText({
    if(input$comparison_metric=="cfr") { 
      paste0("For COVID-19, this displays the proportion of confirmed cases who have subsequently died. When factoring in mild or asymptomatic infections that are not picked up by case surveillance efforts, current estimates place the case fatality rate in the range of 0.3-1%.")
    }
  })
  
  #ethow_orig
  # update region selections
  observeEvent(input$level_select, {
    if (input$level_select=="Global") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = "Global", selected = "Global")
    }
    
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                        selected = c("Africa", "Asia", "Europe", "North America", "South America"))
    }
    
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
                        selected = cv_today_100$country)
    }
    
    #if (input$level_select=="State") {
    #  updatePickerInput(session = session, inputId = "region_select", 
    #                    choices = as.character(cv_today_USstates_N[order(-cv_today_USstates_N$cases),]$state), 
    #                    selected = cv_today_USstates_N$state)
    #}
    
  }, ignoreInit = TRUE)
  
  #ethow_new
  # update region selections
  observeEvent(input$level_select2, {
   
    if (input$level_select2=="State") {
      updatePickerInput(session = session, inputId = "region_select2", 
                        choices = as.character(cv_today_USstates_N[order(-cv_today_USstates_N$cases),]$state), 
                        selected = cv_today_USstates_N$state)
    }
    
    #if (input$level_select2=="foo") {
    #  updatePickerInput(session = session, inputId = "region_select2", 
    #                    choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
    #                    selected = cv_today_100$country)
    #}
    
  }, ignoreInit = TRUE)
  
  #ethow_new2
  # update region selections
  observeEvent(input$level_select3, {
    
    if (input$level_select3=="County") {
      updatePickerInput(session = session, inputId = "region_select3", 
                        choices = as.character(cv_today_UScounties_N[order(-cv_today_UScounties_N$cases),]$state.county), 
                        selected = cv_today_UScounties_N$state.county) 
    }
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$level_select3b, {
    
    #if (input$level_select3b=="State") {
      updatePickerInput(session = session, inputId = "region_select3", 
                        #choices = subset(as.character(cv_today_USstates_N[order(-cv_today_USstates_N$cases),]$state.county), cv_today_USstates_N$state %in% input$region_select3),
                        choices = subset(as.character(cv_today_UScounties_N[order(-cv_today_UScounties_N$cases),]$state.county), cv_today_UScounties_N$state %in% input$level_select3b),
                        selected = subset(cv_today_UScounties_N$state.county, cv_today_UScounties_N$state %in% input$level_select3b))
    #}
    
  }, ignoreInit = TRUE)
  
  #ethow_orig
  # create dataframe with selected countries
  country_reactive_db = reactive({
    if (input$level_select=="Global") { 
      db = cv_cases_global
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = cv_cases_continent 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = cv_cases
      db$region = db$country
    }
    
    if (input$outcome_select=="Cases") { 
      db$outcome = db$cases
      db$new_outcome = db$new_cases
    }
    
    if (input$outcome_select=="Deaths") { 
      db$outcome = db$deaths 
      db$new_outcome = db$new_deaths 
    }
    
    db %>% filter(region %in% input$region_select)
  })
  
  # country-specific plots
  output$country_plot <- renderPlotly({
    country_cases_plot(country_reactive_db(), start_point=input$start_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative <- renderPlotly({
    country_cases_cumulative(country_reactive_db(), start_point=input$start_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative_log <- renderPlotly({
    country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date)
  })
  
#ethow_new  
  # create dataframe with selected states
  state_reactive_db = reactive({
    if (input$level_select2=="State") { 
      db2 = as.data.frame(cv_cases_USstates)
      db2$region = db2$state
    }
    
    #if (input$level_select2=="foo") { 
    #  db2 = cv_cases
    #  db2$region = db2$country
    #}
    
    if (input$outcome_select2=="Cases") { 
      db2$outcome = db2$cases
      db2$new_outcome = db2$new_cases
      #db2$region = db2$state.ordered
    }
    
    if (input$outcome_select2=="Deaths") { 
      db2$outcome = db2$deaths 
      db2$new_outcome = db2$new_deaths 
      #db2$region = db2$state.per1k.ordered
    }
    
    if (input$outcome_select2=="Cases per capita") { 
      db2$outcome = db2$per1k
      db2$new_outcome = db2$new_per1k
      #db2$region = db2$state.ordered
    }
    
    if (input$outcome_select2=="Deaths per capita") { 
      db2$outcome = db2$deaths_per1k 
      db2$new_outcome = db2$new_deaths_per1k 
      #db2$region = db2$state.per1k.ordered
    }
    
    #print(db2$region)
    #print(input$region_select2)
    #print(db2)
    db2 %>% filter(region %in% input$region_select2)
  })
  
  # state plots
  output$region_cases_bar_plot_state <- renderPlotly({
    region_cases_bar_plot(state_reactive_db(), start_point=input$start_date2, graph_start_date=input$graph_start_date2, log_flag=input$log_flag2, ylabel = input$outcome_select2)
  })

  output$region_cases_line_plot_state <- renderPlotly({
    region_cases_line_plot(state_reactive_db(), start_point=input$start_date2, graph_start_date=input$graph_start_date2, log_flag=input$log_flag2, ylabel = input$outcome_select2)
  })
  
  
  #ethow_new2
  # create dataframe with selected counties
  county_reactive_db = reactive({
    if (input$level_select3=="County") { 
      db3 = as.data.frame(cv_cases_UScounties)
      db3$region = db3$state.county
    }
    
    if (input$outcome_select3=="Cases") { 
      db3$outcome = db3$cases
      db3$new_outcome = db3$new_cases
      #db3$region = db3$state.county.ordered
    }
    
    if (input$outcome_select3=="Deaths") { 
      db3$outcome = db3$deaths 
      db3$new_outcome = db3$new_deaths 
      #db3$region = db3$state.county.ordered
    }
    
    if (input$outcome_select3=="Cases per capita") { 
      db3$outcome = db3$per1k
      db3$new_outcome = db3$new_per1k
      #db3$region = db3$state.county.per1k.ordered
                                    #per1k.rev.ordered
    }
    
    if (input$outcome_select3=="Deaths per capita") { 
      db3$outcome = db3$deaths_per1k 
      db3$new_outcome = db3$new_deaths_per1k 
      #db3$region = db3$state.county.per1k.ordered
    }
    
    #print(db3$region)
    #print(input$region_select2)
    #print(db3)
    db3 %>% filter(region %in% input$region_select3)
  })
  
  # county plots
  output$region_cases_bar_plot_county <- renderPlotly({
    region_cases_bar_plot(county_reactive_db(), start_point=input$start_date3, graph_start_date=input$graph_start_date3, log_flag=input$log_flag3, ylabel = input$outcome_select3)
  })
  
  output$region_cases_line_plot_county <- renderPlotly({
    region_cases_line_plot(county_reactive_db(), start_point=input$start_date3, graph_start_date=input$graph_start_date3, log_flag=input$log_flag3, ylabel = input$outcome_select3)
  })
  
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("COVID_data_", cv_today$date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                      recovered, new_recovered, active_cases, 
                                      per100k, newper100k, activeper100k)), file)
    }
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                     recovered, new_recovered, active_cases, 
                                     per100k, newper100k, activeper100k)), input$maxrows), row.names = FALSE)
    options(orig)
  })
  
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")