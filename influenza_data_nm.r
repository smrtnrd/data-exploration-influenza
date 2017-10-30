
options(warn = -1) #ignore warnings

# IMPORTANT: This assumes that all packages in "Rstart.R" are installed,
# and the fonts "Source Sans Pro" and "Open Sans Condensed Bold" are installed
# via extrafont. If ggplot2 charts fail to render, you may need to change/remove the theme call.


source("Rstart.R")
library(ggmap)



sessionInfo()

#CDC information FLU
#devtools::install_github("hrbrmstr/cdcfluview")    
  

library(cdcfluview)
library(statebins)
# current verison
# packageVersion("cdcfluview")

#df.flu <- get_flu_data(region = "census", sub_region = 1:9, data_source = "all", years = 2008:2015)
#df.flu.census <- get_flu_data(region = "census", sub_region = 1:10, data_source = "ilinet", years = 2008:2015)

# I am looking at the ILI activity
df.state <- get_state_data(years=2010:2015)

#display the data 
#df.flu %>% head(10)
#sprintf("# of Rows in Dataframe: %s", nrow(df.flu))
#sprintf("Dataframe Size: %s", format(object.size(df.flu), units = "MB"))

#display the data 
df.state %>% head(10)
sprintf("# of Rows in Dataframe: %s", nrow(df.state))
sprintf("Dataframe Size: %s", format(object.size(df.state), units = "MB"))

#display the data 
#df.flu.census %>% head(10)
#sprintf("# of Rows in Dataframe: %s", nrow(df.flu.census))
#sprintf("Dataframe Size: %s", format(object.size(df.flu.census), units = "MB"))

#columns = c("REGION TYPE","REGION","YEAR","WEEK","AGE 0-4","AGE 25-49","AGE 25-64","AGE 5-24","AGE 50-64","AGE 65","ILITOTAL","TOTAL PATIENTS")

columns = c("statename","activity_level","activity_level_label","weekend", "season", "weeknumber")

# select() require column indices that we find trough which() 

df <- df.state %>% select(which(names(df.state) %in% columns))

df %>% head(10)
sprintf("# of Rows in Dataframe: %s", nrow(df))
sprintf("Dataframe Size: %s", format(object.size(df), units = "MB"))

proper_case <- function(x) {
    return (gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2" , x, perl = TRUE))
}



file <- "data/state-lat-long.csv"
df_state_location <- read_csv(file)
colnames(df_state_location)[1] <- 'statename'

df.location <- tbl_df(df_state_location)

df <- right_join(df,df_state_location) #join the dataframe yeah

#save the dataframe 
write_csv(df, path = "data/train.csv")

df %>% head(10)
sprintf("# of Rows in Dataframe: %s", nrow(df))

# grepl() is the best way to do in-text search
#df_flu <- df %>% filter(grepl("Local Activity", ACTIVITYESTIMATE))

df.flu.activity <- df %>% filter(activity_level > 0)
df.flu.activity %>% head(10)
sprintf("# of Rows in Dataframe: %s", nrow(df.flu.activity))
sprintf("Dataframe Size: %s", format(object.size(df.flu.activity), units = "MB"))

 as.Date("May-17-2014","%b-%d-%Y")

df.flu.weekend <-   df.flu.activity %>%
                    mutate(weekend = as.Date(weekend,"%b-%d-%Y")) %>% #convert to proper date for analysis
                    group_by(weekend) %>%
                    summarize(count = n()) %>%
                    arrange(weekend)

df.flu.weekend %>% head(10)

plot <- ggplot(df.flu.weekend, aes(x = weekend, y = count)) +
    geom_line(color = "#F2CA27", size = 0.1) +
    geom_smooth(color = "#1A1A1A") +
    fte_theme() +
    scale_x_date(breaks = date_breaks("2 years"), labels = date_format("%Y")) +
    labs(x = "Weekend of the obseration", y = "Count of weekly activity > 0 "
         , title = "Weekly ILI Activity in the US from 2015 - 2017")

max_save(plot, "US-ILI-when-1", "US CDC Fluview Data")

# Returns the numeric hour component of a string formatted "HH:MM", e.g. "09:40" input returns 9
get_month <- function(x) {
    date <- as.Date(x, "%b-%d-%Y") #format the data to date
    month <- format(date, "%b") # return the month
    return (month)
}

df.flu.time <- df.flu.activity %>%
                    mutate(month = sapply(weekend, get_month)) %>%
                    group_by(month, season) %>% 
                    summarize(count = n())


df.flu.time %>% head(10)
sprintf("# of Rows in Dataframe: %s", nrow(df.flu.time))
sprintf("Dataframe Size: %s", format(object.size(df.flu.time), units = "MB"))

month_format <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
season_format <- c("2010-11","2011-12","2012-13","2013-14","2014-15","2015-16")

df.flu.time$month <- factor(df.flu.time$month, level = rev(month_format))
df.flu.time$season <- factor(df.flu.time$season, level = rev(season_format))

df.flu.time %>% head(10)

plot <- ggplot(df.flu.time, aes(x = month, y = season, fill = count)) +
    geom_tile() +
    fte_theme() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.6),
          legend.title = element_blank(), legend.position="top", 
          legend.direction="horizontal", legend.key.width=unit(2, "cm"), 
          legend.key.height=unit(0.25, "cm"), legend.margin=unit(0.1,"cm"), 
          panel.margin=element_blank()) +
    labs(x = "Season of ILI activity ", y = "Month", 
         title = "# of ILI activity in U.S. from 2010 - 2015, by Season") +
    scale_fill_gradient(low = "white", high = "#27AE60", labels = comma)

max_save(plot, "US-ILI-when-2", "US CDC Fluview Data", w=6)

df.flu.type <- df.flu.activity %>%
                    group_by(activity_level_label) %>% 
                    summarize(count = n()) %>%
                    arrange(desc(count))

df.flu.type %>% head(20)
sprintf("# of Rows in Dataframe: %s", nrow(df.flu.type))

 
