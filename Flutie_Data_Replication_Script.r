# Code to replicate the data collection for Flutie_Replicattion_Data.csv

# First, it's necessary to load the required libraries
library(tidyverse)
library(reshape2)
library(fredr)
library(rvest)
library(plm)


# First, we need to clean/manage the data from IPEDS. This is actually pretty easy.
# Gathering the data is extremely tedious as it needs to be done manually for each
# school in the data. I have saved you the toruble of doing this by providing the data
# I used in this repository.

# First, load it from the repo
df_school <- read.csv("C:/Users/willy/OneDrive/Data Analytics/df_school.csv")

view(df_school) # Take a quick look to make sure it loaded correctly

# First let's remove that first column - just a quirk of how the data was saved
df_school <- df_school[,-1] 

# Now let's handle some missing values! This data is pretty great because
# it's from the government. There are only two big issues. Penn State didn't report
# any data for 2020 or 2021. Because those were pandemic years and this era of Penn
# state football was marred by a lot of controversy, I made the decision to drop all 
# of their data. Similarly, the Military Academies do not functoin as normal schools
# as far as perspective students are concerned. They are not really comparable to the other
# schools in the data. So I dropped them as well. There were no other missing values.

# I drop Penn State and the Military Academies
df_school <- df_school %>%
  filter(!School %in% c("Pennsylvania State University-Main Campus", 
  "United States Air Force Academy", "United States Naval Academy", 
  "United States Military Academy"))

sum(is.na(df_school)) # Check for missing values - looks good!

# Now we'll handle the football data. I have gathered the data from
# the CFP website manually to save you, dear user, some additional trouble,
# so you can just load it from the repo and follow these steps to manipulate it
# to be ready for analysis.

Natty_df <- read.csv("C:/Users/willy/OneDrive/Data Analytics/Econometrics Project Natties Since 2014.csv")

head(Natty_df) # Take a look at the data - this is too wide!

# This data is in wide format (made it much easier for me to compile)
# so it needs to be converted to long format. This is a pretty simple process:
Natty_df <- Natty_df %>% 
  melt(id.vars = c("RANK","TEAM"), variable.name = "YEAR")

#Rename the columns to be more descriptive
names(Natty_df)[names(Natty_df)=="value"] <- "Champion"

#Remove that weird X hanging out in front of each year
Natty_df$YEAR <- gsub("X", "", Natty_df$YEAR)

# And drop the RANK column - we don't need it for our analysis
Natty_df <- Natty_df[,-1]

#Finally, make the Natty column binary
Natty_df$Champion <- ifelse(Natty_df$Champion == "yes", 1, 0)

head(Natty_df) # Take a look at the data - this is much better!

# We'll do the same thing for the Playoffs.
# This next part is pretty repetitive.

Playoffs_df <- read.csv("C:/Users/willy/OneDrive/Data Analytics/Econometrics Project Playoffs Since 2014.csv")

# Elongating...
Playoffs_df <- Playoffs_df %>% 
  melt(id.vars = c("RANK","TEAM"), variable.name = "YEAR")

# Renaming...
names(Playoffs_df)[names(Playoffs_df)=="value"] <- "Playoffs"

# Removing the X...
Playoffs_df$YEAR <- gsub("X", "", Playoffs_df$YEAR)

# Dropping the RANK column...
Playoffs_df <- Playoffs_df[,-1]


# Dummifying...
Playoffs_df$Playoffs <- ifelse(Playoffs_df$Playoffs == "yes", 1, 0)

head(Playoffs_df) # Take a look at the data - looks good to me!


# Now let's scrape the wins data!

# Initialize the URL
url <- "https://cfbsaturdays.com/most-wins-in-fbs-over-last-10-years/"

# Read the HTML from the page
page <- read_html(url)

# Now grab the table
tables <- page %>% html_elements("table")

# Now assign it to a dataframe for easier manipulation
Wins <- tables[[1]] %>% html_table(fill = TRUE)

# This is a super similar process to the one we just did for the playoff data
# so I won't go into too much detail here.

# First let's drop that "Total column" at the end - we don't need it for our analysis
Wins <- Wins[,-ncol(Wins)]

# Elongating...
Wins <- Wins %>% 
  melt(id.vars = c("Rank","Team"), variable.name = "Year")

# Renaming...
names(Wins)[names(Wins)=="value"] <- "Wins"

# Removing Rank column...
Wins <- Wins[,-1]

head(Wins) # Take a look at the data - this looks good!

# So a number of these teams didn't play for the whole panel because
# they were FCS teams. We need to drop those teams.
# We can do this by filtering out any rows where the Wins column is "N/A"
Wins <- Wins %>% filter(Wins != "N/A")

# Now remove the years that are out of our bounds
Wins <- Wins %>%
  filter(Year != "2013")

# Done!

# Now let's get that grad data! This is done with scraping just like the wins data.

# Assign the URL to a variable
url <- "https://nces.ed.gov/programs/digest/d24/tables/dt24_219.20.asp"

# Read the HTML from the page
page <- read_html(url)

# Extract the table
table_node <- page %>% html_element(".tableWidth") # got this with selector gadget

# Convert to a data frame for easier manipulation
Grad_data <- table_node %>% html_table(fill = TRUE)

head(Grad_data) # preview - this is messy! Should be pretty easy to clean up, though.
view(Grad_data) # Take a look at the data - this is a mess!
# Let's just drop the first 8 rows and keep only the columns we need 
Grad_data <- Grad_data[-c(1:8, 17, 60:77), -c(2:7, 17)]

# This looks a lot better! Now we need to rename the columns to be more useful
colnames(Grad_data) <- c("State", "2014", "2015", "2016", "2017", 
                                "2018", "2019", "2020", "2021", "2022")

# Now we melt!
Grad_data <- Grad_data %>% 
  melt(id.vars = "State", variable.name = "Year")

# Name the value column
colnames(Grad_data)[colnames(Grad_data)=="value"] <- "Grads"

# Let's quickly make the Grads column numeric
Grad_data <- Grad_data %>%
  mutate(Grads = as.numeric(gsub(",", "", Grads))) # remove commas and convert to numeric

# Inspect for missingness
sum(is.na(Grad_data)) # Don't see any - looks good!

view(Grad_data) # Final vaidation - this looks good!

# Now let's get the college poll data. This is a more complicated scrape than
# the simple table above, but it's not too bad. 

#paste URL to initialize scrape
base_url <- "https://www.collegepollarchive.com/football/ap/seasons.cfm?seasonid="

# Initialize an empty dataframe to store results
ap_poll <- data.frame()

# Loop through years to scrape data
for (year in 2014:2023) {
  url <- paste0(base_url, year)
  
  # Reinitialize variables to avoid conflicts
  webpage <- NULL
  table <- NULL
  
  # Read the webpage
  webpage <- tryCatch(read_html(url), error = function(e) {
    message("Error reading URL: ", url)
    return(NULL)
  })
  
  # Skip if the webpage could not be read
  if (is.null(webpage)) next
  
  # Extract the entire table
  table <- tryCatch({
    webpage %>%
      html_node("table") %>%
      html_table(fill = TRUE)
  }, error = function(e) {
    message("Error extracting table from URL: ", url)
    return(NULL)
  })
  
  # Skip if the table could not be extracted
  if (is.null(table)) next
  
  # Add the Year column to the table
  table$Year <- year
  
  # Combine with the main dataset
  ap_poll<- bind_rows(ap_poll, table)
}

view(ap_poll) # Take a look at the data - this is a mess!


# lets do some cleaning
view(ap_poll) # Take a look at the data - this is a mess!
# first, lets remove some columns, rename some others, and convert one more
ap_poll <- ap_poll %>% 
    select(-c("Rank...2", "Rank...3", "Conf", "Rec", , "CP", "Last Week")) %>% # select columns
    rename("Rank" = "Rank...1", "Team" = "Team (FPV)") %>%
    mutate(Rank = as.numeric(Rank)) # convert Rank to numeric

#remove everything after the ( in the "Team" column
ap_poll$Team <- gsub("\\(.*", "", ap_poll$Team)

# We've got some teams in here that got votes but didn't get ranked.
# Let's drop those
ap_poll <- ap_poll %>% filter(Rank > 0)

# Now let's make some independent variables!

# Create "Top25"
ap_poll <- ap_poll %>% mutate(Top25 = ifelse(Rank > 0, 1, 0))

# Create "Top10"

ap_poll <- ap_poll %>% mutate(Top10 = ifelse(Rank <= 10 & Rank > 0, 1, 0))

view(ap_poll) # Take a look at the data - this looks great!

# Time to use the FRED API
# You'll need your own API key for this. You can get one at https://fred.stlouisfed.org/.

fredr_set_key("YOUR_API_KEY") # (replace with your own API key)


# The series IDs for the median household income data
# are in the format "MEHOINUS{state}A672N"
# where {state} is the two-letter state abbreviation.
# We will have to do a very small amount of data science
# wizardry to fix this. Don't worry, it's not that scary!

# grab tate abbreviations (excluding DC)
states <- c(state.abb)

# Create the series IDs (this is that wizardry I was talking about)
series_ids <- paste0("MEHOINUS", states, "A672N")

# Create lookup table
state_series <- tibble::tibble(
  state = states,
  series_id = series_ids
)

# Fetch the data!
state_income_data <- map_dfr(1:nrow(state_series), function(i) {
  fredr(
    series_id = state_series$series_id[i],
    observation_start = as.Date("2014-01-01"),
    observation_end = as.Date("2022-12-31")
  ) %>%
    mutate(state = state_series$state[i])
})

view(state_income_data) # Just to make sure... this looks good!

# Now let's do some cleaning. 

# I'm going to do this all in one line because I'm a pro, but 
# don't try this at home, kids.

state_income_data <- state_income_data %>%
  select(date, value, state) %>%
  mutate(date = as.Date(date),
         Year = format(date, "%Y")) %>%
         select(-date)

# Alright, now we need to get the actual state names in there...

# This is the best way I could think of to do this, please let
# me know if you have a better idea! Or if you have read all of these
# comments in detail...

# Create a lookup table from state.abb and state.name
state_lookup <- tibble::tibble(
  state = state.abb,
  state_name = state.name
)

# Join your income data with the lookup
state_income_named <- state_income_data %>%
  left_join(state_lookup, by = "state")

# Now overwrite the state column with the state_name column

state_income_data$State <- state_income_named$state_name

# And rename the value column to "Income"
state_income_data <- state_income_data %>%
    rename(Income = value)

head(state_income_data) # Take a look at the data - this looks good!

# Now we need one more thing...

# Some of these dataframes have a "State" column, some of them have "School," and some of them have "Team."
# We need a lookup table to convert between these names. I have created one for you, so you don't have to do it manually.
# This took a while but it works great!

# Load the lookup table
lookup_table <- read.csv("C:/Users/willy/OneDrive/Documents/Flutie_Lookup_Table.csv")

# Lets open this up to take a look at it
view(lookup_table)

# "Arkansas State" is loading in with a non breaking space in the 
# T"Team" column. This is a common issue with data from the excel
# I'll fix it here

lookup_table$Team <- gsub("\u00A0", "", lookup_table$Team)


# Now let's put everything together!

# First, we need to make sure the year columns are all the same type

# School data
sapply(df_school, class)
df_school$Year <- as.factor(df_school$Year)

# Natty data
sapply(Natty_df, class)
Natty_df$YEAR <- as.factor(Natty_df$YEAR)

# Playoffs data
sapply(Playoffs_df, class)
Playoffs_df$YEAR <- as.factor(Playoffs_df$YEAR)

# Wins data
sapply(Wins, class) # This is right
Wins$Year <- as.factor(Wins$Year)

# Grad data
sapply(Grad_data, class) # This is right
Grad_data$Year <- as.factor(Grad_data$Year)

# Poll data
sapply(ap_poll, class) 
ap_poll$Year <- as.factor(ap_poll$Year)

# Income data
sapply(state_income_data, class)
state_income_data$Year <- as.factor(state_income_data$Year)

# lookup table
sapply(lookup_table, class) # This is right

# Now lets trim everyting that we're joining on to make sure
# there are no trailing spaces or anything like that
# This is a common issue with data from the government, so it's worth checking for.

# School data
df_school <- df_school %>%
  mutate(School = str_trim(School), Year = str_trim(Year))

# Natty data
Natty_df <- Natty_df %>%
  mutate(TEAM = str_trim(TEAM), YEAR = str_trim(YEAR))

# Playoffs data
Playoffs_df <- Playoffs_df %>%
  mutate(TEAM = str_trim(TEAM), YEAR = str_trim(YEAR))

# Wins data
Wins <- Wins %>%
  mutate(Team = str_trim(Team), Year = str_trim(Year))
View(Wins)

# Grad data
Grad_data <- Grad_data %>%
  mutate(State = str_trim(str_squish(State)), Year = str_trim(Year))

# Poll data
ap_poll <- ap_poll %>%
  mutate(Team = str_trim(str_squish(Team)), Year = str_trim(Year))

# Income data
state_income_data <- state_income_data %>%
  mutate(State = str_trim(State), Year = str_trim(str_squish(Year)))

# lookup table
lookup_table <- lookup_table %>%
  mutate(SCHOOL = str_trim(SCHOOL), Team = str_trim(Team), State = str_trim(str_squish(State)))


view(Natty_df)

# Going to start with the school and football data here because they both have
# observations for the same years and schools.
# Again, I'm going to do the pro-style one-liner here.
df_final <- df_school %>%
    left_join(Natty_df, by = c("School" = "TEAM", "Year"="YEAR")) %>%
    left_join(Playoffs_df, by = c("School" = "TEAM", "Year"="YEAR")) %>%
    left_join(lookup_table, by = c("School" = "SCHOOL")) %>%
    left_join(Wins, by = c("Team" = "Team", "Year"="Year")) %>%
    left_join(Grad_data, by = c("State", "Year")) %>%
    left_join(state_income_data, by = c("State", "Year")) %>%
    left_join(ap_poll, by = c("Team", "Year"))
view(df_final) # Take a look at the data - this looks good!

# Joins can be messy so let's check for some missing values
sum(is.na(df_final))

# Investigating...
view(df_final[!complete.cases(df_final), ])

# looks like we have a couple more rows to drop here. These are schools
# that weren't in the FBS for the whole panel. I'll clean up the stragglers
# here and then we'll move on to the grad data.
df_final <- df_final %>%
  filter(!School %in% c("Coastal Carolina University", "Liberty University", "University of Alabama at Birmingham"))

# Now I'll impute 0s for all missing values because the NAs here represent 0 dummy variables
df_final[is.na(df_final)] <- 0

# Now let's create the dependent variable!

df_final$log.Total.Applicants <- log(df_final$Total.Applicants)

# One last step - lets get the structure just right and select the correct columns
df_final <- df_final %>%
  select(Team, Year, Champion, Playoffs, Top10, Top25, Wins, Grads, Income, 
         Tuition.and.Fees, Avg.Faculty.Salary, Total.Applicants, log.Total.Applicants)
?pdata.frame

view(df_final) # One final look - yup!

# We have successfully replicated the data from the paper using the tools learned
# in ECON 5253 - Data Science for Economists!
