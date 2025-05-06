## Final Project for ECON 5253

This GitHub repository contains all the necessary materials to replicate my project for Data Science for Economists titled **Reexamining the Flutie Effect in the CFP Era: A Dynamic Panel Approach.**

This paper sought to evaluate the "Flutie Effect," or the supposed application surge experienced by universities after a successful college football season, in the College Football Playoff Era. The original CFP format offered an opportunity to evaluate this old hypothesis with new data in a college football environment that has experienced seismic changes in team building, revenue opportunities, and spending on athletes.

## Data

All of the data needed to replicate this project can be found in "FlutieEffect_Replication_Data.csv" housed in this repository. This dataset contains various relevant institutional measures (average faculty salary, tuition and fees, total applications) gathered from the [**Integrated Postsecondary Education Data System**](Integrated Postsecondary Education Data System), state income and the number of graduates by state from [**FRED**](https://fred.stlouisfed.org/release/tables?rid=249&eid=259515&od=2019-01-01#)and the [**NCES**](https://nces.ed.gov/programs/digest/d24/tables/dt24_219.20.asp), respectively, historical AP poll ranking from [**College Poll Archive**](https://www.collegepollarchive.com/) (football fans need to check this site out!), data on wins from [**CFB Saturdays**,](https://cfbsaturdays.com/most-wins-in-fbs-over-last-10-years/#google_vignette) and data on playoff berths and championship wins from the [**College Football Playoff Website**](https://collegefootballplayoff.com/sports/2019/5/22/history).

A script showing how I generated this data is available in this repository.

## Running the Replication Script

This script is quite straightforward to run. Simply install the following packages (if you have not already):

```{r install packages}

install.packages(c("tidyverse", "plm", "stargazer", "kableExtra", "patchwork"))
```

To run the script on your local machine, simply download it (and the associated dataframe) before running it from start to finish, and it will automate the complilation of all figures and tables included in the data.
