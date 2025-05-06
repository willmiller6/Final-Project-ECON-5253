#######################################################################################################
##### Replication materials for automated compilation of figures and tables included in the paper #####
#######################################################################################################

# Load necessary libraries
library(tidyverse)
library(plm)
library(stargazer)
library(kableExtra)
library(patchwork)


# Load the data used for this project (in the github repo)
final_data <- read.csv("FlutieEffect_replication_data.csv")

final_data <- pdata.frame(final_data, index = c("School", "Year"))

str(final_data) # Make sure everything looks good...

# This table contains institutional characteristics about universities

inst_vars <- final_data[, c("Total.Applicants", "Avg.Faculty.Salary", "Tuition.and.Fees", "Grads", "Household.Income")]
stargazer(inst_vars,
        type = "latex",
        title = "Summary Statistics of Institutional Variables",
        out = "FlutiePaperTable.tex")

# And this table contains summary statistics of the football variables
football_vars <- final_data[, c("Wins", "Top25", "Top10", "Playoffs", "Champion")]
stargazer(final_data[, c("Wins", "Top25", "Top10", "Playoffs", "Champion")],
        type = "latex",
        title = "Summary Statistics of Football Variables",
        out = "FlutiePaperTable2.tex")


# Now for the models used in the paper--

# First we need to estimate the main models for the full sample
# This code estimates the panel models referenced in the paper
# Because the data is in a panel format and requires a lagged
# dependent variable, we will use the pgmm function from the
# plm package to estimate the models (using difference GMM)
# This needs to be done for each of the models in the paper,
# so the following is slightly repetitive


# Full sample - full model (all football variables)
full_model <- pgmm(
  log.Total.Applicants ~ lag(log.Total.Applicants, 1) + lag(Top25) + lag(Top10) + lag(Playoffs) + lag(Champion) +
    I(log(Tuition.and.Fees)) +I(log(Avg.Faculty.Salary)) + I(log(Grads)) + I(log(Household.Income))| 
    lag(log.Total.Applicants, 2:3),
  data = final_data,
  collapse = TRUE,
  index=c("Team", "Year"),
  effect = "individual",
  model = "twosteps"
)
summary(full_model) # View the model summary

# Full sample - top 25
top_25_model<- pgmm(
  log.Total.Applicants ~ lag(log.Total.Applicants, 1) + lag(Top25) +
    I(log(Tuition.and.Fees)) +I(log(Avg.Faculty.Salary)) + I(log(Grads)) + I(log(Household.Income)) | 
    lag(log.Total.Applicants, 2:3),
  data = final_data,
    collapse = FALSE,
  index=c("School", "Year"),
  effect = "individual",
  model = "twosteps"
)
summary(top_25_model) # View the model summary


# Full sample - top 10
top_10_model<- pgmm(
  log.Total.Applicants ~ lag(log.Total.Applicants, 1) + lag(Top10) +
    I(log(Tuition.and.Fees)) +I(log(Avg.Faculty.Salary)) + I(log(Grads)) + I(log(Household.Income)) | 
    lag(log.Total.Applicants, 2:3),
  data = final_data,
  collapse = FALSE,
  index=c("School", "Year"),
  effect = "individual",
  model = "twosteps"
)
summary(top_10_model) # View the model summary


# Full sample - playoffs
playoff_model <- pgmm(
  log.Total.Applicants ~ lag(log.Total.Applicants, 1) + lag(Playoffs) +
    I(log(Tuition.and.Fees)) +I(log(Avg.Faculty.Salary)) + I(log(Grads)) + I(log(Household.Income)) | 
    lag(log.Total.Applicants, 2:3),
  data = final_data,
  collapse = FALSE,
  index=c("School", "Year"),
  effect = "individual",
  model = "twosteps"
)
summary(playoff_model) # View the model summary

# Full sample - champion
Natty_model <- pgmm(
  log.Total.Applicants ~ lag(log.Total.Applicants, 1) + lag(Champion) +
    I(log(Tuition.and.Fees)) +I(log(Avg.Faculty.Salary)) + I(log(Grads)) + I(log(Household.Income)) | 
    lag(log.Total.Applicants, 2:3),
  data = final_data,
  collapse = FALSE,
  index=c("School", "Year"),
  effect = "individual",
  model = "twosteps"
)
summary(Natty_model) # View the model summary


# Now we generate the table for the full sample
stargazer(top_25_model, top_10_model, playoff_model, Natty_model, full_model, type="latex", out="FlutiePaperModel.tex")


# Now we need to estimate the models for the "no blue bloods" sample

# First, it is necessary to identify the schools that are considered "blue bloods"
# by counting the number of playoff appearances and championships for each school

# This is done using the following code:
playoff_champ_table <- final_data %>%
  group_by(School) %>%
  summarize(playoffs = sum(Playoffs), championships = sum(Champion)) %>%
  filter(playoffs >= 1) %>%
  arrange(desc(playoffs))

# Which is then exported to a table for the paper:
kable(playoff_champ_table, format = "latex", booktabs = TRUE, caption = "Playoff Appearances and Championships by School") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, width = "3cm") %>%
  column_spec(3, width = "3cm") %>%
  row_spec(0, bold = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# From here, I identify that Alabama, Clemson, Oklahoma, and Ohio 
# State are the "blue bloods"

# The next step is removing these schools from the data set to
# estimate the models for the "no blue bloods" sample

# We can do this by filtering the data set to exclude these schools
# This is done using the following code:
notop_data <- final_data %>%
  filter(School != "The University of Alabama", School != "Clemson University", School != 
  "University of Oklahoma-Norman Campus", School != "Ohio State University-Main Campus")

head(notop_data) # View the data to ensure it is correct

# Now we can use that sample to estimate the models for the "no blue bloods" sample

# Full model (all football variables) - no blue bloods
fmntp <- pgmm(
  log.Total.Applicants ~ lag(log.Total.Applicants, 1)  + lag(Top25) + lag(Top10) + lag(Playoffs) + lag(Champion) +
    I(log(Tuition.and.Fees)) + I(log(Avg.Faculty.Salary)) + I(log(Grads)) + I(log(Household.Income)) | 
    lag(log.Total.Applicants, 2:3),
  data = notop_data,
  collapse = FALSE,
  index=c("School", "Year"),
  effect = "individual",
  model = "twosteps"
)
summary(fmntp) # View the model summary


# Top 25 - no blue bloods
t25ntp <- pgmm(
  log.Total.Applicants ~ lag(log.Total.Applicants, 1) + lag(Top25) + 
    I(log(Tuition.and.Fees)) + I(log(Avg.Faculty.Salary)) + I(log(Grads)) + I(log(Household.Income)) | 
    lag(log.Total.Applicants, 2:3),
  data = notop_data,
  collapse = FALSE,
  index=c("School", "Year"),
  effect = "individual",
  model = "twosteps"
)

summary(t25ntp) # View the model summary

# Top 10 - no blue bloods
t10ntp <- pgmm(
  log.Total.Applicants ~ lag(log.Total.Applicants, 1) + lag(Top10) + 
    I(log(Tuition.and.Fees)) + I(log(Avg.Faculty.Salary)) + I(log(Grads)) + I(log(Household.Income)) | 
    lag(log.Total.Applicants, 2:3),
  data = notop_data,
  collapse = FALSE,
  index=c("School", "Year"),
  effect = "individual",
  model = "twosteps"
)

summary(t10ntp) # View the model summary

# Playoffs - no blue bloods
pntp <- pgmm(
  log.Total.Applicants ~ lag(log.Total.Applicants, 1) + lag(Playoffs) +
    I(log(Tuition.and.Fees)) +I(log(Avg.Faculty.Salary)) + I(log(Grads)) + I(log(Household.Income)) | 
    lag(log.Total.Applicants, 2:3),
  data = notop_data,
  collapse = FALSE,
  index=c("School", "Year"),
  effect = "individual",
  model = "twosteps"
)
summary(pntp) # View the model summary

# Champion - no blue bloods
nntp <- pgmm(
  log.Total.Applicants ~ lag(log.Total.Applicants, 1) + lag(Champion) +
    I(log(Tuition.and.Fees)) +I(log(Avg.Faculty.Salary)) + I(log(Grads)) + I(log(Household.Income)) | 
    lag(log.Total.Applicants, 2:3),
  data = notop_data,
  collapse = FALSE,
  index=c("School", "Year"),
  effect = "individual",
  model = "twosteps"
)

summary(nntp) # View the model summary

# Finally, we need to generate the table for the "no blue bloods" sample
stargazer(fmntp, t25ntp, t10ntp, pntp, nntp, type="text", out="FlutiePaperModel_notop.tex")


# Now we need to generate the figures for the paper

# Figure 1 is a boxplot of applicants by school grouped by win total
# This is done using the following code:
winsbox <- ggplot(final_data, aes(x = as.factor(Wins), y = log.Total.Applicants), alpha=Playoffs) +
  geom_boxplot(fill = "skyblue", color = "black", alpha=1) +
  labs(title = "Figure 1: Distribution of Total Applicants by Win Total",
       x = "Win Total",
       y = "Total Applicants") +
  theme_minimal(base_size = 30) +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 30))
ggsave("boxplotwinsvsapps.png", bg="white", plot = winsbox, width = 13, height = 8, units = "in", dpi = 300)

# Figure 2 shows two line plots of the number of applicants by year for LSU and UGA

# Start by filtering the data for LSU
lsu_data <- final_data %>%
  filter(School == "Louisiana State University and Agricultural & Mechanical College") %>%
  group_by(Year) %>%
  summarise(Total.Applicants = sum(Total.Applicants), .groups = "drop")
head(lsu_data) # View the data to ensure it is correct

# Now plot the data for LSU
plot_lsu <- ggplot(lsu_data, aes(x = Year, y = Total.Applicants)) +
  geom_line(size = 1.2, color = "darkorchid2") +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "gold", size = 1.5) +
  labs(title = "LSU Applicants Over Time", x = "Year", y = "Total Applicants") +
  theme_minimal(base_size = 30) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 14),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
    axis.text.x = element_text(angle = 45, hjust = 1)
    )
plot_lsu # View the plot to ensure it is correct

# Now I do the same for UGA

# Filter the data for UGA
georgia_data <- final_data %>%
  filter(School == "University of Georgia") %>%
  group_by(Year) %>%
  summarise(Total.Applicants = sum(Total.Applicants), .groups = "drop")
head(georgia_data) # View the data to ensure it is correct

# And plot the data for UGA
plot_uga <- ggplot(georgia_data, aes(x = Year, y = Total.Applicants)) +
  geom_line(size = 1.2, color = "firebrick") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "darkgrey", size = 1.5) +
  labs(title = "UGA Applicants Over Time", x = "Year", y = "Total Applicants") +
  theme_minimal(base_size = 30) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 14),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
plot_uga # View the plot to ensure it is correct

# Now I use the patchwork package to combine the plots
combined_plot <- plot_lsu + plot_uga +
  plot_annotation(
    title = "Figure 2: Total Applicants Over Time for LSU and UGA",
    caption = "Dashed lines indicate Championship seasons",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 15, face = "italic"),
    )
  )

combined_plot # View the combined plot to ensure it is correct

# Save the combined plot as a PNG file
ggsave("UGA_LSU_Apps.png", bg="white", plot = combined_plot, width = 12, height = 8)

# Miscellaneous calculations for the paper

# I reference "napkin math" for the numeber of applicants recieved 
# by the median school in the paper, and that figure can be reproduced
# by:

final_data$Total.Applicants %>%
  median() %>%
  `*`(full_model$coefficients[[2]]["lag(Top25)"])

# And this is an equivalent calculation for the number of applicants
# received by the median school after a national championship in the 
# "no blue bloods" sample
final_data$Total.Applicants %>%
  median() %>%
  `*`(fmntp$coefficients[[2]]["lag(Champion)"])

# This code will reproduce the fixed effects model used to estimate first
# order autocorrelation referenced in the methods section of the paper
fe_model <- plm(log.Total.Applicants ~ lag(Top25) + lag(Top10) + lag(Playoffs)
                + lag(Champion) + I(log(Tuition.and.Fees)) + 
                I(log(Avg.Faculty.Salary)) + I(log(Grads)) + 
                I(log(Household.Income)),
                data = final_data,
                index = c("School", "Year"),
                model = "within")
summary(fe_model) # View the model summary

# And this is the resulting Brush-Godfrey test for autocorrelation
pbgtest(fe_model) # View the test results
