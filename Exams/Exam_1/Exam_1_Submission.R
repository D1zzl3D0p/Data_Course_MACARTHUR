# Tasks:
# YOUR TASKS:

# **I.**
#   **Read the cleaned_covid_data.csv file into an R data frame. (20 pts)**
#   
# **II.**
# **Subset the data set to just show states that begin with "A" and save this as an object called A_states. (20 pts)**
#   
# + Use the *tidyverse* suite of packages
# + Selecting rows where the state starts with "A" is tricky (you can use the grepl() function or just a vector of those states if you prefer)
# 
# **III.**
# **Create a plot _of that subset_ showing Deaths over time, with a separate facet for each state. (20 pts)**
#   
# + Create a scatterplot
# + Add loess curves WITHOUT standard error shading
# + Keep scales "free" in each facet
# 
# **IV.** (Back to the full dataset)
# **Find the "peak" of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)**
#   
#   I'm looking for a new data frame with 2 columns:
# 
#  + "Province_State"
#  + "Maximum_Fatality_Ratio"
#  + Arrange the new data frame in descending order by Maximum_Fatality_Ratio
#  
# This might take a few steps. Be careful about how you deal with missing values!
# 
# 
# **V.**
# **Use that new data frame from task IV to create another plot. (20 pts)**
# 
#  + X-axis is Province_State
#  + Y-axis is Maximum_Fatality_Ratio
#  + bar plot
#  + x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
#  + X-axis labels turned to 90 deg to be readable
#  
# Even with this partial data set (not current), you should be able to see that (within these dates), different states had very different fatality ratios.
# 
# **VI.** (BONUS 10 pts)
# **Using the FULL data set, plot cumulative deaths for the entire US over time**
# 
#  + You'll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.

library(tidyverse)

## Task 1: Read the csv file into a data frame
rdata <- read.csv("cleaned_covid_data.csv")

## Task 2: Subset the data into an object called A_states, with just the states that start with A
A_states <- rdata |>
  filter(grepl("^A",rdata$Province_State))

## Task 3: Create a plot of A_states showing Deaths over time, with a separate facet for each state.

A_states |>
  ggplot(aes(x=Last_Update,y=Deaths,group=Province_State))+
  geom_point(aes(color="red"))+
  geom_line(aes(color="blue"))+
  facet_wrap(~Province_State)+
  theme(
    legend.position = "none" 
  )

## Task 4: Find the "peak" of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate.


state_max_fatality_rate <- rdata |> 
  group_by(Province_State) |>
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio,na.rm=T)) |>
  arrange(Maximum_Fatality_Ratio,)  
  
## Task 5: Use that new data frame from task IV to create another plot, where:
#  X-axis is Province_State
#  Y-axis is Maximum_Fatality_Ratio
#  bar plot
#  x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
#  x-axis labels turned to 90 deg to be readable

state_max_fatality_rate |>
  mutate(Province_State = as.factor(Province_State)) |>
  ggplot(aes(x=Province_State,y=Maximum_Fatality_Ratio)) +
  geom_bar(stat="identity") +
  theme(,
    axis.text.x = element_text(angle = 90),
  )

## Task 6: Using the FULL data set, plot cumulative deaths for the entire US over time

rdata |>
  group_by(Last_Update)|>
  summarize(Cumulative_Deaths = sum(Deaths)) |>
  ggplot(aes(x=Last_Update,y=Cumulative_Deaths)) +
  geom_point()







