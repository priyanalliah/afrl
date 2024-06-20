library(tidyverse)
options(dplyr.summarise.inform=FALSE)
theme_set(theme_bw())
library(here)
library(lubridate)

data <- read_csv(here("data", "DLI_data_export_for_Priya.csv"))


# Time that data were collected for a given student -----------------------

# ==> Suggestion: Create a Shiny app in which the user is selected from a dropdown menu and the plot updates accordingly. 
#       Even nicer if the options for the dropdown menu are loaded directly from the data ;)

# From the whole data structure, keep only the data for a single user:
selected_user <- "Zahra"

user_data <- data %>%
  filter(user_name == selected_user)

user_data %>% 
  mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>% 
  ggplot(aes(date, fill = month)) +
  geom_histogram(bins = 30) +
  labs(title = paste(selected_user, "| Distribution of data points over time"),
       subtitle = paste("Range of dates:", min(date(data$date)), "to", max(date(data$date)))) +
  guides(fill = "none")



# Visualize activity on a single day for a given user ---------------------

# ==> Suggestion: Add to the above Shiny app a second plot that shows the activity of the selected user on a given day (that can be selected).

available_days <- user_data %>% 
  pull(day) %>% 
  unique() %>% 
  sort()

# I'll just pick a random one for now:
selected_day <- available_days[sample(1:length(available_days), 1)]

user_day_data <- user_data %>% 
  filter(day == selected_day)

# Create the plot:
user_day_data %>%
  # We have to make sure we treat the timestamp as coming from the correct timezone!
  mutate(timestamp = with_tz(timestamp, tzone = "America/Los_Angeles")) %>%
  ggplot(aes(
    x = timestamp,
    y = factor(foreign_trans_id),
    color = is_correct_answer,
    shape = mode
  )) +
  geom_point(size = 3) +
  labs(
    title = paste(selected_user, "| Activity on", selected_day),
    subtitle = paste("Total number of activities:", nrow(user_day_data)),
    y = "Numeric ID of the item that was studied"
  ) +
  theme(legend.position = "bottom")
