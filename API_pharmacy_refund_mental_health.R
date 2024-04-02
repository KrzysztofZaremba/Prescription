library(httr)
library(ggplot2)


# Define the API URL
api_url <- "https://api.nfz.gov.pl/app-stat-api-ra/monthly-provisions"

### Age groups
# 1 - Under 1
# 2 - 1 - 6
# 3 - 7 - 18
# 4 - 19 - 40
# 5 - 41 - 60
# 6 - 61 - 80
# 7 - 81 and more
# 8 - unknown



### Regions
# 01 – dolnośląskie
# 02 – kujawsko-pomorskie
# 03 – lubelskie
# 04 – lubuskie
# 05 - łódzkie
# 06 – małopolskie
# 07 – mazowieckie
# 08 – opolskie
# 09 – podkarpackie
# 10 – podlaskie
# 11 – pomorskie
# 12 – śląskie
# 13 – świętokrzyskie
# 14 – warmińsko-mazurskie
# 15 – wielkopolskie
# 16 – zachodniopomorskie

query_params_m <- list(
  #province = "06",
  dateFrom = "2015",
  dateTo = "2024",
  ageGroup = "3",
  activeSubstance="Sertralinum", #name of the active substance https://api.nfz.gov.pl/app-stat-api-ra/#active-substances
  gender = "M",  # Specify "M" for males
  format = "json",
  `api-version` = "1.0"
)

query_params_f <- list(
  #province = "06",
  dateFrom = "2015",
  dateTo = "2024",
  ageGroup = "3",
  activeSubstance="Sertralinum",
  gender = "F",  # Specify "F" for females
  format = "json",
  `api-version` = "1.0"
)

# Initialize empty lists to store the data for each gender
data_m <- list()
data_f <- list()
# Initialize an empty list to store the data from all pages
all_data <- list()

# Set the total number of pages you want to retrieve
total_pages <- 50  # Adjust this as needed
rm(response)

all_data_m=list()
all_data_f=list()
# Loop through pages
for (page in 1:total_pages) {
  query_params_m$page <- page  # Set the page parameter
  
  # Make the API GET request
  response_m <- GET(url = api_url, query = query_params_m, accept("text/plain"))
  
  
  # Check if the request was successful
  if (response_m$status_code == 200) {
    # Parse the JSON response
    data <- content(response_m, "parsed")
    
    # Append the data to the list
    all_data_m=append(all_data_m,data$data)
    
  } else {
    cat("Error on page", page, ":", response_m$reason, "\n")
  }
}

for (page in 1:total_pages) {
  query_params_f$page <- page  # Set the page parameter
  
  # Make the API GET request
  response_f <- GET(url = api_url, query = query_params_f, accept("text/plain"))
  
  
  # Check if the request was successful
  if (response_f$status_code == 200) {
    # Parse the JSON response
    data <- content(response_f, "parsed")
    
    # Append the data to the list
    all_data_f=append(all_data_f,data$data)
    
  } else {
    cat("Error on page", page, ":", response_f$reason, "\n")
  }
}


# Combine all the data from different pages into a single data frame
combined_data_m <- do.call(rbind, lapply(all_data_m, as.data.frame))
combined_data_m$Gender="Male"
combined_data_f <- do.call(rbind, lapply(all_data_f, as.data.frame))
combined_data_f$Gender="Female"
combined_data=rbind(combined_data_m,combined_data_f)

# Print the combined data frame
ggplot(combined_data, aes(x = as.Date(paste(year, month, "31", sep = "-")), y = quantity, color=Gender)) +
  geom_line(size=2) +
  labs(
    x = "Date",
    y = "Number of Packages",
    title = "Monthly prescription for an anti-depressant in Poland",
    subtitle="Prescription with Sertralinum in age group 7-18"
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "3 months",  # Adjust the interval as needed
    limits = as.Date(c("2017-01-01", "2022-12-31"))  # Set the date range
  ) +
  scale_y_continuous(limits = c(0, NA)) +  # Start y-axis at 0
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=30))
      #+increase text size
  #geom_vline(xintercept=as.numeric(as.Date("2022-02-27")))

