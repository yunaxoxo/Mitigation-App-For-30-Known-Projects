library(here)

# reads file from a csv file (project data)

project_data <- read.csv(here("data", "project_data.csv"))
pollutant_targets <- read.csv(here("data", "pollutant_targets.csv"))


# display in 4 decimals for uniformity
project_data[] <- lapply(project_data, function(col) {
  if(is.numeric(col))  round(col, 4) else col
})


 
