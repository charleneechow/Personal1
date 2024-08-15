# List the CSV files extracted from the zip file
csv_files <- list.files("./dataverse_files", full.names = TRUE)

# Create an empty dataframe to store the combined data
combined_data <- data.frame()

# Loop through each CSV file, read it, and combine it into the 'combined_data' dataframe
for (csv_file in csv_files) {
  print(csv_file)
  # Read the CSV file
  data <- read.csv(csv_file)
  
  # Combine the data with existing data
  combined_data <- bind_rows(combined_data, data)
}

str(data)
# View the structure of the combined dataframe
#str(combined_data)

# View the first few rows of the combined dataframe
#head(combined_data)
