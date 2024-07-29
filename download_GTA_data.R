# URL of the ZIP file to download
zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid_erased.zip"

# Path where the ZIP file will be saved locally
zip_destfile <- "cl_areal_grid_erased.zip"

# Download the ZIP file if it doesn't already exist
if (!file.exists(zip_destfile)) {
  download.file(zip_url, zip_destfile, method = "auto")
  # Optional: Unzip the downloaded file
  unzip(zip_destfile, exdir = "data")
} else {
  message("ZIP file already exists. Skipping download.")
}

# URL of the ZIP file to download
zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"

# Path where the ZIP file will be saved locally
zip_destfile <- "cl_areal_grid.zip"

# Download the ZIP file if it doesn't already exist
if (!file.exists(zip_destfile)) {
  download.file(zip_url, zip_destfile, method = "auto")
  # Optional: Unzip the downloaded file
  unzip(zip_destfile, exdir = "data")
} else {
  message("ZIP file already exists. Skipping download.")
}

# URL of the CSV file to download
csv_url <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv" 

# Path where the CSV file will be saved locally
csv_destfile <- "data/cl_species.csv"

# Download the CSV file if it doesn't already exist
if (!file.exists(csv_destfile)) {
  download.file(csv_url, csv_destfile, method = "auto")
} else {
  message("CSV file already exists. Skipping download.")
}

# URL of the CSV file to download
csv_url <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv" 

# Path where the CSV file will be saved locally
csv_destfile <- "data/cl_cwp_gear_level2.csv"

# Download the CSV file if it doesn't already exist
if (!file.exists(csv_destfile)) {
  download.file(csv_url, csv_destfile, method = "auto")
} else {
  message("CSV file already exists. Skipping download.")
}
