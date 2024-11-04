# URL of the first ZIP file to download
zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid_erased.zip"
zip_destfile <- "cl_areal_grid_erased.zip"
csv_file_in_zip <- "data/cl_areal_grid_erased.csv" # Specify expected CSV file inside

# Download and unzip the ZIP file only if the CSV doesn't already exist
if (!file.exists(csv_file_in_zip)) {
  if (!file.exists(zip_destfile)) {
    download.file(zip_url, zip_destfile, method = "auto")
    message("ZIP file downloaded.")
  } else {
    message("ZIP file already exists. Skipping download.")
  }
  unzip(zip_destfile, exdir = "data")
} else {
  message("CSV file already exists. Skipping download and unzip.")
}

# URL of the second ZIP file to download
zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"
zip_destfile <- "cl_areal_grid.zip"
csv_file_in_zip <- "data/cl_areal_grid.csv" # Specify expected CSV file inside

# Download and unzip the ZIP file only if the CSV doesn't already exist
if (!file.exists(csv_file_in_zip)) {
  if (!file.exists(zip_destfile)) {
    download.file(zip_url, zip_destfile, method = "auto")
    message("ZIP file downloaded.")
  } else {
    message("ZIP file already exists. Skipping download.")
  }
  unzip(zip_destfile, exdir = "data")
} else {
  message("CSV file already exists. Skipping download and unzip.")
}

# URL and path for first CSV file
csv_url <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv"
csv_destfile <- "data/cl_species.csv"

# Download the CSV file if it doesn't already exist
if (!file.exists(csv_destfile)) {
  download.file(csv_url, csv_destfile, method = "auto")
} else {
  message("CSV file already exists. Skipping download.")
}

# URL and path for second CSV file
csv_url <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv"
csv_destfile <- "data/cl_cwp_gear_level2.csv"

# Download the CSV file if it doesn't already exist
if (!file.exists(csv_destfile)) {
  download.file(csv_url, csv_destfile, method = "auto")
} else {
  message("CSV file already exists. Skipping download.")
}
