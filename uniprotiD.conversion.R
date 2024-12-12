#### Load necessary packages
library(readxl)
library(openxlsx)
library(writexl)
library(UniprotR)
library(dplyr)
library(httr)
library(stringr)
library(progress)
library(furrr)
library(future)

setwd("/Users/teestanaskar/Dropbox/Teesta/Placenta/Human.Placenta/bothsex/Proteomics/")

# Define the file path and load the data
input_file <- read.xlsx("data/Total_Proteomics_MSQNScombined.xlsx", sheet = 4)
input_file = input_file[66:nrow(input_file),]

# Define a function to create entry names from accession codes
create_entry_names <- function(accession_list) {
  paste0(accession_list, "_HUMAN")
}
# Extract the Accession column and create entry names
accession_codes <- input_file$Accession
entry_names <- create_entry_names(accession_codes)

# Initialize results lists
results <- list()
accession_numbers <- list()

# Initialize the progress bar
pb <- progress_bar$new(
  format = "  Processing [:bar] :percent ETA: :eta",
  total = length(entry_names), # Total number of items in the loop
  clear = FALSE,
  width = 60
)

# Set up parallel processing plan
plan(multisession, workers = 10)  # Set the number of parallel workers (adjust as needed)

# Parallel processing function
process_entry <- function(entry) {
  url <- paste0("https://www.uniprot.org/uniprot/", entry, ".txt")
  
  # Fetch the data from UniProt
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Extract the content
    #content <- content(response, as = "text", encoding = "UTF-8")
    text_to_extract <- sub(".*/uniprotkb/([^.]+)\\.txt.*", "\\1", response$url)
    # Extract the accession code using a regular expression (looking after "AC")
    #accession_match <- str_match(content, "AC\\s+([A-Z0-9]+)")[,2]
    
    if (!is.na(text_to_extract)) {
      return(list(entry = entry, accession = text_to_extract))
    } else {
      return(list(entry = entry, accession = "Not_Found"))
    }
  } else {
    return(list(entry = entry, accession = "Not_Found"))
  }
}


# Parallelize the loop using future_map (from furrr)
results <- future_map(entry_names, ~{
  res <- process_entry(.x)
  pb$tick()  # Update the progress bar
  res
}, .progress = TRUE)

# After parallel processing, store results
accession_numbers <- sapply(results, function(res) res$accession)
names(accession_numbers) <- sapply(results, function(res) res$entry)

# Convert results to a data frame
df_results <- data.frame(
  Accession = names(accession_numbers),
  UniProt_Accession = accession_numbers,
  stringsAsFactors = FALSE
)

# Add the results as new columns to the existing data frame
input_file$UniProt_Name <- df_results$Accession

input_file$UniProt_Accession <- df_results$UniProt_Accession

# Save the updated data frame back to the same Excel file (overwrite)
write.xlsx(input_file, "data/unique_in_cannabis_withuniprotID.xlsx") 

cat("Excel file updated successfully.\n")
