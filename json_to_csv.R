#!/usr/bin/env Rscript

# =============================================================================
# JSON TO CSV CONVERTER FOR INSPECT-SR FRAMEWORK
# =============================================================================
# 
# This script converts JSON response files from LLM assessments to CSV format
# for analysis in the main INSPECT-SR framework.
#
# Author: Gagan Dhaliwal
# Date: 2024
# Purpose: Convert LLM JSON responses to structured CSV format
# =============================================================================

# Load required packages
suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
})

cat("🔄 JSON to CSV Converter for INSPECT-SR Framework\n")
cat("================================================\n\n")

# Function to safely parse JSON
safe_parse_json <- function(json_text) {
  tryCatch({
    fromJSON(json_text, simplifyVector = FALSE)
  }, error = function(e) {
    cat("⚠️  JSON parsing error:", e$message, "\n")
    return(NULL)
  })
}

# Function to extract assessment data from JSON
extract_assessment <- function(json_data, study_id) {
  if (is.null(json_data)) return(NULL)
  
  # Extract the assessment section
  assessment <- json_data$assessment
  
  if (is.null(assessment)) {
    cat("⚠️  No assessment section found for study", study_id, "\n")
    return(NULL)
  }
  
  # Create data frame with extracted values
  result <- data.frame(
    study_id = study_id,
    retraction = ifelse(!is.null(assessment$retraction), assessment$retraction, NA),
    expression_of_concern = ifelse(!is.null(assessment$expression_of_concern), assessment$expression_of_concern, NA),
    team_integrity = ifelse(!is.null(assessment$team_integrity), assessment$team_integrity, NA),
    registration_timing = ifelse(!is.null(assessment$registration_timing), assessment$registration_timing, NA),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Function to process JSON files
process_json_files <- function(input_dir, output_file) {
  cat("📁 Processing JSON files from:", input_dir, "\n")
  
  # List all JSON files
  json_files <- list.files(input_dir, pattern = "\\.json$", full.names = TRUE)
  
  if (length(json_files) == 0) {
    cat("❌ No JSON files found in", input_dir, "\n")
    return(FALSE)
  }
  
  cat("📊 Found", length(json_files), "JSON files\n\n")
  
  # Process each JSON file
  all_assessments <- list()
  
  for (file_path in json_files) {
    study_id <- tools::file_path_sans_ext(basename(file_path))
    cat("🔄 Processing:", study_id, "\n")
    
    # Read JSON content
    json_text <- readLines(file_path, warn = FALSE)
    json_data <- safe_parse_json(paste(json_text, collapse = "\n"))
    
    # Extract assessment data
    assessment_data <- extract_assessment(json_data, study_id)
    
    if (!is.null(assessment_data)) {
      all_assessments[[study_id]] <- assessment_data
      cat("✅ Extracted assessment data\n")
    } else {
      cat("❌ Failed to extract data\n")
    }
  }
  
  # Combine all assessments
  if (length(all_assessments) == 0) {
    cat("❌ No valid assessments found\n")
    return(FALSE)
  }
  
  combined_data <- bind_rows(all_assessments)
  
  # Save to CSV
  write_csv(combined_data, output_file)
  cat("\n✅ Successfully saved", nrow(combined_data), "assessments to:", output_file, "\n")
  
  return(TRUE)
}

# Function to validate CSV output
validate_csv_output <- function(csv_file) {
  cat("\n🔍 Validating CSV output...\n")
  
  # Read the CSV
  data <- read_csv(csv_file, show_col_types = FALSE)
  
  cat("📊 CSV Structure:\n")
  cat("  - Rows:", nrow(data), "\n")
  cat("  - Columns:", ncol(data), "\n")
  cat("  - Column names:", paste(names(data), collapse = ", "), "\n\n")
  
  cat("📋 Data Summary:\n")
  print(summary(data))
  
  cat("\n🔍 Missing Data Check:\n")
  missing_summary <- data %>%
    summarise_all(~sum(is.na(.))) %>%
    gather(variable, missing_count) %>%
    mutate(missing_pct = round(missing_count / nrow(data) * 100, 1))
  
  print(missing_summary)
  
  return(data)
}

# Main execution
main <- function() {
  # Default paths
  input_directory <- "json_responses"
  output_csv <- "llm_assessments.csv"
  
  # Check if input directory exists
  if (!dir.exists(input_directory)) {
    cat("❌ Input directory not found:", input_directory, "\n")
    cat("💡 Please create the directory and add JSON response files\n")
    cat("💡 Or modify the script to point to your JSON files location\n")
    return(FALSE)
  }
  
  # Process JSON files
  success <- process_json_files(input_directory, output_csv)
  
  if (success) {
    # Validate output
    validate_csv_output(output_csv)
    
    cat("\n🎉 JSON to CSV conversion completed successfully!\n")
    cat("📁 Output file:", output_csv, "\n")
    cat("💡 You can now use this CSV in your main INSPECT-SR analysis\n")
  } else {
    cat("\n❌ JSON to CSV conversion failed\n")
    cat("💡 Check the error messages above and verify your JSON files\n")
  }
  
  return(success)
}

# Run main function if script is executed directly
if (!interactive()) {
  main()
} else {
  cat("💡 Script loaded in interactive mode. Run main() to execute.\n")
}

# =============================================================================
# USAGE INSTRUCTIONS:
# =============================================================================
# 
# 1. Place your JSON response files in a directory (default: "json_responses/")
# 2. Run the script: Rscript json_to_csv.R
# 3. The script will generate llm_assessments.csv
# 4. Use this CSV in your main INSPECT-SR analysis
#
# JSON FORMAT EXPECTED:
# {
#   "assessment": {
#     "retraction": "yes/no",
#     "expression_of_concern": "yes/no", 
#     "team_integrity": "yes/no",
#     "registration_timing": "yes/no"
#   }
# }
#
# =============================================================================
