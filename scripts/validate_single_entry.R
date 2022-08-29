source("scripts/libraries.R")
source("scripts/utils.R")


# Get final submission feedback file --------------------------------------

# This file has a list of all courts and their sheet links

final_submission_file_id <- "1O7Q24pK4I4vcARppm7tk5q009NVwEDd-XYgIPRAeRlc"

final_submission_file_download <-
  drive_download(
    file = as_id(final_submission_file_id),
    path = "data/final_submission_feedback.xlsx",
    overwrite = TRUE
  )

final_submission_file <- readxl::read_xlsx("data/final_submission_feedback.xlsx", sheet = "Final submission feedback")
final_submission_file <- final_submission_file[,c("ApplicantName","Court","SheetLink")]

courts_to_validate <- c("Chhattisgarh")

court_name <- "Chhattisgarh"
validate_single_entry <- function(court_name){
  file_path <- glue("data/student_files/{court_name}.xlsx")
  file_link <-
    final_submission_file$SheetLink[final_submission_file$Court == court_name]
  court_file_download <-
    drive_download(file = file_link,
                   path = file_path,
                   overwrite = TRUE)
  court_file <- readxl::read_xlsx(path = file_path, sheet = "Datasheet")
  
  summary <- gtsummary::tbl_summary(court_file)

  summary
}

