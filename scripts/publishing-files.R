source("scripts/libraries.R")

# This is the script to:

# 1 . Convert the excel files to csv. These files were 
# created by the team at NLU-O after doing the final review of 
# all files
# 2. Creating a master file which contains details of all judges. 


# Reading xlsx files --------------------------------------------------------

xlsx_folder_path <- "data/final-files-nluo/xlsx"
csv_folder_path <- "data/final-files-nluo/csv"
all_court_titles <- dir(xlsx_folder_path)
# all_court_titles <- all_court_titles[!all_court_titles %in% c("Final (J&K and Ladakh High Court).xlsx")]
all_col_names <-
  c(
    "Name of the Judge",
    "Gender",
    "Date of Birth",
    "State of Birth",
    "Place of Birth",
    "Date of Appointment",
    "Date of Retirement",
    "If Died in Office",
    "If resigned from office",
    "Parent High Court",
    "If transferred to any other High Court",
    "If yes, which High Court 1",
    "Date of such transfer - 1",
    "If yes, which High Court 2",
    "Date of such transfer – 2",
    "If yes, which High Court 3",
    "Date of such transfer - 3",
    "If appointed Chief Justice in another High Court",
    "If yes, which High Court – 1",
    "If yes, which High Court – 2",
    "If yes, which High Court – 3",
    "If appointed to the Supreme Court",
    "Date of appointment to the Supreme Court",
    "Cadre",
    "Experience in Subordinate Judiciary",
    "Litigation Experience",
    "If a Senior Advocate",
    "Experience in High Court Administrative Post",
    "If served as Counsel for Government/PSU/Statutory Body",
    "If served as Advocate General",
    "If empanelled by Banks",
    "If empanelled by Private Companies",
    "Chamber Details",
    "Schooling Information",
    "Graduation Institution",
    "Graduation Specialization",
    "Law Degree Institution",
    "Law Degree Year",
    "Bar Enrolment Year",
    "State Bar Association where enrolled",
    "Foreign Degree in Law",
    "Post-Graduate in another subject",
    "Post-Graduate in Law"
  )

col_types_vec <- c("text", "text", "date", "text", "text", "date", "date", "text", "text", "text", "text", "text", "date", "text", "date", "text", "date", "text", "text", "text", "text", "text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")

# court_title <- all_court_titles[[1]]

excel_to_csv <- function(court_title){

    print(court_title)
  
    xlsx_file_path <- glue::glue("{xlsx_folder_path}/{court_title}")
    xlsx_file <- readxl::read_xlsx(path = xlsx_file_path,col_types = col_types_vec)
    file_nrow <- nrow(xlsx_file)
    file_ncol <- ncol(xlsx_file)
    file_col_names <- names(xlsx_file)
    file_col_names_str <- file_col_names %>% 
      paste0(collapse = "__")
    
    # Update file title
    updated_court_title <- court_title %>% 
      stringr::str_squish() %>% 
      stringr::str_replace_all(pattern = "Final \\(",replacement = "") %>% 
      stringr::str_replace_all(pattern = "\\)\\.xlsx",replacement = "") %>% 
      stringr::str_remove_all(pattern = " ")

    # Check col names
    col_status <- "col_check_passed"
    if(FALSE %in% (file_col_names %in% all_col_names)){
      col_status <- "col_check_failed"
    }
    
    # Curate file details
    file_details <- data.frame("title"=updated_court_title,
                               "rows"=file_nrow,
                               "cols"=file_ncol,
                               "col_names"=file_col_names_str,
                               "col_check"=col_status)
    
        
    # Convert to csv and write to disk
    readr::write_csv(x = xlsx_file, 
                     file = glue::glue("{csv_folder_path}/{updated_court_title}.csv"))
    
    return(file_details)
}

all_file_details <- lapply(X = all_court_titles,
                           FUN = excel_to_csv)

# Create a master file of judges

court_list_csv <- dir("data/final-files-nluo/csv/")
court_title <- court_list_csv %>% 
  stringr::str_remove("HighCourt\\.csv") %>% 
  stringr::str_remove("\\.csv") 

all_judges_df <- c()
for(i in 1:length(court_list_csv)){
  print(court_list_csv[i])
  court_file_i <-
    readr::read_csv(glue::glue("{csv_folder_path}/{court_list_csv[i]}"),
                    col_types = cols(.default = "c"))
  court_file_i$fileTitle <- court_title[[i]]
  all_judges_df <- dplyr::bind_rows(all_judges_df, court_file_i)
}

readr::write_csv(all_judges_df,
                 "data/final-files-nluo/judges_master_file.csv")
