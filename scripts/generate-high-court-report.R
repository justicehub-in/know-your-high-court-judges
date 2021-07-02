
# Source libraries & Utils --------------------------------------------------------

source("scripts/libraries.R")
source("scripts/utils.R")

# Get student details -----------------------------------------------------

# student_details <- create_student_file()
# student_directories <- get_directory_ids()
# student_details <- left_join(student_details, student_directories, by=c('directory_title'='name'))
# student_with_courts <- student_details %>% filter(!is.na(student_details$highcourt))

student_work_status <- get_work_status()
generate_report_courts <- student_work_status %>% filter(Status=="Completed" & CourtReport=="Yes")


courts_to_generate_report <- generate_report_courts$Court %>% unique()

court_reports_df <- purrr::map(courts_to_generate_report, generate_court_report)

jsonlite::write_json(court_reports_df, "data/student_files/courts_data_verification_summary.json")

# # Collect details for each file -------------------------------------------
# 
# get_student_file_details <- function(dir_path)
# {
#   print(dir_path)
#   all_files <- drive_ls(path = as_id(dir_path))
#   all_files['dir_path'] <- dir_path
#   return(all_files)
# }
# 
# student_file_details <- purrr::map(student_with_courts$id, get_student_file_details)
# court_files_df <- c()
# for(i in 1:length(student_file_details)){
#   total_files <- length(student_file_details[[i]]$name)
#   if(total_files>0){
#   for(j in 1:total_files){
#     directory_id <- student_file_details[[i]]$dir_path[[j]]
#     file_id <- student_file_details[[i]]$id[[j]]
#     file_title <- student_file_details[[i]]$name[[j]]
#     file_url <- student_file_details[[i]]$drive_resource[[j]]$webViewLink
#     mime_type <-  student_file_details[[i]]$drive_resource[[j]]$mimeType
#     file_df <-
#       data.frame(
#         "dir_id" = directory_id,
#         "file_id" = file_id,
#         "file_title" = file_title,
#         "file_url" = file_url,
#         "mime_type" = mime_type
#       )
#     court_files_df <- bind_rows(court_files_df, file_df)
#   }
# }
# }
# 
# # Remove PDF's and other docs. Only keep spreadsheets ---------------------
# 
# court_files_df <-
#   court_files_df %>% filter(!mime_type %in% c('application/pdf', 'application/vnd.google-apps.document'))
# 
# 
# # Add high court details to the dataframe ---------------------------------
# 
# court_files_df <- left_join(court_files_df, student_with_courts[,c("id","name","directory_url")], by=c("dir_id"="id"))



# Validate data files -----------------------------------------------------

# rules <- validator(.file = "scripts/rules.yaml")
# x <- confront(student_file, rules)
# 
# is_date = function(x, format) {
#   formatted = try(as.Date(x, format), silent = TRUE)
#   return(as.character(formatted) == x)
# }
# 

# Prepare report ----------------------------------------------------------

# court_files_df$state <- ""
# court_files_df$state[grepl(pattern = "madhya",x = court_files_df$file_title, ignore.case = TRUE)] <- "MP"
# mp_court_files <- court_files_df[court_files_df$state=='MP',]

# List of judges






# list_of_judges <- judge_datasets[,c('NameoftheJudge','DateofBirth','file_title')]
# readr::write_csv(list_of_judges, "data/student_files/list_of_judges_mp.csv")
#
# # Finding common judges using DoB
# common_dob <- list_of_judges %>% group_by(DateofBirth) %>% summarise(total=n()) %>% filter(total>1) %>% pull(DateofBirth)
# common_dob <- common_dob[!is.na(common_dob)]
# judges_with_common_dob <- list_of_judges[list_of_judges$DateofBirth %in% common_dob,]
# readr::write_csv(judges_with_common_dob,file = "data/student_files/process_judge_names.csv")
#
# # Read file with updated judge names
# common_judges <- readr::read_csv("data/student_files/process_judge_names.csv")
#
# # Merge with judge datasets to get ID for each judge
# judge_id_datasets <-
#   inner_join(
#     judge_datasets,
#     common_judges,
#     by = c(
#       'NameoftheJudge' = 'judge_name',
#       'DateofBirth' = 'judge_dob',
#       'file_title' = 'file'
#     )
#   )


# Function to check if judge details match across all entries

# Create a matrix for all selected judges

# judges_matrix <- judge_id_datasets$judge_id %>% unique() %>% map(verify_judge_details) %>% bind_rows()
# readr::write_csv(judges_matrix,"data/student_files/judges_matrix.csv")


# Render Files

files <- list.files(path = "scripts/", pattern = "[.]Rmd$", full.names = TRUE)
for (f in files) rmarkdown::render(f)
