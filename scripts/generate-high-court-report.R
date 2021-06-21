
# Source libraries & Utils --------------------------------------------------------

source("scripts/libraries.R")
source("scripts/utils.R")

# Get student details -----------------------------------------------------

student_details <- create_student_file()
student_directories <- get_directory_ids()
student_details <- left_join(student_details, student_directories, by=c('directory_title'='name'))
student_with_courts <- student_details %>% filter(!is.na(student_details$highcourt))

# Collect details for each file -------------------------------------------

get_student_file_details <- function(dir_path)
{
  print(dir_path)
  all_files <- drive_ls(path = as_id(dir_path))
  all_files['dir_path'] <- dir_path
  return(all_files)
}

student_file_details <- purrr::map(student_with_courts$id, get_student_file_details)
court_files_df <- c()
for(i in 1:length(student_file_details)){
  total_files <- length(student_file_details[[i]]$name)
  if(total_files>0){
  for(j in 1:total_files){
    directory_id <- student_file_details[[i]]$dir_path[[j]]
    file_id <- student_file_details[[i]]$id[[j]]
    file_title <- student_file_details[[i]]$name[[j]]
    file_url <- student_file_details[[i]]$drive_resource[[j]]$webViewLink
    mime_type <-  student_file_details[[i]]$drive_resource[[j]]$mimeType
    file_df <-
      data.frame(
        "dir_id" = directory_id,
        "file_id" = file_id,
        "file_title" = file_title,
        "file_url" = file_url,
        "mime_type" = mime_type
      )
    court_files_df <- bind_rows(court_files_df, file_df)
  }
}
}

# Remove PDF's and other docs. Only keep spreadsheets ---------------------

court_files_df <-
  court_files_df %>% filter(!mime_type %in% c('application/pdf', 'application/vnd.google-apps.document'))


# Add high court details to the dataframe ---------------------------------

court_files_df <- left_join(court_files_df, student_with_courts[,c("id","name","directory_url")], by=c("dir_id"="id"))



# Validate data files -----------------------------------------------------

rules <- validator(.file = "scripts/rules.yaml")
x <- confront(student_file, rules)

is_date = function(x, format) {
  formatted = try(as.Date(x, format), silent = TRUE)
  return(as.character(formatted) == x)
}


# Prepare report ----------------------------------------------------------

# Sample report for MP High Court
name_repair_function <-
  function(nms) {
    str_replace_all(nms, pattern = " ", replacement = "") %>%
      str_replace_all(pattern = "–", replacement = "") %>% 
      str_replace_all(pattern = "-", replacement = "") %>%
      str_replace_all(pattern = ",", replacement = "")
  }
court_files_df$state <- ""
court_files_df$state[grepl(pattern = "madhya",x = court_files_df$file_title, ignore.case = TRUE)] <- "MP"
mp_court_files <- court_files_df[court_files_df$state=='MP',]

# List of judges
judge_datasets <- c()
for(i in 1:nrow(mp_court_files)){
  file_title <- glue("{str_replace_all(str_to_lower(mp_court_files$name[[i]]),' ','')}_MP")
  file_path <- glue("data/student_files/{file_title}.xlsx")
  student_file <- drive_download(file = as_id(mp_court_files$file_id[i]), path = file_path ,overwrite = TRUE)
  student_file <- readxl::read_xlsx(path = file_path,sheet = "Datasheet",.name_repair = name_repair_function)
  student_file$file_title <- file_title
  student_file$NameoftheJudge <- student_file$NameoftheJudge %>% str_to_lower() %>% str_squish()
  judge_datasets <- bind_rows(judge_datasets, student_file)  
}

list_of_judges <- judge_datasets[,c('NameoftheJudge','DateofBirth','file_title')]
readr::write_csv(list_of_judges, "data/student_files/list_of_judges_mp.csv")

# Finding common judges using DoB
common_dob <- list_of_judges %>% group_by(DateofBirth) %>% summarise(total=n()) %>% filter(total>1) %>% pull(DateofBirth)
common_dob <- common_dob[!is.na(common_dob)]
judges_with_common_dob <- list_of_judges[list_of_judges$DateofBirth %in% common_dob,]
readr::write_csv(judges_with_common_dob,file = "data/student_files/process_judge_names.csv")

# Read file with updated judge names
common_judges <- readr::read_csv("data/student_files/process_judge_names.csv")

# Merge with judge datasets to get ID for each judge
judge_id_datasets <-
  inner_join(
    judge_datasets,
    common_judges,
    by = c(
      'NameoftheJudge' = 'judge_name',
      'DateofBirth' = 'judge_dob',
      'file_title' = 'file'
    )
  )

total_judges <- length(unique(judge_id_datasets$judge_id))

# Function to check if judge details match across all entries
verify_judge_details <- function(jid){
judge_details <- judge_id_datasets %>% filter(judge_id==jid)  
judge_matrix <- c()
for (i in 1:length(variables_to_check)){
  # print(i)
  var_title <- variables_to_check[i]
  var_unique_values <- unique(judge_details[,var_title]) %>% pull
  var_unique_values <- as.character(var_unique_values)
  var_unique_values[var_unique_values == "NA"] <- NA_character_
  if(length(var_unique_values) == 1 && !is.na(var_unique_values)){
    var_flag <- "1"
  } else if(length(var_unique_values) > 1 && (NA_character_ %in% var_unique_values)){
    var_flag <- "2"
  } else if(length(var_unique_values) == 1 && is.na(var_unique_values)){
    var_flag <- "3"
  } else {
    var_flag <- "0"
  }
  
  var_matrix <- data.frame("var_title"=var_title, "var_flag"=var_flag)
  judge_matrix <- bind_rows(judge_matrix, var_matrix)
}
judge_matrix$judge_name <- unique(judge_details$updated_judge_name)
judge_matrix$judge_id <- unique(judge_details$judge_id)
return(judge_matrix)
}

# Create a matrix for all selected judges

judges_matrix <- judge_id_datasets$judge_id %>% unique() %>% map(verify_judge_details) %>% bind_rows()
readr::write_csv(judges_matrix,"data/student_files/judges_matrix.csv")
