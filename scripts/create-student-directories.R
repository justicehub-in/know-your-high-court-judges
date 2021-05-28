library(googledrive)
library(readxl)
library(dplyr)

#Auth
googledrive::drive_auth()

# Get details of volunteers (email ID and Name)

volunteer_details_file_id <- "1O7Q24pK4I4vcARppm7tk5q009NVwEDd-XYgIPRAeRlc"

get_student_file <-
  drive_download(
    file = as_id(volunteer_details_file_id),
    path = "data/student_applications.xlsx",
    overwrite = TRUE
  )

student_file <- readxl::read_xlsx(path = "data/student_applications.xlsx",sheet = "Combined Application")
student_file <- student_file[, c(1:3)] %>% data.frame()
names(student_file)[] <- c("selected","email","name")
student_file <- student_file[student_file$selected == "Y",]
student_file$name <- stringr::str_to_title(student_file$name)
student_file$directory_title <- stringr::str_replace_all(
  stringr::str_to_lower(student_file$name),
  pattern = " ",
  replacement = "_"
)

student_file$directory_title <-
  glue::glue("sod_data_{student_file$directory_title}")

# Create folders for all students

base_dir_id <- "1FnvBBGrdBpdvhQ9q5H15c5oOhEGFLcrv"

create_student_drive <- function(folder_name){
  drive_mkdir(folder_name, path = as_id(base_dir_id))
}

lapply(student_file$directory_title, create_student_drive)


# Get directory Id's for all

student_directory_ids <- drive_ls(path = as_id(base_dir_id))
student_directory_ids[["drive_resource"]] <- NULL
student_directory_ids <- student_directory_ids %>% bind_rows() %>% data.frame()
student_file <- left_join(student_file, student_directory_ids, by=c('directory_title'='name'))

# test_id <- "1CmK2tQQI9Aqz9C9FHKD296IfZPKmwPt2"


# drive_share(file = as_id(test_id),
#     role = "commenter",
#     type = "user",
#     emailAddress = ""
#   )
