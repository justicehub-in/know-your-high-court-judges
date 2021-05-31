
# Declare libraries -------------------------------------------------------

library(googledrive)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(glue)


# Authorization -----------------------------------------------------------


googledrive::drive_auth()


# Get volunteers details --------------------------------------------------

volunteer_details_file_id <- "1O7Q24pK4I4vcARppm7tk5q009NVwEDd-XYgIPRAeRlc"

get_student_file <-
  drive_download(
    file = as_id(volunteer_details_file_id),
    path = "data/student_applications.xlsx",
    overwrite = TRUE
  )

student_file <- readxl::read_xlsx(path = "data/student_applications.xlsx",sheet = "Combined Application")
student_file <- student_file[, c(1:5)] %>% data.frame()
names(student_file)[] <- c("selected","marticulated","highcourt","email","name")
student_file <- student_file[student_file$selected == "Y",]
student_file$name <- stringr::str_to_title(student_file$name)
student_file$directory_title <- stringr::str_replace_all(
  stringr::str_to_lower(student_file$name),
  pattern = " ",
  replacement = "_"
)

student_file$directory_title <-
  glue::glue("sod_data_{student_file$directory_title}")



# Create directories for all students -----------------------------------------

base_dir_id <- "1FnvBBGrdBpdvhQ9q5H15c5oOhEGFLcrv"

create_student_drive <- function(folder_name){
  drive_mkdir(folder_name, path = as_id(base_dir_id))
}

lapply(student_file$directory_title, create_student_drive)




# Get directory Id's for all ----------------------------------------------

student_directory_ids <- drive_ls(path = as_id(base_dir_id))
student_directory_ids[["drive_resource"]] <- NULL
student_directory_ids <- student_directory_ids %>% bind_rows() %>% data.frame()
student_file <- left_join(student_file, student_directory_ids, by=c('directory_title'='name'))


# Assign directories to students ------------------------------------------

assign_directories_students <- function(directory_id){
  emailAddress <- student_file$email[student_file$id ==directory_id]
  drive_share(file = as_id(directory_id),
              role = "writer",
              type = "user",
              emailAddress = emailAddress
  )
}


# Filter students who have been marticulated ------------------------------

assign_ids <- student_file %>% filter(marticulated == 'Y') %>% pull(id)

lapply(X = assign_ids, assign_directories_students)



# Copy the template file in all student directories -----------------------

template_id <- "1p3v8RnEY-CnUULzgLAdpmtoIFdKHrfS_0Ig_zcXD_J4"

copy_template_file <- function(directory_id){
  drive_cp(
    file = as_id(template_id),
    path = as_id(directory_id),
    name = "summer-of-data-[high-court-name]",
    overwrite = FALSE
  )
}

lapply(X = assign_ids, copy_template_file)
