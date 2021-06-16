
# Declare libraries -------------------------------------------------------

source("scripts/libraries.R")


# Source utils ------------------------------------------------------------

source("scripts/utils.R")


# Authorization -----------------------------------------------------------


googledrive::drive_auth()


# Get volunteers details --------------------------------------------------
student_file <- create_student_file()


# Create directories for all students -----------------------------------------

create_student_drive <- function(folder_name){
  drive_mkdir(folder_name, path = as_id(base_dir_id))
}

lapply(student_file$directory_title, create_student_drive)

# Get directory Id's for all ----------------------------------------------
student_directory_ids <- get_directory_ids()

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



# Send onboarding e-mails ----------------------------------------------

source("scripts/email_templates.R")

student_emails <- student_file %>% filter(marticulated == 'Y') %>% pull(email)

lapply(X = student_emails, FUN = send_onboarding_email)
