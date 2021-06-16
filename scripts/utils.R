
# GLobal vars -------------------------------------------------------------

base_dir_id <- "1FnvBBGrdBpdvhQ9q5H15c5oOhEGFLcrv"
volunteer_details_file_id <- "1O7Q24pK4I4vcARppm7tk5q009NVwEDd-XYgIPRAeRlc"
variables_to_check <-
  c(
    "Gender",
    "Religion",
    "Caste",
    "DateofBirth",
    "DateofAppointment",
    "DateofRetirement",
    "IfDiedinOffice",
    "Ifresignedfromoffice",
    "IftransferredtoanyotherHighCourt",
    "Dateofsuchtransfer1",
    "Dateofsuchtransfer2",
    "Dateofsuchtransfer3",
    "IfappointedChiefJusticeinanotherHighCourt",
    "IfappointedtotheSupremeCourt",
    "DateofappointmenttotheSupremeCourt",
    "Cadre",
    "ExperienceinSubordinateJudiciary",
    "LitigationExperience",
    "IfaSeniorAdvocate",
    "ExperienceinHighCourtAdministrativePost",
    "IfservedasGovernmentCounsel",
    "IfservedasAdvocateGeneral",
    "IfempanelledbyPSUs",
    "IfempanelledbyBanks",
    "IfempanelledbyanyStatutoryBody",
    "IfempanelledbyPrivateCompanies",
    "ChamberDetails",
    "ForeignDegreeinLaw",
    "PostGraduateinanothersubject",
    "PostGraduateinLaw"
  )
colour_match <- "#8BB174"
colour_nomatch <- "#DE6C83"
colour_NA <- "#ffffff"

# Gmail Auth --------------------------------------------------------------

# create_smtp_creds_key(id = "gmail_creds", user = "apoorv@civicdatalab.in",provider = "gmail", overwrite = TRUE)


# Create base student file ------------------------------------------------

create_student_file <- function(){
  
  get_student_file <-
    drive_download(
      file = as_id(volunteer_details_file_id),
      path = "data/student_applications.xlsx",
      overwrite = TRUE
    )
  
  student_file <- readxl::read_xlsx(path = "data/student_applications.xlsx",sheet = "Combined Application")
  
  student_file <- student_file[, c(1:7)] %>% data.frame()
  names(student_file)[] <- c("selected","marticulated","dropped","highcourt","email","phone","name")
  
  student_file <- student_file[student_file$selected == "Y",]
  student_file$name <- stringr::str_to_title(student_file$name)
  student_file$directory_title <- stringr::str_replace_all(
    stringr::str_to_lower(student_file$name),
    pattern = " ",
    replacement = "_"
  )
  
  student_file$directory_title <-
    glue::glue("sod_data_{student_file$directory_title}")
  
  return(student_file)
}

get_directory_ids <- function(){
  student_directory_ids <- drive_ls(path = as_id(base_dir_id))
  student_directory_ids[["drive_resource"]] <- NULL
  student_directory_ids <- student_directory_ids %>% bind_rows() %>% data.frame()
  student_directory_ids$directory_url <- glue::glue("https://drive.google.com/drive/folders/{student_directory_ids$id}")
  return(student_directory_ids)
}
