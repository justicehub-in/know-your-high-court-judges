
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
colour_nomatch <- "#7B0D1E"
colour_match <- "#08415C"
colour_NA_value <- "#E09F3E"
colour_blank <- "#E0E2DB"

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

get_work_status <- function(){
  student_work_status <-
    drive_download(
      file = as_id(volunteer_details_file_id),
      path = "data/student_work_status.xlsx",
      overwrite = TRUE
    )
  
  student_work_status <- readxl::read_xlsx(path = "data/student_work_status.xlsx",sheet = "Final submission feedback")
  return(student_work_status)
}


col_name_repair_function <-
  function(nms) {
    str_replace_all(nms, pattern = " ", replacement = "") %>%
      str_replace_all(pattern = "–", replacement = "") %>% 
      str_replace_all(pattern = "-", replacement = "") %>%
      str_replace_all(pattern = ",", replacement = "")
  }


# judge_name <- "anand pathak" 
# judge_dob <-  "1968-07-18"
verify_judge_details <- function(judge_datasets, judge_name,judge_dob){
  judge_details <- judge_datasets %>% filter(NameoftheJudge==judge_name & as.character(DateofBirth) == judge_dob)
  
  judge_matrix <- c()
  for (var_i in 1:length(variables_to_check)){
    # print(i)
    var_title <- variables_to_check[var_i]
    
    if(var_title %in% names(judge_details)){
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
      
    } else {
      var_flag <- "0"
    }
    var_matrix <- data.frame("var_title"=var_title, "var_flag"=var_flag)
    judge_matrix <- bind_rows(judge_matrix, var_matrix)
  }
  judge_matrix$judge_name <- unique(judge_name)
  judge_matrix$judge_dob <- unique(judge_dob)
  return(judge_matrix)
}

# court_name <- "Andhra Pradesh"
generate_court_report <- function(court_name){
  print(glue("{court_name \n}"))
  court_details <- list()
  court_files <- generate_report_courts[generate_report_courts$Court==court_name,]
  
  judge_datasets_i <- c()  
  for(i in 1:nrow(court_files)){
    file_title <- glue("{str_replace_all(court_name, pattern=' ', replacement='')}_{i}")
    studentName <- court_files$ApplicantName[[i]]
    file_path <- glue("data/student_files/{file_title}.xlsx")
    student_file <- drive_download(file = as_id(court_files$SheetLink[[i]]), path = file_path ,overwrite = TRUE)
    student_file <- readxl::read_xlsx(path = file_path,sheet = "Datasheet",.name_repair = col_name_repair_function)
    student_file$file_title <- file_title
    student_file$NameoftheJudge <- student_file$NameoftheJudge %>% str_to_lower() %>% str_squish()
    student_file$StudentName <- studentName
    judge_datasets_i <- bind_rows(judge_datasets_i, student_file)  
  }
  
  totalJudgesbyFiles <- table(judge_datasets_i$StudentName) %>% data.frame()
  names(totalJudgesbyFiles)[] <- c("StudentName","TotalJudges")
  
  commonJudges <- judge_datasets_i %>% group_by(NameoftheJudge, DateofBirth) %>% summarise(totalEntries = length(unique(file_title))) %>% filter(totalEntries>1)
  
  if(nrow(commonJudges)>0){
    
  common_judge_matrix <- c()
  for(judge_i in 1:nrow(commonJudges)){
    print(glue("Judge Number: {judge_i}"))
    judge_name_i <- commonJudges$NameoftheJudge[judge_i]
    judge_dob_i <- commonJudges$DateofBirth[judge_i] %>% as.character()
    judge_i_matrix <- verify_judge_details(judge_datasets_i, judge_name_i, judge_dob_i)
    common_judge_matrix <- bind_rows(common_judge_matrix, judge_i_matrix)
  }
  } else {
    commonJudges <- c()
    common_judge_matrix <- c()
  }
  
  common_judge_matrix <- common_judge_matrix %>% pivot_wider(names_from = var_title, values_from = var_flag)
  common_judge_matrix$judge_name <- str_to_title(common_judge_matrix$judge_name)
  
  court_details$court_name <- court_name
  court_details$total_judes <- totalJudgesbyFiles
  court_details$common_judges <- commonJudges
  court_details$judges_matrix <- common_judge_matrix
  return(court_details)
  
}

render_court_report <- function(judges_matrix_wide){
  # var_cols <- names(judges_matrix_wide)
  # var_cols <- var_cols[!var_cols %in% c("judge_name","judge_dob")]
  # var_cols <- variables_to_check
  judges_matrix_wide_t <-
    judges_matrix_wide %>% gt(rowname_col =  c("judge_name")) %>% tab_header(title = md("**Cross checking data for Judges**")) %>% tab_stubhead(label = "Judge Details") %>%  cols_label(judge_dob = 'DateofBirth' ,Gender  =  '1',
                                                                                                                                                                                                                             Religion  =  '2',
                                                                                                                                                                                                                             Caste =  '3',
                                                                                                                                                                                                                             DateofBirth  =  '4',
                                                                                                                                                                                                                             DateofAppointment  =  '5',
                                                                                                                                                                                                                             DateofRetirement  =  '6',
                                                                                                                                                                                                                             IfDiedinOffice  =  '7',
                                                                                                                                                                                                                             Ifresignedfromoffice  =  '8',
                                                                                                                                                                                                                             IftransferredtoanyotherHighCourt  =  '9',
                                                                                                                                                                                                                             Dateofsuchtransfer1  =  '10',
                                                                                                                                                                                                                             Dateofsuchtransfer2  =  '11',
                                                                                                                                                                                                                             Dateofsuchtransfer3  =  '12',
                                                                                                                                                                                                                             IfappointedChiefJusticeinanotherHighCourt  =  '13',
                                                                                                                                                                                                                             IfappointedtotheSupremeCourt  =  '14',
                                                                                                                                                                                                                             DateofappointmenttotheSupremeCourt  =  '15',
                                                                                                                                                                                                                             Cadre  =  '16',
                                                                                                                                                                                                                             ExperienceinSubordinateJudiciary  =  '17',
                                                                                                                                                                                                                             LitigationExperience  =  '18',
                                                                                                                                                                                                                             IfaSeniorAdvocate  =  '19',
                                                                                                                                                                                                                             ExperienceinHighCourtAdministrativePost  =  '20',
                                                                                                                                                                                                                             IfservedasGovernmentCounsel  =  '21',
                                                                                                                                                                                                                             IfservedasAdvocateGeneral  =  '22',
                                                                                                                                                                                                                             IfempanelledbyPSUs  =  '23',
                                                                                                                                                                                                                             IfempanelledbyBanks  =  '24',
                                                                                                                                                                                                                             IfempanelledbyanyStatutoryBody  =  '25',
                                                                                                                                                                                                                             IfempanelledbyPrivateCompanies  =  '26',
                                                                                                                                                                                                                             ChamberDetails  =  '27',
                                                                                                                                                                                                                             ForeignDegreeinLaw  =  '28',
                                                                                                                                                                                                                             PostGraduateinanothersubject  =  '29',
                                                                                                                                                                                                                             PostGraduateinLaw  =  '30'
    )%>% cols_width(all_of(variables_to_check)~px(25)) %>% tab_options(
      heading.background.color = "#F7EFB2",
      column_labels.font.size = "x-small",
      stub.font.weight = "bold",
      table.font.size = "x-small"
    ) %>% data_color(
      columns = all_of(variables_to_check),
      colors = scales::col_factor(
        palette = c(colour_nomatch, colour_match , colour_NA_value, colour_blank),
        domain = c(0, 1, 2, 3)
      )
    ) %>%   text_transform(locations = cells_body(all_of(variables_to_check)), fn = function(x){
      ""
    })%>% tab_style(style = list(cell_borders(
      sides = "all",
      color = "black",
      style = "dashed"
    )),
    locations = list(cells_body(columns = all_of(variables_to_check)))) %>% tab_style(style = list(cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(3)
    )),
    locations = list(cells_column_labels(columns = gt::everything()))) %>%   tab_style(
      style = list(
        cell_fill(color = "darkblue"),
        cell_text(color = "white",align = "left")
      ),
      locations = cells_stub()
    )
  
  return(judges_matrix_wide_t)
  
}


generate_cc_table <- function(){
  cc_table <- data.frame(matrix(nrow = 4,ncol = 2, data = ""))
  names(cc_table)[] <- c('Colour Code','Definition')
  cc_table$`Colour Code` <- c(0,1,2,3)
  cc_table$Definition <- c('Data point does not match','Data point consistent across files','Data point found in at-least one file','No data entry')
  cc_table$`Colour Code` <- cell_spec(cc_table$`Colour Code`, background = factor(cc_table$`Colour Code`,c(0,1,2,3), c(colour_nomatch,colour_match,colour_NA_value,colour_blank)),color = factor(cc_table$`Colour Code`,c(0,1,2,3), c(colour_nomatch,colour_match,colour_NA_value,colour_blank)),background_as_tile = T)
  render_cc_table <- cc_table %>% kbl(escape = F) %>% kable_material(lightable_options = "striped", full_width=F) 
  return(render_cc_table)
}


generate_variable_list <- function(){
  
  var_list_df <- data.frame(ID=c(1:30),Variable=c("Gender" ,
                                                  "Religion",
                                                  "Caste" ,
                                                  "Date of Birth" ,
                                                  "Date of Appointment" ,
                                                  "Date of Retirement" ,
                                                  "If Died in Office" ,
                                                  "If resigned from office" ,
                                                  "If transferred to any other High Court" ,
                                                  "Date of such transfer-1" ,
                                                  "Date of such transfer-2" ,
                                                  "Date of such transfer-3" ,
                                                  "If appointed Chief Justice in another High Court" ,
                                                  "If appointed to the Supreme Court" ,
                                                  "Date of appointment to the Supreme Court" ,
                                                  "Cadre" ,
                                                  "Experience in Subordinate Judiciary" ,
                                                  "Litigation Experience" ,
                                                  "If a Senior Advocate" ,
                                                  "Experience in High Court Administrative Post" ,
                                                  "If served as Government Counsel" ,
                                                  "If served as Advocate General" ,
                                                  "If empanelled by PSUs" ,
                                                  "If empanelled by Banks" ,
                                                  "If empanelled by any StatutoryBody" ,
                                                  "If empanelled by Private Companies" ,
                                                  "Chamber Details" ,
                                                  "Foreign Degree in Law" ,
                                                  "Post Graduate in another subject" ,
                                                  "Post Graduate in Law"))
  render_var_list <- var_list_df %>% kbl() %>% kable_material(lightable_options = "striped")
  return(render_var_list)
}