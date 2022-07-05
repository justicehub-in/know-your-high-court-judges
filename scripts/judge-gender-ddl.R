
# Source libraries --------------------------------------------------------

source("scripts/libraries.R")


# CKANR setup -------------------------------------------------------------

org_url <- "https://justicehub.in/"
ckanr::ckanr_setup(url = org_url)
con <- ckanr::src_ckan(url = org_url)

# Find all datasets under group "summer-of-data" --------------------------

all_datasets_info_json <-
  pblapply(all_datasets_id, ckanr::package_show, as = "json")
all_datasets_info_list <- pblapply(all_datasets_info_json, jsonlite::fromJSON) %>% 
  purrr::map("result")

sod_datasets_id <- c()
for(i in 1:length(all_datasets_info_list)){
  dataset_details <- all_datasets_info_list[[i]]
  dataset_id <- dataset_details$id
  dataset_group_names <- dataset_details$groups$name
  if("summer-of-data" %in% dataset_group_names){
    sod_datasets_id <- c(sod_datasets_id,dataset_id)
  }
}


# Collect dataset URLs for csv files ---------------------------------------

all_csv_links <- c()
for(i in 1:length(sod_datasets_id)){
  package_details <- ckanr::package_show(id = sod_datasets_id[[i]])
  resource_list <-
    purrr::map_df(package_details$resources, `[`, c('format', 'url'))
  csv_link <- resource_list$url[resource_list$format=='CSV']
  all_csv_links <- c(all_csv_links, csv_link)
}

# Read csv files ----------------------------------------------------------

all_judge_names <- c()
for(i in 1:length(all_csv_links)){
  court_file <-
    readr::read_csv(all_csv_links[[i]], col_types = cols(.default='c'))
  name_col <- names(court_file)[grepl(names(court_file), pattern = "name", ignore.case = TRUE)]
  gender_col <- names(court_file)[grepl(names(court_file), pattern = "gender", ignore.case = TRUE)]
  print(glue::glue("{all_files[[i]]} -- {name_col}"))
  judge_names <- court_file[,name_col] %>% unlist(use.names = FALSE)
  judge_names <- stringr::str_replace_all(judge_names,pattern = "\\.|\\'|-|​",replacement = "")
  judge_names <- stringr::str_squish(judge_names)
  judge_gender <- court_file[,gender_col] %>% unlist(use.names = FALSE)
  court_name <- all_files[[i]]
  name_df <-
    data.frame('judge_name' = judge_names, judge_gender = judge_gender, 'court_name' = court_name)
  all_judge_names <- dplyr::bind_rows(all_judge_names, name_df)
}

all_judge_names$id <- 1:nrow(all_judge_names)

# POST request to DDL - Gender Classifier ----------------

## Converting to the required file structure
judge_names_file <- data.frame("name"=all_judge_names[,"judge_name"], "id"=all_judge_names[,"id"])
tmp_file <- tempfile(pattern = "judge-name",fileext = ".csv")
readr::write_csv(judge_names_file,tmp_file)

headers = c(
  'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:101.0) Gecko/20100101 Firefox/101.0',
  'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8',
  'Accept-Language' = 'en-US,en;q=0.5',
  'Accept-Encoding' = 'gzip, deflate',
  'DNT' = '1',
  'Connection' = 'keep-alive',
  'Upgrade-Insecure-Requests' = '1',
  'Sec-GPC' = '1'
)

body = list(
  'file' = upload_file(tmp_file)
)

res <-
  VERB(
    "POST",
    url = "http://gender-classifier.devdatalab.org/",
    body = body,
    add_headers(headers),
    encode = 'multipart'
  )

ddl_gender_file <- content(res,as = "parsed")

# Combine with the main file ----------------------------------------------

judge_details_master <-
  left_join(all_judge_names,
            ddl_gender_file,
            by = "id",
            keep = FALSE)

