file_id <- '1aKmmfordSCpMksKU8men1uGsFQZvkNq8xPS3_ynU6Ko'
get_student_file <-
  drive_get(
    id = as_id(file_id)
  )


x <- drive_get("foo_doc")
req <- gargle::request_build(
  path = paste0("drive/v3/files/", get_student_file$id ,"/comments"),
  method = "GET",
  params = list(
    fileId = get_student_file$id,
    fields = "*"
  ),
  token = googledrive:::drive_token()
)

y <- gargle::response_process(gargle::request_make(req))
