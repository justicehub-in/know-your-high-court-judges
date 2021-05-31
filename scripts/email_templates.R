
# Setup onboarding e-mail -------------------------------------------------

send_onboarding_email <- function(student_email){
date_time <- add_readable_time()

onboarding_deck_id <- "1S9NXC-CJrTO9KC4ELAU-eGK814uNfMeG"
drive_share(file = as_id(onboarding_deck_id),
            role = "reader",
            type = "user",
            emailAddress = student_email
)

student_name <- student_file$name[student_file$email == student_email]
google_drive_link <- student_file$directory_url[student_file$email == student_email]

onboarding_email <- compose_email(body = md(glue::glue("

Dear {student_name},

Thank you so much for joining the onboarding call today. We have officially launched the Justice Hub’s Summer of Data 2021 and you are part of the inaugural cohort.

Please find the links to the resources which we discussed over call today. Details below:

1. Recording of today’s onboarding session - [Link](https://youtu.be/eybzY8hK8Gg)
2. Google drive folder - [Link]({google_drive_link})
3. Code book - [Link](https://justicehub.in/dataset/dataset-on-the-profiles-the-backgrounds-of-high-court-judges-from-across-india)
4. Presentation from onboarding call - [Link](https://drive.google.com/file/d/1S9NXC-CJrTO9KC4ELAU-eGK814uNfMeG/view?usp=sharing)
5. List of sources:

    1. Official website of the respective High Court
    2. Department of Justice Judges Handbook: [Link](https://doj.gov.in/appointment-of-judges/judges-handbook)

Please join the communication platforms:

1. For discord click on this [link](https://discord.gg/MTbVvat4) 
2. For Justice Hub forum click on this [link](https://forum.justicehub.in/c/summer-of-data/19) 
                                            
Note: You will find the datasheet template in the Google drive folder which is titled as _summer-of-data-[high-court-name]_. Rename this file by replacing the _[high-court-name]_ with the name of the high court that you have been assigned.                                              
                                            
                                            ")),     footer = md(glue::glue("Email sent on {date_time}.")))

print(glue("Status for: {student_email} \n"))
onboarding_email %>%
  smtp_send(
    to = student_email,
    cc = "team@justicehub.in",
    from = "apoorv@civicdatalab.in",
    subject = "Summer of Data | Embarking on the journey",
    credentials = creds_key(id = "gmail_creds")
  )

}

