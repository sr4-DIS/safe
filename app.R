#
# 
# SAFE Census Project
#
# last update: 3/11/2021
#
#
#
#

## Load libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(summarytools)) install.packages("summarytools", repos = "http://cran.us.r-project.org")
if(!require(shinipsum)) install.packages("shinipsum", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")
if(!require(googledrive)) install.packages("googledrive", repos = "http://cran.us.r-project.org")
if(!require(googlesheets4)) install.packages("googlesheets4", repos = "http://cran.us.r-project.org")

####################  CUSTOM PLOT STYLING ###############################

bbc_style_1 <-
  function () 
  {
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16, 
                                                      face = "bold", color = "#222222"), 
                   plot.subtitle = ggplot2::element_text(size = 14, 
                                                         margin = ggplot2::margin(9, 0, 9, 0)), 
                   legend.position = "top", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                   legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                   legend.text = ggplot2::element_text(size = 12, 
                                                       color = "#222222"), axis.title = ggplot2::element_blank(), 
                   axis.text = ggplot2::element_text(size = 12, 
                                                     color = "#222222"), axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, 
                                                                                                                                      b = 10)), axis.ticks = ggplot2::element_blank(), 
                   axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                   panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"), 
                   panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                   strip.background = ggplot2::element_rect(fill = "white"), 
                   strip.text = ggplot2::element_text(size = 16, hjust = 0))
  }
################## LOAD DATA AND DECLARE VARIABLES #######################

update <- "11 March, 2021"

#gs4_auth(cache = ".secrets") #for the first time running the app in R to get the OAuth token
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

ugly_url <- "https://docs.google.com/spreadsheets/d/15ek1fWs3rtIJPmVJZbAW7Gfb0MTnYF0KmIxq7GmSilM/edit#gid=1962044478"
ssid <- as_sheets_id(ugly_url) 

Likert <- list("Not at all important","Slightly important","Moderately important", "Very important", "Extremely important")

########## 2020-2021 Census Data
  ## url obtainable by going into Alchemer and selecting Results / Exports CSV / Create Report / Share
  ## census_df <- read.csv(url("https://reporting.alchemer.com/reportsview/?key=705511-11818221-e6c05589051f281df88406e33bfd0f9b"))
  census_df <- read.csv("census_backup1_20210216.csv", fileEncoding = "UTF-8")
  
  ###########relevel factors
  census_df$services_crp_stage <- factor(census_df$services_crp_stage,
                                         levels = c("Early Stage: This is a new effort focusing on recruiting students and educating the campus",
                                                    "Mid Stage: Growing a community of students and institutional support",
                                                    "Late Stage: Have a thriving community of students and supporters with institutional support",
                                                    "Not applicable or designated"))
  census_df$services_crp_stage[is.na(census_df$services_crp_stage)] <- "Not applicable or designated"
  
  census_df$services_crp_focus <- factor(census_df$services_crp_focus,
                                         levels = c("Peer support",
                                                    "Counseling and therapy or clinical support",
                                                    "Social activities and sober fun",
                                                    "Other - Write In",
                                                    "Not applicable or did not respond"))
  census_df$services_crp_focus[is.na(census_df$services_crp_focus)] <- "Not applicable or did not respond"
  
  census_df$services_crp_space <- factor(census_df$services_crp_space,
                                         levels = c("The program has a dedicated space, which is only available to participating students.",
                                                    "The program has a space that is shared with other groups, but is consistently available.",
                                                    "The space that the program uses varies; it is not consistently available.",
                                                    "No space dedicated for the program.",
                                                    "Other - Write In"))
  
  
  ## filter to those who gave consent
  # census_df <- census_df %>% filter(agreement == "Y")

  ## filter to those who did not expressly state that they want their responses withheld
  # census_df <- census_df %>% filter(census_df[,grepl("Thank.you", colnames(census_df))] != "I do not want to publicly share my institution-specific responses.")

  # census_df$services_year <- ymd(as.character(census_df$services_year), truncated = 2L)

  # census_df <- census_df[which(census_df$Response.ID != 65 & census_df$Response.ID != 14), ] ## remove duplicates NCAT and Reno
  
  ## load companion data sets
  # opeid_df <- read.csv("ope_id.csv")
  # scorecard_df <- read.csv("scorecard_respondents.csv")
  

  ## combine with OPEID codes
  # census_df <- left_join(census_df, opeid_df, by = c("Response.ID", "highered_name"))
  # census_df <- left_join(census_df, scorecard_df, by = "ope_id")

public_df <- census_df %>% filter(census_df[,grepl("Thank.you", colnames(census_df))] == "I would like my institutional-specific responses to be publicly shared on the SAFE Campuses Census Dashboard.")



## CCBASIC variable

ccbasic_mapping <- 
          c('-2' = "Not applicable",
             '0' = "(Not classified)",
             '1' = "Associate's Colleges: High Transfer-High Traditional",
             '2' = "Associate's Colleges: High Transfer-Mixed Traditional/Nontraditional",
             '3' = "Associate's Colleges: High Transfer-High Nontraditional",
             '4' = "Associate's Colleges: Mixed Transfer/Career & Technical-High Traditional",
             '5' = "Associate's Colleges: Mixed Transfer/Career & Technical-Mixed Traditional/Nontraditional",
             '6' = "Associate's Colleges: Mixed Transfer/Career & Technical-High Nontraditional",
             '7' = "Associate's Colleges: High Career & Technical-High Traditional",
             '8' = "Associate's Colleges: High Career & Technical-Mixed Traditional/Nontraditional",
             '9' = "Associate's Colleges: High Career & Technical-High Nontraditional",
             '10' = "Special Focus Two-Year: Health Professions",
             '11' = "Special Focus Two-Year: Technical Professions",
             '12' = "Special Focus Two-Year: Arts & Design",
             '13' = "Special Focus Two-Year: Other Fields",
             '14' = "Baccalaureate/Associate's Colleges: Associate's Dominant",
             '15' = "Doctoral Universities: Very High Research Activity",
             '16' = "Doctoral Universities: High Research Activity",
             '17' = "Doctoral/Professional Universities",
             '18' = "Master's Colleges & Universities: Larger Programs",
             '19' = "Master's Colleges & Universities: Medium Programs",
             '20' = "Master's Colleges & Universities: Small Programs",
             '21' = "Baccalaureate Colleges: Arts & Sciences Focus",
             '22' = "Baccalaureate Colleges: Diverse Fields",
             '23' = "Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's",
             '24' = "Special Focus Four-Year: Faith-Related Institutions",
             '25' = "Special Focus Four-Year: Medical Schools & Centers",
             '26' = "Special Focus Four-Year: Other Health Professions Schools",
             '27' = "Special Focus Four-Year: Engineering Schools",
             '28' = "Special Focus Four-Year: Other Technology-Related Schools",
             '29' = "Special Focus Four-Year: Business & Management Schools",
             '30' = "Special Focus Four-Year: Arts, Music & Design Schools",
             '31' = "Special Focus Four-Year: Law Schools",
             '32' = "Special Focus Four-Year: Other Special Focus Institutions",
             '33' = "Tribal Colleges")

census_df$CCBASIC <- as.character(census_df$CCBASIC)
census_df$CCBASIC_MAPPING <- ccbasic_mapping[census_df$CCBASIC]

## CCUGPROF

ccugprof_mapping <- 
  c('-2' = "Not applicable",
    '0' = "Not classified (Exclusively Graduate)",
    '1' = "Two-year, higher part-time",
    '2' = "Two-year, mixed part/full-time",
    '3' = "Two-year, medium full-time",
    '4' = "Two-year, higher full-time",
    '5' = "Four-year, higher part-time",
    '6' = "Four-year, medium full-time, inclusive, lower transfer-in",
    '7' = "Four-year, medium full-time, inclusive, higher transfer-in",
    '8' = "Four-year, medium full-time, selective, lower transfer-in",
    '9' = "Four-year, medium full-time , selective, higher transfer-in",
    '10' = "Four-year, full-time, inclusive, lower transfer-in",
    '11' = "Four-year, full-time, inclusive, higher transfer-in",
    '12' = "Four-year, full-time, selective, lower transfer-in",
    '13' = "Four-year, full-time, selective, higher transfer-in",
    '14' = "Four-year, full-time, more selective, lower transfer-in",
    '15' = "Four-year, full-time, more selective, higher transfer-in"
  )

census_df$CCUGPROF <- as.character(census_df$CCUGPROF)
census_df$CCUGPROF_MAPPING <- ccugprof_mapping[census_df$CCUGPROF]

## CCSIZSET

ccsizset_mapping <- 
  c('-2' = "Not applicable",
    '0' = "(Not classified)",
    '1' = "Two-year, very small",
    '2' = "Two-year, small",
    '3' = "Two-year, medium",
    '4' = "Two-year, large",
    '5' = "Two-year, very large",
    '6' = "Four-year, very small, primarily nonresidential",
    '7' = "Four-year, very small, primarily residential",
    '8' = "Four-year, very small, highly residential",
    '9' = "Four-year, small, primarily nonresidential",
    '10' = "Four-year, small, primarily residential",
    '11' = "Four-year, small, highly residential",
    '12' = "Four-year, medium, primarily nonresidential",
    '13' = "Four-year, medium, primarily residential",
    '14' = "Four-year, medium, highly residential",
    '15' = "Four-year, large, primarily nonresidential",
    '16' = "Four-year, large, primarily residential",
    '17' = "Four-year, large, highly residential",
    '18' = "Exclusively graduate/professional"
  )

census_df$CCSIZSET <- as.character(census_df$CCSIZSET)
census_df$CCSIZSET_MAPPING <- ccsizset_mapping[census_df$CCSIZSET]

census_df$CCSIZSET_MAPPING <- 
  factor(census_df$CCSIZSET_MAPPING, 
         levels = c("Not applicable",
                    "(Not classified)",
                    "Two-year, very small",
                    "Two-year, small",
                    "Two-year, medium",
                    "Two-year, large",
                    "Two-year, very large",
                    "Four-year, very small, primarily nonresidential",
                    "Four-year, very small, primarily residential",
                    "Four-year, very small, highly residential",
                    "Four-year, small, primarily nonresidential",
                    "Four-year, small, primarily residential",
                    "Four-year, small, highly residential",
                    "Four-year, medium, primarily nonresidential",
                    "Four-year, medium, primarily residential",
                    "Four-year, medium, highly residential",
                    "Four-year, large, primarily nonresidential",
                    "Four-year, large, primarily residential",
                    "Four-year, large, highly residential",
                    "Exclusively graduate/professional"))

## LOCALE

locale_mapping <-
          c("11" = "City: Large (population of 250,000 or more)",
            "12" = "City: Midsize (population of at least 100,000 but less than 250,000)",
            "13" = "City: Small (population less than 100,000)",
            "21" = "Suburb: Large (outside principal city, in urbanized area with population of 250,000 or more)",
            "22" = "Suburb: Midsize (outside principal city, in urbanized area with population of at least 100,000 but less than 250,000)",
            "23" = "Suburb: Small (outside principal city, in urbanized area with population less than 100,000)",
            "31" = "Town: Fringe (in urban cluster up to 10 miles from an urbanized area)",
            "32" = "Town: Distant (in urban cluster more than 10 miles and up to 35 miles from an urbanized area)",
            "33" = "Town: Remote (in urban cluster more than 35 miles from an urbanized area)",
            "41" = "Rural: Fringe (rural territory up to 5 miles from an urbanized area or up to 2.5 miles from an urban cluster)",
            "42" = "Rural: Distant (rural territory more than 5 miles but up to 25 miles from an urbanized area or more than 2.5 and up to 10 miles from an urban cluster)",
            "43" = "Rural: Remote (rural territory more than 25 miles from an urbanized area and more than 10 miles from an urban cluster)"
            )

census_df$LOCALE <- as.character(census_df$LOCALE)
census_df$LOCALE_MAPPING <- locale_mapping[census_df$LOCALE]

census_df$LOCALE_MAPPING <- 
  factor(census_df$LOCALE_MAPPING,
         levels = c("City: Large (population of 250,000 or more)",
                    "City: Midsize (population of at least 100,000 but less than 250,000)",
                    "City: Small (population less than 100,000)",
                    "Suburb: Large (outside principal city, in urbanized area with population of 250,000 or more)",
                    "Suburb: Midsize (outside principal city, in urbanized area with population of at least 100,000 but less than 250,000)",
                    "Suburb: Small (outside principal city, in urbanized area with population less than 100,000)",
                    "Town: Fringe (in urban cluster up to 10 miles from an urbanized area)",
                    "Town: Distant (in urban cluster more than 10 miles and up to 35 miles from an urbanized area)",
                    "Town: Remote (in urban cluster more than 35 miles from an urbanized area)",
                    "Rural: Fringe (rural territory up to 5 miles from an urbanized area or up to 2.5 miles from an urban cluster)",
                    "Rural: Distant (rural territory more than 5 miles but up to 25 miles from an urbanized area or more than 2.5 and up to 10 miles from an urban cluster)",
                    "Rural: Remote (rural territory more than 25 miles from an urbanized area and more than 10 miles from an urban cluster)"
           
         )
  )


###################### SHINY DASHBOARD ###################################


######### USER INTERFACE #######


ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                tags$img(src="SAFE_white.png", width = 200, height = 70, style="float:left; margin-top: -25px; color: white;"),
                
                 tabPanel("Home",
                          fluidPage(
                            uiOutput("homepage")
                          )
                 ),
                 
                 tabPanel("Pulse Surveys",
                          useShinyjs(),
                          fluidPage(
                            column(width = 10, offset = 1,
                                   box(width = 12,
                                       fluidPage(id = "questions",    
                                         column(width = 12,
                                                fluidRow(
                                                  div(h4("Collegiate Recovery Programs and other campus support services can play a 
                                                         significant role in helping students in recovery find assistance for ", strong("BASIC NEEDS"), 
                                                         ". We are interested in better understanding assistance requests in your setting."))
                                                  ),
                                                br(),
                                                fluidRow(
                                                  div(h4("For each listed need, please indicate the level of importance for students 
                                                         at your institution."))
                                                  ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q1", 
                                                               label = "Affordable housing", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q2", 
                                                               label = "Child care support", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q3", 
                                                               label = "Educational support", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q4", 
                                                               label = "Employment assistance", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q5", 
                                                               label = "Food: nutrition assistance", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q6", 
                                                               label = "Food: access to healthy options", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q7", 
                                                               label = "General hygiene supplies", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q8", 
                                                               label = "Health care access", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q9", 
                                                               label = "Legal assistance", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q10", 
                                                               label = "Personal safety", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q11", 
                                                               label = "Shelter: short term", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q12", 
                                                               label = "Shelter: long term", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                br(),
                                                fluidRow(
                                                  radioButtons(inputId ="q13", 
                                                               label = "Transportation assistance", 
                                                               choices = Likert, inline = TRUE, selected = character(0))
                                                ),
                                                fluidRow(align = "center",
                                                         actionButton("submit", label = "Submit")
                                                ),
                                                br(),
                                                fluidRow(align = "center",
                                                         actionButton("no_submit", label = "I already responded, just show me the results.")
                                                ),
                                                br()
                                                )
                                            ),
                                       uiOutput("results")
                                   )
                          ))
                 ),
                 
                 navbarMenu("Census Data",
                            
                            tabPanel("Organizational Information",
                                     sidebarLayout(
                                       sidebarPanel(
                                         ## input University name from a menu of choices
                                         selectizeInput(
                                           inputId = "university_select",
                                           label = "Select University:",
                                           selected = NULL,
                                           multiple = FALSE,
                                           choices = as.character(public_df$INSTNM) %>% sort(),
                                           options = list(
                                             placeholder = 'choose one',
                                             onInitialize = I('function() { this.setValue(""); }'))
                                         )
                                       ),
                                       
                                       mainPanel(
                                         uiOutput("organizational_info")
                                       )
                        
                                     ),
                                     fluidPage(
                                     uiOutput("organizational_info_charts")
                                     )
                                     ),
                            
                            
                            
            ##### PROGRAMS AND SERVICES #####                
                            
                            tabPanel("Programs & Services",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    div(h3(strong("Filters"))),
                                                    div("Select one or more criteria on which to filter the data."),
                                                    br(),
                                                    uiOutput("sample_size"),
                                                    br(),
                                                    pickerInput(
                                                      inputId = "stage",
                                                      label = "CRP Stage of Development",
                                                      selected = as.character(census_df$services_crp_stage),
                                                      options = list(`actions-box` = TRUE, `selected-text-format` = paste0("count > ", length(unique(census_df$services_crp_stage))-1),`count-selected-text` = "All Selected"),
                                                      multiple = TRUE,
                                                      choices = levels(census_df$services_crp_stage)),
                                                    pickerInput(
                                                      inputId = "focus",
                                                      label = "CRP Primary Area of Focus",
                                                      selected = as.character(levels(census_df$services_crp_focus)),
                                                      options = list(`actions-box` = TRUE, `selected-text-format` = paste0("count > ", length(levels(census_df$services_crp_focus))-1),`count-selected-text` = "All Selected"),
                                                      multiple = TRUE,
                                                      choices = levels(census_df$services_crp_focus)),
                                                    br(),
                                                    div(h4("Carnegie Classifications")),
                                                    pickerInput(
                                                      inputId = "size",
                                                      label = "Size and Setting",
                                                      selected = as.character(census_df$CCSIZSET_MAPPING),
                                                      options = list(`actions-box` = TRUE, `selected-text-format` = paste0("count > ", length(unique(census_df$CCSIZSET_MAPPING))-1),`count-selected-text` = "All Selected"),
                                                      multiple = TRUE,
                                                      choices = as.character(unique(census_df$CCSIZSET_MAPPING))),
                                                    pickerInput(
                                                      inputId = "locale",
                                                      label = "Locale",
                                                      selected = as.character(levels(census_df$LOCALE_MAPPING)),
                                                      options = list(`actions-box` = TRUE, `selected-text-format` = paste0("count > ", length(levels(census_df$LOCALE_MAPPING))-1),`count-selected-text` = "All Selected"),
                                                      multiple = TRUE,
                                                      choices = levels(census_df$LOCALE_MAPPING))
                                                    ),
                                       mainPanel(width = 9,
                                                 tabBox(
                                                   width = "100%",
                                                   side = "left",
                                                   selected = "Engagement",
                                                   
                                                   tabPanel("Engagement",
                                                            div(h3(strong("Engagement"))),
                                                            div(h4("How many students self-identify as being a part of the collegiate recovery program (CRP)?")),
                                                            fluidRow(column(width = 8, plotOutput("crp_engage_bar")), column(width = 4, uiOutput("crp_engage_kable"))),
                                                            div(h4("How many students regularly attend activities, events or support services that are sponsored by the collegiate recovery program (CRP)?")),
                                                            fluidRow(column(width = 8, plotOutput("crp_participate_bar")), column(width = 4, uiOutput("crp_particpate_kable"))),
                                                            div(h4("How many students in recovery are served by the programs or support services that are sponsored by the institution?")),
                                                            div(h5("Please estimate the number of students in recovery served in past academic year.")),
                                                            fluidRow(column(width = 8, plotOutput("participate_bar")), column(width = 4, uiOutput("participate_kable")))
                                                            ),
                                                   
                                                   tabPanel("Housing",
                                                            div(h3(strong("Housing"))),
                                                            div(h4("What types of recovery supportive housing do you offer?")),
                                                            div(h5("Select all that apply")),
                                                            fluidRow(column(width = 8, plotOutput("services_housing_bar")), column(width = 4, uiOutput("services_housing_kable"))),
                                                            div(h4("What is the level of support for the recovery residence?")),
                                                            fluidRow(column(width = 8, plotOutput("housing_support_level_bar")), column(width = 4, uiOutput("housing_support_level_kable")))
                                                            ),
                                                   
                                                   tabPanel("Recovery Support",
                                                            div(h3(strong("Recovery Support"))),
                                                            div(h4("What types of recovery are served by programs and services available at your institution?")),
                                                            div(h5("Select all that apply")),
                                                            fluidRow(column(width = 8, plotOutput("services_recovery_bar")), column(width = 4, uiOutput("services_recovery_kable"))),
                                                            div(h4("Which of the following support services are offered to students in recovery on your campus?")),
                                                            div(h5("Select all that apply")),
                                                            fluidRow(column(width = 8, plotOutput("services_accomm_bar")), column(width = 4, uiOutput("services_accomm_kable")))
                                                            ),
                                                   
                                                   tabPanel("Prevention and Intervention",
                                                            div(h3(strong("Prevention and Intervention"))),
                                                            div(h4("Please indicate the type of prevention and/or intervention services offered at your educational institution.")),
                                                            fluidRow(column(width = 8, plotOutput("prevention_bar")), column(width = 4, uiOutput("prevention_kable")))
                                                            ),
                                                   
                                                   tabPanel("Admissions",
                                                            div(h3(strong("Admissions"))),
                                                            div(h4("Please select the statement that best describes the school's admission process for students affiliated with the collegiate recovery program (CRP).")),
                                                            fluidRow(column(width = 10, offset = 1, uiOutput("crp_admin_kable"))),
                                                            fluidRow(column(width = 10, offset = 1, 
                                                                            div(h5(strong("Write Ins:"))))),
                                                            fluidRow(column(width = 8, offset = 1, uiOutput("crp_admin_writein_kable"))),
                                                            div(h4("Does your institution offer admissions support for formerly incarcerated/criminal justice system-impacted students?")),
                                                            fluidRow(column(width = 3, uiOutput("admissions_criminal_kable")), 
                                                                     column(width = 8, fluidRow(div(h5(strong("Here is how respondents described the type(s) of admissions support offered to criminal justice system-impacted students.")))),
                                                                            fluidRow(uiOutput("admissions_criminal_describe_kable")))
                                                                     )
                                                            )
                                                  )
                                                 )
                                        )
                                     ),
            
                            
                                            
            ##### PROGRAM CHARACTERISTICS #####
            
                            tabPanel("Program Characteristics",
                                     fluidPage(
                                       titlePanel("Program Characteristics"),
                                       br(),
                                       div(h3(strong("Collegiate Recovery Program (CRP)"))),
                                       div(h4("Please describe your collegiate recovery program's (CRP) stage of development. ")),
                                       fluidRow(column(offset = 3, width = 6, uiOutput("crp_stage_kable"))),
                                       div(h4("What is your collegiate recovery program's (CRP) primary area of focus?")),
                                       fluidRow(column(offset = 3, width = 6, uiOutput("crp_focus_kable"))),
                                       div(h4("Please describe your collegiate recovery program's (CRP) space.")),
                                       fluidRow(column(offset = 3, width = 6, uiOutput("crp_space_kable"))),
                                       div(h3(strong("CRP Practices"))),
                                       div(h4("What are your collegiate recovery program's (CRP) practices?")),
                                       fluidRow(column(width = 8, plotOutput("crp_practices_bar")), column(width = 4, uiOutput("crp_practices_kable"))),
                                       div(h3(strong("CRP Requirements"))),
                                       div(h4("Does your collegiate recovery program (CRP) have any eligibility requirements? Select all that apply.")),
                                       fluidRow(column(width = 8, plotOutput("crp_eligibility_bar")), column(width = 4, uiOutput("crp_eligibility_kable")))
                                     )
                                     )
                 ),
                 
                 tabPanel("About this Application",
                          tags$div(
                            tags$h4("Last update"), 
                            h6(paste0(update)),
                            "This application is updated at regular intervals during the 2021 census period. ", 
                            
                            tags$br(),tags$br(),tags$h4("Collegiate Recovery Census, Sponsored by SAFE Campuses"), 
                            "The effort to promote recovery supportive institutions of higher education across the U.S. includes specific 
                            activities designed to better describe the diverse recovery support services and resources that may 
                            be available for students and their families. Professionals working to support substance use disorder 
                            and mental health (SUD/MH) recovery, as well as educators, parents, and students themselves, need data 
                            and information to find and access a variety of recovery support options that are present or emerging 
                            around educational institutions. Equally important, public health organizations need to be able to view 
                            the national landscape for support and care and see the critically important points of entry for young 
                            adults looking to complete educational goals and transition into stable and rewarding recovery lifestyles. ",
                            tags$br(),tags$br(),
                            "The SAFE Campuses Collegiate Recovery Census is a targeted survey and data analytics instrument being developed 
                            to define and explore the rich spectrum of support resources available for students in recovery at institutions 
                            of higher education. Associated national census activities aim to directly support the collection and dissemination 
                            of programmatic and relational information to directly assist groups and organizations interested in creating 
                            recovery supportive campuses at institutions across the U.S. ",
                            tags$br(),tags$br(),
                            "Collegiate recovery program leaders can request technical assistance services directly from the SAFE Campuses team. These 
                            services aim to equip leaders with essential tools to create sustainable, impactful, campus communities and 
                            initiatives. "),
                          "For more information, visit ",tags$a(href="https://www.safeproject.us/campuses/technical-assistance", "SAFE Campuses Technical Assistance."),
                          tags$br(),tags$br(),tags$h4("Code"),
                          "Code and input data used to generate this Shiny data analytics tool are available on ",tags$a(href="https://github.com/URL", "GitHub."),
                          tags$br(),tags$br(),tags$h4("Sources and Data References"),
                          tags$b("2020-College Scorecard Data: "), tags$a(href="https://collegescorecard.ed.gov/data/", "U.S. Department of Education's College Scorecard"), " is integrated into the app to provide comparative institutional data on key charcateristics. ",
                          tags$br(),tags$b("2017-Census and Definitions for Recovery Support in Higher Education: "), "The research undertaken by ", tags$a(href="https://www.transformingyouthrecovery.org/research/2017-census-and-definitions-for-recovery-support-in-higher-education/", "Transforming Youth Recovery"), " includes research activities intended to update definitions and descriptions for the services and resources that directly support students in recovery at institutions of higher education.",
                          tags$br(),tags$b("2017-Recovery Census Data: "), tags$a(href="http://sgiz.mobi/s3/Recovery-Census-Data-Download", "Data download. "),
                          tags$br(),tags$b("2016-Recovery Support in and around Community College Campuses in the U.S. : "), tags$a(href="https://www.transformingyouthrecovery.org/research/recovery-support-in-and-around-community-college-campuses-in-the-u-s-2016/", "This study sets out to examine the landscape for recovery support in and around community colleges in the U.S. "),tags$br(),
                          tags$br(),tags$br(),tags$h4("Permissions"),
                          "Permission for educational and other not-for-profit groups to download and use census data is granted 
                          with the acknowledgment of SAFE Project as the source. The content of survey instruments is based on work 
                          by sr4-DIS Holdings, Inc. under contract and sponsored by SAFE Project.",tags$br(),
                          tags$br(),tags$br(),tags$h4("Contact"),
                          "dylan.dunn@safeproject.us"
                          
                 )

)
                             
                  
#############################
#############################


###### SERVER ###############
#############################

server <- function(input, output, session) {

  
  ## filter data by university selection
  school_df <- reactive({
    
    census_df %>% filter(INSTNM == input$university_select)
  })
  
  ## filter data by stage selection
  census_filter <- reactive({
    census_df %>% 
      filter(services_crp_stage %in% input$stage,
             services_crp_focus %in% input$focus,
             CCSIZSET_MAPPING %in% input$size,
             LOCALE_MAPPING %in% input$locale
             )
  })
  
  ##### SAFE Census home page #####
  #################################
  
  output$SAFEmain <- renderImage({
    return(list(src = "shiny-header-tablet-safe.png",
                contentType = "image/png"
    ))
  }, deleteFile = FALSE)
  
  ## CCBASIC
  cc_basic_bar <- function() {
    
    census_df %>%
      ggplot() +
      geom_bar(aes(x=CCBASIC_MAPPING), stat = "count", size =1.5, fill = "#186063") +
      bbc_style_1() +
      scale_y_continuous(breaks = seq(0, 20, by = 2)) +
      labs(x = element_blank(),
           y = element_blank()) +
      coord_flip()
  }
  
  output$cc_basic_bar <- 
    renderPlot({
      cc_basic_bar()
    })
  
  ## CCUGPROF
  cc_ugprof_bar <- function() {
    census_df %>%
      ggplot() +
      geom_bar(aes(x=CCUGPROF_MAPPING), stat = "count", size =1.5, fill = "#186063") +
      bbc_style_1() +
      scale_y_continuous(breaks = seq(0, 20, by = 2)) +
      labs(x = element_blank(),
           y = element_blank()) +
      coord_flip()
  }
  
  output$cc_ugprof_bar <- 
    renderPlot({
      cc_ugprof_bar()
    })
  
  ## CCSIZSET
  cc_size_bar <- function() {
    census_df %>%
      ggplot() +
      geom_bar(aes(x=CCSIZSET_MAPPING), stat = "count", size =1.5, fill = "#186063") +
      bbc_style_1() +
      scale_y_continuous(breaks = seq(0, 20, by = 2)) +
      labs(x = element_blank(),
           y = element_blank()) +
      coord_flip()
  }
  
  output$cc_size_bar <- 
    renderPlot({
      cc_size_bar()
    })
  
  ## Services Baseline
  
  services_gather <- function() {
    ## select all columns that end in services_baseline, use Response.ID to organize the rows
    services_baseline <- census_df %>% select(Response.ID, matches("services_baseline$"))
    
    ## gather all of the responses
    services_baseline_gather <-
      services_baseline %>% 
      gather(2:15,
             key = "services_drop",
             value = "services")
    
    ## drop column 2
    services_baseline_gather <- services_baseline_gather %>% select(-2)
    
    ## Remove all rows where services is blank
    services_baseline_gather <- services_baseline_gather[!(services_baseline_gather$services==""), ] %>% arrange(Response.ID)
    
    ## change to factor and set order of levels
    services_baseline_gather$services <- 
      factor(services_baseline_gather$services, levels = c("Collegiate Recovery Community/Program",
                                                           "Counseling",
                                                           "Digital Recovery Support Services",
                                                           "Housing: Substance-Free Housing",
                                                           "Housing: Recovery Housing",
                                                           "Housing: Transitional Living",
                                                           "Mental Health Services",
                                                           "Mutual Aid Meetings",
                                                           "Peer Recovery Specialists",
                                                           "Recovery Coaches",
                                                           "Referrals: Recovery Support Services",
                                                           "Treatment: IOP/Outpatient Care",
                                                           "Treatment: Residential",
                                                           "Other - Write In"))
    
    return(services_baseline_gather)
  }

  output$services_baseline_bar <- 
    renderPlot({
      services_baseline_gather <- services_gather()
      
      services_baseline_gather %>%
        ggplot() +
        geom_bar(aes(x = services), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(services_baseline_gather$services))) +
        coord_flip()
        
    })
  
  output$services_baseline_table <- function() {
    services_baseline_gather <- services_gather()
    
    services_baseline_gather %>%
      count(services, .drop = FALSE) %>% 
      filter(!is.na(services)) %>%
      mutate(percentage = percent(n / nrow(census_df))) %>%
      kable("html", col.names = c("Types of Programs and Services", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  ## Services Baseline Recovery (sbr)
  
  sbr_gather <- function() {
    ## select all columns that end in services_baseline_recovery, use Response.ID to organize the rows
    sbr <- census_df %>% select(Response.ID, matches("services_baseline_recovery$"))
    
    ## gather all of the responses
    sbr_gather <-
      sbr %>% 
      gather(2:11,
             key = "recovery_drop",
             value = "recovery_type")
    
    ## drop column 2
    sbr_gather <- sbr_gather %>% select(-2)
    
    ## Remove all rows where services is blank
    sbr_gather <- sbr_gather[!(sbr_gather$recovery_type==""), ] %>% arrange(Response.ID)
    
    ## change to factor and set order of levels
    sbr_gather$recovery_type <- 
      factor(sbr_gather$recovery_type, levels = c("Substance use disorder recovery",
                                                  "Mental health recovery",
                                                  "Behavioral addiction recovery",
                                                  "Eating disorder recovery",
                                                  "Gambling addiction recovery",
                                                  "Trauma recovery",
                                                  "Video game or other technology recovery",
                                                  "Recovery from problematic sexual behavior",
                                                  "Support for those affected by addiction",
                                                  "Other - Write In"))
    
    return(sbr_gather)
  }
  
  output$sbr_bar <- 
    renderPlot({
      sbr_gather <- sbr_gather()
      
      sbr_gather %>%
        ggplot() +
        geom_bar(aes(x = recovery_type), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(sbr_gather$recovery_type))) +
        coord_flip()
    })
  
  output$sbr_table <- function() {
    sbr_gather <- sbr_gather() 
    
    sbr_gather %>%
      count(recovery_type) %>% 
      mutate(percentage = percent(n / nrow(census_df))) %>%
      kable("html", col.names = c("Recovery Type", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
    
  
  ## Home Page Output
  
  output$homepage <- renderUI({
    fluidPage(
      div(h2(strong("National Campus Census Activities"))),
      div("The SAFE Campuses Census is a data gathering project being undertaken to explore the rich spectrum of 
             support resources available for students in recovery at institutions of higher education. Activities aim 
             to directly support the collection and dissemination of programmatic and information data in order to directly 
             assist groups and organizations interested in creating recovery supportive campuses at institutions across the U.S."),
      
      div(h3(strong("Overview of Dashboard Features"))),
      div("The Campus Census Dashboard is a data reporting and visualization tool directly used by the SAFE Campuses Team as they provide 
          technical assistance services to institutions across the country. It is also available to data contributors and researchers interested 
          in creating recovery supportive campuses at institutions across the U.S."),
      div("We invite you to explore the application."),
      div(h5(strong("Pulse Surveys"))), 
      div("A companion to the full census survey, Pulse Surveys are periodically offered to gather data in areas of particular interest to program 
          coordinators, educational leaders, and technical assistance providers."),
      div(h5(strong("Census Data"))),
      div("View and interact with data from census activities inclusive of institution-specific responses and comparative visualizations. 
          Explore data questions of interest to those working in the field.  "),
      div(h5(strong("About this Application"))),
      div("Resource links for those working and studying in the field. Public datasets available for download and use by the community and recovery science researchers."),
      br(),
      div(h4(strong("Carnegie Classifications for all respondents"))), 
      div(h4("Total Number (N) of Responses = ", strong(nrow(census_df)))),
      tabBox(
        width = "100%",
        side = "left",
        selected = "Basic",
        tabPanel("Basic", plotOutput("cc_basic_bar")),
        tabPanel("Undergraduate Profile", plotOutput("cc_ugprof_bar")),
        tabPanel("Size and Setting", plotOutput("cc_size_bar"))
      ),
      br()
    )
  })

  
  
  ####### PULSE SURVEYS #######
  
  #store the results
  Results <- reactive(
    data.frame(basic_needs.affordable_housing = req(input$q1),
               basic_needs.child_care_support = req(input$q2),
               basic_needs.educational_support = req(input$q3),
               basic_needs.employment_assistance = req(input$q4),
               basic_needs.food_nutrition_assistance = req(input$q5),
               basic_needs.food_access_to_healthy_options = req(input$q6),
               basic_needs.general_hygiene_supplies = req(input$q7),
               basic_needs.health_care_access = req(input$q8),
               basic_needs.legal_assistance = req(input$q9),
               basic_needs.personal_safety = req(input$q10),
               basic_needs.shelter_short_term = req(input$q11),
               basic_needs.shelter_long_term = req(input$q12),
               basic_needs.transportation_assistance = req(input$q13),
               timestamp = Sys.time(), stringsAsFactors = FALSE
    )
  )
  
  observe({
    if((input$q1 == "" || is.null(input$q1)) ||
       (input$q2 == "" || is.null(input$q2)) ||
       (input$q3 == "" || is.null(input$q3)) ||
       (input$q4 == "" || is.null(input$q4)) ||
       (input$q5 == "" || is.null(input$q5)) ||
       (input$q6 == "" || is.null(input$q6)) ||
       (input$q7 == "" || is.null(input$q7)) ||
       (input$q8 == "" || is.null(input$q8)) ||
       (input$q9 == "" || is.null(input$q9)) ||
       (input$q10 == "" || is.null(input$q10)) ||
       (input$q11 == "" || is.null(input$q11)) ||
       (input$q12 == "" || is.null(input$q12)) ||
       (input$q13 == "" || is.null(input$q13))
    ){
      disable("submit")
    }
    else{
      enable("submit")
    }
  })
  

  observeEvent(input$submit | input$no_submit, { 
    
    if (input$submit) {
      req(input$q1, input$q2, input$q3, input$q4, input$q5, input$q6, input$q7,
          input$q8, input$q9, input$q10, input$q11, input$q12, input$q13)
      
      ssid %>% sheet_append(Results()) 
    }
    
    hide("questions", anim = TRUE, animType = "fade", time = 1)
    
    delay(1000, 
          output$results <- renderUI({
            
            output$basic_needs_likert <- renderPlot({
              df <- read_sheet(ssid)
              
              df1 <- 
                df %>%
                select(-14) %>%
                gather(1:13, key = "question", value = "response")
              
              ## remove characters before "."
              df1$question <- gsub("^.*\\.","", df1$question)
              ## change "_" to " "
              df1$question <- gsub("_"," ", df1$question)
              
              ## change response to factor
              df1$response <- factor(df1$response, levels = c("Extremely important",
                                                              "Very important",
                                                              "Moderately important",
                                                              "Slightly important",
                                                              "Not at all important"
              ))
              
              df2 <- 
                df1 %>%
                group_by(question) %>%
                count(response, .drop = FALSE)
              
              df2$question <- factor(df2$question)
              
              # Stacked + percent
              ggplot(df2) + 
                geom_bar(aes(fill=response, y=n, x=question), position="fill", stat="identity") +
                coord_flip() +
                guides(fill = guide_legend(reverse = TRUE)) +
                bbc_style_1() +
                theme(legend.position = "right") +
                scale_fill_brewer(palette = "RdBu") +
                scale_y_continuous(labels = scales::percent) +
                scale_x_discrete(limits = rev(levels(df2$question)))
            })
            
            fluidPage(
              div(h3("Level of importance for various basic needs support")),
              plotOutput("basic_needs_likert")
            )
          })
    )
  }, ignoreInit = TRUE)
    
    
  
  
  
  ####### CENSUS DATA ########
  
  ## Institutional Profile ##

  
    
  output$institutional_profile <- renderUI({
    fluidRow(
      div(h2("Institutional Profile")),
      div(h4(strong("Institution Name:  "), school_df()$INSTNM[1])),
      div(h4(strong("OPEID:  "), school_df()$ope_id[1])),
      div(h4(strong("Locale:  "), school_df()$LOCALE_MAPPING[1])),
      div(h4(strong("Carnegie Classification -- basic:  "), school_df()$CCBASIC_MAPPING[1])),
      div(h4(strong("Carnegie Classification -- undergraduate profile:  "), school_df()$CCUGPROF_MAPPING[1])),
      div(h4(strong("Carnegie Classification -- size and setting:  "), school_df()$CCSIZSET_MAPPING[1])),
      br(),
      div(h4("Carnegie Classifications for all respondents. The total number of respondents in the census is", strong(nrow(census_df)))),
      tabBox(
        width = "100%",
        side = "left",
        selected = "Basic",
        tabPanel("Basic", plotOutput("cc_basic_bar")),
        tabPanel("Undergraduate Profile", plotOutput("cc_ugprof_bar")),
        tabPanel("Size and Setting", plotOutput("cc_size_bar"))
      )
    )
  })
  
  
  
  
  
  ## Organizational Profile ##
  
  ## start year
  start_year_bar <- function() {
    census_df %>%
      ggplot() +
      geom_bar(aes(x= year(services_year)), stat = "count", size =1.5, fill = "#186063") +
      bbc_style_1() +
      scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
      labs(x = "Year",
           y = element_blank())
  }
  
  output$start_year_bar <- 
    renderPlot({
      start_year_bar()
    })
  
  ## crp start year
  crp_start_year_bar <- function() {
    census_df %>%
      ggplot() +
      geom_bar(aes(x= year(services_year)), stat = "count", size =1.5, fill = "#186063") +
      theme_hc() +
      scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
      labs(x = "Year",
           y = element_blank())
  }
  
  output$crp_start_year_bar <- 
    renderPlot({
      crp_start_year_bar()
    })
  
  ## services
  
  output$services_bar_org <- 
    renderPlot({
      services_baseline_gather <- services_gather()
      
      services_baseline_gather %>%
        ggplot() +
        geom_bar(aes(x = services), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(services_baseline_gather$services))) +
        coord_flip()
      
    })
  
  output$services_table_org <- function() {
    services_baseline_gather <- services_gather()
    
    services_baseline_gather %>%
      count(services, .drop = FALSE) %>% 
      filter(!is.na(services)) %>%
      mutate(percentage = percent(n / nrow(census_df))) %>%
      kable("html", col.names = c("Types of Programs and Services", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  ## accomodation
  
  accommodation <- function() {
    dt <-
        school_df() %>%
          select(starts_with("services_crp_accomm")) %>%
          gather(1:7,
             key = "number",
             value = "accommodation") %>%
          filter(!is.na(accommodation)) %>%
          select(2)
    
    dt %>% kable(col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  output$accommodation_dt <-
    renderUI({
      dt <- accommodation()
      
      print(dt, 
            method = 'render',
            totals = FALSE,
            omit.headings = TRUE,
            bootstrap.css = FALSE)
    })
  
  output$organizational_info <- renderUI({
    fluidRow(
      div(h2("Contact Information")),
      div(h4(strong("Institution Name:  "), school_df()$INSTNM[1])),
      div(h4(strong("OPEID:  "), school_df()$ope_id[1])),
      div(h4(strong("Program Name:  "), school_df()$highered_prog_name[1])),
      div(h4(strong("Department:  "), school_df()$highered_program_dept[1])),
      div(h4(strong("e-mail:  "), school_df()$contact_email[1])),
      div(h4(strong("url:  "), school_df()$contact_url[1])),
      div(h4(strong("phone:  "), school_df()$contact_phone[1])),
      div(h4(strong("crisis hotline phone:  "), school_df()$contact_crisis_phone[1])),
      div(h4(strong("hours of operation:  "), school_df()$services_crp_hours[1]))
    )
  })
  
  
  output$organizational_info_charts <- renderUI({
    fluidRow(
      div(h3(strong("Start Year"))),
      div(h4('The collegiate recovery census prompted respondents to answer, "What year did you initiate efforts to 
              serve/support students in recovery?" Respondents indicated their efforts started in the following years:')),
      div(h3("Number of collegiate programs started each year")),
      plotOutput("start_year_bar"),
      br(),
      
      div(h3(strong("Types of Programs and Services"))),
      div(h4('The collegiate recovery census prompted respondents to answer, "What programs or services do you provide at 
             your institution for students in recovery?" Among respondents, the totals for each type of service are as follows:')),
      div(h4("N = ", strong(nrow(census_df)))),
      sidebarLayout(
        mainPanel(
          plotOutput("services_bar_org")
        ),
        sidebarPanel(
          tableOutput("services_table_org")
        )  
      )
    )
  })
  
  
  
  ####### PROGRAMS AND SERVICES ##########
  
  ## Sample Size
  
  output$sample_size <- renderUI({
    div(h4(HTML(paste0("Total number in sample = <b> ", nrow(census_filter()), "</b>"))))
  }) 
  
  ##### Engagement ----------------------------------
  
  ## Students who self-identify
  
  engage_gather <- function() {
    ## select all columns that end in services_crp_engage, use Response.ID to organize the rows
    crp_engage <- census_filter() %>% select(Response.ID, services_crp_engage)
    
    ## Remove all rows where services is blank
    crp_engage <- crp_engage[!(crp_engage$services_crp_engage==""), ] %>% arrange(Response.ID)
    
    ## change to factor and set order of levels
    crp_engage$services_crp_engage <- 
      factor(crp_engage$services_crp_engage, levels = c("0-5",
                                                        "6-10",
                                                        "11-15",
                                                        "16-20",
                                                        "21-30",
                                                        "31-40",
                                                        "41-50",
                                                        "50+",
                                                        "Not known"))
    
    return(crp_engage)
  }
  
  output$crp_engage_bar <- 
    renderPlot({
      df <- engage_gather()
      
      df %>%
        ggplot() +
        geom_bar(aes(x = services_crp_engage), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = levels(df$services_crp_engage))
      
    })
  
  output$crp_engage_kable <- function() {
    df <- engage_gather()
    
    df %>%
      count(services_crp_engage, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_filter()))) %>%
      kable("html", col.names = c("Students in Program", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  
  ## Students who regularly attend
  
  engage_participate_gather <- function() {
    ## select all columns that end in services_crp_participate, use Response.ID to organize the rows
    crp_engage <- census_filter() %>% select(Response.ID, services_crp_participate)
    
    ## Remove all rows where services is blank
    crp_engage <- crp_engage[!(crp_engage$services_crp_participate==""), ] %>% arrange(Response.ID)
    
    ## change to factor and set order of levels
    crp_engage$services_crp_participate <- 
      factor(crp_engage$services_crp_participate, levels = c("0-5",
                                                        "6-10",
                                                        "11-15",
                                                        "16-20",
                                                        "21-30",
                                                        "31-40",
                                                        "41-50",
                                                        "50+",
                                                        "Not known"))
    
    return(crp_engage)
  }
  
  output$crp_participate_bar <- 
    renderPlot({
      df <- engage_participate_gather()
      
      df %>%
        ggplot() +
        geom_bar(aes(x = services_crp_participate), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = levels(df$services_crp_participate)) 
      
    })
  
  output$crp_particpate_kable <- function() {
    df <- engage_participate_gather()
    
    df %>%
      count(services_crp_participate, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_filter()))) %>%
      kable("html", col.names = c("Students Who Regularly Participate", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  ## Students served per year
  
  participate_gather <- function() {
    ## select all columns that end in services_crp_participate, use Response.ID to organize the rows
    crp_engage <- census_filter() %>% select(Response.ID, services_participate)
    
    ## Remove all rows where services is blank
    crp_engage <- crp_engage[!(crp_engage$services_participate==""), ] %>% arrange(Response.ID)
    
    ## change to factor and set order of levels
    crp_engage$services_participate <- 
      factor(crp_engage$services_participate, levels = c("0-10",
                                                         "11-20",
                                                         "21-30",
                                                         "31-40",
                                                         "41-50",
                                                         "50+",
                                                         "Not known"))
    
    return(crp_engage)
  }
  
  output$participate_bar <- 
    renderPlot({
      df <- participate_gather()
      
      df %>%
        ggplot() +
        geom_bar(aes(x = services_participate), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = levels(df$services_participate)) 
      
    })
  
  output$participate_kable <- function() {
    df <- participate_gather()
    
    df %>%
      count(services_participate, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_filter()))) %>%
      kable("html", col.names = c("Students Served per Year", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  
  ##### Housing -----------------------------------------
  
  ## Services Housing
  
  services_housing_gather <- function() {
    ## select all columns that end in services_housing, use Response.ID to organize the rows
    df <- census_filter() %>% select(Response.ID, matches("services_housing$"))
    
    ## gather all of the responses
    df_gather <-
      df %>% 
      gather(2:9,
             key = "drop_value",
             value = "value")
    
    ## drop column 2
    df_gather <- df_gather %>% select(-2)
    
    ## Remove all rows where value is blank
    df_gather <- df_gather[!(df_gather$value==""), ] %>% arrange(Response.ID)
    
    ## Remove all rows where value is NA
    df_gather <- df_gather[!is.na(df_gather$value), ]
    
    ## change to factor and set order of levels
    df_gather$value <- 
      factor(df_gather$value, levels = c("General interest sober lifestyle housing",
                                         "Sober roommate matching",
                                         "Recovery roommate matching",
                                         "A recovery wing, floor, or hall/house on campus",
                                         "A recovery house off campus owned/operated by the school",
                                         "A recovery house off campus owned/operated by a private entity",
                                         "A recovery living learning community",
                                         "Other - Write In"))
    
    return(df_gather)
  }
  
  output$services_housing_bar <- 
    renderPlot({
      df_gather <- services_housing_gather()
      
      df_gather %>%
        ggplot() +
        geom_bar(aes(x = value), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(df_gather$value)), labels = rev(c("General interest sober lifestyle housing",
                                                                           "Sober roommate matching",
                                                                           "Recovery roommate matching",
                                                                           "A recovery wing, floor, or hall/house on campus",
                                                                           "A recovery house off campus\nowned/operated by the school",
                                                                           "A recovery house off campus\nowned/operated by a private entity",
                                                                           "A recovery living learning community",
                                                                           "Other - Write In"))) +
        coord_flip()
    })
  
  output$services_housing_kable <- function() {
    df_gather <- services_housing_gather() 
    
    df_gather %>%
      count(value, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_filter()))) %>%
      kable("html", col.names = c("Housing Services", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  
  ## Housing Support Level
  
  housing_support_level_gather <- function() {
    ## select , use Response.ID to organize the rows
    df <- census_filter() %>% select(Response.ID, services_housing_support_level)
    
    ## Remove all rows where services is blank
    df <- df[!(df$services_housing_support_level==""), ] %>% arrange(Response.ID)
    
    ## change to factor and set order of levels
    df$services_housing_support_level <- 
      factor(df$services_housing_support_level, levels = c("Level 1 - Peer run",
                                                           "Level 2 - Monitored",
                                                           "Level 3 - Supervised",
                                                           "Level 4 - Service provide",
                                                           "Not designated"))
    
    return(df)
  }
  
  output$housing_support_level_bar <- 
    renderPlot({
      df <- housing_support_level_gather()
      
      df %>%
        ggplot() +
        geom_bar(aes(x = services_housing_support_level), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(df$services_housing_support_level))) +
        coord_flip()
      
    })
  
  output$housing_support_level_kable <- function() {
    df <- housing_support_level_gather()
    
    df %>%
      count(services_housing_support_level, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_filter()))) %>%
      kable("html", col.names = c("Housing Support Level", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  
  
  ##### Recovery Support --------------------------------
  
  ## Services Baseline Recovery
  
  services_recovery_gather <- function() {
    ## select all columns that end in services_baseline_recovery, use Response.ID to organize the rows
    df <- census_filter() %>% select(Response.ID, matches("services_baseline_recovery$"))
    
    ## gather all of the responses
    df_gather <-
      df %>% 
      gather(2:11,
             key = "drop_value",
             value = "value")
    
    ## drop column 2
    df_gather <- df_gather %>% select(-2)
    
    ## Remove all rows where value is blank
    df_gather <- df_gather[!(df_gather$value==""), ] %>% arrange(Response.ID)
    
    ## Remove all rows where value is NA
    df_gather <- df_gather[!is.na(df_gather$value), ]
    
    ## change to factor and set order of levels
    df_gather$value <- 
      factor(df_gather$value, levels = c("Substance use disorder recovery",
                                         "Mental health recovery",
                                         "Behavioral addiction recovery",
                                         "Eating disorder recovery",
                                         "Gambling addiction recovery",
                                         "Trauma recovery",
                                         "Video game or other technology recovery",
                                         "Recovery from problematic sexual behavior",
                                         "Support for those affected by addiction",
                                         "Other - Write In"))
    
    return(df_gather)
  }
  
  output$services_recovery_bar <- 
    renderPlot({
      df_gather <- services_recovery_gather()
      
      df_gather %>%
        ggplot() +
        geom_bar(aes(x = value), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(df_gather$value))) +
        coord_flip()
    })
  
  output$services_recovery_kable <- function() {
    df_gather <- services_recovery_gather() 
    
    df_gather %>%
      count(value, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_filter()))) %>%
      kable("html", col.names = c("Types of Recovery Services", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  ## Services Accommodations
  
  services_accomm_gather <- function() {
    ## select all columns that end in services_accomm, use Response.ID to organize the rows
    df <- census_filter() %>% select(Response.ID, matches("services_accomm$"))
    
    ## gather all of the responses
    df_gather <-
      df %>% 
      gather(2:8,
             key = "drop_value",
             value = "value")
    
    ## drop column 2
    df_gather <- df_gather %>% select(-2)
    
    ## Remove all rows where value is blank
    df_gather <- df_gather[!(df_gather$value==""), ] %>% arrange(Response.ID)
    
    ## Remove all rows where value is NA
    df_gather <- df_gather[!is.na(df_gather$value), ]
    
    ## change to factor and set order of levels
    df_gather$value <- 
      factor(df_gather$value, levels = c("Access to lecture notes",
                                         "Early class registration",
                                         "Extra time on tests",
                                         "Medical withdrawals for students to attend treatment",
                                         "Priority housing sign-ups, single rooms, or room changes",
                                         "Not aware of any accommodations for students in recovery",
                                         "Other - Write In"
                                         ))
    
    return(df_gather)
  }
  
  output$services_accomm_bar <- 
    renderPlot({
      df_gather <- services_accomm_gather()
      
      df_gather %>%
        ggplot() +
        geom_bar(aes(x = value), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(df_gather$value)), labels = rev(c("Access to lecture notes",
                                                                               "Early class registration",
                                                                               "Extra time on tests",
                                                                               "Medical withdrawals for students\nto attend treatment",
                                                                               "Priority housing sign-ups, single rooms,\nor room changes",
                                                                               "Not aware of any accommodations\nfor students in recovery",
                                                                               "Other - Write In"
          
        ))) +
        coord_flip()
    })
  
  output$services_accomm_kable <- function() {
    df_gather <- services_accomm_gather() 
    
    df_gather %>%
      count(value, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_filter()))) %>%
      kable("html", col.names = c("Support Services", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  
  ##### Prevention and Intervention ---------------------
  
  prevention_gather <- function() {
    ## select all columns that end in services_prevention, use Response.ID to organize the rows
    prevention <- census_df %>% select(Response.ID, services_crp_stage, matches("services_prevention$"))
    
    ## gather all of the responses
    crp_gather <-
      prevention %>% 
      gather(3:20,
             key = "prevention_drop",
             value = "prevention")
    
    ## drop column 3
    crp_gather <- crp_gather %>% select(-3)
    
    ## Remove all rows where services is blank
    crp_gather <- crp_gather[!(crp_gather$prevention==""), ] %>% arrange(Response.ID)
    
    ## change to factor and set order of levels
    crp_gather$prevention <- 
      factor(crp_gather$prevention, levels = c("Alcohol and Drug Counselors",
                                                        "Alcohol, Tobacco, and Other Drugs (ATOD) prevention",
                                                        "Community collaborations",
                                                        "Early warning system",
                                                        "Harm Reduction: Naloxone training/distribution",
                                                        "Harm Reduction: Drug testing kits",
                                                        "Health and wellness classes",
                                                        "Mental Health Counselors",
                                                        "Multi-Tier System of Supports (MTSS)",
                                                        "Parent education",
                                                        "Prevention clubs",
                                                        "Prevention professionals",
                                                        "School-Based Health Centers (SBHC)",
                                                        "Screening, Brief Intervention, and Referral to Treatment (SBIRT)",
                                                        "Social/Emotional Learning (SEL) programs",
                                                        "Social Workers",
                                                        "Substance misuse prevention programs",
                                                        "Other - Write In"))
    
    return(crp_gather)
  }
  
  prevention_filter <- reactive({
    prevention_gather() %>% filter(services_crp_stage %in% input$stage)
  })
  
  output$prevention_bar <- 
    renderPlot({
      df <- prevention_filter()
      
      df %>%
        ggplot() +
        geom_bar(aes(x = prevention), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(df$prevention))) +
        coord_flip()
      
    })
  
  output$prevention_kable <- function() {
    df <- prevention_filter()
    
    df %>%
      count(prevention, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_filter()))) %>%
      kable("html", col.names = c("Prevention and Intervention Services", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  
  ##### Admissions --------------------------------
  
  ## Admission process
  
  crp_admin_gather <- function() {
    ## select , use Response.ID to organize the rows
    df <- census_filter() %>% select(Response.ID, services_crp_admin)
    
    ## Remove all rows where services is blank
    df <- df[!(df$services_crp_admin==""), ] %>% arrange(Response.ID)
    
    ## change to factor and set order of levels
    df$services_crp_admin <- 
      factor(df$services_crp_admin, levels = c("Students in recovery typically come to us prior to applying, and we guide them through the admissions process.",
                                               "The admissions office will contact us through a formalized or regular process if they become aware of a student in recovery who is applying.",
                                               "The admissions office has occasionally contacted us if they become aware of a student in recovery who is applying, but there is not a formal or regular process.",
                                               "The program has no relationship with the admissions office.",
                                               "None of the above statements best describes our admission process.",
                                               "There is a different statement that best describes our admission process - Write In"))
    
    return(df)
  }
  
  output$crp_admin_bar <- 
    renderPlot({
      df <- crp_admin_gather()
      
      df %>%
        ggplot() +
        geom_bar(aes(x = services_crp_admin), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(df$services_crp_admin))) +
        coord_flip()
      
    })
  
  output$crp_admin_kable <- function() {
    df <- crp_admin_gather()
    
    df %>%
      count(services_crp_admin, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_filter()))) %>%
      kable("html", col.names = c("Response", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  output$crp_admin_writein_kable <- function() {
    df <- census_df %>% select(Response.ID, matches("Write.In.services_crp_admin$"))
    
    colnames(df) <- c("Response.ID", "write_in")
    
    ## Remove all rows where services is blank
    df <- df[!(df$write_in==""), ] %>% arrange(Response.ID)
    
    df$write_in %>%
      kable("html", col.names = NULL) %>%
      kable_styling(c("hover", "condensed"), full_width = F)
  }
  
  ## Admissions Criminal History
  
  output$admissions_criminal_kable <- function() {
    df <- census_filter() %>% select(Response.ID, admissions_criminal)
    
    df$admissions_criminal <- as.character(df$admissions_criminal)
    
    ## Remove all rows where services is blank
    df$admissions_criminal[df$admissions_criminal==""] <- "(No response)"
    
    df$admissions_criminal <- factor(df$admissions_criminal, levels = c("Yes", "No", "(No response)"))
    
    df %>%
      count(admissions_criminal, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_filter()))) %>%
      kable("html", col.names = c("Response", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  output$admissions_criminal_describe_kable <- function() {
    df <- census_filter() %>% select(Response.ID, admissions_criminal_describe)
    
    ## Remove all rows where services is blank
    df <- df[!(df$admissions_criminal_describe==""), ] %>% arrange(Response.ID)
    
    df$admissions_criminal_describe %>%
      kable("html", col.names = NULL) %>%
      kable_styling(c("hover", "condensed"), full_width = T)
  }
  
  
  ####### PROGRAM CHARACTERISTICS ##########
  
  output$crp_stage_kable <- function() {
    census_df %>%
      count(services_crp_stage, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_df))) %>%
      kable("html", col.names = c("Stage of Development", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"))
  }
  
  output$crp_focus_kable <- function() {
    census_df %>%
      count(services_crp_focus, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_df))) %>%
      kable("html", col.names = c("Primary Focus", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"))
  }
  
  output$crp_space_kable <- function() {
    census_df %>%
      count(services_crp_space, .drop = FALSE) %>% 
      mutate(percentage = percent(n / nrow(census_df))) %>%
      kable("html", col.names = c("CRP Space", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"))
  }
  
  ## CRP Practices
  
  practices_gather <- function() {
    ## select all columns that end in services_crp_practices, use Response.ID to organize the rows
    crp_practices <- census_df %>% select(Response.ID, matches("services_crp_practices$"))
    
    ## gather all of the responses
    crp_practices_gather <-
      crp_practices %>% 
      gather(2:28,
             key = "practices_drop",
             value = "practices")
    
    ## drop column 2
    crp_practices_gather <- crp_practices_gather %>% select(-2)
    
    ## Remove all rows where services is blank
    crp_practices_gather <- crp_practices_gather[!(crp_practices_gather$practices==""), ] %>% arrange(Response.ID)
    
    ## Remove unusual characters
    index <- str_detect(crp_practices_gather$practices, "^Plan activities with")
    crp_practices_gather$practices[index] <- "Plan activities with students' families (e.g. parents' weekend)"
    
    ## change to factor and set order of levels
    crp_practices_gather$practices <- 
      factor(crp_practices_gather$practices, levels = c("Advocacy efforts undertaken for student needs",
                                                        "Arrange for access to gyms, sports facilities or intramural activities",
                                                        "Arrange for seminars, classes or academic advising for students",
                                                        "Connect to job-placement, internship and career-day programs",
                                                        "Coordinate events to raise awareness on campus",
                                                        "Facilitate educational and/or life skills workshops",
                                                        "Give presentations on recovery resources in the community",
                                                        "Have a registered student organization or club",
                                                        "Have advocacy, advisory board and/or coalition meetings",
                                                        "Host on-campus 12-step or other mutual aid support groups",
                                                        "Keep consistent drop-in hours",
                                                        "Offer employment opportunities in the CRP",
                                                        "Mentor high school students in recovery",
                                                        "Offer relapse/recurrence prevention training to staff and students",
                                                        "Offer peer mentoring support",
                                                        "Organize sober social events for the recovery community and beyond",
                                                        "Plan activities with students' families (e.g. parents' weekend)",
                                                        "Prevention programs/clubs",
                                                        "Promote community service and other volunteer opportunities",
                                                        "Provide professional counseling",
                                                        "Provide recovery workshops (e.g. spirituality, meditation, 12 steps for self-compassion)",
                                                        "Schedule group meetings other than formal/clinical support group meetings",
                                                        "Set recurring recovery group events (e.g. sober birthday celebrations, weekly dinners etc)",
                                                        "Staff and students attend conferences",
                                                        "Staff-led outings off-campus",
                                                        "Student-led outings off-campus",
                                                        "Other - Write In"))
    
    return(crp_practices_gather)
  }
  
  output$crp_practices_bar <- 
    renderPlot({
      crp_practices_gather <- practices_gather()
      
      crp_practices_gather %>%
        ggplot() +
        geom_bar(aes(x = practices), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(crp_practices_gather$practices))) +
        coord_flip()
      
    })
  
  output$crp_practices_kable <- function() {
    crp_practices_gather <- practices_gather()
    
    crp_practices_gather %>%
      count(practices, .drop = FALSE) %>% 
      filter(!is.na(practices)) %>%
      mutate(percentage = percent(n / nrow(census_df))) %>%
      kable("html", col.names = c("CRP Practices", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F) %>%
      scroll_box(height = "400px")
  }
  
  ## CRP Eligibility Requirements
  
  eligibility_gather <- function() {
    ## select all columns that end in services_crp_practices, use Response.ID to organize the rows
    crp_eligibility <- census_df %>% select(Response.ID, matches("services_crp_require_join$"))
    
    ## gather all of the responses
    crp_eligibility_gather <-
      crp_eligibility %>% 
      gather(2:7,
             key = "requirements_drop",
             value = "requirements")
    
    ## drop column 2
    crp_eligibility_gather <- crp_eligibility_gather %>% select(-2)
    
    ## Remove all rows where services is blank
    crp_eligibility_gather <- crp_eligibility_gather[!(crp_eligibility_gather$requirements==""), ] %>% arrange(Response.ID)
    
    ## change to factor and set order of levels
    crp_eligibility_gather$requirements <- 
      factor(crp_eligibility_gather$requirements, levels = c("Application to be part of the CRP.",
                                                             "Students must be in recovery, including categories other than a substance use disorder, such as mental illness or process.",
                                                             "Students must be in recovery from a substance use disorder in order to participate.",
                                                             "Students must live a substance-free lifestyle in order to participate.",
                                                             "This program is open to any student.",
                                                             "Other - Write In"))
    
    return(crp_eligibility_gather)
  }
  
  output$crp_eligibility_bar <- 
    renderPlot({
      crp_gather <- eligibility_gather()
      
      crp_gather %>%
        ggplot() +
        geom_bar(aes(x = requirements), fill = "#186063") +
        bbc_style_1() +
        scale_x_discrete(limits = rev(levels(crp_gather$requirements))) +
        coord_flip()
      
    })
  
  output$crp_eligibility_kable <- function() {
    crp_gather <- eligibility_gather()
    
    crp_gather %>%
      count(requirements, .drop = FALSE) %>% 
      filter(!is.na(requirements)) %>%
      mutate(percentage = percent(n / nrow(census_df))) %>%
      kable("html", col.names = c("CRP Eligibility Requirements", "n", "%")) %>%
      kable_styling(c("striped", "hover", "condensed"), full_width = F)
  }
  
  ####### DATA FOR ADVOCACY ########
  

  
  ## student involvement
  
  ##Undergrad
    ## frequency
  undergrad_involvement_freq <- function() {
    
    census_tidy$services_crp_engage_undergrad <- 
      factor(census_tidy$services_crp_engage_undergrad, 
             levels = c("0-5","6-10","11-15","16-20","21-30","31-40","41-50","50+"))
    
    census_tidy$services_crp_engage_undergrad %>% freq() %>% kable() %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  output$undergrad_involvement_freq <-
    renderUI({
      uif <- undergrad_involvement_freq()
      
      print(uif, 
            method = 'render',
            totals = FALSE,
            omit.headings = TRUE,
            bootstrap.css = FALSE)
    })
  
    ## bar chart
  undergrad_involvement_bar <- function() {
    census_tidy$services_crp_engage_undergrad <- 
      factor(census_tidy$services_crp_engage_undergrad, 
             levels = c("0-5","6-10","11-15","16-20","21-30","31-40","41-50","50+"))
    
    census_tidy %>%
      ggplot() +
      geom_bar(aes(x=services_crp_engage_undergrad), stat = "count", fill = "#186063") +
      theme_hc() +
      labs(x = "# of undergraduates",
           y = element_blank())
  }
  
  output$undergrad_involvement_barchart <- 
    renderPlot({undergrad_involvement_bar()})
  
  ##Grad
  ## frequency
  grad_involvement_freq <- function() {
    
    census_tidy$services_crp_engage_grad <- 
      factor(census_tidy$services_crp_engage_grad, 
             levels = c("0-5","6-10","11-15","16-20","21-30","31-40","41-50","50+"))
    
    census_tidy$services_crp_engage_grad %>% freq() %>% kable() %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  output$grad_involvement_freq <-
    renderUI({
      gif <- grad_involvement_freq()
      
      print(gif, 
            method = 'render',
            totals = FALSE,
            omit.headings = TRUE,
            bootstrap.css = FALSE)
    })
  
  ## bar chart
  grad_involvement_bar <- function() {
    census_tidy$services_crp_engage_grad <- 
      factor(census_tidy$services_crp_engage_grad, 
             levels = c("0-5","6-10","11-15","16-20","21-30","31-40","41-50","50+"))
    
    census_tidy %>%
      ggplot() +
      geom_bar(aes(x=services_crp_engage_grad), stat = "count", fill = "#186063") +
      theme_hc() +
      labs(x = "# of graduate students",
           y = element_blank())
  }
  
  output$grad_involvement_barchart <- 
    renderPlot({grad_involvement_bar()})
  
  output$findings_page <- renderUI({
    fluidPage(
      fluidRow(
        div(h3(strong("Start Year"))),
        div(h4('The collegiate recovery census prompted respondents to answer, "What year did you start serving/supporting
               students in recovery?" Among respondents they indicated their programs started in the following years:')),
        div(h3("Number of collegiate programs started each year")),
        plotOutput("start_year_bar"),
        br(),
        div(h3(strong("Student involvement"))),
        div(h4('The collegiate recovery census prompted respondents to answer the following three questions regarding student
               involvement: (1) "How many undergraduate students are involved?", (2) "How many graduate students are
               involved?", and (3) "How many students regularly attend activities, events, or support services?" Respondents
               then selected from the following options: (1) 0-5, (2) 6-10, (3) 11-15, (4) 16-20, (5) 21-30, (6) 31-40, (7) 41-50,
               (8) 50+. 113 of 127 respondents answered the question about undergraduate student involvement. 110 of 127
               respondents answered the question about graduate student involvement. 111 of 127 respondents answered the
               question about regular student attendance. Among respondents they outlined student involvement as follows:')),
        
        div(h3("Undergraduate student involvement")),
        box(uiOutput("undergrad_involvement_freq")),
        box(plotOutput("undergrad_involvement_barchart")),
        
        div(h3("Graduate student involvement")),
        box(uiOutput("grad_involvement_freq")),
        box(plotOutput("grad_involvement_barchart"))
      )
    )
  })
  
  ####### WORKSHOP #######
  
  ##### CRP Business Case #####
  
  bc_select <- reactive({
    bc_schools %>% filter(University == input$school)
  })

  ## How many students on campus can benefit?
  students_helped <- function(population) {
    x = population * .1252 ## total population * SUD estimate
    y = x * .0324 ## total with SUD * percentage who will seek help
    print(round(y, digits = 2))
  }
  
  output$students_helped <- renderText({
    paste0(students_helped(bc_select()$Blended.Tuition[1]), " students at your institution could benefit from Collegiate Recovery.")
  })
  
  output$tuition_number <- renderUI({
    fluidRow(
    div(h3("Part 2: Understanding the cost of student withdrawal")),
    tags$head(
      tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } 
                 #inline .form-group { display: table-row;}")
      ),
    tags$div(id = "inline", textInput(inputId = "tuition",
                                      label = "What is the average student contribution for an academic year? $  ",
                                      value = bc_select()$Blended.Tuition[1])),
    p("(The above suggested value is derived by weighing in-state and out-of-state tuition)")
    )
  })
  
}

  
##########################
##########################

### RUN APP
shinyApp(ui, server)

###########################################################
###########################################################
