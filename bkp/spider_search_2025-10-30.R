library(tidyverse)
library(RPostgreSQL)
# library(sf)
library(shiny)

cc <- function(){
    dbConnect(RPostgreSQL::PostgreSQL(),
              dbname = "rnf_db", # your database
              host = "localhost", 
              port = "5432",
              user = "rnf_app", # your user
              password = readLines("/var/sec/rnf_app.pass") # <- put paswd to this file
    )
}


server <- function(input, output, session) {
    
    observeEvent(input$search_run, {
        conditions <- character()
        browser()
        # year
        if(is.null(input$year1) || is.na(input$year1)){
            if(is.null(input$year2) || is.na(input$year2)){
                year_query <- " "
            } else {
                year_query <- paste0(" year <= ", input$year2) 
            }
        } else if(is.null(input$year2) || is.na(input$year2)){
            year_query <- paste0(" year >= ", input$year1)   
        } else {
            year_query <- paste0(" year BETWEEN ", min(c(input$year1, input$year2)), 
                                 " AND ",  max(c(input$year1, input$year2)))
        } 
        conditions <- c(conditions, year_query)
        
        # family
        if (!is.null(input$sql_fam) && 
            !is.na(input$sql_fam) && 
            nchar(str_squish(input$sql_fam)) > 1) {
            fam_query <- paste0("family ILIKE '%",str_squish(input$sql_fam),"%'" )
        } else {
            fam_query <- " "
        }
        conditions <- c(conditions, fam_query)

        
        # genus
        if (!is.null(input$sql_gen) && 
            !is.na(input$sql_gen) && 
            nchar(str_squish(input$sql_gen)) > 1) {
            gen_query <- paste0("genus ILIKE '%",str_squish(input$sql_gen),"%'" )
        } else {
            gen_query <- " "
        }
        conditions <- c(conditions, gen_query)
        
        # species
        if (!is.null(input$sql_sp) && 
            !is.na(input$sql_sp) && 
            nchar(str_squish(input$sql_sp)) > 1) {
            sp_query <- paste0("\"specificEpithet\" ILIKE '%",str_squish(input$sql_sp),"%'" )
        } else {
            sp_query <- " "
        }
        conditions <- c(conditions, sp_query)
        
        #latcoord
        if(is.null(input$lat1) || is.na(input$lat1)){
            if(is.null(input$lat2) || is.na(input$lat2)){
                lat_query <- " "
            } else {
                lat_query <- paste0(" \"decimalLatitude\" <= ", input$lat2) 
            }
        } else if(is.null(input$lat2) || is.na(input$lat2)){
            lat_query <- paste0(" \"decimalLatitude\" >= ", input$lat1)   
        } else {
            lat_query <- paste0(" \"decimalLatitude\" BETWEEN ", min(c(input$lat1, input$lat2)), 
                                " AND ",  max(c(input$lat1, input$lat2)))
        } 
        conditions <- c(conditions, lat_query)

        #longcoord
        if(is.null(input$lon1) || is.na(input$lon1)){
            if(is.null(input$lon2) || is.na(input$lon2)){
                lon_query <- " "
            } else {
                lon_query <- paste0(" \"decimalLongitude\" <= ", input$lon2) 
            }
        } else if(is.null(input$lon2) || is.na(input$lon2)){
            lon_query <- paste0(" \"decimalLongitude\" >= ", input$lon1)   
        } else {
            lon_query <- paste0(" \"decimalLongitude\" BETWEEN ", min(c(input$lon1, input$lon2)), 
                                " AND ",  max(c(input$lon1, input$lon2)))
        } 
        conditions <- c(conditions, lon_query)

        # region ?????
        adm_dict <- readxl::read_excel("adm_list_2025-03-17.xlsx")
        
        if (!is.null(input$sql_province) && 
            !is.na(input$sql_province) && 
            nchar(str_squish(input$sql_province)) > 1) {
            if (grepl("^[a-zA-Z\\s]+$", 
                      str_squish(input$sql_province), perl = TRUE) == TRUE){
                prov_query <- paste0("\"stateProvince\"  ILIKE '%",
                                     str_squish(input$sql_province),"%'" ) 
            } else if (grepl("^[А-Яа-яЁё\\s\\-]+$",
                             str_squish(input$sql_province), perl = TRUE) == TRUE){
                idx <- grep(str_squish(input$sql_province), adm_dict$region, ignore.case = TRUE)
                if (length(idx) > 0) {
                    prov_query <- paste0("\"stateProvince\"  ILIKE '%",
                                         adm_dict$en_region[idx[1]],"%'" ) 
                } else {
                    prov_query <- " "
                }
            }
        } else { 
            prov_query <- " "
        }
        conditions <- c(conditions, prov_query)
        
        # sex
        if (input$sql_males == TRUE) {
            if (input$sql_females == TRUE) {
                sex_query <- " "
            } else {
                sex_query <- "sex = 'male'"
            }
        } else if(input$sql_females == TRUE){
            sex_query <- "sex = 'female'"  
        }  else { 
            sex_query <- " "
        }
        conditions <- c(conditions, sex_query)
        
        # life stage
        if (input$sql_adult == TRUE) {
            if (input$sql_juv == TRUE) {
                lifestage_query <- " "
            } else {
                lifestage_query <- "\"lifeStage\" = 'adult'"
            }
        } else if(input$sql_juv == TRUE){
            lifestage_query <- "\"lifeStage\" = 'juvenile'"  
        } else { 
            lifestage_query <- " "
        }
        conditions <- c(conditions, lifestage_query)
        
        # prepare querry
        # drop empty conditions
        conditions <- conditions[conditions != " "]
        
        query <- paste0("SELECT * FROM spiders_test WHERE ", 
                        paste0(conditions, collapse = " AND "), 
                        ";")
        query <- str_squish(query) %>%
            str_replace("WHERE AND", "WHERE") %>%
            gsub("(AND\\s+)+", "AND ", .)
        
        con <- cc()
        result <- dbGetQuery(con, query) # %>% as_tibble()
        dbDisconnect(con)
        
        if(nrow(result)<1){
            output$search_results <- DT::renderDataTable({
                tibble(`Результаты поиска:` = "отсутствуют...")
            }, escape = FALSE)
            shinyalert::shinyalert(title = "Не найдено", text = "", type = "warning")
        } else {
            output$search_results <- DT::renderDataTable({result}, escape = FALSE)
            shinyalert::shinyalert(
                title = ("Успех!"), 
                text = paste0("Обнаружено ", nrow(result), " находок"), 
                type = "success")
        }
    })
}

ui <- fluidPage( 
    sidebarLayout( 
        sidebarPanel(
            h2("Условия поиска"),
            br(),
            h4("Годы находок"),
            fluidRow(
                column(6,numericInput("year1", label = "с", value = 2015, min = 1770, max = 2025)),
                column(6, numericInput("year2", label = "по", value = 2015, min = 1770, max = 2025))
            ),
            br(),
            h4("Научное название"),
            textInput(inputId = "sql_fam", label = "Семейство"),
            textInput(inputId = "sql_gen", label = "Род"),
            textInput(inputId = "sql_sp", label = "Видовой эпитет"),
            br(),
            h4("Координаты находок"),
            fluidRow(
                column(6, numericInput("lat1", label = "South", value = -90, min = -90, max = 0)),
                column(6, numericInput("lat2", label = "North", value = 90, min = 0, max = 90))
            ),
            fluidRow(
                column(6, numericInput("lon1", label = "West", value = -180, min = -180, max = 0)),
                column(6, numericInput("lon2", label = "East", value = 180, min = 0, max = 180))
            ),
            br(),
            h4("Регион"),
            textInput(inputId = "sql_province", label = NULL),
            br(),
            fluidRow(
               column(6,
                      h4("Пол особей"),
                      checkboxInput("sql_males", "Самцы", value = FALSE),
                      checkboxInput("sql_females", "Самки", value = FALSE)),
               column(6,
                      h4("Возраст особей"),
                      checkboxInput("sql_adult", "Взрослые", value = FALSE),
                      checkboxInput("sql_juv", "Ювенильные", value = FALSE))
           ),
            actionButton("search_run", "Поиск") 
        ), 
        mainPanel(
            DT::DTOutput("search_results")
        ) 
    ) 
)  

# Run the app -------------------------------------------------------------
shinyApp(ui = ui, 
         server = server, 
         onStart = function() {
             onStop(function() {
                 cat("Doing application cleanup...\n")
                 # dbDisconnect(con)
                 cat("Application cleanup is done!\n")
             })
         }, 
         options = list(port = 3333, host = "0.0.0.0", launch.browser = F)
)
