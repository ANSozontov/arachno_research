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
    
    # values <- reactiveValues(
    #     
    # )
    
    observeEvent(input$search_run, {
        conditions <- character()
        
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
        #fam_query
        
        # genus
        if (!is.null(input$sql_gen) && 
            !is.na(input$sql_gen) && 
            nchar(str_squish(input$sql_gen)) > 1) {
            gen_query <- paste0("genus ILIKE '%",str_squish(input$sql_gen),"%'" )
        } else {
            gen_query <- " "
        }
        conditions <- c(conditions, gen_query)
        
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
            h4("Интервал дат"),
            numericInput("year1", label = NULL, value = 2015, min = 1770, max = 2025), 
            numericInput("year2", label = NULL, value = 2015, min = 1770, max = 2025),
            br(),
            h4(),
            textInput(
                inputId = "sql_fam", 
                label = "Семейство"
            ),
            textInput(
                inputId = "sql_gen", 
                label = "Род"
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