# initial -----------------------------------------------------------------
library(sf)
library(shiny)
library(leaflet)
library(tidyverse)
library(RPostgreSQL)
cc <- function(){
    dbConnect(RPostgreSQL::PostgreSQL(),
              dbname = "rnf_db", # your database
              host = "localhost", 
              port = "5432",
              user = "rnf_app", # your user
              password = readLines("/var/sec/rnf_app.pass") # <- put paswd to this file
    )
}
adm_dict <- readxl::read_excel("adm_list_2025-03-17.xlsx")
tax_list <- readxl::read_excel("wsc_list_2025-08-08.xlsx") %>% 
    mutate (scientificname = paste0(genus, " ", species), .before = 6)
# tax_dict
vcenter <- "/* Center the numeric input container vertically */
      .center-numeric {
        display: flex;
        align-items: center;   /* Vertical alignment */
        justify-content: center; /* Horizontal alignment (optional) */
        height: 12vh;         /* Full viewport height for demonstration */
      }
      /* Adjust label and input alignment if needed */
      .center-numeric .form-group {
        margin-bottom: 0;      /* Remove default bottom margin */
      }
    "

# server ------------------------------------------------------------------
server <- function(input, output, session) {
    
    values <- reactiveValues(
        last_querry = "" , 
        last_table = tibble(x = numeric())
    )
    
    observeEvent(input$change_language, {
        shinyalert::shinyalert(
            title = "Смена языка",
            text = "Смена языка пока не реализована<br> <br>Language change is not implemented yet",
            type = "info", 
            html = TRUE
        )
    })
    
    output$download_block <- NULL 
    
    output$coords_filter  <- NULL
    
    output$box_fam <- renderUI({
        selectInput(
            "sql_fam", 
            label = NULL, 
            choices = c("", unique(tax_list$family)),
            # selected = NULL,
            # multiple = TRUE,
            selectize = TRUE
            )
    })
    
    # output$box_gen <- renderUI({
    #     selectInput(
    #         "sql_gen", 
    #         label = "Род", 
    #         choices = c("", unique(tax_list$genus)),
    #         # selected = NULL,
    #         # multiple = TRUE,
    #         selectize = TRUE
    #     )
    # })
 
    # output$box_spec <- renderUI({
    #     selectInput(
    #         "sql_sp", 
    #         label = "Вид", 
    #         choices = c("", unique(tax_list$species)),
    #         # selected = NULL,
    #         # multiple = TRUE,
    #         selectize = TRUE
    #     )
    # })
    
    output$box_scname <- renderUI({
        selectInput(
            "sql_sp",
            label = NULL,
            choices = c("", unique(tax_list$scientificname)),
            # selected = NULL,
            # multiple = TRUE,
            selectize = TRUE
        )
    })
    
    output$box_region <- renderUI({
        selectInput(
            "sql_province", 
            label = NULL, 
            choices = c("", unique(adm_dict$region), unique(adm_dict$en_region)),
            # selected = NULL,
            # multiple = TRUE,
            selectize = TRUE
        )
    })
    
    observeEvent(input$sql_coords, {
        if(input$sql_coords){
            output$coords_filter <- renderUI({tagList(fluidRow(
                column(
                    4,
                    div(
                        class = "center-numeric", 
                        numericInput("lon1", label = "West", value = -180, min = -180, max = 0)
                    )
                ),
                column(
                    4, 
                    numericInput(
                        "sql_lat2", 
                        label = "North", 
                        value = 90, min = 0, max = 90
                    ), 
                    numericInput("sql_lat1", label = "South", value = -90, min = -90, max = 0)
                ),
                column(
                    4,
                    div(
                        class = "center-numeric", 
                        numericInput("lon2", label = "East", value = 180, min = 0, max = 180)
                    )
                ), 
            ))})
        } else {
            output$coords_filter <- NULL
        }
        })

    observeEvent(input$search_run, {
        # browser()
        conditions <- character()
        
        # year
        {if(is.null(input$sql_year1) || is.na(input$sql_year1)){
            if(is.null(input$sql_year2) || is.na(input$sql_year2)){
                year_query <- " "
            } else {
                year_query <- paste0(" year <= ", input$sql_year2) 
            }
        } else if(is.null(input$sql_year2) || is.na(input$sql_year2)){
            year_query <- paste0(" year >= ", input$sql_year1)   
        } else {
            year_query <- paste0(" year BETWEEN ", min(c(input$sql_year1, input$sql_year2)), 
                                 " AND ",  max(c(input$sql_year1, input$sql_year2)))
        } }
        conditions <- c(conditions, year_query)
        
        # TAXA
        {# family
            if (!is.null(input$sql_fam) && 
                !is.na(input$sql_fam) && 
                nchar(str_squish(input$sql_fam)) > 1) {
                fam_query <- paste0("family ILIKE '%",str_squish(input$sql_fam),"%'" )
            } else {
                fam_query <- " "
            }
            
            # genus
            if (!is.null(input$sql_gen) && 
                !is.na(input$sql_gen) && 
                nchar(str_squish(input$sql_gen)) > 1) {
                gen_query <- paste0("genus ILIKE '%",str_squish(input$sql_gen),"%'" )
            } else {
                gen_query <- " "
            }
            
            # species
            if (!is.null(input$sql_sp) &&
                !is.na(input$sql_sp) &&
                nchar(str_squish(input$sql_sp)) > 1) {
                sp_query <- paste0("\"scientificName\" ILIKE '%",str_squish(input$sql_sp),"%'" )
            } else {
                sp_query <- " "
            }}
        conditions <- c(conditions, fam_query)
        conditions <- c(conditions, gen_query)
        conditions <- c(conditions, sp_query)
        
        # COORDS
        {
            # browser()
            if(input$sql_coords) {
                
                # Не выполнять вообще никакого поиска 
                # если поля для координат заполнены значениями по умолчанию
                
                #latcoord
                if(is.null(input$sql_lat1) || is.na(input$sql_lat1)){
                    if(is.null(input$sql_lat2) || is.na(input$sql_lat2)){
                        lat_query <- " "
                    } else {
                        lat_query <- paste0(" \"decimalLatitude\" <= ", input$sql_lat2) 
                    }
                } else if(is.null(input$sql_lat2) || is.na(input$sql_lat2)){
                    lat_query <- paste0(" \"decimalLatitude\" >= ", input$sql_lat1)   
                } else {
                    lat_query <- paste0(" \"decimalLatitude\" BETWEEN ", min(c(input$sql_lat1, input$sql_lat2)), 
                                        " AND ",  max(c(input$sql_lat1, input$sql_lat2)))
                } 
                
                
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
            } else {
                lat_query <- " "
                lat_query <- " "
            }
        }
        conditions <- c(conditions, lat_query)
        conditions <- c(conditions, lat_query)
        
        # region 
        {
            if (!is.null(input$sql_province) && 
                !is.na(input$sql_province) && 
                nchar(str_squish(input$sql_province)) > 1) {
                if (grepl("^[a-zA-Z\\s]+$", 
                          str_squish(input$sql_province), perl = TRUE) == TRUE){
                    prov_query <- paste0("stateprovince  ILIKE '%",
                                         str_squish(input$sql_province),"%'" ) 
                } else if (grepl("^[А-Яа-яЁё\\s\\-]+$",
                                 str_squish(input$sql_province), perl = TRUE) == TRUE){
                    idx <- grep(str_squish(input$sql_province), adm_dict$region, ignore.case = TRUE)
                    if (length(idx) > 0) {
                        prov_query <- paste0("stateprovince  ILIKE '%",
                                             adm_dict$en_region[idx[1]],"%'" ) 
                    } else {
                        prov_query <- " "
                    }
                }
            } else { 
                prov_query <- " "
            }
        }
        conditions <- c(conditions, prov_query)
        
        # Sex & Age
        {
            # sex
            if (input$sql_males == TRUE) {
                if (input$sql_females == TRUE) {
                    sex_query <- " "
                } else {
                    sex_query <- " (sex ilike 'male%' OR sex ilike '% male%')"
                }
            } else if(input$sql_females == TRUE){
                sex_query <- "sex ilike '%female%'"  
            }  else { 
                sex_query <- " "
            }
            
            
            # life stage
            if (input$sql_adult == TRUE) {
                if (input$sql_juv == TRUE) {
                    lifestage_query <- " "
                } else {
                    lifestage_query <- "\"lifeStage\" ilike '%ad%'"
                }
            } else if(input$sql_juv == TRUE){
                lifestage_query <- "\"lifeStage\" ilike '%juv%'"  
            } else { 
                lifestage_query <- " "
            }
        }
        conditions <- c(conditions, sex_query)
        conditions <- c(conditions, lifestage_query)
        
        # prepare querry
        # drop empty conditions
        conditions <- conditions[conditions != " "]
        
        query <- paste0("SELECT * FROM spiders_test WHERE ", 
                        paste0(conditions, collapse = " AND "), 
                        ";")
        values$last_query <- str_squish(query) %>%
            str_replace("WHERE AND", "WHERE") %>%
            gsub("(AND\\s+)+", "AND ", .)
        
        showNotification(values$last_query, duration = 10, type = "message")
        
        con <- cc()
        # r2 <- dbGetQuery(con, "SELECT * FROM spiders_test;")  %>% rename_with(tolower) %>% as_tibble() %>% select (scientificname)
        values$last_table <- dbGetQuery(con, values$last_query) %>% 
            rename_with(tolower)
        # result <- dbGetQuery(con, values$last_query) # %>% as_tibble()
        dbDisconnect(con)
        
        # for download
        
        #for map
        for.map.result <- values$last_table %>%
            filter(!is.na(decimallatitude), !is.na(decimallongitude)) 
        
        if(nrow(for.map.result) > 0){
            output$results_map <- for.map.result %>% 
                arrange(occurrenceid) %>% 
                group_by(decimallongitude, decimallatitude) %>% 
                summarise(
                    N = paste0("находок: ", n()),
                    occurrenceid = paste0(occurrenceid, collapse = ", "),
                    .groups = "drop") %>% 
                st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) %>% 
                leaflet() %>% 
                addTiles() %>% 
                addCircleMarkers(
                    label = ~N,
                    popup = ~ occurrenceid
                ) %>% 
                renderLeaflet()
        } else {
            output$results_map <- leaflet() %>% addTiles() %>% renderLeaflet()
        }
        
        if(nrow(values$last_table)<1){
            output$results_table <- DT::renderDataTable({
                tibble(`Результаты поиска:` = "отсутствуют...")
            }, escape = FALSE)
            
            output$download_block <- NULL
            
            shinyalert::shinyalert(title = "Не найдено", text = "", type = "warning")
        } else {
            output$results_table <- DT::renderDataTable({
                values$last_table %>% 
                    mutate(y = str_extract(bibliographiccitation, "[:digit:]{4}")) %>% 
                    separate(bibliographiccitation, into = c("author"), sep = "[0-9]", extra = "drop") %>% 
                    mutate(
                        author = str_remove_all(author, "[:upper:]{1}\\."),
                        author = str_replace_all(author, " ,", ",")
                    ) %>% 
                    separate(author, c("a1", "a2", "a3"), sep = ", ", extra = "drop", fill = "right") %>% 
                    mutate(
                        author = case_when(
                            is.na(a2) ~ paste0(str_squish(a1), ", ", y),
                            is.na(a3) ~ paste0(str_squish(a1), ", ", str_squish(a2), ", ", y),
                            TRUE ~ paste0(str_squish(a1), " et al., ", y)
                        ), 
                        .keep = "unused"
                    ) %>% 
                    transmute(
                        article = case_when(
                            is.na(references) ~ author,
                            TRUE ~ paste0(
                                '<a href="',
                                references, '" target="_blank">',
                                author,
                                '</a>')
                        ),
                        Пол = sex,
                        Возраст = lifestage,
                        Страна = countrycode, # country
                        Регион = stateprovince,
                        Место = locality,
                        `N & E` = paste0(
                            round(decimallatitude,  1), "N, ",
                            round(decimallongitude, 1), "E"
                        ),
                        Дата = eventdate,
                        Биотоп = habitat,
                        Семейство = family, # 
                        Вид = scientificname
                        # rank = substr(taxonrank, 1, 3)
                    )
            }, escape = FALSE)
            
            output$download_block <- renderUI({tagList(
                HTML("<h4 style='text-align: center;'>Скачать найденное</h4>"),
                fluidRow(
                    column(
                        6,
                        downloadButton(
                            "download_xls", label = "Excel", style = "width:100%;"
                        ),
                    ),
                    column(
                        6, 
                        downloadButton(
                            "download_tsv", label = "tsv", style = "width:100%;"
                        ),
                    )
                ),
            )})
            
            shinyalert::shinyalert(
                title = ("Успех!"), 
                text = paste0("Обнаружено находок: ", nrow(values$last_table)), 
                type = "success")
        }
    })
    
    output$download_xls <- {downloadHandler(
        filename = function() {
            paste0("result_", format(Sys.time(), "%d-%m-%Y_%Hh%Mm"), ".xlsx")
        },
        content = function(file) {
            writexl::write_xlsx(values$last_table, path = file)
            
        }
    )}
    
    output$download_tsv <- {downloadHandler(
        filename = function() {
            paste0("result_", format(Sys.time(), "%d-%m-%Y_%Hh%Mm"), ".tsv")
        },
        content = function(file) {
            readr::write_delim(
                values$last_table,
                file = file, 
                delim = "\t", 
                na = ""
            )
        }
    )}
    
}


# UI ----------------------------------------------------------------------
ui <- fluidPage( 
    shinyjs::useShinyjs(),
    fluidRow(
        column(
            width = 1, 
            tags$div(style="position: relative; margin-right: 90px", 
                     tags$img(src="logo.png", height = "80px"))
        ), 
        column(width = 10, titlePanel(
            "Arachno(RE)search: поиск литературных данных о находках пауков", 
            windowTitle = "Arachno(RE)search"
        )),
        column(1, actionButton("change_language",
            "RU", 
            icon = icon("globe"),
            style = "position: absolute; top: 8px; right: 5px; z-index:10000;"))
    ),
    tags$style(HTML(".black-hr {border: 1px solid black;margin: 20px 0;}")),
    tags$head(
        tags$link(rel="shortcut icon", href="favicon.ico"), 
        tags$style(HTML(vcenter)),
    ),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            # h2("Условия поиска"),
            # br(),
            HTML("<h4 style='text-align: left;'>Семейство</h4>"),
            uiOutput("box_fam"),
            # uiOutput("box_gen"),
            # uiOutput("box_spec"), 
            HTML("<h4 style='text-align: left;'>Вид (род + видовой эпитет)</h4>"),
            uiOutput("box_scname"),
            div(
                style = "text-align: right; font-size: 10px; font-style: italic;", 
                "*указать актуальное название, синонимия не учтена"
            ),
            # textInput(inputId = "sql_fam", label = "Семейство"),
            # textInput(inputId = "sql_gen", label = "Род"),
            #textInput(inputId = "sql_sp", label = "Вид", placeholder = "Актуальное название. Синонимия не учтена"),
            br(),
            fluidRow(
                column(6, actionButton(
                    inputId = "search_run", 
                    label = "Поиск", 
                    width = "100%", 
                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                column(6, actionButton(
                    inputId = "clear_search", 
                    label = "Очистить", 
                    width = "100%")
                )
            ),
            uiOutput("download_block"),
            hr(),
            # br(),
            HTML("<h4 style='text-align: center;'>Годы находок</h4>"),
            fluidRow(
                column(5, numericInput(
                    "sql_year1", 
                    label = NULL,
                    value = 1770,
                    min = 1770, 
                    max = year(Sys.Date()), 
                    width = "100%"
                    )
                ),
                column(2, HTML('<p style="text-align: center;">–</p>')),
                column(5, numericInput(
                    "sql_year2", 
                    label = NULL, 
                    value = year(Sys.Date()),
                    min = 1770, 
                    max = year(Sys.Date()), 
                    width = "100%"
                    )
                )
            ),
            # br(),
            h4("Координаты"),
            checkboxInput("sql_coords", "Только с координатами", value = FALSE),
            uiOutput("coords_filter"),
            # br(),
            h4("Регион"),
            uiOutput("box_region"), 
            #textInput(inputId = "sql_province", label = NULL),
            # br(),
            fluidRow(
               column(6,
                      h4("Пол"),
                      checkboxInput("sql_males", "Самцы", value = FALSE),
                      checkboxInput("sql_females", "Самки", value = FALSE)),
               column(6,
                      h4("Возраст"),
                      checkboxInput("sql_adult", "Взрослые", value = FALSE),
                      checkboxInput("sql_juv", "Ювенильные", value = FALSE))
           )
        ), 
        mainPanel(tabsetPanel(
            tabPanel(
                "Таблица с результатами",
                DT::DTOutput("results_table")
            ), 
            tabPanel(
                "Карта",
                leafletOutput("results_map")
            ), 
        ))
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
