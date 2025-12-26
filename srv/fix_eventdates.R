a <- dbGetQuery(con, "select distinct publ_id, eventdate, year, scientificname, scientificnameauthorship from spiders ") %>% 
    as_tibble() 

a <- dbGetQuery(con, "select distinct eventdate, year from spiders where year is null order by eventdate") %>% 
    as_tibble() %>% 
    mutate(
        n_char = nchar(eventdate), 
        n_slash = str_count(eventdate, "/"),
        n_defis = str_count(eventdate, "-")
        ) %>% 
    filter(
        # str_detect(eventdate, "/", negate = T), 
        n_char == 7, 
        n_slash == 0, 
        n_defis == 1
        ) %>% 
    mutate(year = as.numeric(substr(eventdate, 1, 4)))

for(i in 1:nrow(a)){
    cat(i, "\n")
    paste0(
        'update spiders set "year" = ',
        a$year[i], 
        " where eventdate = '",
        a$eventdate[i], 
        "';"
    ) %>% 
        dbSendQuery(con, .)
}


