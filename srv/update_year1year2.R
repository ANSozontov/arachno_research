dbDisconnect(con)
con <- cc()
e <- dbGetQuery(con, "select distinct eventDate from spiders") %>% 
    as_tibble() %>% 
    separate(eventdate, into = c("t1", "t2"), sep = "/", remove = F, fill = "right") %>% 
    mutate(
        test = str_detect(t2, "[:digit:]{4}"),
        test = case_when(is.na(test) ~ FALSE, TRUE ~ test),
        y1 = as.numeric(str_extract(t1, "[:digit:]{4}")),
        y2 = case_when(
            test == FALSE ~ y1,
            TRUE ~ as.numeric(str_extract(t2, "[:digit:]{4}"))
        )
    )
i <- 1
for(i in 1:2101){
    paste0(
        "UPDATE spiders set year1 = ", 
        e$y1[i],
        ", year2 = ", 
        e$y2[i],
        " WHERE eventDate = '",
        e$eventdate[i], 
        "';"
    ) %>% 
        dbSendQuery(con, .)
    if(i %% 100 == 0){cat(i, "\n")}
    }
