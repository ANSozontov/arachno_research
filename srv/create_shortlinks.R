con <- cc()
p <- dbGetQuery(con, "select publ_id, author, year from publs where publ_id in(select distinct(publ_id) from spiders)")
p <- as_tibble(p)
p <- p %>% 
    # slice(4:6) %>% 
    pull(author) %>% 
    str_split(", ") %>% 
    map(~map_chr(.x, ~str_split_1(.x, " ")[1])) %>% 
    sapply(function(x){
        if(length(x)==1){
            return(x)
        } else if(length(x)==2) {
            return(paste0(x[1:2], collapse = ", "))
        } else {
            return(paste0(x[1], " et al."))
        }
    }) %>% 
    transmute(
        p, 
        publ_id, 
        shortlink = ., 
        shortlink = paste0(shortlink, ", ", year)
    )

ids <- dbGetQuery(con, "select publ_id, occurrenceID from spiders")

for(i in 2:nrow(p)){
    cat(i, "\n")
    dbSendQuery(con, paste0(
        "update spiders set shortlink = '", 
        p$shortlink[i], 
        "' where publ_id = ", 
        p$publ_id[i]
    ))
}

test <- dbGetQuery(con, "SELECT s.shortlink, s.publ_id, p.year, p.author 
FROM (SELECT DISTINCT shortlink, publ_id FROM spiders) AS s
LEFT JOIN (SELECT publ_id, year, author FROM publs) AS p ON s.publ_id = p.publ_id;"
)




