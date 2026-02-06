files <- dir(pattern = ".sql")

for(i in 1:length(files)){
    paste0(
        "zip -9 ", 
        str_replace(files[i], ".sql", ".zip "),
        files[i]
    ) %>% 
        system()
}
