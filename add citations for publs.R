library(tidyverse)
p <- p %>% 
    separate(links, into = c("ipt", "doi", "gbif"), sep = "\r\n") %>% 
    select(-ipt) %>% 
    transmute(
        publ_id, 
        # doi, 
        gbif = case_when(str_detect(doi, "doi.org") ~ gbif, TRUE ~ doi)
    )
p$citation <- rep("", 37)

for(i in 1:37){
    htm <- readLines(p$gbif[i])
    p$citation[i] <- htm %>% 
        str_subset("Faunistics International D I") %>% 
        str_replace_all("Faunistics International D I", "Faunistics International") %>% 
        str_remove_all("<(.*?)>") %>% 
        str_squish()
    cat(i, "end\n")
}



