# aggregate the results and draw pictures
rm(list = ls())
sapply(c("dplyr", "ggplot2"), function(pkg) if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
})

gpt_date <- as.Date("2022-11-30")
gpt_date <- as.Date("2022-11-30")

result_files <- list.files(path = "./data/fetched/raw_without_text", full.names = TRUE)
result_df_list <- lapply(result_files, read.delim)
names(result_df_list) <- sub(".*raw_without_text/(.*).txt", "\\1", result_files)
result <- bind_rows(result_df_list, .id = "filetag")
result$date <- sub(".*-(20.*)", "\\1", result$filetag)
result$journal <- sub("(.*)-20.*", "\\1", result$filetag)
result$filetag <- NULL

result_papers_per_date <- result %>% 
    group_by(journal, date) %>% 
    summarise(n_papers = n(), sum_chars = sum(Nchars), .groups = "keep") %>% 
    ungroup()

# Simple incidence over time plot=======
incidence_prob <- result %>% 
    filter(Nchars > 2000) %>% # crude filter for editorials and other small non-papers
    select(-Nwords, -pmc_id) %>% 
    pivot_longer(-c(journal, date, Nchars), names_to = "word", values_to = "n") %>% 
    mutate(incidence = ifelse(n > 0, 1, 0)) %>% 
    group_by(journal, date, word) %>%
    summarise(papers_with_word = sum(incidence),
              .groups = "keep") %>% 
    ungroup() %>% 
    left_join(result_papers_per_date, by = c("journal", "date")) %>% 
    mutate(prob_paper_with_word = papers_with_word/n_papers)

ggplot(incidence_prob,
       aes(x = as.Date(date), y = prob_paper_with_word,
           color = journal, lty = journal))+
    geom_line(lwd = 0.5)+
    # scale_color_manual(values = ggsci::pal_futurama(palette = c("planetexpress"), alpha = 1)(length(names(table(links_df$journal)))))+
    geom_vline(xintercept = as.numeric(gpt_date), lty = 2, color = "black")+
    facet_wrap(~ word, scales = "free_y")+
    labs(x = "Date", y = "Portion of papers with the word over a 4 month period")

ggsave("./images/incidence_prob.png")
