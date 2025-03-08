# aggregate the results and draw pictures
rm(list = ls())
sapply(c("dplyr", "tidyr", "ggplot2"), function(pkg) if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
})

if(!dir.exists("./images")) {
    dir.create("./images")
}

# gpt3_date   <- as.Date("2020-06-11")
gpt3.5_date <- as.Date("2022-11-30")
# gpt4_date   <- as.Date("2023-03-14")

result_files <- list.files(path = "./data/fetched/raw_without_text", full.names = TRUE)
result_df_list <- lapply(result_files, read.delim)
names(result_df_list) <- sub(".*raw_without_text/(.*).txt", "\\1", result_files)
result <- bind_rows(result_df_list, .id = "filetag")
result$date <- sub(".*-(20.*)", "\\1", result$filetag)
result$journal <- sub("(.*)-20.*", "\\1", result$filetag)
result$filetag <- NULL

Nchars_threshold <- 5000 # Crude filter for editorials, commentaries and other small non-papers
papers_by_journal_per_date <- result %>% 
    filter(Nchars > Nchars_threshold) %>%
    group_by(journal, date) %>% 
    summarise(n_papers = n(), sum_chars = sum(Nchars), .groups = "keep") %>% 
    ungroup()

# Consistent colors=======
if (length(unique(papers_by_journal_per_date$journal)) <= 32) {
    # based on: cat(Seurat::DiscretePalette(32, palette = "glasbey"), sep="\", \"")
    journal_colors <- c("#93D4FF", "#FE8F42", "#40E0D0", "#766C95", "#B1CC71", "#858567", "#FF0000",
                        "#02AD24", "#FFD300", "#3d608d", "#783FC1", "#DC5E93", "#009FFF", "#005300",
                        "#F1085C", "#201A01", "#DD00FF", "#000033", "#720055", "#9A4D42", "#FF00B6",
                        "#00FF00", "#886C00", "#FFB79F", "#858567", "#A10300", "#14F9FF", "#0000FF",
                        "#C8FF00", "#004CFF", "#F2F318", "#00FFBE", "#FFACFD")
    journal_colors <- journal_colors[1:length(unique(papers_by_journal_per_date$journal))]
    names(journal_colors) <- unique(papers_by_journal_per_date$journal)
} else {
    stop("Too many journals for visualisation\n")
}

# Word's rarity & popularity thresholds-------
too_high_word_prob_threshold <- 0.8
too_low_word_prob_threshold <- 0.01

incidence_prob_summary <- result %>% 
    filter(Nchars > Nchars_threshold) %>%
    select(-Nwords, -pmc_id) %>% 
    pivot_longer(-c(journal, date, Nchars), names_to = "word", values_to = "n") %>% 
    mutate(incidence = ifelse(n > 0, 1, 0)) %>% 
    group_by(journal, date, word) %>%
    summarise(papers_with_word = sum(incidence), .groups = "keep") %>% 
    ungroup() %>% 
    left_join(papers_by_journal_per_date, by = c("journal", "date")) %>% 
    mutate(prob_paper_with_word = papers_with_word/n_papers) %>% 
    group_by(word, date) %>% 
    summarise(prob_word_median_by_date = median(prob_paper_with_word), .groups = "keep") %>% 
    ungroup()

too_popular_words <- incidence_prob_summary %>% 
    group_by(word) %>% 
    summarise(prob_word_median = median(prob_word_median_by_date), .groups = "keep") %>% 
    ungroup() %>% 
    filter(prob_word_median > too_high_word_prob_threshold) %>% 
    pull(word)
cat(sprintf("Words that are too popular: %s\n", paste(too_popular_words, collapse = ", ")))

too_rare_words <- incidence_prob_summary %>% 
    group_by(word) %>% 
    summarise(prob_word_median = median(prob_word_median_by_date), .groups = "keep") %>% 
    ungroup() %>% 
    filter(prob_word_median < too_low_word_prob_threshold) %>% 
    pull(word)
cat(sprintf("Words that are too rare: %s\n", paste(too_rare_words, collapse = ", ")))

if(length(unique(incidence_prob_summary$word[!(incidence_prob_summary$word %in% c(too_popular_words,too_rare_words))])) == 0) {
    stop("Rarity/popularity thresholds are too tight!\n")
}

# N papers per period plot===========
n_papers_df <- read.delim("./data/fetched/n_papers_per_quarter.txt")
    
ggplot(data = n_papers_df,
       aes(x = as.Date(date), y = n, color = journal, lty = journal))+
    geom_line()+
    coord_trans(y = "log")+
    scale_y_continuous(breaks = c(500, 1000, 2000, 4000, 8000))+
    scale_color_manual(values = journal_colors)+
    scale_linetype_manual(values = rep(c("solid", "dashed", "longdash", "dotdash"), 100))+
    geom_vline(xintercept = as.numeric(gpt3.5_date), lty = 2, color = "black")+
    geom_label(data = NULL,
               aes(x = gpt3.5_date,
                   y = max(n),
                   label = "GPT3.5"), 
               fill =  "grey95", color = "black", size = 2.5, angle = 90, label.size = 0)+
    labs(x = "Date", y = "N papers per 3 month period")
ggsave("./images/n_papers_over_time.png", width = 800, height = 550, units = "px", dpi = 100)

# Simple incidence over time plot=======
incidence_prob <- result %>% 
    filter(Nchars > Nchars_threshold) %>%
    select(-Nwords, -pmc_id) %>% 
    pivot_longer(-c(journal, date, Nchars), names_to = "word", values_to = "n") %>% 
    mutate(incidence = ifelse(n > 0, 1, 0)) %>% 
    group_by(journal, date, word) %>%
    summarise(papers_with_word = sum(incidence), .groups = "keep") %>% 
    ungroup() %>% 
    left_join(papers_by_journal_per_date, by = c("journal", "date")) %>% 
    mutate(prob_paper_with_word = papers_with_word/n_papers)

# **** plots--------
if ("delve" %in% incidence_prob$word) {
    ggplot(incidence_prob[incidence_prob$word == "delve",],
           aes(x = as.Date(date), y = prob_paper_with_word,
               color = journal, lty = journal))+
        geom_line()+
        scale_color_manual(values = journal_colors)+
        scale_linetype_manual(values = rep(c("solid", "dashed", "longdash", "dotdash"), 100))+
        geom_vline(xintercept = as.numeric(gpt3.5_date), lty = "dotted", color = "black")+
        geom_label(data = NULL,
                   aes(x = gpt3.5_date,
                       y = max(incidence_prob$prob_paper_with_word[incidence_prob$word == "delve"]),
                       label = "GPT3.5"), 
                   fill =  "grey95", color = "black", size = 2.5, angle = 90, label.size = 0)+
        labs(x = "Date", y = "Portion of papers with the word over a 3 month period")
    ggsave("./images/incidence_prob_delve.png", width = 800, height = 550, units = "px", dpi = 100)
}

ggplot(incidence_prob %>% 
           filter(!(word %in% too_popular_words)) %>% 
           filter(!(word %in% too_rare_words))
           ,
       aes(x = as.Date(date), y = prob_paper_with_word,
           color = journal, lty = journal))+
    geom_line(lwd = 0.5)+
    scale_color_manual(values = journal_colors)+
    scale_linetype_manual(values = rep(c("solid", "dashed", "longdash", "dotdash"), 100))+
    geom_vline(xintercept = as.numeric(c(gpt3.5_date)), lty = "dotted", color = "black")+
    facet_wrap(~ word, scales = "free_y")+
    labs(x = "Date", y = "Portion of papers with the word over a 3 month period")
ggsave("./images/incidence_prob.png", width = 1200, height = 550, units = "px", dpi = 100)

# Normalised appearance of a word time plot=======
normalised_prob <- result %>% 
    filter(Nchars > Nchars_threshold) %>% # crude filter for editorials, commentaries and other small non-papers
    pivot_longer(-c(journal, date, Nchars, Nwords, pmc_id), names_to = "word", values_to = "n")

if ("delve" %in% normalised_prob$word) {
    the_most_delving_papers <- normalised_prob %>% 
        filter(word == "delve") %>% 
        arrange(desc(n)) %>% 
        mutate(link = sprintf("https://pmc.ncbi.nlm.nih.gov/articles/PMC%s", pmc_id)) %>% 
        dplyr::slice(1:3) %>% 
        pull(link)
    cat("The hardest delving papers are:\n", the_most_delving_papers[1], "\n", 
        the_most_delving_papers[2], "\n", the_most_delving_papers[3], "\n")
    
}

normalised_prob <- normalised_prob %>% 
    group_by(journal, date, word) %>% 
    summarise(words_per_paper = sum(n), .groups = "keep") %>% 
    ungroup() %>% 
    left_join(papers_by_journal_per_date, by = c("journal", "date")) %>% 
    mutate(prob_paper_with_word = words_per_paper/sum_chars)

# **** plots--------
if ("delve" %in% normalised_prob$word) {
    ggplot(normalised_prob[normalised_prob$word == "delve",],
           aes(x = as.Date(date), y = prob_paper_with_word,
               color = journal, lty = journal))+
        geom_line()+
        scale_color_manual(values = journal_colors)+
        scale_linetype_manual(values = rep(c("solid", "dashed", "longdash", "dotdash"), 100))+
        geom_vline(xintercept = as.numeric(gpt3.5_date), lty = "dotted", color = "black")+
        geom_label(data = NULL,
                   aes(x = gpt3.5_date,
                       y = max(normalised_prob$prob_paper_with_word[normalised_prob$word == "delve"]),
                       label = "GPT3.5"), 
                   fill =  "grey95", color = "black", size = 2.5, angle = 90, label.size = 0)+
        labs(x = "Date", y = "Normalised N words over a 3 month period")
    ggsave("./images/normalised_prob_delve.png", width = 800, height = 550, units = "px", dpi = 100)
}

ggplot(normalised_prob %>% 
           filter(!(word %in% too_popular_words)) %>% 
           filter(!(word %in% too_rare_words))
       ,
       aes(x = as.Date(date), y = prob_paper_with_word,
           color = journal, lty = journal))+
    geom_line(lwd = 0.5)+
    scale_color_manual(values = journal_colors)+
    scale_linetype_manual(values = rep(c("solid", "dashed", "longdash", "dotdash"), 100))+
    geom_vline(xintercept = as.numeric(c(gpt3.5_date)), lty = "dotted", color = "black")+
    facet_wrap(~ word, scales = "free_y")+
    labs(x = "Date", y = "Portion of papers with the word over a 3 month period")
ggsave("./images/normalised_prob.png", width = 1200, height = 550, units = "px", dpi = 100)

# Delve winners plot=====
if ("delve" %in% names(result)) {
    papers_by_journal_per_period <- result %>% 
        filter(Nchars > Nchars_threshold) %>%
        mutate(period = case_when(
            as.Date(date) < gpt3.5_date ~ "pre_gpt",
            as.Date(date) >= gpt3.5_date ~ "post_gpt",
            TRUE ~ "other"
        )) %>% 
        filter(period != "other") %>% 
        group_by(journal, period) %>% 
        summarise(n_papers = n(), sum_chars = sum(Nchars), .groups = "keep") %>% 
        ungroup()
    
    delve_winners <- result %>% 
        filter(Nchars > Nchars_threshold) %>%
        select(-Nwords, -pmc_id) %>% 
        pivot_longer(-c(journal, date, Nchars), names_to = "word", values_to = "n") %>% 
        mutate(incidence = ifelse(n > 0, 1, 0)) %>% 
        filter(word == "delve") %>% 
        dplyr::select(-word) %>% 
        mutate(period = case_when(
            as.Date(date) < gpt3.5_date ~ "pre_gpt",
            as.Date(date) >= gpt3.5_date ~ "post_gpt",
            TRUE ~ "other"
        )) %>% 
        filter(period != "other") %>% 
        group_by(journal, period) %>% 
        summarise(incidence_per_period = sum(incidence), .groups = "keep") %>% 
        ungroup() %>% 
        left_join(papers_by_journal_per_period, by = c("journal", "period")) %>% 
        mutate(prob_paper_with_word = incidence_per_period/n_papers) %>%
        dplyr::select(-n_papers, -incidence_per_period, -sum_chars) %>% 
        pivot_wider(names_from = period, values_from = prob_paper_with_word) %>% 
        mutate(excess = post_gpt - pre_gpt) %>% 
        arrange(excess)
    
    delve_winners$journal <- factor(delve_winners$journal, levels = delve_winners$journal)
    ggplot(data = delve_winners %>% arrange(excess),
           aes(x = excess, y = journal, fill = journal))+
        geom_col()+
        scale_fill_manual(values = journal_colors)+
        labs(x = "Incidence Excess", y = "")+
        theme(legend.position = "none")
    ggsave("./images/delve_incidence_excess.png", width = 800, height = 500, units = "px", dpi = 100)
}

