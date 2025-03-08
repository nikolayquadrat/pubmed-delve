# script fetches xml for a list of PMC ids and for each of them calculates incidence of a given word 
rm(list = ls())
sapply(c("dplyr", "data.table", "xml2"), function(pkg) if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
})

# Function to parse xml content by headers and count a word occurrence=====
get_word_occurence_from_xml <- function(word, xml_content, xpath) {
    target_node <- xml_find_first(xml_content, xpath)
    cleaned_text <- gsub("[\n\r\t]+", " ", target_node)
    cleaned_text <- gsub("\\s+", " ", cleaned_text)
    cleaned_text <- trimws(cleaned_text)
    
    if (!is.na(cleaned_text)) {
        Nchars <- nchar(cleaned_text)
        Nwords <- length(unlist(strsplit(cleaned_text, "\\s+")))    
        
        pattern <- sprintf("(?:\\b|\\W|\\d)%s(?:\\b|\\W|\\d)", word) # captures only in the starting word context 
        gregexpr_result <- gregexpr(pattern, cleaned_text, ignore.case = TRUE)
        if (gregexpr_result[[1]][1] != -1) {
            Nmatches <- length(gregexpr_result[[1]])
        } else {
            Nmatches <- 0
        }
    } else {
        Nchars <- -1
        Nwords <- -1
        Nmatches <- -1
    }
    
    return(c(Nchars, Nwords, Nmatches))
}

# Load the data========
tag <- "Elife"
cat("Note: The script was tested for the Elife's sections only\n")

result_files <- list.files(path = "./data/fetched/raw_with_text", pattern = sprintf("%s-", tag), full.names = TRUE)
if (length(result_files) == 0) {
    stop(sprintf("No raw files with text for the \"%s\" journal were found\n", tag))
}

result_df_list <- lapply(result_files, data.table::fread, sep = "\t")
names(result_df_list) <- sub(".*raw_with_text/(.*).txt", "\\1", result_files)
result <- bind_rows(result_df_list, .id = "filetag")
result$date <- sub(".*-(20.*)", "\\1", result$filetag)
result$journal <- sub("(.*)-20.*", "\\1", result$filetag)
result$filetag <- NULL

xpaths = c(
    # main text
    "introduction" = "/pmc-articleset/article/body/sec[@id='s1']",
    "results" = "/pmc-articleset/article/body/sec[@id='s2']",
    "discussion" = "/pmc-articleset/article/body/sec[@id='s3']",
    "methods" = "/pmc-articleset/article/body/sec[@id='s4']",
    # public reviews and author's response (the last "sa" section available)
    "sa1" = "/pmc-articleset/article/sub-article[@id='sa1']/body",
    "sa2" = "/pmc-articleset/article/sub-article[@id='sa2']/body",
    "sa3" = "/pmc-articleset/article/sub-article[@id='sa3']/body",
    "sa4" = "/pmc-articleset/article/sub-article[@id='sa4']/body",
    "sa5" = "/pmc-articleset/article/sub-article[@id='sa5']/body",
    "sa6" = "/pmc-articleset/article/sub-article[@id='sa6']/body" # sa6 probably does not exist (that would be five review rounds), just in case
)

words_df <- read.table("./data/words.txt", sep="\t", header=FALSE, stringsAsFactors=FALSE)
words <- setNames(words_df$V2, words_df$V1)

if(!dir.exists("./data/parsing-by-section")) {
    dir.create("./data/parsing-by-section")
}

# Main loop======
for (word in words) {
    file_name <- sprintf("./data/parsing-by-section/%s-%s.txt", tag, names(words)[which(words == word)])
    if(file.exists(file_name)) {
        cat(sprintf("File exists for the word \"%s\"\n", names(words)[which(words == word)]))
        next
    } else {
        cat(sprintf("Parsing %s for the word \"%s\"\n", tag, names(words)[which(words == word)]))
    }
    
    total_iterations <- nrow(result) * length(xpaths)
    result_paper_section <- data.table(
        journal = character(total_iterations),
        date = character(total_iterations),
        pmc_id = character(total_iterations),
        paper_section = character(total_iterations),
        word = character(total_iterations),
        Nchar = integer(total_iterations),
        Nwords = integer(total_iterations),
        n = integer(total_iterations)
    )
    pb <- txtProgressBar(min = 0, max = total_iterations)
    
    row_index <- 1
    for (i in seq_len(nrow(result))) {
        xml_parsed <- read_xml(result$text[i])
        for (xpath in xpaths) {
            occurence_stat <- get_word_occurence_from_xml(word, xml_parsed, xpath)
            
            result_paper_section[row_index, ] <- list(
                tag, result$date[i], result$pmc_id[i], 
                names(xpaths)[which(xpaths == xpath)], 
                names(words)[which(words == word)], 
                occurence_stat[1], occurence_stat[2], occurence_stat[3]
            )
            
            row_index <- row_index + 1
            setTxtProgressBar(pb, row_index)
        }
    }
    
    write.table(result_paper_section, file = file_name, sep = "\t", row.names = FALSE, quote = FALSE)
    close(pb)
}

# beepr::beep(sound = 4)

# Elife's sections plot==========
sections_files <- list.files("./data/parsing-by-section", full.names = TRUE)
sections_data_list <- lapply(sections_files, read.delim)
names(sections_data_list) <- sub(".*parsing-by-section/.*?-(.*).txt", "\\1", sections_files)
sections_data <- bind_rows(sections_data_list, .id = "word")

# define which "sa" section is a response
sections_data_sa <- sections_data %>% 
    select(pmc_id, paper_section, n) %>% 
    filter(!(paper_section %in% c("introduction", "results", "methods", "discussion"))) %>% 
    mutate(sa_num = as.numeric(sub(".*(\\d+).*", "\\1", paper_section))) %>% 
    filter(n != -1) %>% 
    group_by(pmc_id) %>% 
    arrange(desc(sa_num)) %>% 
    dplyr::slice(1) %>% 
    ungroup() %>% 
    mutate(paper_section_refined = case_when(
        paper_section == "sa1" ~ "review",
        TRUE ~ "response"
    )) %>% 
    select(pmc_id, paper_section, paper_section_refined)

sections_data <- sections_data %>% 
    filter(n != -1) %>% 
    left_join(sections_data_sa, by = c("pmc_id", "paper_section")) %>% 
    mutate(paper_section = case_when(
        !is.na(paper_section_refined) ~ paper_section_refined,
        grepl("^sa", paper_section) ~ "review",
        TRUE ~ paper_section
    )) %>% 
    group_by(date, paper_section, word, pmc_id) %>% 
    summarise(n = sum(n), Nchar = sum(Nchar), Nwords = sum(Nwords), .groups = "keep") %>% # aggregate multiple review sections
    ungroup()

# define which papers have an expected paper structure
full_section_sets <- sections_data %>% 
    distinct(pmc_id, paper_section) %>% 
    group_by(pmc_id) %>% 
    summarise(n_sections = n(), .groups = "keep") %>% 
    ungroup() %>% 
    filter(n_sections == 6) %>% 
    pull(pmc_id)

# here the normalisation by word count makes sense
sections_data <- sections_data %>% 
    filter(pmc_id %in% full_section_sets) %>% 
    mutate(n_normalised = n/Nwords) %>% 
    group_by(date, paper_section, word) %>% 
    summarise(word_prob = mean(n_normalised), .groups = "keep") %>% 
    ungroup()

# define which word prob is too low for the plot
word_prob_max <- sections_data %>% 
    group_by(word) %>% 
    summarise(word_prob_max = max(word_prob), .groups = "keep") %>% 
    arrange(desc(word_prob_max)) %>% 
    dplyr::slice(1) %>% 
    ungroup() %>% 
    arrange(desc(word_prob_max)) %>% 
    dplyr::slice(1:20) %>% 
    pull(word)

sections_data$paper_section <- factor(sections_data$paper_section, levels = c("introduction",
                                                                              "methods",
                                                                              "results",
                                                                              "discussion",
                                                                              "review",
                                                                              "response"
                                                                              ))
gpt3.5_date <- as.Date("2022-11-30")

ggplot(sections_data[sections_data$word %in% word_prob_max, ],
       aes(x = as.Date(date), y = word_prob, colour = paper_section))+
    geom_line()+
    geom_vline(xintercept = as.numeric(c(gpt3.5_date)), lty = "dotted", color = "black")+
    facet_wrap(~ word, scales = "free_y")+
    labs(x = "Date", y = "Probability to find the word in the section", colour = "Paper Section")
ggsave("./images/elife_sections_all.png", width = 1200, height = 550, units = "px", dpi = 100)

ggplot(sections_data[sections_data$word == "delve", ],
       aes(x = as.Date(date), y = word_prob, colour = paper_section))+
    geom_line()+
    geom_vline(xintercept = as.numeric(gpt3.5_date), lty = 2, color = "black")+
    geom_label(data = sections_data[sections_data$word == "delve", ][1,],
               aes(x = gpt3.5_date,
                   y = max(sections_data$word_prob[sections_data$word == "delve"]),
                   label = "GPT3.5"),
               fill =  "grey95", color = "black", size = 2.5, angle = 90, label.size = 0)+
    labs(x = "Date", y = "Probability to find the word in the section", colour = "Paper Section")
ggsave("./images/elife_sections_delve.png", width = 800, height = 550, units = "px", dpi = 100)

# Way to investigate a paper's sections hierarchy=======
if (0) { 
    find_parent_hierarchy <- function(node) {
        parents <- c()  # Store parent names
        parent <- xml_parent(node)  # Get the immediate parent
        while (!is.null(parent) && length(parent) > 0) {
            node_name <- xml_name(parent)  # Get parent node name
            node_id <- xml_attr(parent, "id", default = NA)
            if (!is.na(node_id) && !is.null(node_id)) {
                parents <- c(paste0(node_name, " [", node_id, "]"), parents)
            } else {
                parents <- c(node_name, parents)
            }
            next_parent <- tryCatch(xml_parent(parent), error = function(e) NULL)
            if (is.null(next_parent) || length(next_parent) == 0) break  # Stop if no more parents
            parent <- next_parent
        }
        if (length(parents) == 0) {
            return("No parent found")
        }
        
        return(paste(parents, collapse = " --> "))
    }
    
    get_end_node_content <- function(node) {
        return(xml_text(node, trim = TRUE))
    }
    
    pmc_id_of_interest <- 8709576
    xml_content <- read_xml(result$text[result$pmc_id == pmc_id_of_interest])
    text_nodes <- xml_find_all(xml_content, ".//text()")
    search_word <- "When it comes to defining scientific disagreement, scholars disagree" # for example
    matching_nodes <- text_nodes[grepl(search_word, xml_text(text_nodes))]
    
    if (length(matching_nodes) > 0) {
        parent_hierarchies <- sapply(matching_nodes, find_parent_hierarchy)
        end_node_contents <- sapply(matching_nodes, get_end_node_content)
        result <- data.frame(Hierarchy = parent_hierarchies, Content = end_node_contents, stringsAsFactors = FALSE)
    } else {
        result <- NULL
    }
    
    print(result)
}