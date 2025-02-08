# script fetches xml for a list of PMC ids and for each of them calculates incidence of a given word 
rm(list = ls())
sapply(c("dplyr", "purrr", "rentrez", "xml2", "beepr"), function(pkg) if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
})

journals_df <- read.table("./data/journals-issn-codes.txt", sep="\t", header=FALSE, stringsAsFactors=FALSE)
journals <- setNames(as.list(journals_df$V2), journals_df$V1)

dates <- as.Date(readLines("./data/dates.txt")) # dates of the end of a quarter interval

if (0) { # test
    journals <- journals[1:2]
    dates <- dates[1:2]
}

words_df <- read.table("./data/words.txt", sep="\t", header=FALSE, stringsAsFactors=FALSE)
words <- setNames(words_df$V2, words_df$V1)

pmc_ids_df <- read.table("./data/fetched/pmc_ids.txt", sep="\t", header=TRUE, stringsAsFactors=FALSE)
pmc_ids_df$Nchars <- NA
pmc_ids_df$Nwords <- NA
pmc_ids_df$text <- NA
new_columns <- as.data.frame(t(purrr::set_names(rep(NA,length(words)), names(words))), drop = FALSE)
pmc_ids_df <- bind_cols(pmc_ids_df, new_columns)

safe_read_from_entrez <- function(pmc_id, max_retries = 10, delay = 10) {
    for (i in 1:max_retries) {
        tryCatch({
            full_text_xml <- entrez_fetch(db = "pmc", id = sprintf("PMC%s", pmc_id), rettype = "full", retmode = "xml")
            xml_content <- read_xml(full_text_xml)
            return(as.character(xml_content))  # Exit the function if successful
        }, error = function(e) {
            message(sprintf("Attempt %d failed. Retrying in %d seconds...", i, delay))
            Sys.sleep(delay)  # Wait for the specified delay before retrying
        })
    }
    stop("Failed to fetch the webpage after ", max_retries, " attempts.")
}

if (!dir.exists("./data/fetched/raw_with_text")) {
    dir.create("./data/fetched/raw_with_text")
}
if (!dir.exists("./data/fetched/raw_without_text")) {
    dir.create("./data/fetched/raw_without_text")
}

for(j in sort(names(journals))) {
    for(d in sort(names(table(pmc_ids_df$date)))) {
        if(!file.exists(sprintf("./data/raw/%s-%s.txt", j, d))) {
            pmc_ids_df_partial <- pmc_ids_df[pmc_ids_df$journal == j & pmc_ids_df$date == d, ]
            for(i in 1:dim(pmc_ids_df_partial)[1]) {
                Sys.sleep(0.2)
                
                pmc_id <- pmc_ids_df_partial$pmc_id[i]
                combined_text <- safe_read_from_entrez(pmc_id)
                cleaned_text <- gsub("[\n\r\t]+", " ", combined_text)
                cleaned_text <- gsub("\\s+", " ", cleaned_text)
                cleaned_text <- trimws(cleaned_text)
                
                pmc_ids_df_partial$text[i] <- cleaned_text
                pmc_ids_df_partial$Nchars[i] <- nchar(cleaned_text)
                pmc_ids_df_partial$Nwords[i] <- length(unlist(strsplit(cleaned_text, "\\s+")))
                
                for(word in words) {
                    pattern <- sprintf("(?:\\b|\\W|\\d)%s(?:\\b|\\W|\\d)", word) # captures only in the starting word context 
                    gregexpr_result <- gregexpr(pattern, cleaned_text, ignore.case = TRUE)
                    word_name <-names(words)[which(words == word)]
                    if (gregexpr_result[[1]][1] != -1) {
                        pmc_ids_df_partial[i, word_name] <- length(gregexpr_result[[1]])
                    } else {
                        pmc_ids_df_partial[i, word_name] <- 0
                    }
                }
                
                cat(pmc_ids_df_partial$journal[i], "\t", as.character(pmc_ids_df_partial$date[i]), "\t",
                    pmc_ids_df_partial$Nwords[i], "\t",
                    "delve:", pmc_ids_df_partial$delve[i], "\t", 
                    "assess:",pmc_ids_df_partial$assess[i], "\t", 
                    "analyse:",pmc_ids_df_partial$analyse[i], "\t", 
                    "\n")
                
            }
            write.table(pmc_ids_df_partial, sprintf("./data/fetched/raw_with_text/%s-%s.txt", j, d),
                        row.names = FALSE, sep = "\t", quote = FALSE)
            write.table(pmc_ids_df_partial %>% dplyr::select(-text), sprintf("./data/fetched/raw_without_text/%s-%s.txt", j, d),
                        row.names = FALSE, sep = "\t", quote = FALSE)
        }
    }
}

beepr::beep(sound = 4)
