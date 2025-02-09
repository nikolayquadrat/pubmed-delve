# script collects PMC ids for a given list of journals and dates
rm(list = ls())
sapply(c("lubridate", "rentrez", "xml2", "dplyr", "rvest"), function(pkg) if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
})

journals_df <- read.table("./data/journals-issn-codes.txt", sep="\t", header=FALSE, stringsAsFactors=FALSE)
journals <- setNames(as.list(journals_df$V2), journals_df$V1)

dates <- readLines("./data/dates.txt") # dates of the end of a quarter interval

if (0) { # test
    journals <- journals[1:2]
    dates <- dates[1:2]
}

if (!dir.exists("./data/fetched")) {
    dir.create("./data/fetched")
}

# get number of papers per quarter for a journal======
safe_read_url_from_entrez <- function(url, max_retries = 10, delay = 10) {
    for (i in 1:max_retries) {
        tryCatch({
            webpage <- read_html(url)
            return(webpage)
        }, error = function(e) {
            message(sprintf("Attempt %d failed. Retrying in %d seconds...", i, delay))
            Sys.sleep(delay)  # Wait for the specified delay before retrying
        })
    }
    stop("Failed to fetch the webpage after ", max_retries, " attempts.")
}

n_papers_per_quarter <- data.frame(
    "journal" = rep(names(journals), length(dates)),
    "date" = sort(rep(dates, length(journals))),
    "n" = rep(NA, length(journals) * (length(dates)))
)

if (!file.exists("./data/fetched/n_papers_per_quarter.txt")) {
    for (j in names(journals)) {
        for (d in dates) {
            Sys.sleep(2)
            journal_string = sprintf('("%s"[Journal])', journals[[j]])
            date_string = sprintf('("%s"[Publication Date]:"%s"[Publication Date])',
                                  gsub("-", "/", ymd(d) %m-% months(3) %m+% days(1)),
                                  gsub("-", "/", ymd(d))
            )
            url <- sprintf("https://www.ncbi.nlm.nih.gov/pmc?term=%sAND%s",
                           journal_string, date_string)
            url <- gsub("\\s+", "%20", url)
            webpage <- safe_read_url_from_entrez(url)
            counter_text <- webpage %>% html_nodes(xpath = '//*[@id="maincontent"]/div/div[3]/div[1]/h3') %>% html_text()
            cat(j, "\t", as.character(ymd(d)), "\t", sub(".*?(\\d+)$", "\\1", counter_text), "\n")
            n_paper_per_period <- as.numeric(sub("Items: 1 to \\d+ of (\\d+)", "\\1", counter_text))
            n_papers_per_quarter$n[n_papers_per_quarter$journal == j & n_papers_per_quarter$date == d] <- n_paper_per_period
        }
    }
    write.table(n_papers_per_quarter, "./data/fetched/n_papers_per_quarter.txt", sep = "\t", row.names = FALSE, quote = FALSE)
}

# get random PMC paper ids per period=======
safe_read_term_from_entrez <- function(term_string, max_retries = 10, delay = 10) {
    for (i in 1:max_retries) {
        tryCatch({
            search_results <- entrez_search(db = "pubmed", term = term_string, retmax = 100)
            return(search_results)
        }, error = function(e) {
            message(sprintf("Attempt %d failed. Retrying in %d seconds...", i, delay))
            Sys.sleep(delay)  # Wait for the specified delay before retrying
        })
    }
    stop("Failed to fetch the webpage after ", max_retries, " attempts.")
}

if (!dir.exists("./data/fetched/pmc_ids_raw")) {
     dir.create("./data/fetched/pmc_ids_raw")
}

for (j in names(journals)) {
    if (!file.exists(sprintf("./data/fetched/pmc_ids_raw/pmc_ids_%s.txt", j))) {
        journal_string = sprintf('("%s"[Journal])', journals[[j]])
        pmc_ids_journal_df <- data.frame(
            "journal" = character(),
            "date" = ymd(),
            "pmc_id" = character()
        )
        for (d in dates) {
            Sys.sleep(1)
            pmc_ids_all <- character()
            for (m in 3:1) {
                for (w in 0:3) {
                    Sys.sleep(1)
                    date_start <- ymd(d) %m-% months(m) %m+% weeks(w) %m+% days(1)
                    if (w != 3) {
                        date_end <- ymd(d) %m-% months(m) %m+% weeks(w + 1)
                    } else {
                        date_end <- ymd(d) %m-% months(m - 1)
                    }
                    date_string = sprintf('("%s"[Publication Date]:"%s"[Publication Date])',
                                          gsub("-", "/", date_start),
                                          gsub("-", "/", date_end)
                    )
                    
                    term_string <- sprintf("%sAND%s", journal_string, date_string)
                    search_results <- safe_read_term_from_entrez(term_string)
                    if (length(search_results$ids) != 0) {
                        pubmed_ids <- search_results$ids
                        link_results <- entrez_link(dbfrom = "pubmed", id = pubmed_ids, db = "pmc")
                        pmc_ids <- link_results$links$pubmed_pmc
                    } else {
                        pmc_ids <- character()
                    }
    
                    cat(as.character(j), "\t",
                        as.character(date_start), "\t",
                        as.character(date_end), "\t",
                        length(pmc_ids), "\n" )
                    
                    if (length(pmc_ids) > 0) {
                        pmc_ids_all <- c(pmc_ids_all,
                                         sample(pmc_ids, min(25, length(pmc_ids), na.rm = TRUE))
                                         )
                    }
                }
            }
            if (length(pmc_ids_all) > 0) {
                pmc_ids_journal_df <- bind_rows(pmc_ids_journal_df, data.frame(
                    "journal" = rep(j, length(pmc_ids_all)),
                    "date"    = rep(ymd(d), length(pmc_ids_all)),
                    "pmc_id"  = pmc_ids_all
                ))
            }
        }
    cat(sprintf("Portion of duplicated PMC IDs = %.4f for %s\n", (dim(pmc_ids_journal_df)[1] - length(unique(pmc_ids_journal_df$pmc_id)))/dim(pmc_ids_journal_df)[1], j))
        pmc_ids_journal_df <- pmc_ids_journal_df %>% distinct(pmc_id, .keep_all = TRUE)
        write.table(pmc_ids_journal_df, sprintf("./data/fetched/pmc_ids_raw/pmc_ids_%s.txt", j), sep = "\t", row.names = FALSE, quote = FALSE)
    }
}

pmc_ids_files <- list.files(path = "./data/fetched/pmc_ids_raw", full.names = TRUE)
pmc_ids_df_list <- lapply(pmc_ids_files, read.delim)
pmc_ids_df <- bind_rows(pmc_ids_df_list)
cat("Portion of duplicated PMC IDs = %.3f accross all journals\n", length(unique(pmc_ids_df$pmc_ids))/dim(pmc_ids_df)[1], j)
pmc_ids_df <- pmc_ids_df %>% distinct(pmc_id, .keep_all = TRUE)
write.table(pmc_ids_df, "./data/fetched/pmc_ids.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# beepr::beep(sound = 4)