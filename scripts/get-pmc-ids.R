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
n_papers_per_quarter <- data.frame(
    "journal" = rep(names(journals), length(dates)),
    "date" = sort(rep(dates, length(journals))),
    "n" = rep(NA, length(journals) * (length(dates)))
)

if (!file.exists("./data/fetched/n_papers_per_quarter.txt")) {
    for (j in names(journals)) {
        for (d in dates) {
            Sys.sleep(1)
            pmc_common <- "https://www.ncbi.nlm.nih.gov/pmc?term"
            journal_string = sprintf('("%s"[Journal])', journals[[j]])
            date_string = sprintf('("%s"[Publication Date]:"%s"[Publication Date])',
                                  gsub("-", "/", ymd(d) %m-% months(3) %m+% days(1)),
                                  gsub("-", "/", ymd(d))
            )
            url <- sprintf("%s=%sAND%s", pmc_common, journal_string, date_string)
            url <- gsub("\\s+", "%20", url)
            webpage <- read_html(url)
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
pmc_ids_df <- data.frame(
    "journal" = character(),
    "date" = character(),
    "pmc_id" = character()
)

for (j in names(journals)) {
    journal_string = sprintf('("%s"[Journal])', journals[[j]])
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
                
                if(any(pmc_ids %in% pmc_ids_df$pmc_id)) {
                    print(pmc_ids[any(pmc_ids %in% pmc_ids_df$pmc_id)])
                    break
                }
                
                if (length(pmc_ids) > 0) {
                    pmc_ids_all <- c(pmc_ids_all,
                                     sample(pmc_ids, min(25, length(pmc_ids), na.rm = TRUE))
                                     )
                }
            }
        }
        if (length(pmc_ids_all) > 0) {
            pmc_ids_df <- bind_rows(pmc_ids_df, data.frame(
                "journal" = rep(j, length(pmc_ids_all)),
                "date"    = rep(ymd(d), length(pmc_ids_all)),
                "pmc_id"  = pmc_ids_all
            ))
        }
    }
}
write.table(pmc_ids_df, "./data/fetched/pmc_ids.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# beepr::beep(sound = 4)
