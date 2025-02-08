---
tags:
  - twitter
  - Science
  - R
  - github
---
## Delve and other words frequencies in scientific papers
The repo is a collection of scripts to reproduce the fact of the rise of usage of the word delve in scientific literature.
### Input files
- *dates.txt* -- dates of the end of a quarter period used to fetch papers
- *journals-issn-codes.txt* -- selected journals (mass open access journals indexed in the [PMC](https://pmc.ncbi.nlm.nih.gov/))
- *words.txt* -- words to seek in the fetched content and their respective R regular expression, aimed to capture all possible forms of the word (i.e. `(?i)delv(?:es|ed|e|ing)` for delve).
### Usage
In R terminal from copyed repo:
```
setwd("pubmed-delve") # or other directory with the repo
source(scripts/get-pmc-ids.R) # collects PMC ids for a given list of journals and dates
source(scripts/get-pmc-content.R) # fetches xml for a list of PMC ids and for each of them calculates incidence of a given word
source(scripts/aggregate-and-report.R) # aggregates the results and draw pictures
```
Tested for R v ...
### Output files
- *pmc_ids.txt* -- selected random PMC ids for a given data and a journal
- *n_papers_per_quarter.txt* -- amount of papers per periaod for a journal
- *data\fetched\raw_without_text directory* -- word statistics for each paper
- *data\fetched\raw_with_text directory* -- word statistics for each paper with fetched content, ==huge size!==


