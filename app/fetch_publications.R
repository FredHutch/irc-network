library(rentrez)

# read in members
members <- read.csv("members.csv", stringsAsFactors = FALSE)
rownames(members) <- members$Name


# fetch publications
years <- as.character(2013:2019)
res <- lapply(years, function(year) {
  temp <- lapply(members$Name, function(name) {
    term <- paste0(
      members[name, "Institution"], "[AFFL] AND ",
      name, "[AUTH] AND ",
      year,"[PDAT]"
    )
    x <- data.frame(
      pubid = entrez_search("pubmed", term = term, retmax = 100)$ids,
      stringsAsFactors = FALSE
    )
    if (nrow(x) > 0) x$member <- name
    x
  })
  temp2 <- do.call(rbind, temp)
  temp2$year <- year
  temp2
})


# row bind results by year
res2 <- do.call(rbind, res)


# remove duplicates (PDAT picks up both submission and publication dates)
res2 <- res2[!duplicated(res2[, c("pubid", "member")]), ]


# save publications
write.csv(res2, "publications.csv", row.names = FALSE)

