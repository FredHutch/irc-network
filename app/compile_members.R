library(XLConnect)


# read in sheets
files <- c(
  "Exported Membership List TDS.xlsx",
  "IIRC Membership Exported List.xlsx",
  "Exported PAM Membership list.xlsx"
)
data <- lapply(files, function(file) {
  sheet <- readWorksheetFromFile(file, sheet = 1)
  sheet$Name <- paste0(trimws(sheet$Last), ", ", gsub(" \\(\\S+\\)", "", trimws(sheet$First)))
  
  # clean names to reduce duplicates when merging by name
  sheet$First[sheet$Name == "Riddell, Stan"] <- "Stanley"
  sheet$First[sheet$Name == "Greenberg, Phil"] <- "Philip"
  sheet$First[sheet$Name == "Pierce, Rob"] <- "Robert"
  sheet$First[sheet$Name == "Warren, Hootie"] <- "Edus"
  sheet$Name[sheet$Name == "Riddell, Stan"] <- "Riddell, Stanley"
  sheet$Name[sheet$Name == "Greenberg, Phil"] <- "Greenberg, Philip"
  sheet$Name[sheet$Name == "Pierce, Rob"] <- "Pierce, Robert"
  sheet$Name[sheet$Name == "Warren, Hootie"] <- "Warren, Edus"
  
  sheet[, c("Name", "Last", "First", "Division", "Faculty.Rank")]
})
names(data) <- c("TDS", "IIRC", "PAM")
data$TDS$TDS <- TRUE
data$IIRC$IIRC <- TRUE
data$PAM$PAM <- TRUE


# join three sheets
df <- full_join(full_join(data$TDS,data$IIRC, by = "Name"), data$PAM, by = "Name")
df$TDS[is.na(df$TDS)] <- FALSE
df$IIRC[is.na(df$IIRC)] <- FALSE
df$PAM[is.na(df$PAM)] <- FALSE


# Fill in empty cells
df$Last[is.na(df$Last)] <- df$Last.x[is.na(df$Last)]
df$Last[is.na(df$Last)] <- df$Last.y[is.na(df$Last)]
df$First[is.na(df$First)] <- df$First.x[is.na(df$First)]
df$First[is.na(df$First)] <- df$First.y[is.na(df$First)]
df$Division[is.na(df$Division)] <- df$Division.x[is.na(df$Division)]
df$Division[is.na(df$Division)] <- df$Division.y[is.na(df$Division)]
df$Faculty.Rank[is.na(df$Faculty.Rank)] <- df$Faculty.Rank.x[is.na(df$Faculty.Rank)]
df$Faculty.Rank[is.na(df$Faculty.Rank)] <- df$Faculty.Rank.y[is.na(df$Faculty.Rank)]


# Clean division
df$Division <- gsub("Basic Sciences", "BS", df$Division)
df$Division <- gsub("Basic", "BS", df$Division)
df$Division <- gsub("Human Biology", "HB", df$Division)
df$Division <- gsub("BC SC", "UBC", df$Division)
df$Division <- gsub("Affiliate - UW", "UW", df$Division)
df$Institution <- "Fred Hutchinson"
df$Institution[grep("UW", df$Division)] <- "University of Washington"
df$Institution[grep("Affiliate - PATH", df$Division)] <- "PATH"
df$Institution[grep("Affiliate - SC", df$Division)] <- "Seattle Children's Research Institute"
df$Institution[grep("Affiliate - UBC", df$Division)] <- "University of British Columbia"
df$Institution[grep("Affiliate - IDRI", df$Division)] <- "Infectious Disease Research Institute"
df$Division <- gsub("Affiliate - \\w+", "Affiliate", df$Division)
df$Division <- gsub("PHS Comp Bio", "PHS", df$Division)
df$Division <- gsub("PHS Biostats", "PHS", df$Division)
df$Division <- gsub("VIDD BBE", "VIDD", df$Division)
df$Division <- gsub(", EVP & Deputy Dir", "", df$Division)
df$Division <- gsub("Data Visualization", "HDC", df$Division)
df$Division <- gsub("/", ",", df$Division)
df$Primary.Division <- sapply(df$Division, function(x) strsplit(x, ",")[[1]][1], USE.NAMES = FALSE)
df$Secondary.Division <- sapply(df$Division, function(x) strsplit(x, ",")[[1]][2], USE.NAMES = FALSE)


# fix names
df$Name[df$Name == "Appelbaum, Fred"] <- "Appelbaum, Frederick"
# df$Name[df$Name == "Avgousti, Daphne"] <- ""
# df$Name[df$Name == "Bullman, Susan"] <- ""
df$Name[df$Name == "Bradley, Phil"] <- "Bradley, Philip"
df$Name[df$Name == "Cheever, Mac"] <- "Cheever, Martin"
df$Name[df$Name == "Dey, Neel"] <- "Dey, Neelendu"
df$Name[df$Name == "Fitzmaurice, Tina"] <- "Fitzmaurice, Christina"
df$Name[df$Name == "Grady, Bill"] <- "Grady, William"
df$Name[df$Name == "Gujral, Taran"] <- "Gujral, Taranjit"
df$Name[df$Name == "Halloran, Betz"] <- "Halloran, M. Elizabeth"
df$Name[df$Name == "Hawes, Steve"] <- "Hawes, Stephen"
df$Name[df$Name == "Matsen, Erick"] <- "Matsen, Frederick"
df$Name[df$Name == "McElrath, Julie"] <- "McElrath, Juliana"
df$Name[df$Name == "McGuire, Andy"] <- "McGuire, Andrew"
df$Name[df$Name == "Polyak, Steve"] <- "Polyak, Stephen"
df$Name[df$Name == "Randolph, Tim"] <- "Randolph, Timothy"
df$Name[df$Name == "Rodriguez, Christina"] <- "Rodriguez, Cristina"
df$Name[df$Name == "Rose, Tim"] <- "Rose, Timothy"
df$Name[df$Name == "Schwartz, Steve"] <- "Schwartz, Stephen"
df$Name[df$Name == "Stamatatos, Leo"] <- "Stamatatos, Leonidas"
df$Name[df$Name == "Stephan, Mattias"] <- "Stephan, Matthias"
df$Name[df$Name == "Stetson, Dan"] <- "Stetson, Daniel"
df$Name[df$Name == "Subramaniam, Avind"] <- "Subramaniam, Arvind"
# df$Name[df$Name == "Trunnell, Matthew"] <- ""
df$Name[df$Name == "Wasserheit, Judy"] <- "Wasserheit, Judith"
df$Name[df$Name == "Wu, Vicky"] <- "Wu, Qian"


# subset columns
columns <- c(
  "Name", "Last", "First", "Division", "Primary.Division", "Secondary.Division",
  "Institution", "Faculty.Rank", "TDS", "IIRC", "PAM"
)
df <- df[, columns]


# save
write.csv(df, "members.csv", row.names = FALSE)
