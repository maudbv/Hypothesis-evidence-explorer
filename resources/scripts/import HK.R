# Data import
# Import and pre-process data for the hi-knowledge shiny app
# 2022 Maud Bernard-Verdier

import_HK <- function() {
require(stringr)
require(purrr)
require(dplyr)
# require(readr)

all_files <- list.files('resources/HiKnowledge data/csv', pattern = 'comparison*', full.names = TRUE)
df_list <- map(all_files,
               ~.x %>% readr::read_csv(show_col_types = FALSE))

# homogenize column names
#unique_columns <- unlist(lapply(1:9, function(x) setdiff(tmp[[x]], tmp[[x+1]])))

# #Sub-hyp & sub-sub hyp column names:   TO COMPLETE
#  list("Measure of species similarity",
#    "Measure of Species Relationship",
#    
#  )

# Correct one wrong column name
df_list <-  lapply(df_list,function(x) {
  names(x)[which(names(x) == "number of plant species")] = "Number of species"
  return(x)
})

# # Import comparison tables exported from ORKG via python package ####
# darwin <- read.csv(file = "resources/csv/comparison_R53407_Darwin's naturalisation.csv")
# enemy <- read.csv(file = "resources/csv/comparison_R58002_Enemy release.csv")
# 
# # Merge all tables in one ####
# df_list = list(darwin,enemy)

# Reduce to one dataframe
total_df <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  df_list
)

# correct some strange typos:
total_df$publication <- str_replace_all(total_df$publication,"\n","")
total_df$publication <- str_replace_all(total_df$publication,"\"","\'")

total_df$index <- str_replace_all(total_df$index,"\n","")
total_df$index <- str_replace_all(total_df$index,"\"","\'")

# reformat column names
total_df$Title <- total_df$publication
names(total_df) <- stringr::str_replace_all(names(total_df), "\\ ","_")


# Add DOI and abstracts #####

## Read DOI file prepared by Marc Brinner
doi_abstract <-  read_csv("resources/additional data/hi-k_DOI_title_abstract.csv", show_col_types = FALSE)

# dim(doi_abstract) # 954
# length(unique(total_df$Title)) # 961  strange

##homogenize formats
doi_abstract$Title <- str_replace_all(doi_abstract$Title,"\"","\'")

# correct titles in datasets
# Merge doi and abstracts with the main table
lj = left_join(total_df, doi_abstract, by = "Title")

to_be_corrected = select(lj, Title, DOI)%>% filter(is.na(DOI))%>% unique()

to_be_corrected$match <- 
  sapply(to_be_corrected$Title, FUN = function(i) {
    index = agrep(i, doi_abstract$Title,max.distance =5)
    if (length(index) > 0) doi_abstract$Title[index] else NA
  })

replaced_titles_indices <- na.omit(match( to_be_corrected$Title,total_df$Title))
total_df$Title[replaced_titles_indices ] <- to_be_corrected$match[match(total_df$Title[replaced_titles_indices], to_be_corrected$Title)]

# Fix the titles in doi_abstract
doi_abstract$Title[which(doi_abstract$Title == "Alien flora of Europe: species diversity, temporal trends, geographical patterns and research needs.")] <-
  "Alien flora of Europe: species diversity, temporal trends, geographical patterns and research needs"


# Merge doi and abstracts with the main table
total_df <- left_join(total_df, doi_abstract)
# View(select(lj, Title, DOI)%>% filter(is.na(DOI))%>% unique())
#### TODO still 20 left to correct #######


# Make DOIs into links
# TODO


# Export cleaned data table
write.csv(total_df, "resources/HiKnowledge data/Hi Knowledge data.csv", row.names = FALSE)

return(total_df)

}
