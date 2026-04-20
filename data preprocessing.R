library(readxl)
library(writexl)
library(readr)
library(purrr)
library(tidyverse)
library(magrittr)

file_paths <- list.files(path = 'data', 
                         pattern = "\\.xlsx$", 
                         full.names = TRUE)

list_df = lapply(file_paths, function(x) read_excel(x))

mapping <- read.csv2("mapping.csv", 
                     fileEncoding = "Windows-1251",
                     stringsAsFactors = FALSE)

clean_colnames <- function(df) {
  names(df) <- gsub("\\[н\\]$", "", names(df))
  names(df) <- gsub("\\[н\\] ", "", names(df))
  names(df) <- gsub("\\s+", " ", names(df))
  names(df) <- trimws(names(df))
  return(df)
}

rename_with_mapping <- function(df, mapping) {
  old_names <- mapping$old
  new_names <- mapping$new
  names(new_names) <- old_names
  
  present <- intersect(old_names, colnames(df))
  if (length(present) == 0) {
    warning("Ни один из столбцов mapping не найден в df.")
    return(df)
  }
  
  new_for_present <- new_names[present]
  if (any(duplicated(new_for_present))) {
    dup <- new_for_present[duplicated(new_for_present)]
    stop("Конфликт новых имён: ", paste(unique(dup), collapse = ", "), 
         " встречаются несколько раз. Уточните mapping.")
  }
  
  rename_vec <- new_for_present
  names(rename_vec) <- present
  
  colnames(df)[match(present, colnames(df))] <- rename_vec
  
  missing <- setdiff(old_names, present)
  if (length(missing) > 0) {
    warning("Следующие столбцы из mapping отсутствуют в df: ", 
            paste(missing, collapse = ", "))
  }
  
  return(df)
}


new_list <- list_df %>%
  lapply(clean_colnames) %>%
  lapply(function(x) rename_with_mapping(x, mapping))

dim(new_list[[1]])
dim(new_list[[2]])
dim(new_list[[3]])

names(new_list[[1]])
names(new_list[[2]])
names(new_list[[3]])

df <- bind_rows(new_list)
dim(df)
names(df)

write_xlsx(df, "full_data_with_nan.xlsx")



df_clean <- df[, !names(df) %in% c("section_edu", "section_science", "section_staff", "section_international", "section_infra", "section_finance", "has_ebook_system")]
df_clean <- na.omit(df_clean)
dim(df_clean)
names(df_clean)

write_xlsx(df_clean, "full_data.xlsx")
key_vars <- c(
  "scopus_pubs_per_100_staff",
  "rinc_pubs_per_100_staff",
  "rd_income_per_staff_excl_budget",
  "pct_staff_doc_sci",
  "pct_young_staff",
  "pct_staff_cand_sci",
  "coauthor_foreign_articles",
  "pct_foreign_students_total",
  "num_dissertation_councils",
  "num_shared_equip_centers",
  "pct_income_rd_total",
  "salary_ratio_to_regional_avg",
  "grants_per_100_staff",
  "VUZ",
  "Region",
  "Type",
  "Site",
  "ID",
  "year"
)
df_sci <- df[, key_vars]
df_sci <- na.omit(df_sci)
dim(df_sci)

names(df_sci)
write_xlsx(df_sci, "data_science1.xlsx")          
