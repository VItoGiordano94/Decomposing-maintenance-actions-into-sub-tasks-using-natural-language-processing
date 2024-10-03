library(tidyverse)
library(readxl)
library(writexl)
library(tidytext)
library(tidystringdist)
library(tidytext)
library(hunspell)
library(tidygraph)

# Import data -------------------------------------------------------------

# Read work orders file
work_order <- read_xlsx("Data/Wip/work_order_final.xlsx")

# Remove empty ------------------------------------------------------------

# Remove empty element
work_order_clean <- work_order %>% 
  filter(!is.na(failure)|str_detect(failure, "[aA-aZ]")) %>% 
  filter(!is.na(repair_action)|str_detect(repair_action, "[aA-aZ]"))

# Case Normalization ------------------------------------------------------

# Trasform all in lower case
work_order_clean_to_lower <- work_order_clean %>% 
  mutate(failure = str_squish(str_to_lower(failure))) %>% 
  mutate(repair_action = str_squish(str_to_lower(repair_action))) %>% 
  # Separate numbers from words
  mutate(failure = str_replace_all(failure, "([a-z]+)([0-9]+)", "\\1 \\2")) %>% 
  mutate(failure = str_replace_all(failure, "([0-9]+)([a-z]+)", "\\1 \\2")) %>% 
  mutate(repair_action = str_replace_all(repair_action, "([a-z]+)([0-9]+)", "\\1 \\2")) %>% 
  mutate(repair_action = str_replace_all(repair_action, "([0-9]+)([a-z]+)", "\\1 \\2"))

# Abbrevation expansion ---------------------------------------------------

# Read the list of abbreviations
abbreviation_lexicon <- read_xlsx("Data/Input/acronimi.xlsx")

# Abbraviation expansion in Failure
for(i in 1:nrow(work_order_clean_to_lower)){
  for(j in 1:nrow(abbreviation_lexicon)){
    
    # Abbraviation expansion in Component
    work_order_clean_to_lower$component[i] <- str_replace_all(work_order_clean_to_lower$component[i],
                                                            str_c("(\\b)", abbreviation_lexicon$acronimo[j], "(\\b)"), 
                                                            str_c("\\1", abbreviation_lexicon$tag[j], "\\2"))
    
    # Abbraviation expansion in Failure
    work_order_clean_to_lower$failure[i] <- str_replace_all(work_order_clean_to_lower$failure[i],
                                                            str_c("(\\b)", abbreviation_lexicon$acronimo[j], "(\\b)"), 
                                                            str_c("\\1", abbreviation_lexicon$tag[j], "\\2"))
    
    # Abbraviation expansion in Repair actions
    work_order_clean_to_lower$repair_action[i] <- str_replace_all(work_order_clean_to_lower$repair_action[i],
                                                            str_c("(\\b)", abbreviation_lexicon$acronimo[j], "(\\b)"), 
                                                            str_c("\\1", abbreviation_lexicon$tag[j], "\\2"))
  }
  print(i)
}



# Misspelling correction --------------------------------------------------

# Merge text all togheter
text_work_order <-  work_order_clean_to_lower %>% select(Ordine, text = failure) %>% 
  bind_rows(work_order_clean_to_lower %>% select(Ordine, text = repair_action))

# Tokenize Text (unigrams)
unigrams <- text_work_order %>% 
  unnest_tokens(output = token, input = text)

# Tokenize Text (bigrams)
bigrams <- text_work_order %>% 
  unnest_tokens(output = token, input = text, token = "ngrams", n = 2)

# Extract unigrams vector
unigrams_vec <- unigrams %>% 
  pull(token) %>%
  .[str_count(.) > 3] %>% 
  unique() 

# Create a dataset for comparing unigrams
unigrams_pair <- tidy_comb_all(unigrams_vec) %>% 
  tidy_stringdist(method = "lv")  %>% 
  filter(lv <= 1)

# Extract bigrams vector
bigrams_vec <- bigrams %>% 
  pull(token) %>%
  .[str_count(.) > 3] %>% 
  unique() 

# Create a dataset for comparing unigrams
bigrams_pair <- expand_grid(V1 = unigrams_vec, V2 = bigrams_vec) %>% 
  tidy_stringdist(method = "lv")  %>% 
  filter(lv <= 1)

# Merge token and remove no letter tokens
token <- bind_rows(unigrams, bigrams) %>% 
  filter(str_detect(token, "[a-z]")) 

# Most frequent token
token_count <- token %>% 
  count(token, sort = T) 

# Merge the results
token_similarity <- bind_rows(unigrams_pair, bigrams_pair)

# Create a dataset to check misspelling removing: 
# (1) Words that for both exsist 
# (2) Words that start for which change only the first S
token_similarity_to_parse <- token_similarity %>% 
  filter(str_detect(V1, "[a-z]") & str_detect(V2, "[a-z]")) %>% 
  filter(!((str_detect(V1, V2) & str_detect(str_remove(V1, V2), "^s")|str_detect(V2, V1) & str_detect(str_remove(V2, V1), "^s")) & lv == 1)) %>% 
  mutate(no_error_V1 = hunspell_find(text = V1, format = "text", dict = "Data/Input/it_dict/it_IT.dic") == "character(0)") %>% 
  mutate(no_error_V2 = hunspell_find(text = V2, format = "text", dict = "Data/Input/it_dict/it_IT.dic") == "character(0)") %>% 
  filter(!(no_error_V1 & no_error_V2 | !no_error_V1 & !no_error_V2)) %>% 
  filter(!(str_detect(V1, "^[0-9]")|str_detect(V2, "^[0-9]"))) %>%
  filter(!(str_count(V1, "[a-z]") <= 2|str_count(V2, "[a-z]") <= 2)) %>% 
  arrange(V2, V1) %>% 
  select(-no_error_V1, -no_error_V2)
  
# Save results
write_xlsx(token_similarity_to_parse, "Data/Wip/Text Preprocessing/Levenstain to Parse.xlsx")

# Read manually checked table
levenstain_table <- read_xlsx("Data/Wip/Text Preprocessing/Levenstain to Parse - OK.xlsx")

# Remove false positve pairs
levenstain_table_clean <- levenstain_table %>% 
  filter(ok == 1)

# Group similar tokens
chunk_clusters <- levenstain_table_clean %>%
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  mutate(componet = group_components()) %>%
  as_tibble()

# Selezionare la parola per ogni gruppo di interventi simili
selected_names <- chunk_clusters %>%
  left_join(token_count %>% rename(name = token)) %>% 
  group_by(componet) %>%
  arrange(desc(n)) %>% 
  slice(1) %>%
  rename(tag = name) %>%
  ungroup() %>% 
  select(-n)

# associare il nome ad ogni gruppo
switch_table <- chunk_clusters %>%
  left_join(selected_names) %>%
  select(-componet) %>% 
  filter(!(tag == name)) %>% 
  arrange(desc(str_count(name)))

# Correct misspelling
for(i in 1:nrow(work_order_clean_to_lower)){
  for(j in 1:nrow(switch_table)){
 
    # Failure
    work_order_clean_to_lower$failure[i] <- str_replace_all(work_order_clean_to_lower$failure[i],
                                                            str_c("(\\b)", switch_table$name[j], "(\\b)"), 
                                                            str_c("\\1", switch_table$tag[j], "\\2"))
    
    # Repair actions
    work_order_clean_to_lower$repair_action[i] <- str_replace_all(work_order_clean_to_lower$repair_action[i],
                                                            str_c("(\\b)", switch_table$name[j], "(\\b)"), 
                                                            str_c("\\1", switch_table$tag[j], "\\2"))
    
  }
  print(i)
}

# Save results
write_xlsx(work_order_clean_to_lower, "Data/Wip/work_order_processed.xlsx")