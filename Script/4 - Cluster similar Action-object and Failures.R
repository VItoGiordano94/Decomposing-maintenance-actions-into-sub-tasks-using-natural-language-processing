library(tidyverse)
library(readxl)
library(tidystringdist)
library(tidygraph)
library(writexl)

# Import Data -------------------------------------------------------------

# Import action & object dataset
work_orders <- read_xlsx("Data/WIP/work_order_NER.xlsx")

# Import semantic similarity table
semantic_similiraty <- read_csv("Data/Wip/Similarity - Copia/Similarity Pair DONE.csv")


# Prepare Dataset ---------------------------------------------------------

# Split the action-objet data 
work_orders_split <- work_orders %>% 
  mutate(action_object = str_split(action_object, "; ")) %>% 
  unnest(keep_empty = T) %>% 
  mutate(action_object_to_split = str_replace(action_object, "(^((e|E)seguit(a|e|i|o|u) vari(e?)|(e|E)ffettuat(a|e|i|o|u) vari(e?)|(e|E)seguit(a|e|i|o|u) nuov(a|e|i|o|u)|(e|E)ffettuat(a|e|i|o|u) nuov(a|e|i|o|u)|(e|E)seguit(a|e|i|o|u) prim(a|e|i|o|u)|(e|E)ffettuat(a|e|i|o|u) prim(a|e|i|o|u)|(e|E)seguit(a|e|i|o|u) numeros(a|e|i|o|u)|(e|E)ffettuat(a|e|i|o|u) numeros(a|e|i|o|u)|(e|E)seguit(a|e|i|o|u) molt((eplic)?)(a|e|i|o|u)|(e|E)ffettuat(a|e|i|o|u) molt((eplic)?)(a|e|i|o|u)|(e|E)seguit(a|e|i|o|u) l(a|e|i|o|u)|(e|E)ffettuat(a|e|i|o|u) l(a|e|i|o|u)|(e|E)ffettuat(a|e|i|o|u)|(e|E)seguit(a|e|i|o|u))\\s*\\w+|^\\w+)", "\\1_SPLIT_")) %>%
  separate(col = action_object_to_split, into = c("action", "object"), sep = "_SPLIT_") %>% 
  mutate(action = str_trim(action)) %>% 
  mutate(object = str_trim(object)) 

# Similarity of Failure ---------------------------------------------------

# Count most occurred failure
failure_count <- work_orders_split %>% 
  select(Ordine, failure) %>% 
  unique() %>% 
  count(failure) %>% 
  rename(name = failure) %>% 
  filter(!is.na(name))

# Select only synonyms based on Model 2 and thrashold 0.85
failure_simili <- semantic_similiraty %>% 
  filter(element == "failure") %>% 
  filter(similarity_2 >= 0.85) %>% 
  rename(failure_1 = V1, failure_2 = V2)

# Cluster failures that are synonyms
chunk_clusters <- failure_simili %>%
  select(failure_1, failure_2) %>% 
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  mutate(componet = group_components()) %>%
  as_tibble()

# Select the words for each group
selected_names <- chunk_clusters %>%
  left_join(failure_count) %>% 
  group_by(componet) %>%
  arrange(desc(n)) %>% 
  slice(1) %>%
  rename(tag = name) %>%
  ungroup() %>% 
  select(-n)

# Prepare switch table
switch_table_failure <- chunk_clusters %>%
  left_join(selected_names) %>%
  select(-componet) %>% 
  rename(failure = name, tag_failure = tag)


# Similarity of action ----------------------------------------------------

# Count most occrred actions
action_count <- work_orders_split %>% 
  select(Ordine, action) %>% 
  unique() %>% 
  count(action) %>% 
  rename(name = action) %>% 
  filter(!is.na(name))

# Select only synonyms based on Model 2 and thrashold 0.85
action_simili <- semantic_similiraty %>% 
  filter(element == "action") %>% 
  filter(similarity_2 >= 0.85) %>% 
  rename(action_1 = V1, action_2 = V2)


# Group similar actions
chunk_clusters <- action_simili %>%
  select(action_1, action_2) %>% 
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  mutate(componet = group_components()) %>%
  as_tibble()

# Select the words for each group
selected_names <- chunk_clusters %>%
  left_join(action_count) %>% 
  group_by(componet) %>%
  arrange(desc(n)) %>% 
  slice(1) %>%
  rename(tag = name) %>%
  ungroup() %>% 
  select(-n)

# Prepare switch table
switch_table_action <- chunk_clusters %>%
  left_join(selected_names) %>%
  select(-componet) %>% 
  rename(action = name, tag_action = tag)

# Similarity of object ----------------------------------------------------

# Count most occrred objects
object_count <- work_orders_split %>% 
  select(Ordine, object) %>% 
  unique() %>% 
  count(object) %>% 
  rename(name = object) %>% 
  filter(!is.na(name))

# Select only synonyms based on Model 2 and thrashold 0.85
object_simili <- semantic_similiraty %>% 
  filter(element == "object") %>% 
  filter(similarity_2 >= 0.85) %>% 
  rename(object_1 = V1, object_2 = V2)


# Group similar actions
chunk_clusters <- object_simili %>%
  select(object_1, object_2) %>% 
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  mutate(componet = group_components()) %>%
  as_tibble()

# Select the words for each group
selected_names <- chunk_clusters %>%
  left_join(action_count) %>% 
  group_by(componet) %>%
  arrange(desc(n)) %>% 
  slice(1) %>%
  rename(tag = name) %>%
  ungroup() %>% 
  select(-n)

# Prepare switch table
switch_table_object <- chunk_clusters %>%
  left_join(selected_names) %>%
  select(-componet) %>% 
  rename(object = name, tag_object = tag)

# Labelling data
work_orders_labelled <- work_orders_split %>% 
  left_join(switch_table_failure) %>% 
  mutate(tag_failure = if_else(!is.na(tag_failure), tag_failure, failure)) %>% 
  left_join(switch_table_action) %>% 
  mutate(tag_action = if_else(!is.na(tag_action), tag_action, action)) %>% 
  left_join(switch_table_object) %>% 
  mutate(tag_object = if_else(!is.na(tag_object), tag_object, object)) %>%
  mutate(tag_action = str_trim(str_remove(tag_action, "^eseguit(a|e|i|o|u)|^effettuat(a|e|i|o|u)\\s+"))) %>% 
  mutate(tag_action = if_else(str_detect(tag_action, "verifica|veriifcato|verica|veriifca|veriifica"), "verificato", tag_action)) %>% 
  mutate(tag_action = if_else(tag_action == "pulizia", "pulito", tag_action)) %>% 
  mutate(tag_failure = str_trim(str_remove(tag_failure, "(^alta%|^alta %)\\s+"))) %>% 
  mutate(tag_failure = str_replace(tag_failure, "error(e|i)|problem(a|i)|allarm(e|i)|avari(a|e)", "problema")) 

  
  
write_xlsx(work_orders_labelled, "Data/WIP/work_order_Clustered.xlsx")
