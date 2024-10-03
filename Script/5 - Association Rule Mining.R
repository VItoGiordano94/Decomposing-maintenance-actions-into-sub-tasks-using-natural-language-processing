library(readxl)
library(arules)
library(tidygraph)
library(ggraph)
library(tidyverse)

# Load the data
work_order <- read_excel("Data/Wip/work_order_Clustered.xlsx")

# Prepare dataset ---------------------------------------------------------

# Prepare dataset
transactions_data <- work_order %>% 
  select(component, tag_failure, tag_action) %>% 
  filter(!is.na(tag_action)) %>% 
  mutate(component = str_squish(str_replace_all(component, "flussaggio|microlution|#|\\-", " "))) %>% 
  mutate(component = str_c("[", component, "]")) %>% 
  unite(failure, component, tag_failure, sep = "\n") 

# Combine the columns to create transactions
transactions_data$combined <- apply(transactions_data, 1, paste, collapse = "_____")

# Split the combined data into a list, each entry representing a transaction
transaction_list <- strsplit(transactions_data$combined, "_____")

# Convert the list into a transactions object
trans <- as(transaction_list, "transactions")


# Perform association rule mining
rules <- apriori(trans, parameter = list(supp = 0.0002, conf = 0.01))

# Trasform in a dataframe the rules
rules_df <- as(rules, "data.frame")

# Extracting LHS and RHS separately for plotting
rules_df_clean <- rules_df %>%
  mutate(LHS = sapply(strsplit(as.character(rules_df$rules), "=>"), function(x) x[1]),
         RHS = sapply(strsplit(as.character(rules_df$rules), "=>"), function(x) x[2])) %>% 
  select(LHS, RHS, support, confidence, coverage, lift, count) %>% 
  mutate(LHS = str_trim(str_remove_all(LHS, "\\{|\\}"))) %>% 
  mutate(RHS = str_trim(str_remove_all(RHS, "\\{|\\}"))) %>% 
  filter(LHS != "") %>% 
  mutate(node_type = ifelse(grepl("failure", LHS), "failure", "action")) %>% 
  filter(str_detect(LHS, "\\[")) %>% 
  filter(count > 3)

# Write for tranlation
rules_df_clean %>% write_xlsx("Data/Wip/Graph Translation.xlsx")

rules_df_clean <- read_xlsx("Data/Wip/Graph Translation - DONE.xlsx")

# Create nodes dataframe with unique nodes and type
nodes <- data.frame(
  name = unique(c(rules_df_clean$LHS, rules_df_clean$RHS)),
  type = ifelse(unique(c(rules_df_clean$LHS, rules_df_clean$RHS)) %in% rules_df_clean$LHS, "failure", "action")
)

# Map coverage values to the nodes
nodes <- nodes %>%
  left_join(rules_df_clean %>% select(LHS, count) %>% rename(name = LHS), by = "name") %>%
  mutate(count = ifelse(is.na(count), 0, count))


# Create edges dataframe with from, to, and confidence
edges <- data.frame(
  from = match(rules_df_clean$LHS, nodes$name),
  to = match(rules_df_clean$RHS, nodes$name),
  confidence = rules_df_clean$confidence
)

# Create tidygraph object
rules_graph <- tbl_graph(nodes = nodes, edges = edges)

# Map the degree to node size
rules_graph <- rules_graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(mode = "all"))%>% 
  filter(degree > 0) 

set.seed(seed = "222")

# Plotting the graph
ggraph(rules_graph, layout = "fr") +
  geom_node_point(aes(fill = type, size = degree), shape = 21) +
  geom_edge_link(aes(width = confidence), arrow = arrow(type = "closed", length = unit(2, "mm")), alpha = 0.6, end_cap = circle(2, 'mm'), color = "gray70") +
  geom_node_text(aes(label = name, size = degree), repel = TRUE) +
  scale_fill_manual(values = c("failure" = "#ADB9C9", "action" = "#3394A3")) +
  scale_size_continuous(range = c(5, 20)) + # Adjust the size range as needed
  scale_edge_width_continuous(range = c(0.2, 2)) + # Set edge width range for thinner lines
  theme_void() +
  theme(legend.position = "none")


# save
ggsave("Data/Output/graph.png", width = 12, height = 8)

