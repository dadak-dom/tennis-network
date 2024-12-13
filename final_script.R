# Getting all the data and storing in memory
library(readr)
library(dplyr)
library(igraph)
library(purrr)
library(ggplot2)
library(tidyr)
load_files = function(file_vector){
  df <- tibble()
  for(file in file_vector){
    df = bind_rows(df, read_csv(paste("data/atp_matches_", file, '.csv', sep='')))
  }
  return(df)
}
setwd("C:/Users/dadak/Desktop/personal-projects/tennis-network")
doubles_data = load_files(paste0('doubles_', seq(2000, 2020)))
singles_data = load_files(seq(2000, 2020))

#################################################
# Constructing the networks
head_to_head <- singles_data %>%
  mutate(
    player1 = pmap_chr(list(winner_name, loser_name), ~ sort(c(...))[1]),
    player2 = pmap_chr(list(winner_name, loser_name), ~ sort(c(...))[2]),
    winner_is_player1 = winner_name == player1
  ) %>%
  group_by(player1, player2) %>%
  summarise(
    player1_wins = sum(winner_is_player1),
    player2_wins = sum(!winner_is_player1),
    .groups = "drop"
  )

head_to_head_diff = head_to_head %>% mutate(diff = player1_wins - player2_wins) %>% mutate(diff = abs(diff)) %>% select(player1, player2, diff)

singles_g = graph_from_data_frame(singles_data %>% select(winner_name, loser_name), directed=F)

head_to_head_diff <- head_to_head_diff %>% # Add edge keys - will be useful later
  mutate(
    edge_key = paste(pmin(player1, player2), pmax(player1, player2), sep = "_")
  )

edge_keys_graph <- as_data_frame(singles_g, what = "edges") %>%
  mutate(
    edge_key = paste(pmin(from, to), pmax(from, to), sep = "_")
  )

# Assign weights based on matching edge keys
E(singles_g)$weight <- head_to_head_diff$diff[match(edge_keys_graph$edge_key, head_to_head_diff$edge_key)]

# First graph: 
# Finding the distribution of edge weights in the network
tibble(E(singles_g)$weight) %>% rename(X = names(.)[1]) %>% 
  ggplot(aes(x = X)) + geom_histogram(binwidth = 1) + xlab("Edge Weight (difference in Win/Loss)") + ggtitle("Distribution of Edge Weights in Singles Network") + ylim(c(0,35000))

# Building the Doubles Network
temp = doubles_data %>% select(winner1_name, winner2_name) %>% rename(player1 = winner1_name, player2 = winner2_name)
temp2 = doubles_data %>% select(loser1_name, loser2_name) %>% rename(player1 = loser1_name, player2 = loser2_name)
doubles_g = graph_from_data_frame(bind_rows(temp, temp2) %>% na.omit(), directed = F)

# Some basic calculations for the doubles network
d = tibble(igraph::degree(doubles_g))
# Make a histogram of the node degree distribution
d = d %>% rename(X = names(.)[1])
d %>% 
  ggplot(aes(x=X)) + geom_histogram(binwidth=5) + xlab("Node Degree") + ggtitle("Distribution of Node Degrees in Doubles Network (Number of Unique Partnerships)")

# Distribution of edge weights in doubles network, after transferring existing edges
doubles_edges <- as_data_frame(doubles_g, what = "edges") %>%
  mutate(
    edge_key = paste(pmin(from, to), pmax(from, to), sep = "_")
  )
# Match and assign weights to the doubles graph
E(doubles_g)$weight <- head_to_head_diff$diff[match(
  doubles_edges$edge_key,
  head_to_head_diff$edge_key,
  # nomatch= 0
)]

# Distribution of edge weights in doubles
tibble(E(doubles_g)$weight) %>% rename(X = names(.)[1]) %>% 
  ggplot(aes(x = X)) + geom_histogram(binwidth = 1) + ylim(c(0, 35000)) + xlab("Edge Weight") + ggtitle("Distribution of Edge Weights in Doubles Network")
#######################################################################
# What about the overlap between the two networks?
common_nodes <- intersect(V(singles_g)$name, V(doubles_g)$name)
overlap_singles = induced_subgraph(singles_g, V(singles_g)$name %in% common_nodes)
overlap_doubles = induced_subgraph(doubles_g, V(doubles_g)$name %in% common_nodes)
# Here's the histogram for singles...
tibble(E(overlap_singles)$weight) %>% rename(X = names(.)[1]) %>% 
  ggplot(aes(x = X)) + geom_histogram(binwidth = 1) + ylim(c(0,35000)) + xlab("Edge Weight") + ggtitle("Distribution of Edge Weights (Figure 4)", subtitle = "(only looking at overlap between singles and doubles, Singles Network)")
# and for doubles
tibble(E(overlap_doubles)$weight) %>% rename(X = names(.)[1]) %>% 
  ggplot(aes(x = X)) + geom_histogram(binwidth = 1) + ylim(c(0,35000)) + xlab("Edge Weight") + ggtitle("Distribution of Edge Weights (Figure 5)", subtitle = "(only looking at overlap between singles and doubles, Doubles Network)")

###############################################################################
# Some results
# Made with ChatGPT prompt: how would I be able to, for each node, calculate the average weight of each edge attached?
# Calculate the average edge weight for each node
average_edge_weights <- sapply(V(overlap_singles), function(v) {
  edges <- incident(overlap_singles, v, mode = "all")  # Get edges connected to the node
  mean(E(overlap_singles)$weight[edges])              # Compute the average weight
})

# Add as a vertex attribute for convenience
V(overlap_singles)$average_weight <- average_edge_weights

# Print results
data.frame(
  node = V(overlap_singles)$name,
  avg_weight = V(overlap_singles)$average_weight
)

average_edge_weights2 <- sapply(V(overlap_doubles), function(v) {
  edges <- incident(overlap_doubles, v, mode = "all")  # Get edges connected to the node
  mean(E(overlap_doubles)$weight[edges], na.rm = T)              # Compute the average weight
})

# Add as a vertex attribute for convenience
V(overlap_doubles)$average_weight <- average_edge_weights2

# Print results
x = data.frame(
  node = V(overlap_doubles)$name,
  avg_weight = V(overlap_doubles)$average_weight
)
max(x$avg_weight, na.rm = T)
# Results

cor(V(overlap_singles)$average_weight, V(overlap_doubles)$average_weight, use='pairwise.complete.obs')
cor(igraph::degree(overlap_doubles), V(overlap_doubles)$average_weight, use='pairwise.complete.obs')

df = tibble(dgr = igraph::degree(overlap_doubles), aw = V(overlap_doubles)$average_weight)
# cor(igraph::degree(overlap_doubles), df %>% filter(aw != 1) %>% select(aw), use='pairwise.complete.obs')
df2 = df %>% filter(aw != 1, dgr > 1)
cor(df2$dgr, df2$aw)
df2 %>% ggplot() + 
  geom_point(aes(x = dgr, y = aw)) + 
  geom_smooth(aes(x=dgr, y= aw), method = lm, se=F) + 
  expand_limits(y=0) + ggtitle("Double's Player Node Degree vs. Average Parter H2H Differential (Figure 6)") + xlab("Player degree") + ylab("Average partner singles head-to-head differential")


# df %>% filter(aw != 1) %>% ggplot() + 
#   geom_point(aes(x = dgr, y = aw)) + 
#   geom_smooth(aes(x=dgr, y= aw), method = lm, se=F) + 
#   expand_limits(y=0) + ggtitle("Double's Player Node Degree vs. Average Parter H2H Differential (Figure 6)") + xlab("Player degree") + ylab("Average partner singles head-to-head differential")
###
mtext(
  "Figure 6",
  side = 1,        
  line = 4,        
  adj = 0,          
  cex = 1.5,       
  font = 3         
)

plot(doubles_g, vertex.label=NA, vertex.size=3)

clust = cluster_walktrap(doubles_g)
com = communities(clust)

plot(doubles_g, mark.groups = com, vertex.label=NA, vertex.size=4, main = "Doubles Pairings Network w/ Highlighted Clusters")

mtext(
  "Figure 2: Doubles network with highlighted clusters",
  side = 1,        
  line = 4,        
  adj = 0,          
  cex = 1.5,       
  font = 3         
)

clust2 = cluster_walktrap(singles_g)
com2 = communities(clust2)

plot(singles_g, mark.groups = com2, vertex.label=NA, vertex.size=4, main = "Singles Pairings Network w/ Highlighted Clusters")

mtext(
  "Figure 1: Singles network with highlighted clusters",
  side = 1,         
  line = 4,         
  adj = 0,          
  cex = 1.5,        
  font = 3          
)

# ChatGPT prompt: given this code, how could I make a plot of the doubles graph where the size of the vertex indicate the number of matches that player had played?
# Count the total matches each player participated in
player_matches <- doubles_data %>%
  pivot_longer(cols = c(winner1_name, winner2_name, loser1_name, loser2_name), 
               names_to = "role", 
               values_to = "player") %>%
  count(player) %>%
  filter(!is.na(player))  # Remove any NA values

# Assign these counts to the vertices in the doubles graph
V(doubles_g)$match_count <- player_matches$n[match(V(doubles_g)$name, player_matches$player)]

# Scale vertex sizes for better visualization (optional)
vertex_sizes <- V(doubles_g)$match_count / max(V(doubles_g)$match_count, na.rm = TRUE) * 40
# vertex_sizes <- V(doubles_g)$match_count

# Plot the graph with vertex size indicating match count
plot(
  doubles_g,
  vertex.size = vertex_sizes,
  vertex.label = NA,  # Hide labels for clarity
  main = "Doubles Network with Vertex Size Indicating Match Count"
)

# Add a legend (optional)
legend(
  "bottomleft",
  legend = c("Few matches", "Many matches"),
  pt.cex = c(1, 3),
  col = "black",
  pch = 21,
  pt.bg = "grey",
  title = "Match Count"
)
mtext(
  "Figure 3",
  side = 1,         
  line = 4,         
  adj = 0,          
  cex = 1.5,        
  font = 3          
)
