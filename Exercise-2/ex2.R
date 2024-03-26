##Which seat should you pick?
#1. Create a dataset where edges are based
#on seat adjacency as described above
#2. For each seat choice (A-D), assuming the
#other open seats are filled, calculate:
#  – Degree centrality
#– Closeness centrality
#– Betweenness centrality
#3. Discuss possible consequences of your
#choice of a seat. When would this choice
#be beneficial? When would it be not so
#beneficial?
#  4. Plot the network graph with labels and
#centrality values

library(tidygraph)
library(igraph)
library(readr)
library(ggraph)

#Load data
edges <- read_csv("GitHub/desktop-tutorial/Exercise-2/ex2_edges.csv")
nodes <- read_csv("GitHub/desktop-tutorial/Exercise-2/ex2_nodes.csv")

# List of already filled seats
filled_seats <- c('1', '2', '3', '4', '5', '6')

# Filter out rows representing already filled seats
filtered_edges <- edges %>%
  filter(!from %in% filled_seats)


# Create a complete graph with remaining seats as nodes
g <- make_full_graph(length(remaining_seats)) %>%
  set_vertex_attr("name", value = remaining_seats)

# Add edges from filtered dataset
g <- add_edges(g, filtered_edges)

# Step 2: Calculate Centrality Measures
remaining_seats <- c('A', 'B', 'C', 'D')
centrality_measures <- data.frame(
  Seat = remaining_seats,
  Degree_Centrality = centr_degree(g)[remaining_seats],
  Closeness_Centrality = centr_clo(g)[remaining_seats],
  Betweenness_Centrality = centr_betw(g)[remaining_seats]
)

# Step 3: Plot the Network Graph with Labels and Centrality Values
ggraph(g, layout = 'fr') +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE) +
  geom_node_text(data = centrality_measures, aes(label = paste("Seat:", Seat, "\nDegree:", Degree_Centrality, "\nCloseness:", Closeness_Centrality, "\nBetweenness:", Betweenness_Centrality)),
                 hjust = -0.1, vjust = -1, size = 3, color = "red") +
  labs(title = "Bus Seat Network Graph with Centrality Values")
