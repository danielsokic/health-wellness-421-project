library(igraph)
library(readr)
library(dplyr)
library(ggraph)
library(ggplot2)


nutrient_data <- read_csv("food_nutrients.csv")

# We treat foods and nutrients as separate node sets
edges <- nutrient_data %>%
  select(food, nutrientName, value)

# Build bipartite graph (undirected)
g <- graph_from_data_frame(edges, directed = FALSE)

# Bipartite node types: TRUE for nutrients, FALSE for foods
V(g)$type <- V(g)$name %in% unique(edges$nutrientName)

# edge weights (nutrient amount)
E(g)$weight <- edges$value

cat("\nGraph summary:\n")
print(g)

# Analyis
cat("\n--- NETWORK METRICS ---\n")
cat("Number of nodes:", vcount(g), "\n")
cat("Number of edges:", ecount(g), "\n")
cat("Is bipartite:", is_bipartite(g), "\n")

# Degree distribution
deg <- degree(g)
cat("\nNode degree summary:\n")
print(summary(deg))

# Bipartite projection
# Separate into food-food and nutrient-nutrient networks
proj <- bipartite_projection(g)
food_net <- proj$proj1
nutrient_net <- proj$proj2

# Assign names for clarity
V(food_net)$label <- V(food_net)$name
V(nutrient_net)$label <- V(nutrient_net)$name

# Centrality metrics (foods only)
food_centrality <- data.frame(
  food = V(food_net)$name,
  degree = degree(food_net),
  betweenness = betweenness(food_net),
  closeness = closeness(food_net)
) %>%
  arrange(desc(degree))

cat("\nTop 5 foods by degree:\n")
print(head(food_centrality, 5))

# Visualizations
# (A) Bipartite network view
p1 <- ggraph(g, layout = "bipartite") +
  geom_edge_link(aes(width = weight), alpha = 0.5, color = "gray60") +
  geom_node_point(aes(color = type), size = 5) +
  scale_color_manual(values = c("tomato", "steelblue"),
                     labels = c("Food", "Nutrient")) +
  theme_void() +
  ggtitle("Food–Nutrient Bipartite Network")

# (B) Projected food–food network
p2 <- ggraph(food_net, layout = "fr") +
  geom_edge_link(alpha = 0.4, color = "gray60") +
  geom_node_point(color = "darkgreen", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  ggtitle("Food–Food Network (shared nutrients)")

# Display plots
print(p1)
print(p2)

# Export
write_csv(food_centrality, "food_centrality.csv")
write_graph(g, "food_nutrient_network.graphml", format = "graphml")