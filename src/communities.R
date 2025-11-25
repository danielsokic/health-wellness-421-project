# build_network.R
library(igraph)
library(readr)
library(dplyr)
library(ggraph)
library(ggplot2)
library(scales)

# -------------------------------
# Load & clean data
# -------------------------------
nutrient_data <- read_csv("food_nutrients.csv")

nutrient_data <- nutrient_data %>%
  filter(!is.na(value_per_100kcal), value_per_100kcal > 0)

cat("Filtered nutrient data loaded.\n")
cat("Rows remaining:", nrow(nutrient_data), "\n")

# -------------------------------
# Build bipartite graph
# -------------------------------
edges <- nutrient_data %>%
  select(food, nutrientName, value_per_100kcal)

g <- graph_from_data_frame(edges, directed = FALSE)

V(g)$type <- V(g)$name %in% unique(edges$nutrientName)
E(g)$weight <- edges$value_per_100kcal

cat("\n--- GRAPH SUMMARY ---\n")
print(g)
cat("Nodes:", vcount(g), "\nEdges:", ecount(g), "\n")
cat("Is bipartite:", is_bipartite(g), "\n\n")

# -------------------------------
# Bipartite Projection (foods only)
# -------------------------------
proj <- bipartite_projection(g)
food_net <- proj$proj1

V(food_net)$label <- V(food_net)$name

# -------------------------------
# Food centrality metrics
# -------------------------------
food_centrality <- data.frame(
  food = V(food_net)$name,
  degree = degree(food_net),
  betweenness = betweenness(food_net),
  closeness = closeness(food_net)
) %>%
  arrange(desc(degree))

cat("Top 5 foods by degree:\n")
print(head(food_centrality, 5))

write_csv(food_centrality, "food_centrality.csv")

# -------------------------------
# COMMUNITY DETECTION (Louvain)
# -------------------------------
comm <- cluster_louvain(food_net)
V(food_net)$community <- membership(comm)

num_comms <- length(unique(V(food_net)$community))
community_colors <- hue_pal()(num_comms)

cat("\nDetected", num_comms, "communities.\n")
print(sizes(comm))

# -------------------------------
# PLOT 1 — Bipartite Network (Food ↔ Nutrient)
# -------------------------------
p1 <- ggraph(g, layout = "bipartite") +
  geom_edge_link(aes(width = weight), alpha = 0.4, color = "gray70") +
  geom_node_point(aes(color = type), size = 4) +
  geom_node_text(aes(label = name, color = type),
                 repel = TRUE, size = 2.7, show.legend = FALSE) +
  scale_color_manual(values = c("tomato", "steelblue"),
                     labels = c("Food", "Nutrient"),
                     name = "Node Type") +
  scale_edge_width(range = c(0.2, 2)) +
  theme_void() +
  labs(title = "Food–Nutrient Bipartite Network")

print(p1)

# -------------------------------
# PLOT 2 — Food–Food Network + Communities
# -------------------------------
p2 <- ggraph(food_net, layout = "fr") +
  geom_edge_link(alpha = 0.25, color = "gray65") +
  geom_node_point(aes(color = factor(community)), size = 6) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = community_colors, name = "Community") +
  theme_void() +
  labs(
    title = "Food–Food Projection (Shared Nutrients)",
    subtitle = "Communities detected using Louvain modularity"
  )

print(p2)

# -------------------------------
# Additional Global Metrics
# -------------------------------
bip_deg <- degree(g)
cat("\nTop 10 nodes by degree:\n")
print(sort(bip_deg, decreasing = TRUE)[1:10])

bip_bet <- betweenness(g)
cat("\nTop 10 nodes by betweenness:\n")
print(sort(bip_bet, decreasing = TRUE)[1:10])

bip_close <- closeness(g)
cat("\nTop 10 nodes by closeness:\n")
print(sort(bip_close, decreasing = TRUE)[1:10])

# Export bipartite metrics for foods only
bip_metrics <- data.frame(
  node = names(V(g)),
  degree = degree(g),
  betweenness = betweenness(g),
  closeness = closeness(g)
)

bip_food_metrics <- bip_metrics[V(g)$type == FALSE, ]
write_csv(bip_food_metrics, "bipartite_food_metrics.csv")

# Save full network
write_graph(g, "food_nutrient_network.graphml", format = "graphml")

cat("\n Analysis complete. Plots rendered and files exported.\n")
