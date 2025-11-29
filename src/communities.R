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

# IMPORTANT: type = TRUE → FOOD, type = FALSE → NUTRIENT
V(g)$type <- V(g)$name %in% unique(edges$food)
V(g)$node_type <- ifelse(V(g)$type, "Food", "Nutrient")

# Edge weights = nutrient density per 100 kcal
E(g)$weight <- edges$value_per_100kcal

cat("\n--- GRAPH SUMMARY ---\n")
print(g)
cat("Nodes:", vcount(g), "\nEdges:", ecount(g), "\n")
cat("Is bipartite:", is_bipartite(g), "\n\n")

# -------------------------------
# Bipartite Projection (foods only)
# -------------------------------
# With types given, proj1 = vertices where type == TRUE (foods)
proj <- bipartite_projection(g, types = V(g)$type)
food_net <- proj$proj1

V(food_net)$label <- V(food_net)$name

# -------------------------------
# Food centrality metrics
# -------------------------------
food_centrality <- data.frame(
  food       = V(food_net)$name,
  degree     = degree(food_net),
  betweenness = betweenness(food_net),
  closeness   = closeness(food_net)
) %>%
  arrange(desc(degree))

cat("Top 5 foods by degree (shared nutrients):\n")
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
  geom_node_point(aes(color = node_type), size = 4) +
  geom_node_text(
    aes(label = name, color = node_type),
    repel = TRUE, size = 2.7, show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Food" = "tomato", "Nutrient" = "steelblue"),
    name = "Node Type"
  ) +
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
bip_metrics <- data.frame(
  node       = V(g)$name,
  node_type  = V(g)$node_type,
  degree     = degree(g),
  betweenness = betweenness(g),
  closeness   = closeness(g)
)

cat("\nTop 10 FOOD nodes by degree:\n")
print(
  bip_metrics %>%
    filter(node_type == "Food") %>%
    arrange(desc(degree)) %>%
    head(10)
)

cat("\nTop 10 FOOD nodes by betweenness:\n")
print(
  bip_metrics %>%
    filter(node_type == "Food") %>%
    arrange(desc(betweenness)) %>%
    head(10)
)

cat("\nTop 10 FOOD nodes by closeness:\n")
print(
  bip_metrics %>%
    filter(node_type == "Food") %>%
    arrange(desc(closeness)) %>%
    head(10)
)

# Export bipartite metrics for foods only
bip_food_metrics <- bip_metrics %>%
  filter(node_type == "Food")

write_csv(bip_food_metrics, "bipartite_food_metrics.csv")

# Save full network
write_graph(g, "food_nutrient_network.graphml", format = "graphml")

cat("\n✅ Analysis complete. Plots rendered and files exported.\n")
