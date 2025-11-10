# build_network.R
library(igraph)
library(readr)
library(dplyr)
library(ggraph)
library(ggplot2)

# Load and filter data
nutrient_data <- read_csv("food_nutrients.csv")

# Filter out missing or zero nutrient values
# Use normalized values (value_per_100kcal)
nutrient_data <- nutrient_data %>%
  filter(!is.na(value_per_100kcal), value_per_100kcal > 0)

cat("Filtered nutrient data loaded.\n")
cat("Rows remaining:", nrow(nutrient_data), "\n")

# Build edge list
edges <- nutrient_data %>%
  select(food, nutrientName, value_per_100kcal)

# Build bipartite graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Bipartite types: TRUE = Nutrient, FALSE = Food
V(g)$type <- V(g)$name %in% unique(edges$nutrientName)

# Assign edge weights using normalized nutrient density
E(g)$weight <- edges$value_per_100kcal

cat("\n--- GRAPH SUMMARY ---\n")
print(g)
cat("Number of nodes:", vcount(g), "\n")
cat("Number of edges:", ecount(g), "\n")
cat("Is bipartite:", is_bipartite(g), "\n")

# Degree analysis
deg <- degree(g)
cat("\nNode degree summary:\n")
print(summary(deg))

# Bipartite projections
proj <- bipartite_projection(g)
food_net <- proj$proj1
nutrient_net <- proj$proj2

V(food_net)$label <- V(food_net)$name
V(nutrient_net)$label <- V(nutrient_net)$name

# Centrality metrics for foods
food_centrality <- data.frame(
  food = V(food_net)$name,
  degree = degree(food_net),
  betweenness = betweenness(food_net),
  closeness = closeness(food_net)
) %>%
  arrange(desc(degree))

cat("\nTop 5 foods by degree (shared nutrients):\n")
print(head(food_centrality, 5))

# Visualizations

# (A) Food–Nutrient Bipartite Network
p1 <- ggraph(g, layout = "bipartite") +
  geom_edge_link(aes(width = weight), alpha = 0.5, color = "gray60") +
  geom_node_point(aes(color = type), size = 5) +
  geom_node_text(aes(label = name, color = type), repel = TRUE, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("tomato", "steelblue"),
                     labels = c("Food", "Nutrient")) +
  scale_edge_width_continuous(
    name = "Nutrient Density",
    labels = function(x) sprintf("%.1f mg per 100 kcal", x),
    range = c(0.2, 2.5)
  ) +
  theme_void() +
  labs(title = "Food–Nutrient Bipartite Network (per 100 kcal)",
       color = "Node Type") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# (B) Projected Food–Food Network
p2 <- ggraph(food_net, layout = "fr") +
  geom_edge_link(aes(width = weight), color = "gray70", alpha = 0.5) +
  geom_node_point(aes(size = degree(food_net)), color = "darkgreen") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width_continuous(
    name = "Shared Nutrients",
    labels = function(x) sprintf("%.0f nutrients in common", x),
    range = c(0.3, 2)
  ) +
  theme_void() +
  labs(title = "Food–Food Projection (Shared Nutrients)",
       subtitle = "Edge weight = number of shared nutrients") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Display plots
print(p1)
print(p2)

# Export metrics
write_csv(food_centrality, "food_centrality.csv")
write_graph(g, "food_nutrient_network.graphml", format = "graphml")

cat("\n✅ Analysis complete. Files exported.\n")
