# build_network.R
library(igraph)
library(readr)
library(dplyr)
library(ggraph)
install.packages("ggplot2")
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

p1 <- ggraph(g, layout = "bipartite") +
  geom_edge_link(
    aes(
      edge_color = weight,       
      edge_width = weight        
    ),
    alpha = 0.8
  ) +
  
  geom_node_point(aes(color = type), size = 5) +
  geom_node_text(
    aes(label = name, color = type),
    repel = TRUE,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("tomato", "steelblue"),
    labels = c("Food", "Nutrient"),
    name = "Node Type"
  ) +
  
  # Edge color gradient (continuous)
  scale_edge_color_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Nutrient Density"
  ) +
  
  # Edge width scale
  scale_edge_width(
    range = c(0.3, 3),
    name = "Nutrient Density"
  ) +
  theme_void() +
  labs(
    title = "Food–Nutrient Bipartite Network",
    subtitle = "Edge gradient + thickness represent nutrient density"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# (B) Projected Food–Food Network
p2 <- ggraph(food_net, layout = "fr") +
  geom_edge_link(
    aes(color = as.factor(weight)),    
    alpha = 0.8,
    show.legend = TRUE
  ) +
  geom_node_point(aes(size = degree(food_net)), color = "darkgreen") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = Inf) +
  scale_color_brewer(
    palette = "Set1",                
    name = "Shared Nutrients",
    labels = function(x) paste(x, "nutrients in common")
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3))
  ) +
  theme_void() +
  labs(
    title = "Food–Food Projection (Shared Nutrients)",
    subtitle = "Edge color = number of shared nutrients"
  ) +
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

# Degree (foods vs nutrients)
bip_deg <- degree(g)
top_deg <- sort(bip_deg, decreasing = TRUE)[1:10]
cat("Top 10 nodes by degree:\n")
print(top_deg)

# Betweenness centrality
bip_bet <- betweenness(g)
top_bet <- sort(bip_bet, decreasing = TRUE)[1:10]
cat("\nTop 10 nodes by betweenness:\n")
print(top_bet)

# Closeness centrality
bip_close <- closeness(g)
top_close <- sort(bip_close, decreasing = TRUE)[1:10]
cat("\nTop 10 nodes by closeness:\n")
print(top_close)

# Combine metrics for foods only
bip_metrics <- data.frame(
  node = names(V(g)),
  degree = degree(g),
  betweenness = betweenness(g),
  closeness = closeness(g)
)

# Keep only foods (type == FALSE)
bip_food_metrics <- bip_metrics[V(g)$type == FALSE, ]

# Export bipartite metrics
write_csv(bip_food_metrics, "bipartite_food_metrics.csv")

write_graph(g, "food_nutrient_network.graphml", format = "graphml")

cat("\n✅ Analysis complete. Files exported.\n")