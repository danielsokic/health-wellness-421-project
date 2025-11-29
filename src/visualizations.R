# Libraries
library(igraph)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggraph)
library(viridis)

# Ensure the 'plots' folder exists in the project root
if (!dir.exists("plots")) {
    dir.create("plots")
}

# Load data
food_data <- read.csv("food_nutrients.csv")
food_proj <- read_graph("food_nutrient_network.graphml", format = "graphml")

# --- FIXED: Use food–food projection instead of full bipartite graph ---
# proj1 = food–food network | proj2 = nutrient–nutrient network
proj <- bipartite_projection(food_proj)$proj1

# Compute PageRank on the projected food network
pr <- page.rank(proj)$vector

pr_df <- data.frame(
  food = names(pr),
  pagerank = pr
) %>%
  arrange(desc(pagerank)) %>%
  slice(1:40)

# Plot: Top 20 Foods by PageRank
p1 <- ggplot(pr_df, aes(x = reorder(food, pagerank), y = pagerank)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "4 0 Foods by PageRank",
    x = "Food",
    y = "PageRank Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    plot.title = element_text(color = "white")
  )
ggsave("plots/top20_pagerank.png", p1, width = 8, height = 6)

# Heatmap: Food × Nutrient Matrix
food_nutrient_matrix <- food_data %>%
    group_by(food, nutrientName) %>%
    summarize(value_per_100kcal = mean(value_per_100kcal, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
        names_from = nutrientName,
        values_from = value_per_100kcal,
        values_fill = 0
    )

matrix_long <- melt(food_nutrient_matrix)

p2 <- ggplot(matrix_long, aes(x = variable, y = food, fill = value)) +
    geom_tile() +
    scale_fill_viridis() +
    theme_minimal() +
    theme(
        axis.text.x = element_text(color = "white", angle = 90, vjust = 0.5),
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        plot.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
    ) +
    labs(
        title = "Food × Nutrient Density Heatmap",
        x = "Nutrient",
        y = "Food",
        fill = "Density"
    )

ggsave("plots/food_nutrient_heatmap.png", p2, width = 10, height = 8)

# Scatterplot: Nutrient Density vs Energy
p3 <- ggplot(food_data,
    aes(x = value_per_100kcal, y = value)) +
    geom_point(alpha = 0.7, color = "darkorange") +
    geom_smooth(method = "lm") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        plot.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
    ) +
    labs(
        title = "Nutrient Density vs Energy",
        x = "Nutrient Density (per 100 kcal)",
        y = "Energy (kcal)"
    )

ggsave("plots/nutrient_density_vs_energy.png", p3, width = 7, height = 5)

# Ensure no zero-weight edges
E(food_proj)$weight <- ifelse(E(food_proj)$weight <= 0, 1e-6, E(food_proj)$weight)

deg <- degree(food_proj)
btw <- betweenness(food_proj, weights = E(food_proj)$weight)
eig <- eigen_centrality(food_proj, weights = E(food_proj)$weight)$vector

central_df <- data.frame(
  degree = deg,
  betweenness = btw,
  eigenvector = eig
) %>%
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  )

E(food_proj)$weight <- ifelse(E(food_proj)$weight <= 0, 1e-6, E(food_proj)$weight)

deg <- degree(food_proj)
btw <- betweenness(food_proj, weights = E(food_proj)$weight)
eig <- eigen_centrality(food_proj, weights = E(food_proj)$weight)$vector

central_df <- data.frame(
  metric = c(
    rep("betweenness", length(btw)),
    rep("degree", length(deg)),
    rep("eigenvector", length(eig))
  ),
  value = c(btw, deg, eig)
)

# Add dummy x-axis to center boxplots
central_df$dummy_x <- ""

p4 <- ggplot(central_df, aes(x = dummy_x, y = value)) +
  geom_boxplot(fill = "tomato", outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, color = "white") +
  facet_wrap(~ metric, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "white"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "white"),
    strip.text = element_text(color = "white", size = 18, face = "bold"),
    plot.title = element_text(color = "white", size = 24)
  ) +
  labs(
    title = "Distribution of Centrality Metrics",
    y = "Value"
  )

 
ggsave("plots/centrality_boxplot.png", p4, width = 7, height = 5)