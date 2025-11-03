| **Member**                  | **Role / Responsibilities**                                                               |
| --------------------------- | ----------------------------------------------------------------------------------------- |
| **Harjot (97986475)**       | Collect food and nutrient data, clean the data, and create the nutrient density dataset.  |
| **Jaxson (41706011)**       | Build the network in R, calculate basic centrality measures, and check network structure. |
| **Travis (60897899)**       | Create visualizations (e.g., plots) and summarize main findings.                          |
| **Daniel Sokic (12213229)** | Write the report, interpret results in simple terms, and design the final slides.         |


# Problem Statement and Goals

University students often rely on a small set of inexpensive, easy-to-prepare foods, leading to hidden micronutrient deficiencies (e.g., iron, folate, or vitamin D). Existing nutrition tools (MyFitnessPal, Canada’s Food Guide, etc.) evaluate foods individually rather than as complements that jointly cover nutrient gaps.

Our goal is to use network science to identify affordable, diverse sets of foods that together maximize micronutrient coverage per calorie and per dollar, and remain robust under limited budgets or supply disruptions. We will build and analyze a Food-Nutrient Network using data from the USDA FoodData Central (FDC) AP

## We aim to answer:
- Which foods are most central to a balanced student diet?
- How resilient is the diet to the loss of certain “keystone” foods?
- How can network insights improve food choices for low-budget, time-constrained students?
- Which nutrients are the hardest to obtain on a student budget, and which affordable foods help fill those gaps?

While existing literature and diet apps focus on individual food rankings or recipes, few studies frame nutrition as a network problem of combinational nutrient coverage.

This project introduces:
- Bipartite food–nutrient modeling (foods to nutrients) weighted by nutrient density per 100 kcal.
- Food-to-food projection showing complementarity foods that together fill nutrient gaps.
- Robust set selection tolerant to missing foods (e.g., dietary restrictions).
- Network interpretation of diet resilience using centrality, community structure, and assortativity.


## Metrics and Analysis Plan:
- Degree centrality: number of nutrients (for foods) or foods (for nutrients).
- Eigenvector/PageRank centrality: importance of foods connected to valuable nutrients.
- Betweenness centrality: “bridge” foods that link nutrient clusters.
- Clustering coefficient & modularity: identify communities (food categories or nutrient clusters).
- Average degree & density: overall nutrient diversity and redundancy.

## Where will you get your data from? 
We will use the USDA FoodData Central API (https://fdc.nal.usda.gov/api-guide) to query nutrient profiles for a curated list of common “student foods” under $5 (grains, proteins, fruits, vegetables, snacks, and beverages).
Each query will return 100 g and 100 kcal nutrient values for vitamins, iron, calcium, magnesium, potassium, zinc, folate, etc.

## Node and Edge Definition
The network will first be bipartite, connecting foods to the nutrients they contain, and then projected into a food–food network to study how foods complement each other nutritionally.

## Nodes:
There will be two types of nodes in our network:
- Foods: each food item will be represented as a node. Each food node will include extra information such as cost per 100 kcal, and whether it fits certain diets (like vegetarian or vegan).
- Nutrients: each important micronutrient (like iron, calcium, potassium, vitamin A, B12, C, D, and folate) will also be a node.

## Edges:
An edge connects a food to a nutrient if that food contains the nutrient.
- The edge weight will represent how much of the nutrient the food provides per 100 kcal, which makes it fair to compare foods with different calorie amounts.
- Optionally, we may adjust edge weights by cost or prep time to see which foods give the most nutrients for the least money or effort






