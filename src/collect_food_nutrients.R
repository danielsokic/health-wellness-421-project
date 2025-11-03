library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

api_key <- "MvBTNqqBAgVlqcDRQEf4VNw0eclyMxMAZ8U1nuyZ" 

get_food_nutrients_safe <- function(food_name, debug = TRUE) {
  # Search for food
  search_url <- paste0(
    "https://api.nal.usda.gov/fdc/v1/foods/search?query=",
    URLencode(food_name),
    "&pageSize=1&api_key=", api_key
  )
  res <- GET(search_url)
  search_data <- fromJSON(content(res, "text"), simplifyVector = TRUE)
  if (length(search_data$foods) == 0) return(NULL)
  
  fdc_id <- search_data$foods$fdcId[1]
  
  # Get food detail
  food_url <- paste0("https://api.nal.usda.gov/fdc/v1/food/", fdc_id, "?api_key=", api_key)
  food_res <- GET(food_url)
  food_data <- fromJSON(content(food_res, "text"), simplifyVector = FALSE)
  
  if (is.null(food_data$foodNutrients)) {
    message("No nutrient info found for ", food_name)
    return(NULL)
  }
  
  # Optional: Print first nutrient structure for debugging
  if (debug) {
    message("Structure of first foodNutrients entry for ", food_name, ":")
    print(str(food_data$foodNutrients[[1]], max.level = 2))
  }
  
  # Safely extract data rows
  rows <- lapply(food_data$foodNutrients, function(x) {
    # Case 1: Nested nutrient list
    if (!is.null(x$nutrient) && is.list(x$nutrient)) {
      return(data.frame(
        nutrientName = x$nutrient$name %||% NA,
        value = x$amount %||% NA,
        unitName = x$nutrient$unitName %||% NA,
        stringsAsFactors = FALSE
      ))
    }
    # Case 2: Flat fields (no nesting)
    if (!is.null(x$name) || !is.null(x$amount) || !is.null(x$unitName)) {
      return(data.frame(
        nutrientName = x$name %||% NA,
        value = x$amount %||% NA,
        unitName = x$unitName %||% NA,
        stringsAsFactors = FALSE
      ))
    }
    # Case 3: atomic or weird type
    return(data.frame(
      nutrientName = NA,
      value = NA,
      unitName = NA,
      stringsAsFactors = FALSE
    ))
  })
  
  nutrients <- bind_rows(rows)
  
  # Step 4: Filter to key nutrients
  nutrients <- nutrients %>%
    filter(nutrientName %in% c(
      "Iron, Fe", "Calcium, Ca", "Magnesium, Mg", "Potassium, K",
      "Zinc, Zn", "Folate, total", "Vitamin A, RAE",
      "Vitamin C, total ascorbic acid", "Vitamin D (D2 + D3)",
      "Vitamin B-12", "Vitamin E (alpha-tocopherol)"
    )) %>%
    mutate(food = food_name)
  
  return(nutrients)
}

# ---- TEST RUN ----
foods <- c("oats", "eggs", "spinach", "milk")
data_list <- lapply(foods, function(f) {
  tryCatch(get_food_nutrients_safe(f), error = function(e) {
    message("Error for ", f, ": ", e$message)
    NULL
  })
})
nutrient_data <- bind_rows(data_list)
print(nutrient_data)


write.csv(nutrient_data,
          "C:/Users/danie/Semester1/sem1/COSC421/health-wellness-421-project/food_nutrients.csv",
          row.names = FALSE)

message("CSV file saved as: ", output_file)
