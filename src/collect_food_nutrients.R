# collect_food_nutrients.R

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# Setup--
api_key <- Sys.getenv("FDC_API_KEY")  # safer: set via Sys.setenv(FDC_API_KEY="yourkey")
if (api_key == "") api_key <- "MvBTNqqBAgVlqcDRQEf4VNw0eclyMxMAZ8U1nuyZ"  # fallback for testing

# Helper function: safely extract nutrients
get_food_nutrients_safe <- function(food_name, debug = TRUE) {
  message("\nFetching data for: ", food_name)
  
  # 1. Search endpoint (first try Foundation + SR Legacy)
  search_url <- paste0(
    "https://api.nal.usda.gov/fdc/v1/foods/search?query=",
    URLencode(food_name),
    "&pageSize=1&dataType=Foundation,SR%20Legacy&api_key=", api_key
  )
  
  res <- GET(search_url)
  text_res <- content(res, "text", encoding = "UTF-8")
  search_data <- tryCatch(fromJSON(text_res, simplifyVector = TRUE), error = function(e) NULL)
  
  # Retry with broader data types if not found
  if (is.null(search_data$foods) || nrow(search_data$foods) == 0) {
    message("Retrying with all data types for ", food_name)
    search_url <- paste0(
      "https://api.nal.usda.gov/fdc/v1/foods/search?query=",
      URLencode(food_name),
      "&pageSize=1&api_key=", api_key
    )
    res <- GET(search_url)
    text_res <- content(res, "text", encoding = "UTF-8")
    search_data <- tryCatch(fromJSON(text_res, simplifyVector = TRUE), error = function(e) NULL)
  }
  
  if (is.null(search_data$foods) || nrow(search_data$foods) == 0) {
    message("No foods found for ", food_name)
    return(NULL)
  }
  
  fdc_id <- search_data$foods$fdcId[1]
  data_type <- search_data$foods$dataType[1]
  message("ound match for ", food_name, " (FDC ID: ", fdc_id, ", DataType: ", data_type, ")")
  
  # 2. Detail endpoint
  food_url <- paste0("https://api.nal.usda.gov/fdc/v1/food/", fdc_id, "?api_key=", api_key)
  food_res <- GET(food_url)
  
  if (status_code(food_res) == 404) {
    message("Skipping ", food_name, " (404 detail missing)")
    return(NULL)
  }
  if (status_code(food_res) != 200) {
    message("iled to get details for ", food_name, " (status ", status_code(food_res), ")")
    return(NULL)
  }
  
  food_data <- tryCatch(
    fromJSON(content(food_res, "text", encoding = "UTF-8"), simplifyVector = FALSE),
    error = function(e) {
      message("JSON parse error in food details for ", food_name, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(food_data$foodNutrients)) {
    message("No nutrient info found for ", food_name)
    return(NULL)
  }
  
  if (debug) {
    message("Structure of first foodNutrients entry for ", food_name, ":")
    print(str(food_data$foodNutrients[[1]], max.level = 2))
  }
  
  # 3. Extract nutrient rows safely
  rows <- lapply(food_data$foodNutrients, function(x) {
    if (!is.null(x$nutrient) && is.list(x$nutrient)) {
      return(data.frame(
        nutrientName = x$nutrient$name %||% NA,
        value = x$amount %||% NA,
        unitName = x$nutrient$unitName %||% NA,
        stringsAsFactors = FALSE
      ))
    }
    if (!is.null(x$name) || !is.null(x$amount) || !is.null(x$unitName)) {
      return(data.frame(
        nutrientName = x$name %||% NA,
        value = x$amount %||% NA,
        unitName = x$unitName %||% NA,
        stringsAsFactors = FALSE
      ))
    }
    return(data.frame(nutrientName = NA, value = NA, unitName = NA))
  })
  
  nutrients <- bind_rows(rows)
  
  # 4. Filter to key nutrients (include Energy for normalization)
  nutrients <- nutrients %>%
    filter(nutrientName %in% c(
      "Energy",
      "Iron, Fe", "Calcium, Ca", "Magnesium, Mg", "Potassium, K",
      "Zinc, Zn", "Folate, total", "Vitamin A, RAE",
      "Vitamin C, total ascorbic acid", "Vitamin D (D2 + D3)",
      "Vitamin B-12", "Vitamin E (alpha-tocopherol)"
    )) %>%
    mutate(food = food_name) %>%
    filter(!is.na(value), value > 0)
  
  # 5. Normalize nutrient values to per 100 kcal
  if ("Energy" %in% nutrients$nutrientName) {
    energy_row <- nutrients %>% filter(grepl("kcal", unitName))
    if (nrow(energy_row) == 1) {
      kcal_value <- energy_row$value[1]
      if (!is.na(kcal_value) && kcal_value > 0) {
        nutrients <- nutrients %>%
          mutate(value_per_100kcal = ifelse(
            nutrientName != "Energy",
            (value / kcal_value) * 100,
            100
          ))
      }
    } else {
      message("Skipping normalization for ", food_name, " (no kcal entry)")
      nutrients$value_per_100kcal <- NA
    }
  } else {
    nutrients$value_per_100kcal <- NA
  }
  
  # 6. Return clean data frame
  return(nutrients)
}

# Run for selected foods
foods <- c("oats", "eggs", "spinach", "milk")

data_list <- lapply(foods, function(f) {
  tryCatch({
    Sys.sleep(0.5)
    get_food_nutrients_safe(f)
  }, error = function(e) {
    message("⚠️ Error for ", f, ": ", e$message)
    NULL
  })
})

nutrient_data <- bind_rows(data_list)
print(nutrient_data)

# Save to CSV
output_file <- "food_nutrients.csv"
write.csv(nutrient_data, output_file, row.names = FALSE)
message("CSV file saved as: ", output_file)
