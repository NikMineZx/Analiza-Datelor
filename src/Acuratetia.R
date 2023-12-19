library(tidyverse)
library(caret)
library(Metrics)

movies_df <- read.csv("C://mobile//Bird//Analiza-Datelor//Data//Setul de date Filme.csv")
head(movies_df)

convert_to_list <- function(string) {
  tryCatch({
    string <- gsub("\\[|\\]|'", "", string)  # Remove brackets and '
    li <- strsplit(string, ", ")[[1]]
  }, error = function(e) {
    return(string)
  })
}

find_avg_rating <- function(li, col_name) {
  avg_ratings <- c()
  for (elem in li) {
    temp_df <- subset(movies_df, grepl(elem, movies_df[[col_name]]))
    avg_ratings <- c(avg_ratings, mean(temp_df$rating, na.rm = TRUE))
  }
  return(avg_ratings)
}

movies_df[] <- lapply(movies_df, function(x) ifelse(x == '', NA, x))

if (is.character(movies_df$year)) {
  movies_df$year <- substr(movies_df$year, nchar(movies_df$year) - 3, nchar(movies_df$year))
}
movies_df$year <- as.numeric(movies_df$year)
movies_df$runtime <- as.numeric(movies_df$runtime)
movies_df$rating <- as.numeric(movies_df$rating)
movies_df$metascore <- as.numeric(movies_df$metascore)
movies_df$votes <- as.numeric(movies_df$votes)
movies_df$gross <- as.numeric(movies_df$gross)

movies_df$director <- lapply(movies_df$director, convert_to_list)
movies_df$stars <- lapply(movies_df$stars, convert_to_list)
movies_df$genre <- lapply(movies_df$genre, convert_to_list)

summary(movies_df)

# Filter out rows with missing values in numeric columns
meta_df <- movies_df[sapply(movies_df, is.numeric) | sapply(movies_df, function(x) all(!is.na(unlist(x)))), ]

# Create a binary variable 'top1k' based on metascore threshold (e.g., 75)
meta_df$top1k <- as.numeric(meta_df$metascore > 75)

# Split the data into training and testing sets
meta_df <- meta_df[complete.cases(meta_df$top1k), ]

# Split the data into training and testing sets
set.seed(1)
split <- createDataPartition(meta_df$top1k, p = 0.8, list = FALSE)

train_glm <- meta_df[split, ]
test_glm <- meta_df[-split, ]

# Fit logistic regression model
modelrank <- glm(top1k ~ metascore, family = binomial, data = train_glm)

# Predict on the test set
fitted.results <- predict(modelrank, newdata = test_glm, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

# Calculate accuracy
misClasificError <- mean(fitted.results != test_glm$top1k)
print(paste('Accuracy:', 1 - custom_misClasificError))


custom_threshold <- 0.5
predicted_classes <- ifelse(fitted.results > custom_threshold, 1, 0)


table(fitted.results)
# Calculate accuracy with the custom threshold
custom_misClasificError <- mean(predicted_classes != test_glm$top1k)
print(paste('Custom Accuracy:', 1 - custom_misClasificError))



threshold_values <- seq(0, 1, by = 0.1)

for (custom_threshold in threshold_values) {
  predicted_classes <- ifelse(fitted.results > custom_threshold, 1, 0)
  custom_misClasificError <- mean(predicted_classes != test_glm$top1k)
  print(paste('Custom Accuracy (Threshold:', custom_threshold, '):', 1 - custom_misClasificError))
}
