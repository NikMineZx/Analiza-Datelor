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

# Keep only numeric columns
meta_df <- meta_df[, sapply(meta_df, is.numeric)]
meta_df <- meta_df[complete.cases(meta_df$metascore), ]
X <- meta_df[, !colnames(meta_df) %in% "metascore"]
y <- meta_df$metascore
set.seed(5)
groups <- 10
split <- createDataPartition(y, p = 0.8, list = FALSE)
# Calculate quantiles with handling missing values
quantiles <- sapply(1:groups, function(i) {
  quantile(y, probs = (i - 1) / groups, na.rm = TRUE, na.rm.nan = TRUE)
})

train_X <- X[split, ]
test_X <- X[-split, ]
train_y <- y[split]
test_y <- y[-split]

meta_lr <- lm(train_y ~ ., data = train_X)
summary(meta_lr)

train_X_sel_lr <- train_X[, names(coef(meta_lr))[-1]]
test_X_sel_lr <- test_X[, names(coef(meta_lr))[-1]]

meta_knn <- knnreg(train_X, train_y)
summary(meta_knn)

train_X_sel_knn <- train_X[, names(coef(meta_knn))[-1]]
test_X_sel_knn <- test_X[, names(coef(meta_knn))[-1]]

k_grid <- expand.grid(k = seq(1, floor(sqrt(nrow(train_X))), by = 1))
meta_knn_gscv <- train(x = train_X_sel_knn, y = train_y, method = "knn", tuneGrid = k_grid)
best_params <- meta_knn_gscv$results[which.min(meta_knn_gscv$results$RMSE), ]
meta_knn <- knnreg(train_X_sel_knn, train_y, k = best_params$n_neighbors)
summary(meta_knn)

test_y_pred_lr <- predict(meta_lr, test_X_sel_lr)
test_y_pred_knn <- predict(meta_knn, test_X_sel_knn)

meta_mse_lr <- mse(test_y, test_y_pred_lr)
meta_r2_lr <- R2(test_y, test_y_pred_lr)

meta_mse_knn <- mse(test_y, test_y_pred_knn)
meta_r2_knn <- R2(test_y, test_y_pred_knn)

cat("Linear Regression - MSE:", meta_mse_lr, "R^2:", meta_r2_lr, "\n")
cat("K-Nearest Neighbors - MSE:", meta_mse_knn, "R^2:", meta_r2_knn, "\n")

movies_df$pred_metascore <- predict(meta_knn, data.frame(rating = movies_df$rating))

X <- meta_df[, !colnames(meta_df) %in% "gross"]
y <- meta_df$gross
set.seed(5)
split <- createDataPartition(y, p = 0.8, list = FALSE)
train_X <- X[split, ]
test_X <- X[-split, ]
train_y <- y[split]
test_y <- y[-split]

gross_lr <- lm(train_y ~ ., data = train_X)
summary(gross_lr)

train_X_sel_lr <- train_X[, names(coef(gross_lr))[-1]]
test_X_sel_lr <- test_X[, names(coef(gross_lr))[-1]]

gross_knn <- knnreg(train_X, train_y)
summary(gross_knn)

train_X_sel_knn <- train_X[, names(coef(gross_knn))[-1]]
test_X_sel_knn <- test_X[, names(coef(gross_knn))[-1]]

k_grid <- data.frame(n_neighbors = seq(1, floor(sqrt(nrow(train_X))), by = 1))
gross_knn_gscv <- train(x = train_X_sel_knn, y = train_y, method = "knn", tuneGrid = k_grid)
best_params <- gross_knn_gscv$results[which.min(gross_knn_gscv$results$RMSE), ]
gross_knn <- knnreg(train_X_sel_knn, train_y, k = best_params$n_neighbors)
summary(gross_knn)

test_y_pred_lr <- predict(gross_lr, test_X_sel_lr)
test_y_pred_knn <- predict(gross_knn, test_X_sel_knn)

gross_mse_lr <- mse(test_y, test_y_pred_lr)
gross_r2_lr <- R2(test_y, test_y_pred_lr)
gross_mse_knn <- mse(test_y, test_y_pred_knn)
gross_r2_knn <- R2(test_y, test_y_pred_knn)

cat("Linear Regression - MSE:", gross_mse_lr, "R^2:", gross_r2_lr, "\n")
cat("K-Nearest Neighbors - MSE:", gross_mse_knn, "R^2:", gross_r2_knn, "\n")

movies_df$pred_gross <- predict(gross_knn, data.frame(votes = movies_df$votes))

summary(movies_df)

movies_df %>% summary()

options(repr.plot.width=10, repr.plot.height=10)
theme_set(theme_minimal())

sample_size <- 1000
set.seed(1)
random_sample <- movies_df[sample(nrow(movies_df), sample_size), ]

rating_scatterplot <- ggplot(random_sample, aes(x = runtime, y = rating)) +
  geom_point(size = 2) +
  labs(x = "Runtime (m)", y = "Rating") +
  ggtitle("IMDb Ratings by Runtime") +
  theme(plot.title = element_text(hjust = 0.5))

print(rating_scatterplot)

cat("Correlation between movie length and IMDb rating:", cor(movies_df$runtime, movies_df$rating), "\n")

unique_genres <- unique(unlist(movies_df$genre))
genre_avg <- find_avg_rating(unique_genres, "genre")
genre_df <- data.frame(genre = unique_genres, avg_rating = genre_avg) %>%
  arrange(desc(avg_rating))

genre_count <- table(unlist(movies_df$genre))
genre_order <- as.data.frame(sort(genre_count, decreasing = TRUE))

par(mfrow = c(1, 2), mar = c(5, 10, 2, 2))
barplot(genre_df$avg_rating, names.arg = genre_df$genre, las = 2, main = "Average IMDb Rating by Genre",
        xlab = "Genre", ylab = "Average IMDb Rating", cex.names = 0.7)
barplot(genre_order$Freq, names.arg = genre_order$Var1, las = 2, main = "Number of Top Movies by Genre",
        xlab = "Genre", ylab = "# of Movies", cex.names = 0.7)

year_order <- as.data.frame(sort(table(movies_df$year), decreasing = TRUE))

unique_years <- unique(movies_df$year)
year_ratings <- find_avg_rating(unique_years, "year")
year_df <- data.frame(year = unique_years, avg_rating = year_ratings) %>%
  arrange(desc(avg_rating))

par(mfrow = c(1, 2), mar = c(5, 10, 2, 2))
plot(year_df$year, year_df$avg_rating, main = "Average IMDb Rating by Year",
     xlab = "Year", ylab = "Average IMDb Rating", pch = 16, cex = 1.5)
barplot(year_order$Freq, names.arg = year_order$Var1, las = 2, main = "Number of Top Movies by Year",
        xlab = "Year", ylab = "# of Movies", cex.names = 0.7)
