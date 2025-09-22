# ------------------------------------------- #
# Q6) Run conditional inference trees and forests. [For R users, you can use ctree from partykit or party package.
# ------------------------------------------- #

# Install packages 
install.packages("party")
install.packages("partykit")

library(party)
library(partykit)

# Load dataset
df <- read.csv("C:/Users/anirb/OneDrive/Documents/Desktop/credit_risk.csv")

#displaying head values and summary
head(df)
str(df)
summary(df)

df$default <- as.factor(df$default)

# Train,test & split
set.seed(42)
train_idx <- sample(1:nrow(df), 0.8 * nrow(df))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

# Condititonal inference trees

# Convert character columns to factor
train[] <- lapply(train, function(x) {
  if (is.character(x)) as.factor(x) else x
})

# fit the model
ctree_model <- ctree(default ~ ., data = train)

# Plot the tree
plot(ctree_model)

# Predictions
pred_ctree <- predict(ctree_model, newdata = test)

# Evaluation
table(Predicted = pred_ctree, Actual = test$default)

# Conditional Inference Forest
train[] <- lapply(train, function(x) if(is.character(x)) as.factor(x) else x)

# Fit Conditional Inference Forest
cforest_model <- cforest(default ~ ., data = train,
                         control = cforest_unbiased(ntree = 500, mtry = 3))

# Variable Importance
varimp <- varimp(cforest_model)
print(varimp)

# Plot importance
barplot(sort(varimp, decreasing = TRUE)[1:10],
        las = 2, col = "steelblue",
        main = "Top 10 Feature Importances - Conditional Inference Forest")

#Predictions
predict.cforest<- predict(cforest_model, newdata=test)

#Evaluation
table(Predicted = pred_ctree, Actual = test$default)
