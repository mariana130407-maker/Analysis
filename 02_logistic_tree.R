
# ПІБ: Яценко Мар'яна Ярославівна
# Номер / Варіант: 10
# Назва датасету: DNA


library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rattle)

# Завантаження чистих даних з попереднього кроку
df <- read.csv("data/clean_DNA.csv")
df$Class <- factor(df$Class, levels = c("ei", "other"))

# Оскільки ознаки V1-V180 вже є бінарними (0 та 1), залишаємо їх числовими 


#  Розбиття на навчальну та тестову вибірки


# Розбиття 70/30
set.seed(13042007) 
train_index <- createDataPartition(df$Class, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data  <- df[-train_index, ]

print("Розподіл класів у навчальній вибірці:")
print(table(train_data$Class))
print("Розподіл класів у тестовій вибірці:")
print(table(test_data$Class))

# Власна функція для оцінювання якості бінарної класифікації
evaluate_binary <- function(conf_matrix) {
  TP <- conf_matrix[1, 1]
  FN <- conf_matrix[2, 1]
  FP <- conf_matrix[1, 2]
  TN <- conf_matrix[2, 2]
  
  accuracy    <- (TP + TN) / (TP + TN + FP + FN)
  error_rate  <- 1 - accuracy
  sensitivity <- TP / (TP + FN) # Recall
  specificity <- TN / (TN + FP)
  precision   <- TP / (TP + FP)
  f1_score    <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  metrics <- data.frame(
    Accuracy = round(accuracy, 4),
    Error_Rate = round(error_rate, 4),
    Sensitivity_Recall = round(sensitivity, 4),
    Specificity = round(specificity, 4),
    Precision = round(precision, 4),
    F1_Score = round(f1_score, 4)
  )
  return(metrics)
}

#  Логістична регресія та аналіз порогу класифікації

lr_model <- glm(Class ~ ., data = train_data, family = binomial)
print("Модель логістичної регресії побудовано")

# Прогнозовані ймовірності
lr_probs <- predict(lr_model, newdata = test_data, type = "response")

# Дослідження впливу різних порогів (0.3, 0.5, 0.7)
thresholds <- c(0.3, 0.5, 0.7)
lr_results <- list()

for (t in thresholds) {
  #  glm прогнозує ймовірність другого рівня (other)
  pred_class <- ifelse(lr_probs < t, "ei", "other") 
  pred_class <- factor(pred_class, levels = c("ei", "other"))
  
  cm <- table(Predicted = pred_class, Actual = test_data$Class)
  print(paste("Матриця помилок для порогу", t, "---"))
  print(cm)
  
  lr_results[[as.character(t)]] <- evaluate_binary(cm)
  print(lr_results[[as.character(t)]])
}

# Побудова ROC-кривої
roc_lr <- roc(test_data$Class, lr_probs, levels = c("other", "ei"))
png("img/logistic_curve.png", width = 800, height = 600)
plot(roc_lr, col = "blue", lwd = 2, main = paste("ROC-крива Логістичної Регресії (AUC =", round(auc(roc_lr), 4), ")"))
dev.off()

# Дерево рішень


tree_model <- rpart(Class ~ ., data = train_data, method = "class")
print("Таблиця складності дерева (cptable):")
print(tree_model$cptable)

# Графік cp
png("img/tree_cp.png", width = 800, height = 600)
plotcp(tree_model)
dev.off()

# Обрізання дерева
best_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"]
pruned_tree <- prune(tree_model, cp = best_cp)

# Візуалізація обрізаного дерева
png("img/decision_tree.png", width = 1000, height = 800)
fancyRpartPlot(pruned_tree, sub = "Обрізане дерево рішень для DNA")
dev.off()

# Прогноз та метрики для дерева
tree_preds <- predict(pruned_tree, newdata = test_data, type = "class")
cm_tree <- table(Predicted = tree_preds, Actual = test_data$Class)

print(" Матриця помилок для Дерева рішень ")
print(cm_tree)
metrics_tree <- evaluate_binary(cm_tree)
print(metrics_tree)

# Збереження результатів у файл для наступних скриптів
save(lr_results, metrics_tree, roc_lr, test_data, train_data, evaluate_binary, file = "data/temp_models.RData")

