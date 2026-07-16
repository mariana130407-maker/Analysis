
# ПІБ: Яценко Мар'яна Ярославівна
# Номер  / Варіант: 10
# Назва датасету: DNA


library(randomForest)
library(e1071)
library(pROC)

# Завантажуємо дані та змінні з попереднього кроку
load("data/temp_models.RData")


#  Випадковий ліс (Random Forest)

set.seed(13042007)
rf_model <- randomForest(Class ~ ., data = train_data, ntree = 100, importance = TRUE)
print("Модель випадкового лісу:")
print(rf_model)

# Прогноз та метрики для Random Forest
rf_preds <- predict(rf_model, newdata = test_data)
cm_rf <- table(Predicted = rf_preds, Actual = test_data$Class)

print(" Матриця помилок для Випадкового лісу ")
print(cm_rf)
metrics_rf <- evaluate_binary(cm_rf)
print(metrics_rf)

# Важливість змінних
png("img/rf_importance.png", width = 800, height = 800)
varImpPlot(rf_model, sort = TRUE, n.var = 20, main = " 20 найважливіших ознак (Random Forest)")
dev.off()


#  Метод опорних векторів (SVM)
# Базова модель SVM
svm_model <- svm(Class ~ ., data = train_data, probability = TRUE)
print("Базова модель SVM побудована")

svm_preds <- predict(svm_model, newdata = test_data)
cm_svm <- table(Predicted = svm_preds, Actual = test_data$Class)
metrics_svm <- evaluate_binary(cm_svm)

# Налаштування SVM через tune.svm
# Використання невеликої сітки параметрів, щоб не чекати дуже довго
print("Підбір параметрів для SVM")
set.seed(13042007)
tuned_svm <- tune.svm(Class ~ ., data = train_data, cost = c(0.1, 1, 10), gamma = c(0.01, 0.1))
print(tuned_svm)

best_svm <- tuned_svm$best.model
svm_tuned_preds <- predict(best_svm, newdata = test_data)
cm_svm_tuned <- table(Predicted = svm_tuned_preds, Actual = test_data$Class)
metrics_svm_tuned <- evaluate_binary(cm_svm_tuned)


# Порівняння ROC-кривих
#  ймовірності для Random Forest
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")[, "ei"]
roc_rf <- roc(test_data$Class, rf_probs, levels = c("other", "ei"))

png("img/roc_models.png", width = 800, height = 600)
plot(roc_lr, col = "blue", lwd = 2, main = "Порівняння ROC-кривих (LR vs RF)")
plot(roc_rf, col = "red", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Logistic Regression", "Random Forest"), col = c("blue", "red"), lwd = 2)
dev.off()

# Збереження  результатів для останнього скрипта оцінки
auc_lr_val <- auc(roc_lr)
auc_rf_val <- auc(roc_rf)
save(lr_results, metrics_tree, metrics_rf, metrics_svm, metrics_svm_tuned, auc_lr_val, auc_rf_val, file = "data/final_metrics.RData")
