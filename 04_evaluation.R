
# ПІБ: Яценко Мар'яна Ярославівна
# Номер  / Варіант: 10
# Назва датасету: DNA

# Завантаження фінальних результатів з попереднього кроку
load("data/final_metrics.RData")

# Порівняння моделей та фінальна таблиця

#  Результати логістичної регресії для стандартного порогу 0.5
metrics_lr_05 <- lr_results[["0.5"]]

# Об'єднання всіх метрик
summary_table <- rbind(
  "Logistic Regression" = metrics_lr_05,
  "Decision Tree" = metrics_tree,
  "Random Forest" = metrics_rf,
  "SVM" = metrics_svm,
  "Tuned SVM" = metrics_svm_tuned
)

# Додавання стовпця AUC 
# Для інших  NA 
summary_table$AUC <- c(round(auc_lr_val, 4), NA, round(auc_rf_val, 4), NA, NA)

print(" Підсумкова таблиця метрик для всіх моделей")
print(summary_table)

# Збереження фінальної таблиці у файл
write.csv(summary_table, "data/model_metrics_DNA.csv", row.names = TRUE)
