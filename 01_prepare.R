
# ПІБ: Яценко Мар'яна Ярославівна
# Номер  / Варіант: 10
# Назва датасету: DNA (пакет mlbench)
# Цільова змінна: Class

# 1. Підготовка середовища
print(getwd())

# Встановлення пакетів  та завантаження
required_packages <- c("tidyverse", "caret", "pROC", "rpart", "rattle", "partykit", "randomForest", "e1071", "mlbench")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rattle)
library(partykit)
library(randomForest)
library(e1071)
library(mlbench)


?mlbench::DNA  

# 2. Завантаження датасету 
data(DNA)
df <- as.data.frame(DNA)

# Збереження вихідних даних у папку data
write.csv(df, "data/raw_DNA.csv", row.names = FALSE)

# Демонстрація структури
str(df)
print(dim(df))
print(names(df))
print(head(df))
summary(df)

# Побудова та збереження графіка розподілу класів
png("img/class_balance.png", width = 800, height = 600)
print(
  ggplot(df, aes(x = Class, fill = Class)) +
    geom_bar(color = "black") +
    theme_minimal() +
    labs(title = "Розподіл класів у вихідному датасеті DNA", x = "Клас", y = "Кількість")
)
dev.off()

# 3. Підготовка даних до класифікації
# Перевірка на пропуски
missing_vals <- sum(!complete.cases(df))
print(paste("Кількість пропусків:", missing_vals))

# Бінаризація цільової змінної: виділяємо клас 'ei'  проти всіх інших
df$Class <- ifelse(df$Class == "ei", "ei", "other")
df$Class <- factor(df$Class, levels = c("ei", "other"))

# Всі інші змінні  є категоріальними  
df[, 1:60] <- lapply(df[, 1:60], factor)

# Збереження підготовлених даних
write.csv(df, "data/clean_DNA.csv", row.names = FALSE)
