# Яценко Мар'яна Ярославівна, №10, USJudgeRatings, 10 варіант

# 1. Перевірка та створення папки для даних, якщо її немає
if (!dir.exists("data")) {
  dir.create("data")
}

# 2. Перевірка поточної робочої директорії
print("Робоча директорія:")
getwd()

# 3. Виклик довідки для функції кластерного аналізу
?kmeans

# --- Завантаження датасету та первинний опис ---

# 1. Завантаження датасету
data("USJudgeRatings")
df_raw <- USJudgeRatings

# 2. Перевірка формату
print("Клас об'єкта:")
class(df_raw) 

# 3. Збереження вихідних даних
# Використовуємо прямий шлях "data/", бо ми працюємо в межах проекту
write.csv(df_raw, file = "data/raw_USJudgeRatings.csv", row.names = TRUE)
print("Вихідні дані успішно збережено в data/raw_USJudgeRatings.csv")

# 4. Демонстрація базових функцій для опису даних
print("Розмірність (dim):")
dim(df_raw)

print("Перші рядки (head):")
head(df_raw)

# Перевірка на наявність пропусків
print("Кількість пропусків:")
sum(is.na(df_raw))


# --- Підготовка даних до кластеризації ---

# 1. Масштабування числових ознак 
df_scaled <- as.data.frame(scale(df_raw))

# 2. Збереження підготовлених даних у файл
write.csv(df_scaled, file = "clean_USJudgeRatings.csv", row.names = TRUE)
print("Очищені дані збережено в data/clean_USJudgeRatings.csv")

# Перевірка результату масштабування
print("Дані після масштабування:")
head(df_scaled)


