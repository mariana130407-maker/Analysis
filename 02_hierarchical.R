# Яценко Мар'яна Ярославівна, №10, USJudgeRatings, 10 варіант
# df_scaled <- read.csv("data/clean_USJudgeRatings.csv", row.names = 1)
#  Ієрархічна кластеризація
# 1. Обчислення матриці відстаней для числових даних через dist()
dist_mat <- dist(df_scaled, method = "euclidean")

# 2. Побудова ієрархічних кластеризацій
hc_ward <- hclust(dist_mat, method = "ward.D2")
hc_average <- hclust(dist_mat, method = "average")

# 3. Побудова та збереження дендрограм у папку img/
# Перша дендрограма (Ward)
png("img/dendrogram_ward.png", width = 800, height = 600)
plot(hc_ward, main = "Дендрограма (Метод Ward.D2)", cex = 0.8, hang = -1)
rect.hclust(hc_ward, k = 3, border = "red") # додаємо умовні рамки для наочності
dev.off()

# Друга дендрограма (Average)
png("img/dendrogram_average.png", width = 800, height = 600)
plot(hc_average, main = "Дендрограма (Метод Average)", cex = 0.8, hang = -1)
dev.off()

print("Графіки успішно збережено у папці img/ !")

# 4. Обчислення коефіцієнта кофенетичної кореляції
cor_ward <- cor(dist_mat, cophenetic(hc_ward))
cor_average <- cor(dist_mat, cophenetic(hc_average))

print(" Кофенетична кореляція ")
print(paste("Метод Ward.D2:", round(cor_ward, 4)))
print(paste("Метод Average:", round(cor_average, 4)))