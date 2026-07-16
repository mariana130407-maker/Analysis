# Яценко Мар'яна Ярославівна, №10, USJudgeRatings, 10 варіант

# Підключення необхідної бібліотеки 
library(cluster)
#  Вибір кількості кластерів (k)
# ==========================================

print("Рахуємо метод ліктя та силуету")

# 1. Метод ліктя (Elbow method)
wss <- numeric(10)
for (i in 1:10) {
  wss[i] <- kmeans(df_scaled, centers = i, nstart = 20)$tot.withinss
}

png("img/elbow.png", width = 800, height = 600)
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Кількість кластерів (k)",
     ylab = "Сума квадратів (WSS)",
     main = "Метод ліктя")
dev.off()



#  Ітеративна кластеризація
k_opt <- 2 # Наше обране оптимальне число кластерів

# 1. k-means кластеризація
set.seed(123) # Для відтворюваності результатів
km_res <- kmeans(df_scaled, centers = k_opt, nstart = 20)

print("Розміри кластерів k-means:")
print(km_res$size)

# 2. PAM (k-medoids) кластеризація
pam_res <- pam(df_scaled, k = k_opt)

print("Розміри кластерів PAM:")
print(pam_res$clusinfo[, "size"])

# Отримання кластерів з ієрархічної кластеризації (Ward) для порівняння
hc_clusters <- cutree(hc_ward, k = k_opt)

# 3. Створення та збереження спільної таблиці результатів
clusters_df <- data.frame(
  Judge = rownames(df_scaled),
  hclust_ward = hc_clusters,
  kmeans = km_res$cluster,
  pam = pam_res$clustering
)

write.csv(clusters_df, file = "data/clusters_USJudgeRatings.csv", row.names = FALSE)
print("Таблицю з результатами кластеризації збережено в data/clusters_USJudgeRatings.csv")

# 2. Метод середнього силуету (Silhouette method)
sil_width <- numeric(10)
for (i in 2:10) {
  pam_fit <- pam(df_scaled, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

png("img/silhouette.png", width = 800, height = 600)
plot(2:10, sil_width[2:10], type = "b", pch = 19, frame = FALSE,
     xlab = "Кількість кластерів (k)",
     ylab = "Середня ширина силуету",
     main = "Метод силуету")
dev.off()

print("Графіки elbow.png та silhouette.png збережено у папці img/")