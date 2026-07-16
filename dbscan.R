# Яценко Мар'яна Ярославівна, №10, USJudgeRatings, 10 варіант

# Підключаємо пакет для DBSCAN 
library(dbscan)

# DBSCAN та аналіз аномалій
# 1. Побудова kNNdistplot для пошуку оптимального радіуса (eps)
# Оскільки датасет дуже малий (43 об'єкти), візьмемо MinPts = 4
min_pts <- 4

png("img/kNNdistplot.png", width = 800, height = 600)
kNNdistplot(df_scaled, k = min_pts)
abline(h = 2.5, col = "red", lty = 2) #  червона лінія на око для "зламу"
dev.off()
print("Графік kNNdistplot.png збережено!")

# 2. Виконання DBSCAN 
# Значення eps = 2.5 беремо з орієнтовного місця різкого підйому на графіку
eps_val <- 2.5
db_res <- dbscan(df_scaled, eps = eps_val, minPts = min_pts)

print("Результат DBSCAN")
print(db_res)

# 3. Графік кластерів та шуму ( PCA з попереднього кроку для 2D)
png("img/dbscan_clusters.png", width = 800, height = 600)
plot(pca_res$x[, 1], pca_res$x[, 2], 
     col = db_res$cluster + 1, # Кольори (+1 щоб шум (0) був чорним)
     pch = ifelse(db_res$cluster == 0, 4, 19), # 4 - хрестики для шуму, 19 - кружечки для кластерів
     cex = 1.5, 
     main = "Кластеризація DBSCAN (Хрестики - це аномалії/шум)",
     xlab = "Головна компонента 1 (PC1)", ylab = "Головна компонента 2 (PC2)")
legend("topright", 
       legend = c("Аномалії (Шум)", paste("Кластер", 1:max(db_res$cluster))),
       col = 1:(max(db_res$cluster) + 1), 
       pch = c(4, rep(19, max(db_res$cluster))))
dev.off()
print("Графік dbscan_clusters.png збережено!")