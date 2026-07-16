# Яценко Мар'яна Ярославівна, №10, USJudgeRatings, 10 варіант

# Підключаємо пакет для порівняння кластерів (ARI)
library(mclust)

#  Порівняння методів, візуалізація та профілювання

# 1. Порівняння результатів різних методів (Adjusted Rand Index)
ari_hc_km <- adjustedRandIndex(hc_clusters, km_res$cluster)
ari_km_pam <- adjustedRandIndex(km_res$cluster, pam_res$clustering)

print(" Adjusted Rand Index (ARI) ")
print(paste("Ward vs k-means:", round(ari_hc_km, 4)))
print(paste("k-means vs PAM:", round(ari_km_pam, 4)))

# 2. Візуалізація кластерів у просторі перших головних компонент (PCA)
pca_res <- prcomp(df_scaled)
pca_data <- data.frame(pca_res$x[, 1:2], Cluster = as.factor(km_res$cluster))

png("img/pca_clusters.png", width = 800, height = 600)
plot(pca_data$PC1, pca_data$PC2, col = pca_data$Cluster, pch = 19, cex = 1.5,
     main = "Візуалізація кластерів (PCA)",
     xlab = "Головна компонента 1 (PC1)", ylab = "Головна компонента 2 (PC2)")
legend("topright", legend = paste("Кластер", levels(pca_data$Cluster)), 
       col = 1:2, pch = 19)
dev.off()
print("Графік pca_clusters.png збережено!")

# 3. Таблиця профілів кластерів (середні значення вихідних немасштабованих змінних)
profiles <- aggregate(df_raw, by = list(Cluster = km_res$cluster), FUN = mean)

print(" Профілі кластерів (середні значення)")
print(profiles)

# 4. Візуалізація профілів
png("img/cluster_profiles.png", width = 800, height = 600)
matplot(t(profiles[, -1]), type = "b", pch = 19, lty = 1, col = 1:2,
        xaxt = "n", ylab = "Середній бал", main = "Профілі кластерів суддів",
        xlab = "Критерії оцінювання")
axis(1, at = 1:ncol(df_raw), labels = colnames(df_raw), las = 2)
legend("bottomright", legend = paste("Кластер", 1:2), col = 1:2, lty = 1, pch = 19)
dev.off()
print("Графік cluster_profiles.png збережено!")