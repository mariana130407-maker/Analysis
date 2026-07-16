# ПІБ: Яценко Мар'яна Ярославівна
# Номер: 10
# Датасет: warpbreaks


getwd()

help(plot)

data(warpbreaks)

# формат data.frame
class(warpbreaks)

write.csv(warpbreaks, "data/raw_warpbreaks.csv", row.names = FALSE)
# Переглядаємо базові функції [cite: 28]
str(warpbreaks)      # Структура датасету [cite: 29]
dim(warpbreaks)      # Рядки та стовпці [cite: 29]
names(warpbreaks)    # Назви колонок [cite: 29]
head(warpbreaks)     # Перші 6 рядків [cite: 30]
summary(warpbreaks)  # Статистичний опис [cite: 30]


# У датасеті немає пропущених значень , тому [cite: 34, 35]

#  1 назви колонок до нижнього регістру [cite: 39]
names(warpbreaks) <- tolower(names(warpbreaks))

# 2 нова ознака (категоризація числової змінної) [cite: 41]
#  колонка "breaks_level", яка показує "high", якщо розривів більше 26, і "low", якщо менше
warpbreaks$breaks_level <- ifelse(warpbreaks$breaks > 26, "high", "low")

head(warpbreaks)
write.csv(warpbreaks, "data/clean_warpbreaks.csv", row.names = FALSE)
# 1. Доступ до колонки через $ [cite: 45]
wool_col <- warpbreaks$wool

# 2. Вибір рядків (де розривів більше 30) [cite: 46]
high_breaks <- warpbreaks[warpbreaks$breaks > 30, ]

# 3. Вибір підтаблиці по рядках/стовпцях через індекси (перші 10 рядків, перші 2 стовпці) [cite: 47]
sub_table <- warpbreaks[1:10, 1:2]

# 4.  [cite: 48]
wool_A_data <- subset(warpbreaks, wool == "A")


#  Гістограма [cite: 54]
png("img/plot1.png") # [cite: 59]
hist(warpbreaks$breaks, 
     main = "Гістограма розривів", # [cite: 56]
     xlab = "Кількість розривів", ylab = "Частота", # [cite: 56]
     col = "lightblue") # [cite: 57]
dev.off() # [cite: 59]

#  Boxplot [cite: 53]
png("img/plot2.png") # [cite: 59]
boxplot(breaks ~ wool, data = warpbreaks, 
        main = "Розриви залежно від типу шерсті", # [cite: 56]
        xlab = "Тип шерсті", ylab = "Кількість розривів", # [cite: 56]
        col = c("pink", "lightgreen")) # [cite: 57]
dev.off() # [cite: 59]

#  Scatter plot [cite: 51]
png("img/plot3.png") # [cite: 59]
plot(warpbreaks$breaks, 
     main = "Точковий графік розривів", # [cite: 56]
     xlab = "Індекс спостереження", ylab = "Кількість розривів", # [cite: 56]
     pch = 16, col = "blue", type = "p") # [cite: 57]
dev.off() # [cite: 59]
