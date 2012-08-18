# Импорт данных
dt <- read.table("data.csv", sep = ";", header = TRUE)[-1]

# Загрузка библиотек
library("plotrix")
library("ltm")
library("car")

# Метки значений переменных
dt$Пол <- factor(dt$Пол, labels = c("Мужской", "Женский"))
dt$Возраст <- factor(dt$Возраст, labels = c("20-19", "30-39", "40-49", "50-59", "> 60"))
dt$Стаж <- factor(dt$Стаж, labels = c("до года", "1-2 года", "2-5 лет", "5-7 лет", "более 7"))
dt$Образование <- factor(dt$Образование, labels = c("Высшее", "Неоконченное высшее", "Средне-специальное", "Среднее"))
dt$Должность <- factor(dt$Должность, labels = c("начальник отдела", "руководитель группы", "специалист"))
dt$Компания <- factor(dt$Компания, labels = 
  c("Bosco",
    "Logic trade",
    "Оптика",
    "Фитнес-клуб",
    "Центр сервисных услуг",
    "Касперский", 
    "Энергетика",
    "Sife",
    "Aiesec",
    "Кадровое агенство",
    "Кадровое делопроизводство"))
dt$Сфера.деятельности.компании <- factor(dt$Сфера.деятельности.компании, labels = 
  c("торговля",
    "логистика, склад",
    "услуги",
    "IT-сфера",
    "энергетика",
    "некоммеряеская, волонтерская",
    "рекрутмент",
    "юриспруденция"))
#dt$Отдел <- factor(dt$Отдел)
dt$Отдел <- recode(dt$Отдел,
  "21="Процессинговый центр";
  22="Инвестиционная служба";
  23:30="Бухгалтерия";
  31="Дирекция";
  32="Логистика";
  33:38="Склад";
  39="Спа-салон";
  40="Копировальная техника";
  41:42="Маркетинг";
  43="Отдел внешних коммуникаций";
  44="Дирекция";
  45:46="Кадровый отдел"",
as.factor.result = TRUE)

# 1. Описание выборки
summary(dt)
# Графики
pie3D(table(dt$Пол), labels = levels(dt$Пол), labelcex = 1, explode=0.1, radius=0.7)
pie3D(table(dt$Возраст), labels = levels(dt$Возраст), labelcex = 1, explode=0.2, radius=0.9, theta = pi/3, main = "Возраст", start = 2)
pie3D(table(dt$Стаж), labels = levels(dt$Стаж), labelcex = 1, explode=0.1, radius=0.9, theta = pi/3, main = "Стаж", start = 1)
pie3D(table(dt$Образование), labels = levels(dt$Образование), labelcex = 1, explode=0.1, radius=0.9, theta = pi/3, main = "Образование", start = 0)

# 2. Исследование типов лояльности организации (корреляционный анализ)
tlo <- rbind(sapply(dt[9:13], FUN = mean), sapply(dt[9:13], FUN = sd))
rownames(tlo) <- c("Среднее", "Стд. отклонение")
tlo.corr <- rcor.test(dt[,9:13])

# 3. Анализ влияния социо-демографических факторов на лояльность сотрудников  (однофакторный ANOVA)
# Анализ влияния пола сотрудников на тип их лояльности организации
tlo.gender.var <- data.frame(sapply(dt[,9:13], FUN = function(x) var.test(x ~ dt$Пол)))[1:3,]
tlo.gender.anova <- data.frame(sapply(dt[,9:13], FUN = function(x) oneway.test(x ~ dt$Пол)))[1:3,]
tlo.gender.means <- aggregate(dt[,c(9, 13)], by = list(dt$Пол), FUN = mean)
barplot(as.matrix(tlo.gender.means[-1]), beside = TRUE, legend = levels(dt$Пол))

# Анализ влияния возраста сотрудников на тип их лояльности организации
tlo.age.var <- data.frame(sapply(dt[,9:13], FUN = function(x) bartlett.test(x ~ dt$Возраст)))[1:3,]
tlo.age.anova <- data.frame(sapply(dt[,9:13], FUN = function(x) oneway.test(x ~ dt$Возраст)))[1:3,]

# Анализ влияния уровня образования сотрудников на тип их лояльности организации
tlo.edu.var <- data.frame(sapply(dt[,9:13], FUN = function(x) bartlett.test(x ~ dt$Образование)))[1:3,]
tlo.edu.anova <- data.frame(sapply(dt[,9:13], FUN = function(x) oneway.test(x ~ dt$Образование)))[1:3,]
tlo.edu.means <- aggregate(dt[,c(10:12)], by = list(dt$Образование), FUN = mean)
tlo.edu.post <- list(
  TukeyHSD(aov(dt[,10] ~ dt$Образование)),
  TukeyHSD(aov(dt[,11] ~ dt$Образование)),
  TukeyHSD(aov(dt[,12] ~ dt$Образование))
)

# 4. Анализ влияния организационных факторов  на формы лояльности сотрудников (однофакторный ANOVA)
# Анализ связи стажа сотрудников на типы их лояльности организации
tlo.ros.var <- data.frame(sapply(dt[,9:13], FUN = function(x) bartlett.test(x ~ dt$Стаж)))[1:3,]
tlo.ros.anova <- data.frame(sapply(dt[,9:13], FUN = function(x) oneway.test(x ~ dt$Стаж)))[1:3,]

# Анализ роли отделов в формировании у сотрудника типов лояльности организации
tlo.dep.var <- data.frame(sapply(dt[,9:13], FUN = function(x) bartlett.test(x ~ dt$Отдел)))[1:3,]
tlo.dep.anova <- data.frame(sapply(dt[,9:13], FUN = function(x) oneway.test(x ~ dt$Отдел)))[1:3,]
tlo.dep.means <- aggregate(dt[,c(9, 11)], by = list(dt$Отдел), FUN = mean)
tlo.dep.post <- list(
  TukeyHSD(aov(dt[,9] ~ dt$Отдел)),
  TukeyHSD(aov(dt[,11] ~ dt$Отдел))
)

# 5. Анализ связи организационной культуры в компании и типами лояльности сотрудников (корреляционный анализ)
tlo.oc.corr <- rcor.test(dt[c(9:13, 17:20)])

# 6. Анализ связи стиля руководства в компании и типами лояльности сотрудников (корреляционный анализ)
tlo.ds.corr <- rcor.test(dt[c(9:13, 14:16)])

# 7. Выявление факторов, составляющих типы лояльности (регерссионный анализ)
# Анализ факторов, обуславливающих лояльность как «привязанность»
aff.fit <- lm(dt$Привязанность ~ dt$Пол + dt$Возраст + dt$Стаж + dt$Образование + dt$Должность + dt$Д.Стиль + dt$А.Стиль + dt$П.Стиль + dt$ООК + dt$ПрОК + dt$БОК + dt$ПОК)
aff.step.fit <- step(aff.fit, direction="both")
summary(aff.fit)

# Анализ факторов, обуславливающих лояльность как «приверженность»
adh.fit <- lm(dt$Приверженность ~ dt$Пол + dt$Возраст + dt$Стаж + dt$Образование + dt$Должность + dt$Д.Стиль + dt$А.Стиль + dt$П.Стиль + dt$ООК + dt$ПрОК + dt$БОК + dt$ПОК)
adh.step.fit <- step(aff.fit, direction="both")
summary(adh.fit)

# Анализ факторов, обуславливающих лояльность как «повиновение»
ob.fit <- lm(dt$Повиновение ~ dt$Пол + dt$Возраст + dt$Стаж + dt$Образование + dt$Должность + dt$Д.Стиль + dt$А.Стиль + dt$П.Стиль + dt$ООК + dt$ПрОК + dt$БОК + dt$ПОК)
ob.step.fit <- step(aff.fit, direction="both")
summary(ob.fit)

# Анализ факторов, обуславливающих лояльность как «партнерство»
pr.fit <- lm(dt$Партнерство ~ dt$Пол + dt$Возраст + dt$Стаж + dt$Образование + dt$Должность + dt$Д.Стиль + dt$А.Стиль + dt$П.Стиль + dt$ООК + dt$ПрОК + dt$БОК + dt$ПОК)
pr.step.fit <- step(aff.fit, direction="both")
summary(pr.fit)

# Анализ факторов, обуславливающих лояльность как «патриотизм»
pt.fit <- lm(dt$Патриотизм ~ dt$Пол + dt$Возраст + dt$Стаж + dt$Образование + dt$Должность + dt$Д.Стиль + dt$А.Стиль + dt$П.Стиль + dt$ООК + dt$ПрОК + dt$БОК + dt$ПОК)
pr.step.fit <- step(aff.fit, direction="both")
summary(pr.fit)

# 8. Анализ факторов, влияющих на уровень лояльности организации
# Кластерный анализ
aff.clust <- kmeans(dt$Привязанность, 2, iter.max = 1000)
adh.clust <- kmeans(dt$Приверженность, 2, iter.max = 1000)
ob.clust <- kmeans(dt$Повиновение, 2, iter.max = 1000)
pr.clust <- kmeans(dt$Партнерство, 2, iter.max = 1000)
pt.clust <- kmeans(dt$Патриотизм, 2, iter.max = 1000)

clust.means <- data.frame(aff.clust$centers, adh.clust$centers, ob.clust$centers, pr.clust$centers, pt.clust$centers)
colnames(clust.means) <- colnames(dt[,9:13])
# ANOVA для подтверждения различий в полученных групапх
aff.clust.anova <- oneway.test(dt$Привязанность ~ aff.clust$cluster)[1:3]
adh.clust.anova <- oneway.test(dt$Приверженность ~ adh.clust$cluster)[1:3]
ob.clust.anova <- oneway.test(dt$Повиновение ~ ob.clust$cluster)[1:3]
pr.clust.anova <- oneway.test(dt$Партнерство ~ pr.clust$cluster)[1:3]
pt.clust.anova <- oneway.test(dt$Патриотизм ~ pt.clust$cluster)[1:3]

##### Кластерный анализ типов лояльности
dtc <- dt[,9:13]
# Иерархический кластерный анализ
d <- dist(dtc, method = "euclidean")
clust.fit <- hclust(d, method="ward")
plot(fit)
rect.hclust(fit, k=4, border="red")

# K-means
wss <- (nrow(dtc) -1)*sum(apply(dtc, 2, var))
for (i in 2:5) wss[i] <- sum(kmeans(dtc, centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
clust.fit <- kmeans(dtc, 4)

# Кластерный анализ на оснвое подгонки модели (по BIC)
library("mclust")
clust.fit <- Mclust(dtc)
plot(clust.fit)
summary(clust.fit)