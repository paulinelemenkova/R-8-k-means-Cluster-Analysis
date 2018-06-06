# libraries: 'factoextra', 'FactoMiner' ‘zip’, ‘openxlsx’, ‘carData’, ‘pbkrtest’, ‘rio’, ‘car’, ‘flashClust’, ‘leaps’, ‘scatterplot3d’, ‘FactoMineR’, ‘ca’, ‘igraph’

# ЧАСТЬ 1: делаем data.frame с геоморфологией 
	# шаг-1. загружаем таблицу, делаем датафрейм
MorDF <- read.csv("Morphology.csv", header=TRUE, sep = ",")
head(MorDF)
summary(MorDF)

# ЧАСТЬ 2: рисуем центроид по двум главным дискриминантам функции
#Centroid Plot against 1st 2 discriminant functions
	# шаг-2. рисуем центроид по двум главным дискриминантам функции

set.seed(518) # размер выборки. здесь: 518 точек в каждом профиле
# 3 центра
fit3 <- kmeans(MorDF, 3)
# 4 центра
fit4 <- kmeans(MorDF, 4)
# 5 центров
fit5 <- kmeans(MorDF, 5)
# 6 центров
fit6 <- kmeans(MorDF, 6)

fit2 <- clusplot(MorDF, fit4$cluster, color=TRUE, shade=TRUE, 
   labels=2, lines=0) # визуализируем (выводим на плот) fit4
fit3 <- clusplot(MorDF, fit5$cluster, color=TRUE, shade=TRUE, 
   labels=2, lines=0) # визуализируем fit5
fit7 <- clusplot(MorDF, fit6$cluster, color=TRUE, shade=TRUE, 
   labels=2, lines=0) # визуализируем fit3
fit8 <- clusplot(MorDF, fit3$cluster, color=TRUE, shade=TRUE, 
   labels=2, lines=0)


# ЧАСТЬ 3: выбираем лучший метод кластеризации
# comparing 2 cluster solutions
	# шаг-3. рисуем центроид по двум главным дискриминантам функции
library(mclust)
fit <- Mclust(MorDF)
plot(fit) 
# plot results выдает 4 разных типа графиков: см. картинку "methods-model-fitting.jpg". 
# картинки можно сохранять как pdf. здесь получились: Mclust-uncertainty.pdf, Mclust-BIC.pdf, Mclust-density.pdf, Mclust-classification.pdf
summary(fit) # display the best model. здесь: BIC - лучший метод.
 


