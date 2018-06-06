# libraries: 'factoextra', 'FactoMiner' ‘zip’, ‘openxlsx’, ‘carData’, ‘pbkrtest’, ‘rio’, ‘car’, ‘flashClust’, ‘leaps’, ‘scatterplot3d’, ‘FactoMineR’, ‘ca’, ‘igraph’

# ЧАСТЬ 1: делаем data.frame с геоморфологией 
	# шаг-2. загружаем таблицу, делаем датафрейм
MorDF <- read.csv("Morphology.csv", header=TRUE, sep = ",")
head(MorDF)
summary(MorDF)

# ЧАСТЬ 2: Pairwise Standard Scatter Correlation Plots
	# шаг-2.  далем кластерный анализ с k=6 (оптимальное в данном случае)
k6MorDF <- kmeans(MorDF, centers = 6, nstart = 25) 
str(6MorDF)
k6MorDF
fviz_cluster(k6MorDF, data = MorDF)


	#  шаг-3 попарная кластеризация по двум факторам standard pairwise scatter plots to illustrate the 	clusters compared to the original variables.
		# 3.1.for Mariana Plate // для Марианской плиты + углы желоба
PairM <- MorDF %>%
  as_tibble() %>%
  mutate(cluster = k6MorDF$cluster,
         profile = row.names(MorDF)) %>%
  ggplot(aes(x = plate_maria, y = tg_angle, color = factor(cluster), label = profile)) +
  geom_text()  
		# 3.2.for Philippine Plate // для Филиппинской плиты + углы желоба
PairPh <- MorDF %>%
  as_tibble() %>%
  mutate(cluster = k6MorDF$cluster,
         profile = row.names(MorDF)) %>%
  ggplot(aes(x = plate_phill, y = tg_angle, color = factor(cluster), label = profile)) +
  geom_text()
PairPh  
  		# 3.3.for Pacific Plate //для Тихоокеанской плиты + углы желоба
PairPc<- MorDF %>%
  as_tibble() %>%
  mutate(cluster = k6MorDF$cluster,
         profile = row.names(MorDF)) %>%
  ggplot(aes(x = plate_pacif, y = tg_angle, color = factor(cluster), label = profile)) +
  geom_text()
PairPc   
  		# 3.4.for Caroline Plate //для Тихоокеанской плиты + углы желоба
PairC<- MorDF %>%
  as_tibble() %>%
  mutate(cluster = k6MorDF$cluster,
         profile = row.names(MorDF)) %>%
  ggplot(aes(x = plate_carol, y = tg_angle, color = factor(cluster), label = profile)) +
  geom_text()

	# шаг-4. подписываем каждый график
p1<- PairM + ggtitle("MARIANA Plate; Trench Profiles 1:25; Trench Angles (tg(A/H)") + theme(plot.title = element_text(size = 8), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8), axis.title = element_text(size = 8))
p2<- PairPh + ggtitle("PHILIPPINE Plate; Trench Profiles 1:25; Trench Angles (tg(A/H)") + theme(plot.title = element_text(size = 8), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8), axis.title = element_text(size = 8))
p3<- PairPc + ggtitle("PACIFIC Plate; Trench Profiles 1:25; Trench Angles (tg(A/H)") + theme(plot.title = element_text(size = 8), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8), axis.title = element_text(size = 8))
p4<- PairC + ggtitle("CAROLINE Plate; Trench Profiles 1:25; Trench Angles (tg(A/H)") + theme(plot.title = element_text(size = 8), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8), axis.title = element_text(size = 8))

	# шаг-5. располагаем 4 графика на одной стр.

Pair_figure <-plot_grid(p1, p2, p3, p4, labels = c("1", "2", "3", "4"), ncol = 2, nrow = 2)
	
	# шаг-6. добавляем к ним общий заголовок, подзаголовок и нижнюю сноску.
PairwisePlates <- Pair_figure +								
	labs(title="马里亚纳海沟。剖面1-25。Mariana Trench, Profiles Nr.1-25.", 
	subtitle = "统计图表。地貌聚类分析。Pairwise Standard Scatter Plots of k-means Cluster Correlation",
	caption = "Clusters compared to the original variables \nStatistics Processing and Graphs: R Programming. Data Source: QGIS") +
	theme(
		plot.margin = margin(5, 10, 20, 5),
		plot.title = element_text(margin = margin(t = 0, r = 20, b = 5, l = 0), family = "Kai", face = "bold", size = 12), # китайский шрифт "Кай"
		plot.subtitle = element_text(margin = margin(t = 0, r = 20, b = 4, l = 0), family = "Hei", face = "bold", size = 10), # китайский шрифт "Хэй"
		plot.caption = element_text(face = 2, size = 6),
		panel.background=ggplot2::element_rect(fill = "white"),
		legend.justification = "bottom", 
		legend.position = "bottom",
		legend.box.just = "right",
		legend.direction = "horizontal",
		legend.box = "horizontal",
		legend.box.background = element_rect(colour = "honeydew4",size=0.2),
		legend.background = element_rect(fill = "white"),
		legend.key.width = unit(1,"cm"),
		legend.key.height = unit(.5,"cm"),
		legend.spacing.x = unit(.2,"cm"),
		legend.spacing.y = unit(.1,"cm"),
		legend.text = element_text(colour="black", size=6, face=1),
		legend.title = element_text(colour="black", size=6, face=1))
PairwisePlates
 

fit <- kmeans(MorDF, 5)
library(cluster) 

fit2<- clusplot(MorDF, fit$cluster, color=TRUE, shade=TRUE, 
   labels=2, lines=0)
library(fpc) 
plotcluster(MorDF, fit$cluster) 

set.seed(123)


