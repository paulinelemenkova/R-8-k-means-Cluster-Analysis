# ЧАСТЬ 1: делаем data.frame с геоморфологией 
	# шаг-1. загружаем таблицу, делаем датафрейм
MorDF <- read.csv("Morphology.csv", header=TRUE, sep = ",")
head(MorDF)
summary(MorDF)

# ЧАСТЬ 2: Clustering
	#  шаг-2.  далем несколько вариантов кластерного анализа с разными значениями k (кол-во центров)
k2MorDF <- kmeans(MorDF, centers = 2, nstart = 25) 
str(k2MorDF)
k2MorDF
fviz_cluster(k2MorDF, data = MorDF)

k3MorDF <- kmeans(MorDF, centers = 3, nstart = 25) 
str(k3MorDF)
k3MorDF
fviz_cluster(k3MorDF, data = MorDF)

k4MorDF <- kmeans(MorDF, centers = 4, nstart = 25) 
str(k4MorDF)
k4MorDF
fviz_cluster(k4MorDF, data = MorDF)

k5MorDF <- kmeans(MorDF, centers = 5, nstart = 25) 
str(k5MorDF)
k5MorDF
fviz_cluster(k5MorDF, data = MorDF)

k6MorDF <- kmeans(MorDF, centers = 6, nstart = 25) 
str(6MorDF)
k6MorDF
fviz_cluster(k6MorDF, data = MorDF)

k7MorDF <- kmeans(MorDF, centers = 7, nstart = 25) 
str(k7MorDF)
k7MorDF
fviz_cluster(k7MorDF, data = MorDF)

	#  шаг-3. сохраняем значения графиков в p1-p7
p2 <- fviz_cluster(k2MorDF, geom = "point",  data = MorDF) + ggtitle("Nr. of centers k = 2") + theme(plot.title = element_text(size = 10), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8))
p3 <- fviz_cluster(k3MorDF, geom = "point",  data = MorDF) + ggtitle("Nr. of centers k = 3") + theme(plot.title = element_text(size = 10), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8)) 
p4 <- fviz_cluster(k4MorDF, geom = "point",  data = MorDF) + ggtitle("Nr. of centers k = 4") + theme(plot.title = element_text(size = 10), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8)) 
p5 <- fviz_cluster(k5MorDF, geom = "point",  data = MorDF) + ggtitle("Nr. of centers k = 5") + theme(plot.title = element_text(size = 10), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8)) 
p6 <- fviz_cluster(k6MorDF, geom = "point",  data = MorDF) + ggtitle("Nr. of centers k = 6") + theme(plot.title = element_text(size = 10), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8))
p7 <- fviz_cluster(k7MorDF, geom = "point",  data = MorDF) + ggtitle("Nr. of centers k = 7") + theme(plot.title = element_text(size = 10), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8))

	# шаг-4. собираем все графики на один лист для сравнения // plots to compare 
figure <-plot_grid(p2, p3, p4, p5, p6, p7, labels = c("1", "2", "3", "4", "5", "6"), ncol = 2, nrow = 3)

	# шаг-5. добавляем к ним общий заголовок, подзаголовок и нижнюю сноску.
ClustersMariana6 <- figure +								
	labs(title="马里亚纳海沟。剖面1-25。Mariana Trench, Profiles Nr.1-25.", 
	subtitle = "统计图表。地貌聚类分析。Geomorphological Cluster Analysis (k-means)",
	caption = "Statistics Processing and Graphs: \nR Programming. Data Source: QGIS") +
	theme(
		plot.margin = margin(5, 10, 20, 5),
		plot.title = element_text(family = "Kai", face = "bold", size = 12), # китайский шрифт "Кай"
		plot.subtitle = element_text(family = "Hei", face = "bold", size = 10), # китайский шрифт "Хэй"
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
ClustersMariana6
