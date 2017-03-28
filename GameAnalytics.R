library(googleVis)

setwd("/home/marcburt/BU DA/Web_Analytics/game_analysis")

load(file = "glitch1.RData")
head(data)

nodes.info <- do.call("rbind", lapply(data$nodes, data.frame))

attach(nodes.info)
names(nodes.info)
joining.df <- aggregate(joining, by = list(month), "sum")
departing.df <- aggregate(departing, by = list(month), "sum")
joining.names <- c("Months", "Joining")
departing.names <- c("Months", "Departing")
names(joining.df) <- joining.names
names(departing.df) <- departing.names

join.depart.df <- merge(joining.df, departing.df, sort = FALSE)
join.depart.df

plot1 <- gvisLineChart(join.depart.df)
plot2 <- gvisColumnChart(join.depart.df)
plot.merged <- gvisMerge(plot1, plot2)
plot(plot.merged)
cat(plot.merged$html$chart, file = 'ChartMerged.html')

gauge.chart <- gvisGauge(departing.df, 
						options = list(min = 0, max = 4030))

cat(gauge.chart$html$chart, file = 'gaugechart.html')

gauge.chart.m <- gvisGauge(departing.df, 
						options = list(min = 0, max = 4030, 
						redFrom = 2000, redTo = 4030,
						yellowFrom = 1000, yellowTo = 2000,
						greenFrom = 0, greenTo = 1000))
cat(gauge.chart.m$html$chart, file = 'gaugechartm.html')

plot(gauge.chart)
plot(gauge.chart.m)


#Part 2 NBA Stats

library(SportsAnalytics)

nba.data <- fetch_NBAPlayerStatistics("13-14")
head(nba.data)
names(nba.data)
attach(nba.data)

nba.data$FieldGoalPercentage <- FieldGoalsMade/FieldGoalsAttempted
nba.data$FieldGoalPercentage[!is.finite(nba.data$FieldGoalPercentage)] <- 0
best.field <- max(nba.data$FieldGoalPercentage, na.rm = TRUE)
indx <- nba.data$FieldGoalPercentage == best.field
best.field <- nba.data[indx,]
best.field$Name

nba.data$FreeThrowPercentage <- FreeThrowsMade/FreeThrowsAttempted
nba.data$FreeThrowPercentage[!is.finite(nba.data$FreeThrowPercentage)] <- 0
best.free <- max(nba.data$FreeThrowPercentage, na.rm = TRUE)
indx.free <- nba.data$FreeThrowPercentage == best.free
best.free <- nba.data[indx.free,]
best.free$Name

nba.data$ThreePercentage <- ThreesMade/ThreesAttempted
nba.data$ThreePercentage[!is.finite(nba.data$ThreePercentage)] <- 0
best.three <- max(nba.data$ThreePercentage, na.rm = TRUE)
indx.three <- nba.data$ThreePercentage == best.three
best.three <- nba.data[indx.three,]
best.three$Name

nba.data$TotalPoints = 2*(FieldGoalsMade - ThreesMade) + FreeThrowsMade + 3*ThreesMade
top.points <- nba.data[order(TotalPoints, decreasing = TRUE),]
top.points
top.ten.points<-head(top.points$Name, 10)
top.ten.points


names(top.points)
top.points[1:5,]
chart1 <- 
	gvisColumnChart(top.points[1:5,],
					xvar = "Name",
					yvar = "TotalPoints",
					options = list(
						legend = "top"
						))


top20.points <- top.points[1:20, ]
names(top20.points)
top20.points <- top20.points[c('Name', 'Team', 'Position', 'TotalPoints',  'ThreesMade', 'FieldGoalsMade')]
top20.points$ThreesPoints <- top20.points$ThreesMade*3
top20.points$PointsWOThrees <- top20.points$TotalPoints - top20.points$ThreesPoints
top.teams.points <- aggregate(TotalPoints ~ Team, data = nba.data, "sum")

chart2 <- gvisTable(top20.points)

chart3 <- gvisColumnChart(top20.points[1:10,],
							xvar = 'Name',
							yvar = c('ThreesPoints', 'TotalPoints'))

chart4 <- gvisBarChart(top.teams.points)

chart5 <- gvisColumnChart(top20.points[1:10,],
							xvar = 'Name',
							yvar = c('PointsWOThrees', 'ThreesMade'),
								options = list(isStacked = TRUE))

merged.1 <- gvisMerge(chart1, chart3, horizontal = TRUE)
merged.2 <- gvisMerge(chart4, chart5, horizontal = TRUE)
merged.total <- gvisMerge(merged.1, merged.2)

plot(merged.total)

cat(merged.total$html$chart, file = 'mergedtotal.html')


#Part 3

library(XML)
library(stringr)


url <- "http://www.landofbasketball.com/championships/year_by_year.htm"
champ.data <- readHTMLTable(url, which = 1, stringsAsFactors = FALSE)
champ.data["Year\n      Champion"]
names(champ.data) <- c("Input")
champ.data[1, "Input"]


winning.teams <- c()
MVPnames <- c()
sweep.count <- 0
full.count <- 0
for(i in 1:length(champ.data[,1])){
	vector <- unlist(strsplit(champ.data[i,"Input"], '\\s+{2}'))
	if (vector[3] == "4-0"){
		sweep.count <- sweep.count + 1
	}else if(vector [3] == "4-3"){
		full.count <- full.count + 1
	}

	winning.teams <- c(winning.teams, vector[2])
	MVPnames <- c(MVPnames, vector[6])
}
sweep.count
full.count
winning.df <- as.data.frame(table(winning.teams))
winning.df <- winning.df[order(-winning.df$Freq), ]
head(winning.df$winning.teams, 5)

MVPnames <- as.data.frame(table(MVPnames[1:48]))
names <- MVPnames[which(MVPnames$Freq >1),]
names$Var1