setwd("/home/marcburt/BU DA/Web_Analytics/game_analysis")

library(googleVis)

library(RCurl)
library(RJSONIO)

webpage <- 
  paste0("http://powerful-meadow-8588.herokuapp.com/",
         "data/12months_departures_joiners.json", sep="")

data <- fromJSON(getURL(webpage)) 

##
save(data, file="glitch1.RData")
load(file="glitch1.RData")
##

typeof(data)
names(data)

typeof(data$nodes)
length(data$nodes)

data$nodes[[1]]
data$nodes[1]

nodes.info <- 
  do.call("rbind", 
          lapply(data$nodes, data.frame))

head(nodes.info)

tail(nodes.info)

months <- unique(nodes.info$month)
months

table(nodes.info$month)

levels <- unique(nodes.info$name)
levels

table(nodes.info$name)


# Links 

typeof(data$links)
length(data$links)

data$links[[1]]

links.info <- 
  data.frame(do.call("rbind", data$links))

head(links.info)

tail(links.info)

links.info$SourceName <- 
  paste0(links.info$source, "-",
         nodes.info[links.info$source+1, "name"])

head(links.info)

links.info$TargetName <- 
  paste0(links.info$target, "-",
         nodes.info[links.info$target+1, "name"])

head(links.info)

# Sankey chart data for first two monthly periods

source.data <- 
  nodes.info[nodes.info$month %in% 
               c("Nov-11", "Dec-11", "Jan-12"), 
             "node"]

source.data

links.data <- 
  links.info[links.info$source %in% source.data,
             c("SourceName", "TargetName", "value")]

head(links.data)

tail(links.data)


options <- "{
  node: {
    label: {
      fontName: 'Times-Roman',
      fontSize: 12,
      color: '#000',
      bold: true,
      italic: false
    },
    labelPadding: 6, 
    nodePadding: 30, 
    width: 5         
  },
  link: {
    color: { fill: '#d799ae' } 
  }
}"



chart.data <- 
  gvisSankey(links.data,
             from="SourceName", 
             to="TargetName",
             weight="value",
             options=list(
               width=600, height=500,
               sankey=options
             ))
plot(chart.data)

months.names <- data.frame(
  rbind(c("Nov-11...", 
          "Dec-11...", 
          "Jan-12...")))
         
months.names

names(months.names) <- 
  paste(c("Nov-11", "Dec-11", "Jan-12"),
        c("Dec-11", "Jan-12", "Feb-12"),
        sep="====>")

months.names

chart.names <- gvisTable(months.names, 
                         options=list(
                           width=600))

plot(chart.names)

months.info <- 
  nodes.info[nodes.info$month %in% 
               c("Nov-11", "Dec-11", "Jan-12", "Feb-12"), 
             c("month", "node", "name")]

chart.table <- gvisTable(months.info)

plot(chart.table)

chart1 <- gvisMerge(
  gvisMerge(chart.data, chart.names),
  chart.table, horizontal = TRUE)

plot(chart1)

# Encapsulate the above in the following function

glitch.chart <- function(src_months, tbl_months) {
  # nodes for the src_months
  source.nodes <- 
    nodes.info[nodes.info$month %in% src_months, "node"]
  
  # links data for the source nodes
  df <- links.info[links.info$source %in% source.nodes,
                   c("SourceName", "TargetName", "value")]
  
  chart.sankey <- 
    gvisSankey(df,
               from="SourceName", 
               to="TargetName", 
               weight="value",
               options=list(
                 width=750, height=500,
                 sankey=options
               ))
  
  months.names <- data.frame(rbind(
        paste0(as.character(src_months), "...", sep="")))
  
  names(months.names) <- paste(src_months, tbl_months[-1], 
                               sep="====>")
  
  chart.names <- gvisTable(months.names, 
                           options=list(
                             width=750))
    
  months.info <- 
    nodes.info[nodes.info$month %in% tbl_months, 
               c("month", "node", "name")]
  
  chart.table <- gvisTable(months.info)
                           
  return (gvisMerge(
            gvisMerge(chart.sankey, chart.names),
                    chart.table, horizontal = TRUE))
}

chart1 <- glitch.chart(months[1:3], months[1:4])
chart2 <- glitch.chart(months[4:6], months[4:7])
chart3 <- glitch.chart(months[7:10], months[7:11])
chart4 <- glitch.chart(months[11:13], months[11:14])

plot(chart1)
plot(chart2)
plot(chart3)
plot(chart4)

chart.final <- gvisMerge(
  gvisMerge(chart1, chart2),
  gvisMerge(chart3, chart4))

plot(chart.final)

print(chart1, file = "chart1.html")
print(chart2, file = "chart2.html")
print(chart3, file = "chart3.html")
print(chart4, file = "chart4.html")
print(chart.final, file="chart_final.html")


#####

hardcore <- 
  nodes.info[nodes.info$name == 'Hardcore', ]

hardcore

chart5 <- gvisColumnChart(hardcore,
                       xvar="month", 
                       yvar=c("joining","departing"),
                       options=list(
                         legend="top",
                         title="Hardcore Players"))
plot(chart5)

print(chart5, file = "chart5.html")

# order levels from game website

order.levels <- c("Hardcore", 
                  "Forum", 
                  "Moderate", "Moderate Winners",
                  "Moderate Losers", "Moderate Farmers", 
                  "Moderate Miscellanea",
                  "Casual", "Casual Winners", 
                  "Casual Losers", "Casual Forum")
order.levels

months <- as.character(months)
months

totals <- matrix(NA, 
                 nrow=length(months), 
                 ncol=length(order.levels))

colnames(totals) <- 
  order.levels[length(order.levels):1]
rownames(totals) <- months

totals[1:5,c(1:2, 10:11)]

# nodes for first month

m <- months[1]
m

m.nodes <- nodes.info[nodes.info$month == m, "node"]
m.nodes

# all the source links for first month

m.links <- links.info[
             links.info$source %in% m.nodes, 
             c("source", "value")]

head(m.links, n = 3)
tail(m.links, n = 3)

df <- aggregate(m.links$value, 
                by=list(m.links$source), 
                FUN = sum)
df

names(df) <- c("node", "Total")
df

df$name <- 
  nodes.info[df$node+1, "name"]
df
  
print(sum(df$Total))

totals[m,as.character(df$name)] <- df$Total

print(cbind(totals[m,]))



# For the middle months

for (m in months[2:(length(months)-1)])
{
  print(m)
  m.nodes <- nodes.info[nodes.info$month == m, "node"]
  m.nodes
  
  m.targetlinks <- 
    links.info[links.info$target %in% m.nodes, 
               c("target", "value")]
  head(m.targetlinks, n = 3)
  
  m.sourcelinks <- 
    links.info[links.info$source %in% m.nodes, 
               c("source", "value")]
  head(m.sourcelinks, n = 3)
  
  df.targetlinks <- 
    aggregate(m.targetlinks$value, 
              by=list(m.targetlinks$target), 
              FUN = sum)
  
  names(df.targetlinks) <- 
    c("node", "TargetTotal")
  
  df.targetlinks
  
  df.sourcelinks <- 
    aggregate(m.sourcelinks$value,
              by=list(m.sourcelinks$source), 
              FUN = sum)
  
  names(df.sourcelinks) <- 
    c("node", "SourceTotal")
  
  df.sourcelinks
  
  df <- merge(df.targetlinks, 
              df.sourcelinks, 
              by = "node")
  df
  
  m.info <- nodes.info[nodes.info$month == m,]
  m.info
  
  df$TargetTotal - m.info$departing + m.info$joining
  
  df$SourceTotal
  
  df$Total <- apply(df[-1], 1, max)
  df
  
  df$name <- nodes.info[df$node+1, "name"]
  df
  
  print(sum(df$Total))
  
  totals[m,as.character(df$name)] <- df$Total

  print(cbind(totals[m,]))
}

# For the last month
m <- months[length(months)]
{
  print(m)
  
  m.nodes <- nodes.info[nodes.info$month == m, "node"]
  m.nodes
  
  m.links <- links.info[
               links.info$target %in% m.nodes, 
               c("target", "value")]
  
  head(m.links, n = 3)
  
  df <- aggregate(m.links$value, 
                  by=list(m.links$target), 
                  FUN = sum)
  names(df) <- c("node", "Total")
  df
  
  df$name <- nodes.info[df$node+1, "name"]
  df
  
  print(sum(df$Total))
  
  totals[m,as.character(df$name)] <- df$Total
  
  print(cbind(totals[m,]))
}

# Plot

df <- data.frame(Month = months, totals)

df[1:5, c(2:3, 11:12)]


nodes.colors <- 
  data.frame(node=levels(nodes.info$name), 
             color=levels(nodes.info$color))

rownames(nodes.colors) <- nodes.colors$node

nodes.colors

color <- nodes.colors[
  order.levels[length(order.levels):1], 
  "color"]

color <- as.character(color)
color

color <- paste0('"', color, '"')
color <- paste0(color, collapse=",")
color <- paste0('[', color, ']')

color

chart6 <- 
  gvisColumnChart(
    df,
    options=list(
      height=750, width=850,
      isStacked = TRUE,
      colors=color))
plot(chart6)

print(chart6, file = "chart6.html")







  