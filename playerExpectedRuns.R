library(dplyr)
library(data.table)
library(stringr)

pitch.atbat <- fread('C:/baseball/data sets/expected.runs.data.csv')
ex.runs.matrix <- fread('C:/baseball/data sets/expected.runs.csv')
ex.runs.matrix <- rbind(ex.runs.matrix,t(c(state='3000',exp.runs='0')))
pitch.atbat <- merge(pitch.atbat,ex.runs.matrix,by.x='start.state',by.y='state')
names(pitch.atbat)[names(pitch.atbat)=='exp.runs'] <- 'start.exp.runs'
pitch.atbat <- merge(pitch.atbat,ex.runs.matrix,by.x='end.state',by.y='state')
names(pitch.atbat)[names(pitch.atbat)=='exp.runs'] <- 'end.exp.runs'
pitch.atbat <- pitch.atbat[order(gameday_link,num)]

pitch.atbat$end.exp.runs <- as.numeric(pitch.atbat$end.exp.runs)
pitch.atbat$start.exp.runs <- as.numeric(pitch.atbat$start.exp.runs)
batter.runs <- pitch.atbat[,.(rc=sum(end.exp.runs-start.exp.runs+runs.on.play),(pa=.N)),by=.(batter_name,start.state)]
# pitcher <- pitch.atbat[, .SD[c(1,.N)], by=.(pitcher_name,gameday_link,inning)]
# pitcher.runs <- pitcher[,.(rc=sum(end.exp.runs[.N]-start.exp.runs[1]+runs.on.play)),by=.(batter_name,start.state)]

batter.runs <- batter.runs[,':='(trc=sum(rc),tv2=sum(V2)),by=.(batter_name)]
stats <- batter.runs[,c('batter_name','trc'),with=F]
stats <- stats[!duplicated(stats)]
stats <- stats[order(-trc)]
head(stats,25)
