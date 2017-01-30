
library(pitchRx)
library(RSQLite)
library(dplyr)
library(data.table)
library(splitstackshape)
library(reshape2)
library(data.table)


masterList <- list()
atbat <- list()
pitch <- list()
action <- list()
runner <- list()
#drop.cols <- c('on_1b','on_2b','on_3b')
pitch.names <- c('type','pitch_type','inning_side','inning','on_1b','on_2b',
                 'on_3b','gameday_link','num','count')
atbat.names <- c('pitcher','batter','gameday_link','num','b','s','o','stand',
                 'p_throws','event','home_team_runs','away_team_runs',
                 'batter_name','pitcher_name')
action.names <- c('event','gameday_link','num')
runner.names <- c('score','event','gameday_link','num')
### If we download too many files, we get a runtime error, so I download only
### one week at a time.
### For each week, I'm rbinding the new tables all in one step
offseason <- FALSE
a <- as.Date('2015-04-05')
i <- 1
while(a < as.Date('2016-11-03')) {
  b <- a + 7
  masterList <- scrape(start = a, end = b)
  if(i==1){
    atbat <- masterList[['atbat']][,c(atbat.names)]
    pitch <- masterList[['pitch']][,pitch.names]
    action <- masterList[['action']][,c(action.names)]
    runner <- masterList[['runner']][,c(runner.names)]
  }else{
    atbat <- rbind(atbat,masterList[['atbat']][,c(atbat.names)])
    pitch <- rbind(pitch,masterList[['pitch']][,pitch.names])
    action <- rbind(action,masterList[['action']][,c(action.names)])
    runner <- rbind(runner,masterList[['runner']][,c(runner.names)])
  }
  a <- a + 7
  i <- i+1
   #let's skip over the offseason dates
     if(a > '2015-11-02' & !offseason){
       a <- as.Date('2016-04-03')
       offseason <- TRUE
     }
}

pitch <- data.table(pitch)
atbat <- data.table(atbat)
action <- data.table(action)
runner <- data.table(runner)
pitch.atbat <- merge(pitch,atbat,by=c('gameday_link','num'))
pitch.atbat <- pitch.atbat[order(gameday_link,num)]
### change the file paths to save to your own folder.
# write.csv(pitch,'C:/baseball/datasets/pitch_2015_2016.csv',row.names=F)
# write.csv(atbat,'C:/baseball/datasets/atbat_2015_2016.csv',row.names=F)
# write.csv(action,'C:/baseball/datasets/action_2015_2016.csv',row.names=F)
# write.csv(runner,'C:/baseball/datasets/runner_2015_2016.csv',row.names=F)
pitch.atbat <- pitch.atbat[,c('gameday_link','num','type','inning_side','inning',
                              'on_2b','on_1b','on_3b','count','pitcher','batter',
                              'b','s','o','stand','p_throws','event',
                              'batter_name','pitcher_name','home_team_runs',
                              'away_team_runs')]


pitch.atbat$o.init <- ifelse(pitch.atbat$event %in% 
                        c('Batter Interference','Bunt Groundout',
                          'Bunt Lineout','Bunt Pop Out','Fan interference',
                          'Fielders Choice Out','Flyout','Forceout','Groundout',
                          'Lineout','Pop Out','Runner Out','Sac Bunt','Sac Fly',
                          'Strikeout'),pitch.atbat$o-1,ifelse(pitch.atbat$event 
                            %in% c('Double Play','Grounded Into DP',
                          'Sac Fly DP','Sacrifice Bunt DP','Strikeout - DP'),
                           pitch.atbat$o-2,
                           ifelse(pitch.atbat$event=='Triple Play',
                           pitch.atbat$o-3,pitch.atbat$o)))

pitch.atbat$on_1b <- ifelse(is.na(pitch.atbat$on_1b),0,1)
pitch.atbat$on_2b <- ifelse(is.na(pitch.atbat$on_2b),0,1)
pitch.atbat$on_3b <- ifelse(is.na(pitch.atbat$on_3b),0,1)

pitch.atbat$lag.home.runs <- lag(pitch.atbat$home_team_runs)
pitch.atbat$lag.away.runs <- lag(pitch.atbat$away_team_runs)
hitter <- pitch.atbat[, .SD[c(1,.N)], by=.(gameday_link,num,batter)]
pitcher <- pitch.atbat[, .SD[c(1,.N)], by=.(gameday_link,inning,pitcher)]

hitter$lag.away.runs.1 <- lag(hitter$lag.away.runs)
hitter$lag.home.runs.1 <- lag(hitter$lag.home.runs)
hitter$on_1b.lead <- lead(hitter$on_1b)
hitter$on_2b.lead <- lead(hitter$on_2b)
hitter$on_3b.lead <- lead(hitter$on_3b)
hitter <- hitter[, .SD[c(2)], by=.(gameday_link,num)]
hitter$start.state <- paste0(hitter$o.init,hitter$on_1b,hitter$on_2b,hitter$on_3b)
hitter$end.state <- paste0(hitter$o,hitter$on_1b.lead,hitter$on_2b.lead,hitter$on_3b.lead)
hitter$runs.on.play <- ifelse(hitter$inning_side=='top',
                              as.numeric(hitter$away_team_runs)-
                                as.numeric(hitter$lag.away.runs.1),
                              as.numeric(hitter$home_team_runs)-
                                as.numeric(hitter$lag.home.runs.1))
hitter$runs.on.play <- ifelse(hitter$num==1,0,hitter$runs.on.play)
hitter$lag.away.runs[1] <- 0
hitter$lag.away.runs.1[1] <- 0
hitter$lag.home.runs[1] <- 0
hitter$lag.home.runs.1[1] <- 0

rc <- data.table(cbind(c('0000','0100','0010','0001','0110','0101','0011','0111',
        '1000','1100','1010','1001','1110','1101','1011','1111',
        '2000','2100','2010','2001','2110','2101','2011','2111',
        '3000','3100','3010','3001','3110','3101','3011','3111'),
        c(0.49,0.85,1.1,1.36,1.41,1.76,1.93,2.19,0.27,0.51,0.66,0.94,0.87,1.12,
          1.35,1.51,0.12,0.23,0.33,0.36,0.43,0.49,0.55,0.73,0,0,0,0,0,0,0,0)))
names(rc) <- c('state','exp.runs')

hitter <- merge(hitter,rc,by.x='start.state',by.y='state',all.x=TRUE)
hitter <- merge(hitter,rc,by.x='end.state',by.y='state',all.x=TRUE)
hitter <- hitter[order(gameday_link,num)]
names(hitter)[names(hitter)=='exp.runs.x'] <- 'start.rc'
names(hitter)[names(hitter)=='exp.runs.y'] <- 'end.rc'

hitter$o.init <- pmax(0,hitter$o.init)
hitter$runs.on.play <- pmax(0,hitter$runs.on.play)## otherwise the first hitter of 
                                              ## a new game would get the runs
                                              ## from the last record of the previous game
hitter$runs.created <- as.numeric(hitter$runs.on.play)+
                      as.numeric(hitter$end.rc)-as.numeric(hitter$start.rc)
hitter$runs.created[nrow(hitter)] <- 0 ## last play of season
hitter$end.rc[nrow(hitter)] <- 0
hitter <- hitter[,':='(rc=sum(runs.created)),by=.(batter_name)]
batters <- unique(hitter,by=c('batter_name'))[,c('batter_name','rc'),with=F]
batters <- batters[order(-rc)]


pitcher$lead.away.runs <- lead(pitcher$away_team_runs)
pitcher$lead.home.runs <- lead(pitcher$home_team_runs)
pitcher$on_1b.lead <- lead(pitcher$on_1b)
pitcher$on_2b.lead <- lead(pitcher$on_2b)
pitcher$on_3b.lead <- lead(pitcher$on_3b)
pitcher$o.end <- lead(pitcher$o)
 
pitcher <- pitcher[, .SD[c(1)], by=.(gameday_link,inning,inning_side,pitcher)]
pitcher$start.state <- paste0(pitcher$o.init,pitcher$on_1b,pitcher$on_2b,pitcher$on_3b)
pitcher$end.state <- paste0(pitcher$o.end,pitcher$on_1b.lead,pitcher$on_2b.lead,pitcher$on_3b.lead)
pitcher$inning.runs <- ifelse(pitcher$inning_side=='top',
                              as.numeric(pitcher$lead.away.runs)-
                                as.numeric(pitcher$away_team_runs),
                              as.numeric(pitcher$lead.home.runs)-
                                as.numeric(pitcher$home_team_runs))

pitcher <- merge(pitcher,rc,by.x='start.state',by.y='state',all.x=TRUE)
pitcher <- merge(pitcher,rc,by.x='end.state',by.y='state',all.x=TRUE)
pitcher <- pitcher[order(gameday_link,num)]
names(pitcher)[names(pitcher)=='exp.runs.x'] <- 'start.rc'
names(pitcher)[names(pitcher)=='exp.runs.y'] <- 'end.rc'

pitcher$o.init <- pmax(0,pitcher$o.init)
pitcher$inning.runs <- pmax(0,pitcher$inning.runs)## otherwise the first pitcher of 
## a new game would get the runs
## from the last record of the previous game
pitcher$runs.created <- as.numeric(pitcher$inning.runs)+
  as.numeric(pitcher$end.rc)-as.numeric(pitcher$start.rc)
pitcher$pitcher.inning.outs <- pitcher$o.end-pitcher$o.init
pitcher <- pitcher[,':='(rc=sum(runs.created),innings=sum(pitcher.inning.outs/3)),by=.(pitcher_name)]
batters <- unique(pitcher,by=c('batter_name','rc'))[,c('batter_name','rc'),with=F]
batters <- batters[order(-rc)]

pitchers <- unique(pitcher,by=c('pitcher_name'))[,c('pitcher_name','rc','innings'),with=F]
pitchers <- pitchers[order(rc)]
