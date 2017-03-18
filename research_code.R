require(XLConnect)

####################################################################
####################################################################
##################       Election Results      #####################
####################################################################
####################################################################


r14 <- read.csv("results14.csv")
r12 <- read.csv("results12.csv")
r10 <- read.csv("results10.csv")
r08 <- read.csv("results08.csv")
r06 <- read.csv("results06.csv")
r04 <- read.csv("results04.csv")

###################
###################
## Data Cleaning ##
###################
###################


## Drop vars r14 ##
drops <- c("X1","PRIMARY.VOTES","PRIMARY..","RUNOFF.VOTES","RUNOFF..","COMBINED.GE.PARTY.TOTALS..CT..NY..SC.","COMBINED....CT..NY..SC.","FOOTNOTES","X","X.1","GE.RUNOFF.ELECTION....LA.")
r14 <- r14[ , !(names(r14) %in% drops)]


## Drop vars r12 ##
drops <- c("X1","PRIMARY.VOTES","PRIMARY..","RUNOFF.VOTES","RUNOFF..","COMBINED.GE.PARTY.TOTALS..CT..NY..SC.","COMBINED....CT..NY..SC.","FOOTNOTES","X","X.1","GE.RUNOFF.ELECTION....LA.")
r12 <- r12[ , !(names(r12) %in% drops)]


## Drop vars r10 ##
drops1 <- c("X1","PRIMARY","PRIMARY..","RUNOFF","RUNOFF..","COMBINED.GE.PARTY.TOTALS..CT..NY..SC.","COMBINED....CT..NY..SC.","FOOTNOTES","X","X.1","X.1","X.2","GE.RUNOFF.ELECTION....LA.")
r10 <- r10[ , !(names(r10) %in% drops1)]


## Drop vars r08 ##
drops2 <- c("X1","PRIMARY","PRIMARY..","RUNOFF","RUNOFF..","COMBINED.GE.PARTY.TOTALS..CT..NY.","COMBINED....CT..NY.","FOOTNOTES","X","X.1","X.1","X.2","GE.RUNOFF..")
r08 <- r08[ , !(names(r08) %in% drops2)]


## Drop vars r06 ##
drops3 <- c("X.","PRIMARY","PRIMARY..","RUNOFF","RUNOFF..","COMBINED.GE.PARTY.TOTALS..NY..SC.","COMBINED....NY..SC.","Notes..See.Endnotes.Tab.","X","X.1","X.1","X.2","GE.RUNOFF..")
r06 <- r06[ , !(names(r06) %in% drops3)]


## Drop vars r04 ##
r04 <- r04[,1:18]
r04 <- r04[ , !(names(r04) %in% drops3)]


###################
###################
#### New Vars  ####
###################
###################

## r14 ##
r14$gewin <- ifelse(r14$GE.WINNER.INDICATOR!="",1,0)
r14$GE.WINNER.INDICATOR <- NULL
r14$year <- 2014

## r12 ##
r12$gewin <- ifelse(r12$GE.WINNER.INDICATOR!="",1,0)
r12$GE.WINNER.INDICATOR <- NULL
r12$year <- 2012

## r10 ##
r10$rovotes <- ""
r10$rovotes <-as.factor(r10$rovotes)
r10$gewin <- 0
r10$year <- 2010

## r08 ##
r08$gewin <- 0
r08$year <- 2008

## r06 ##
r06$gewin <- 0
r06$year <- 2006

## r04 ##
r04$gewin <- 0
r04$year <- 2004

## r14 Var names ##
names14 <- c("abbr","state","distr","id","incr","first","last","namer", "totvote","party","gevotes","geperr","rovotes","gewinr","year")
colnames(r14) <- names14

## r12 Var names ##
colnames(r12) <- names14

## r10 Var names ##
names10 <- c("state","abbr","distr","id","incr","first","last","namer","totvote","party","gevotes","geperr","rovotes","gewinr","year")
colnames(r10) <- names10

## r08 Var names ##
names08 <- c("state","abbr","distr","id","incr","first","last","namer","totvote","party","gevotes","geperr","rovotes","gewinr","year")
colnames(r08) <- names08

## r06 Var names ##
colnames(r06) <- names08

## r04 Var names ##
colnames(r04) <- names08


###################
###################
#### Appending ####
###################
###################


results <- rbind(r14, r12, r10, r08, r06, r04)
results$distr[results$distr==" 05"] <- "05"
results$dist1 <- substr(results$distr, 0, 2)
drops4 <- c("S","S ", "SF", "SU")
results <- results[!(results$dist1 %in% drops4),]

dvote <- subset(results, totvote=="District Votes:")
dvote <- subset(dvote, select = c("abbr","distr","gevotes","year"))
colnames(dvote)[3] <- "dvote"
results <- merge(results, dvote, by = c("abbr","distr","year"))

results <- subset(results, id != "")
results <- subset(results, id != "n/a")
results <- subset(results,gevotes != "Unopposed")
results <- subset(results, geperr != "n/a")
results <- subset(results, rovotes == "")



results$distr <- gsub("\\s+","",results$distr)
results$distr <- gsub("*","",results$distr)
results <- results[which(results$distr==results$dist1),]
results$dist1 <- NULL
results$totvote <- NULL

results$rovotes <- gsub("\\s+","",results$rovotes)
results <- subset(results, rovotes =="")

results$geperr <- gsub("%","",results$geperr)

results <- results[!(results$id==""),]
results <- subset(results, gevotes!="#")

results$rep <- ifelse(results$party %in% c("R","REP"), 1, 0)
results$indp <- ifelse(results$party!="R",ifelse(results$party!="D", 1,0),0)
results$party <- NULL
results$first <- NULL
results$last <- NULL

####################################################################
####################################################################
##################       Campaign Finance      #####################
####################################################################
####################################################################


cs14 <- read.csv("cs14.csv")
cs12 <- read.csv("cs12.csv")
cs10 <- read.csv("cs10.csv")
cs08 <- read.csv("cs08.csv")
cs06 <- read.csv("cs06.csv")
cs04 <- read.csv("cs04.csv")

###################
###################
#### Var Edits ####
###################
###################

cs14$year <- 2014
cs12$year <- 2012
cs10$year <- 2010
cs08$year <- 2008
cs06$year <- 2006
cs04$year <- 2004

cs04 <- subset(cs04, runoff=="")
cs04$runoff <- NA
cs06$runoff <- NA

cs <- rbind(cs14, cs12, cs10, cs08, cs06, cs04)
cs$gewin[cs$gewin=="l"] <- "L"

cs <- subset(cs, is.na(runoff))
cs$runoff <- NULL
cs$race <- substr(cs$id,0,1)
cs <- subset(cs, cs$race =="H")
cs$race <- NULL

## Merge ##
test <- merge(cs,results, by = c("id","year","abbr"))

## Dummies ##
test$incm <- ifelse(test$inc=="I",1,0)
test$open <- ifelse(test$inc=="O",1,0)
test$rep <- ifelse(test$pcode == 2,1,0)
test$indp <- ifelse(test$pcode == 3,1,0)


###################
###################
## String Clean ###
###################
###################

test$dvote <- gsub("\\D+","",test$dvote)
test$dvote <- as.numeric(test$dvote)

test$geperr <- gsub("%","",test$geperr)
test$geperr <- as.numeric(test$geperr)
test$gevotes <- gsub("\\D+","",test$gevotes)
test$gevotes <- as.numeric(test$gevotes)
test$geperr <- ifelse(is.na(test$geperr), test$gevotes/test$dvote, test$geperr)

drops <- c("last..first","inc","pcode","party.x","spec","gewin","geper","distr","incr","first","last","party.y","rovotes")
test <- test[,!(names(test) %in% drops)]

dat <- subset(test, complete.cases(geperr)) ## 5309 obs
lista <- c("AS","DC","GU","MP","VI","PR")
dat <- dat[!(dat$abbr %in% lista),]

dat$state <- gsub("\\s","",dat$state)

dat$namer <- gsub("\\s{2}","\\s",dat$namer)
# dat$namer <- gsub("^(\\S+\\s\\S+)\\s.*$","\\1",dat$namer)

spend <- aggregate(dat$otherpolcom, by=list(dat$abbr,dat$dist,dat$year), FUN=sum, na.rm=TRUE)


####################################################################
####################################################################
##################       Data & Ideology       #####################
####################################################################
####################################################################

require(plyr)
require(dplyr)
require(data.table)
require(zoo)

ideol <- read.csv("ideol.csv")
ideolr <-read.csv("ideol redistrict.csv")
fips <- read.csv("fips.csv")

ideolr$red <- 1
ideol$red <- 0
ideol <- rbind(ideol,ideolr)

dat <- merge(dat, fips, by=c("abbr"))
dat$dist <- ifelse(dat$dist==0 & dat$year<2014,1,dat$dist)
dat$fips <- 100*dat$fips
dat$fips <- dat$fips + dat$dist
dat$red <- ifelse(dat$year==2014,1,0)

dat <- merge(dat,ideol, by=c("abbr","fips","red"))




####################################################################
####################################################################
##################       Data & Super Pac      #####################
####################################################################
####################################################################

ie10 <- read.csv("labelled-expenditures-2010.csv")
ie12 <- read.csv("labelled-expenditures-2012.csv")
ie14 <- read.csv("labelled-expenditures-2014.csv")

ie10 <- subset(ie10, can_off=="H" & ele_typ=="G")
ie12 <- subset(ie12, can_off=="H" & ele_typ=="G")
ie14 <- subset(ie14, can_off=="H" & ele_typ=="G")

ie10$year <- 2010
ie12$year <- 2012
ie14$year <- 2014

ie <- rbind(ie10,ie12,ie14)

dat <- dat[!(dat$gevotes==5 & dat$id=="H4RI01034"),]
dat <- dat[!(dat$gevotes==5 & dat$id=="H0RI01073"),]

dat2 <- dat %>%
    group_by(id,year,abbr,fips,red,totrec,totdis,candcont,indcont,dist,otherpolcom,partycont,state,dvote,open,rep,indp) %>%
    summarise(
        namer = first(namer),
        gevotes = sum(gevotes),
        geperr = sum(geperr),
        incm = max(incm)
    )
dat3 <- dat2 %>%
    group_by(id,year) %>%
    filter(n()>1)
    
    
sp <- subset(ie, super_pac==1)
