#===============================================================================
#Implementing Mixture Model; finding probability distributions for key variables
#===============================================================================

#We want to subset yards gained given ... 
#Running plays 

#Import packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tukeyGH)
library(fitdistrplus)

#Estimate this data using 2019 data 
Year2019 = read.csv("/Users/samliss/Desktop/Sports Analytics/2021 UVA Football Data-selected/2019 PFF All Plays.csv")

table(Year2019$pff_DEFPERSONNEL)

#========================================================================================================

#Let's start with subsetting running plays 
RunPlays = Year2019%>%
  filter(pff_RUNPASS == "R" | pff_QBSCRAMBLE == 'L' | pff_QBSCRAMBLE == 'C' | pff_QBSCRAMBLE == 'R')

#Create histogram of run plays
hist(RunPlays$pff_GAINLOSS, probability = TRUE)
#Apply density curve to histogram of run plays
lines(density(RunPlays$pff_GAINLOSS, na.rm = TRUE), col = "blue")   

#Find a distribution to model density curve from above
#Use TukeyGH, source: https://cran.r-project.org/web/packages/tukeyGH/tukeyGH.pdf
runs = RunPlays$pff_GAINLOSS[!is.na(RunPlays$pff_GAINLOSS)]
model_runs = fitG(runs, verbose = "v")
x = seq(min(runs),max(runs),length = 1000)
y = dg(x, a =  3.0000, b = 4.6280, g = 0.1912)
#Plot original density curve and model density curve to compare 
plot(x,y,type="l",lwd=2,col="red")
lines(density(RunPlays$pff_GAINLOSS, na.rm = TRUE), col = "blue")   

#========================================================================================================

#Next lets subset sacks 
SackPlays = Year2019%>%
  filter(pff_SACK != "")
#Since the distirbution has some plays where sacks lead to positive gain loss we must delete these entries 
sacks = SackPlays$pff_GAINLOSS[which(SackPlays$pff_GAINLOSS <= 0)]

#Initial exploratory analysis 
hist(SackPlays$pff_GAINLOSS)
plot(density(SackPlays$pff_GAINLOSS), lwd = 2, col = "blue")

#Transform sack plays to make all entries positive with no zeros 
sacks_trans = (sacks*-1)+1

#Picking distributions, source: https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf
#Exploratory analysis 
plotdist(sacks_trans, histo = TRUE, demp = TRUE)
descdist(sacks_trans, boot = 1000)
fw <- fitdist(sacks_trans, "weibull")
fg <- fitdist(sacks_trans, "gamma")
fln <- fitdist(sacks_trans, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)

#conclusion: weibull offers the best fit for transformed sacks distribution 
fw
x = seq(min(sacks_trans),max(sacks_trans),length = 1000)
curve(dweibull(x, shape=2.115261, scale = 7.950869), from=min(x), to=max(x))
lines(density(sacks_trans), lwd = 2, col = "blue")

#========================================================================================================

#Next let's look at all pass plays
#First step: remove all empty strings and instances where pass result is interception, run
PassPlays = Year2019%>%
  filter(pff_PASSRESULT != "")%>%
  filter(pff_PASSRESULT != "INTERCEPTION")%>%
  filter(pff_PASSRESULT != "RUN") %>%
  filter(pff_PASSRESULT != "SACK")

#Second step: new column with complete vs incomplete
PassPlays = PassPlays %>%
  mutate(across('pff_PASSRESULT', str_replace, 'HIT AS THREW|SPIKE|THROWN AWAY|BATTED PASS', 'INCOMPLETE')) %>%
  mutate(across('pff_PASSRESULT', str_replace, 'LATERAL', 'COMPLETE'))
table(PassPlays$pff_PASSRESULT)

ggplot(PassPlays, aes(x=pff_PASSRESULT)) +
  geom_bar()

#Code Pass result column as 1(complete) and 0(incomplete)
PassPlays = PassPlays%>%
  mutate(PassPlays, binary_PR = ifelse(pff_PASSRESULT == 'COMPLETE', 1, 0))

Prob_complete = sum(PassPlays$binary_PR)/nrow(PassPlays)
Prob_incomplete = 1 - sum(PassPlays$binary_PR)/nrow(PassPlays)


#========================================================================================================

#New dataframe with only complete passes
PassPlays.c = PassPlays%>%
  filter(binary_PR == 1)

#Add new column with short, medium, long passes 
PassPlays.c$pass_length <- cut(PassPlays.c$pff_PASSDEPTH, breaks=c(-Inf, 10, 20, Inf), labels=paste(c("Short","Medium","Long"), sep=""))
str(PassPlays.c$pass_length)

#Convert all NAs in pff_GAINLOSS to 0
PassPlays.c$pff_GAINLOSS <- replace(PassPlays.c$pff_GAINLOSS, is.na(PassPlays.c$pff_GAINLOSS), 0)

#========================================================================================================

#Fifth step: Now make new tables for pass plays
short_passPlays = PassPlays.c%>%
  filter(PassPlays.c$pass_length == 'Short')
hist(short_passPlays$pff_GAINLOSS)
lines(density(short_passPlays$pff_GAINLOSS, na.rm = TRUE), col = "blue")  

#Drop all NA values from short passes 
short_passes = short_passPlays$pff_GAINLOSS[!is.na(short_passPlays$pff_GAINLOSS)]
hist(short_passes)

#Transform short passes so all entries are positive
#Transform by +25
min(short_passes)
sp_trans = short_passes + 25


#Implement fit distribution process
plotdist(sp_trans, histo = TRUE, demp = TRUE)
descdist(sp_trans, boot = 1000)
fw <- fitdist(sp_trans, "weibull")
fg <- fitdist(sp_trans, "gamma")
fln <- fitdist(sp_trans, "lnorm")
fn <- fitdist(sp_trans, "norm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma", "normal")
denscomp(list(fw, fln, fg, fn), legendtext = plot.legend)


#In conclusion we are going to fit the gamma distribution to short passes
fg
x = seq(min(sp_trans),max(sp_trans),length = 1000)
curve(dgamma(x, shape=23.1653280, rate = 0.7019524), from=min(x), to=max(x))
lines(density(sp_trans), lwd = 2, col = "blue")

#========================================================================================================

medium_passPlays = PassPlays.c%>%
  filter(PassPlays.c$pass_length == 'Medium')
hist(medium_passPlays$pff_GAINLOSS)

medium_passes = medium_passPlays$pff_GAINLOSS

#Fit tukey gh to medium passes
model_mp = fitG(medium_passes, verbose = "v")
model_mp
x = seq(min(medium_passes),max(medium_passes),length = 1000)
y = dg(x, a = 16.000000, b = 6.443094, g = 0.325172 )

#Plot original density curve and model density curve to compare 
plot(x,y,type="l",lwd=2,col="red")
lines(density(medium_passPlays$pff_GAINLOSS, na.rm = TRUE), col = "blue")   

#========================================================================================================

long_passPlays = PassPlays.c%>%
  filter(PassPlays.c$pass_length == 'Long')
hist(long_passPlays$pff_GAINLOSS)

long_passes = long_passPlays$pff_GAINLOSS

#Fit tukey gh to long passes
model_lp = fitG(long_passes, verbose = "v")
model_lp
x = seq(min(long_passes),max(long_passes),length = 1000)
y = dg(x, a = 33.000000, b = 12.1665961, g = 0.4823053)

#Plot original density curve and model density curve to compare 
plot(x,y,type="l",lwd=2,col="red")
lines(density(long_passPlays$pff_GAINLOSS, na.rm = TRUE), col = "blue")   

#========================================================================================================
#Lets look at punts
Punt_Plays = Year2019%>%
  filter(pff_SPECIALTEAMSTYPE == 'PUNT')
Punt_Plays = Punt_Plays[!grepl("FAKE", Punt_Plays$pff_KICKRESULT),]
Punt_Plays = Punt_Plays %>%
  mutate(across('pff_KICKRESULT', str_replace, 'FAIR CATCH|DOWNED|OUT OF BOUNDS|RETURNED|OUT', 'KICKED'))
  #mutate(across('pff_KICKRESULT', str_replace, 'FAIR CATCH|DOWNED', 'FIXED'))%>%
  #mutate(across('pff_KICKRESULT',str_replace, 'OUT OF BOUNDS', 'OUT'))
Punt_Plays = Punt_Plays%>%
  mutate(Punt_Plays, endfield = ifelse(pff_PLAYENDFIELDPOSITION >= 0, 100-pff_PLAYENDFIELDPOSITION, -1*pff_PLAYENDFIELDPOSITION))%>%
  mutate(Punt_Plays, fieldposition = ifelse(pff_FIELDPOSITION >= 0, 100-pff_FIELDPOSITION, -1*pff_FIELDPOSITION))

#Field position between 0 and 15 yards
table(Punt_Plays$pff_KICKRESULT[which(Punt_Plays$fieldposition > 0 & Punt_Plays$fieldposition <= 15)])
outcome_0and15 = c(rep('BLOCKED',3),rep('KICKED',820),rep('TOUCHBACK',0))
punt0and15 = Punt_Plays%>%
  filter(fieldposition > 0 & fieldposition <= 15 & pff_KICKRESULT == 'KICKED')
plotdist(punt0and15$endfield, histo = TRUE, demp = TRUE)
descdist(punt0and15$endfield, boot = 1000)
fw <- fitdist(punt0and15$endfield, "weibull")
fg <- fitdist(punt0and15$endfield, "gamma")
fln <- fitdist(punt0and15$endfield, "lnorm")
fn <- fitdist(punt0and15$endfield, "norm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma", "normal")
denscomp(list(fw, fln, fg, fn), legendtext = plot.legend)
fn
x = seq(min(punt0and15$endfield),max(punt0and15$endfield),length = 1000)
min(punt0and15$endfield)
max(punt0and15$endfield)
curve(dnorm(x, mean =  49.47805, sd = 14.48829), from=min(x), to=max(x))
lines(density(punt0and15$endfield), lwd = 2, col = "blue")

#Field position between 15 and 30 yards
table(Punt_Plays$pff_KICKRESULT[which(Punt_Plays$fieldposition > 15 & Punt_Plays$fieldposition <= 30)])
outcome_15and30 = c(rep('BLOCKED',35),rep('KICKED',2912),rep('TOUCHBACK',19))
punt15and30 = Punt_Plays%>%
  filter(fieldposition > 15 & fieldposition <= 30 & pff_KICKRESULT == 'KICKED')
plotdist(punt15and30$endfield, histo = TRUE, demp = TRUE)
descdist(punt15and30$endfield, boot = 1000)
fw <- fitdist(punt15and30$endfield, "weibull")
fg <- fitdist(punt15and30$endfield, "gamma")
fln <- fitdist(punt15and30$endfield, "lnorm")
fn <- fitdist(punt15and30$endfield, "norm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma", "normal")
denscomp(list(fw, fln, fg, fn), legendtext = plot.legend)
fn
x = seq(min(punt15and30$endfield),max(punt15and30$endfield),length = 1000)
min(punt15and30$endfield)
max(punt15and30$endfield)
curve(dnorm(x, mean =  64.75309, sd = 13.21342), from=min(x), to=max(x))
lines(density(punt15and30$endfield), lwd = 2, col = "blue")

#Field position between 30 and 45 yards
table(Punt_Plays$pff_KICKRESULT[which(Punt_Plays$fieldposition > 30 & Punt_Plays$fieldposition <= 45)])
outcome_0and15 = c(rep('BLOCKED',18),rep('KICKED',2955),rep('TOUCHBACK',148))
punt30and45 = Punt_Plays%>%
  filter(fieldposition > 30 & fieldposition <= 45 & pff_KICKRESULT == 'KICKED')
plotdist(punt30and45$endfield, histo = TRUE, demp = TRUE)
descdist(punt30and45$endfield, boot = 1000)
fw <- fitdist(punt30and45$endfield, "weibull")
fg <- fitdist(punt30and45$endfield, "gamma")
fln <- fitdist(punt30and45$endfield, "lnorm")
fn <- fitdist(punt30and45$endfield, "norm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma", "normal")
denscomp(list(fw, fln, fg, fn), legendtext = plot.legend)
fn
x = seq(min(punt30and45$endfield),max(punt30and45$endfield),length = 1000)
min(punt30and45$endfield)
max(punt30and45$endfield)
curve(dnorm(x, mean =  77.27817, sd = 11.67367), from=min(x), to=max(x))
lines(density(punt30and45$endfield), lwd = 2, col = "blue")

#Field position between 45 and 60 yards
table(Punt_Plays$pff_KICKRESULT[which(Punt_Plays$fieldposition > 45 & Punt_Plays$fieldposition <= 60)])
outcome_0and15 = c(rep('BLOCKED',7),rep('KICKED',1537),rep('TOUCHBACK',329))
punt45and60 = Punt_Plays%>%
  filter(fieldposition > 45 & fieldposition <= 60 & pff_KICKRESULT == 'KICKED')
plotdist(punt45and60$endfield, histo = TRUE, demp = TRUE)
descdist(punt45and60$endfield, boot = 1000)
fw <- fitdist(punt45and60$endfield, "weibull")
fg <- fitdist(punt45and60$endfield, "gamma")
fln <- fitdist(punt45and60$endfield, "lnorm")
fn <- fitdist(punt45and60$endfield, "norm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma", "normal")
denscomp(list(fw, fln, fg, fn), legendtext = plot.legend)
fw
x = seq(min(punt45and60$endfield),max(punt45and60$endfield),length = 1000)
min(punt45and60$endfield)
max(punt45and60$endfield)
curve(dweibull(x, shape =  14.33961, scale = 90.81987), from=min(x), to=max(x))
lines(density(punt45and60$endfield), lwd = 2, col = "blue")

 
#========================================================================================================


