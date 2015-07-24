
rm(list = setdiff(ls(), lsf.str()))
setwd("/Users/tabuwalda/Documents/2015LearnLabSummerSchool/ShinyApp/LatencyLearningCurves/")

library(lme4)
library(dplyr)
options(dplyr.width = Inf)


# Read In Data - This is only practice Data, we also want pre- and posttest
dat <- read.table("../../spatial-data/SILC Data/SILC datashop upload with millisecond.txt",header=TRUE,sep="\t",fill=TRUE,stringsAsFactors=FALSE)
pre <- read.table("../../spatial-data/PrePostAlina/KC_pre.txt",header=TRUE,sep="\t",fill=TRUE,stringsAsFactors=FALSE)
post <- read.table("../../spatial-data/PrePostAlina/KC_post.txt",header=TRUE,sep="\t",fill=TRUE,stringsAsFactors=FALSE)
pp <- rbind(pre,post)

pp$Anon.Student.Id <- pp$Subj
pp$Subj <- NULL
pp$Session.Id <- NA
pp$Time <- NA
pp$Level.Unit. <- NA
pp$Problem.Name <- pp$Condition
pp$Condition <- NULL
pp$Step.Name <- "Step1"
pp$Outcome <- NA
pp$Selection <- NA # Are NA in dat
pp$Action <- NA # Are NA in dat
pp$Input <- NA
pp$KC..Mental.Spatial.Skills. <- NA
pp$CF..Trial.Number. <- NA
pp$CF..Reaction.Time.in.Milliseconds. <- NA
pp$CF..Outlier.Max. <- NA
pp$CF..Outlier.Min. <- NA
pp$CF..Trim.Min.Value. <- NA





# Make new columns
dat$Part <- Training
dat$Task <- dat$KC..Mental.Spatial.Skills.
dat$KC..Mental.Spatial.Skills. <- NULL
dat$Subj <- dat$Anon.Student.Id
dat$Problem <- ifelse(dat$Task == "MPFT", substr(as.character(dat$Problem.Name),1,
	nchar(as.character(dat$Problem.Name))-6), substr(as.character(dat$Problem.Name),1,
	nchar(as.character(dat$Problem.Name))-10)) 
dat$Index <- paste(dat$Subj,dat$Problem,sep="-")
dat$RT <- dat$CF..Reaction.Time.in.Milliseconds.
dat$Acc <- ifelse(dat$Outcome == "CORRECT", 1, 
			ifelse(dat$Outcome == "INCORRECT", 0, NA))
dat$logRT <- log(dat$CF..Reaction.Time.in.Milliseconds.)
dat$Rotation.MRT <- ifelse(dat$Task == "MRT", gsub(".*Y", "", dat$Problem), NA)
dat$ArmAngle.MRT <- ifelse(dat$Task == "MRT", gsub(".*_|Y.*","",dat$Problem), NA)
dat$Construction.MRT <- ifelse(dat$Task == "MRT", gsub("R|_[^_]+$","",dat$Problem), NA)
dat$ArrowRelPosition.MPFT <- ifelse(dat$Task == "MPFT",gsub(".?Code|[1234567].*","",dat$Problem), NA)
dat$SquaresCut.MPFT <- ifelse(dat$Task == "MPFT",gsub("[^1234567]","",dat$Problem), NA) # Apparently this can be 7 as well
dat$ArrowLocation.MPFT <- ifelse(dat$Task == "MPFT",gsub(".*[1234567]|_.*","",dat$Problem), NA)
dat$Rotation.MPFT <- ifelse(dat$Task == "MPFT",gsub(".*[^IV]","",dat$Problem), NA)
dat$Month <- as.numeric(gsub("/.*","",dat$Session.Id))
dat$Day <- as.numeric(gsub("/","",substr(dat$Session.Id,3,4)))
dat$Date <- as.numeric(paste(dat$Month,dat$Day,sep=""))






aics <- data.frame(KCmodel=rep(NA,100),AIC=NA)

# Make KC model - Similarity (levels: same, different)
dat$KC.Similarity <- ifelse(substr(dat$Problem,1,1) %in% c("F","R"), "Different","Same")
dat$KC.Similarity.Idx <- paste(dat$Subj,dat$KC.Similarity,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
  group_by(KC.Similarity.Idx) %>%
  mutate(KC.Similarity.Opp = cumsum(tmpVar)) # Opportunity
similarity.lmer <- lmer(logRT~KC.Similarity +
  KC.Similarity:KC.Similarity.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[1,]$KCmodel <- "Similarity"
aics[1,]$AIC <- AIC(similarity.lmer)


# Make KC model - Problem (levels: unique(dat$Problem))
dat$KC.Problem <- dat$Problem
dat$KC.Problem.Idx <- paste(dat$Subj,dat$KC.Problem,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
group_by(KC.Problem.Idx) %>%
mutate(KC.Problem.Opp = cumsum(tmpVar)) # Opportunity 
problem.lmer <- glmer(logRT~KC.Problem +
  KC.Problem:KC.Problem.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[2,]$KCmodel <- "Problem"
aics[2,]$AIC <- AIC(problem.lmer)

# Make KC model - Task (levels: MRT, MPFT)
dat$KC.Task <- dat$Task
dat$KC.Task.Idx <- paste(dat$Subj,dat$KC.Task,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
group_by(KC.Task.Idx) %>%
mutate(KC.Task.Opp = cumsum(tmpVar)) # Opportunity 
task.lmer <- glmer(logRT~KC.Task +
  KC.Task:KC.Task.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[3,]$KCmodel <- "Task"
aics[3,]$AIC <- AIC(task.lmer)

# Make KC model - Rotation MRT (levels: 50, 100, 150)
dat$KC.Rotation.MRT <- dat$Rotation.MRT
dat$KC.Rotation.MRT.Idx <- paste(dat$Subj,dat$KC.Rotation.MRT,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
group_by(KC.Rotation.MRT.Idx) %>%
mutate(KC.Rotation.MRT.Opp = cumsum(tmpVar)) # Opportunity 
rotationmrt.lmer <- glmer(logRT~KC.Rotation.MRT +
  KC.Rotation.MRT:KC.Rotation.MRT.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[4,]$KCmodel <- "Rotation.MRT"
aics[4,]$AIC <- AIC(rotationmrt.lmer)

# Make KC model - ArmAngle MRT (levels: 0, 90, 180, 270)
dat$KC.ArmAngle.MRT <- dat$ArmAngle.MRT
dat$KC.ArmAngle.MRT.Idx <- paste(dat$Subj,dat$KC.ArmAngle.MRT,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
group_by(KC.ArmAngle.MRT.Idx) %>%
mutate(KC.ArmAngle.MRT.Opp = cumsum(tmpVar)) # Opportunity 
armanglemrt.lmer <- glmer(logRT~KC.ArmAngle.MRT +
  KC.ArmAngle.MRT:KC.ArmAngle.MRT.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[5,]$KCmodel <- "ArmAngle.MRT"
aics[5,]$AIC <- AIC(armanglemrt.lmer)

# Make KC model - Construction MRT (levels: unique(dat$Construction.MRT) - e.g. 3_3_4_2)
dat$KC.Construction.MRT <- dat$Construction.MRT
dat$KC.Construction.MRT.Idx <- paste(dat$Subj,dat$KC.Construction.MRT,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
group_by(KC.Construction.MRT.Idx) %>%
mutate(KC.Construction.MRT.Opp = cumsum(tmpVar)) # Opportunity 

constructionmrt.lmer <- glmer(logRT~KC.Construction.MRT +
  KC.Construction.MRT:KC.Construction.MRT.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[6,]$KCmodel <- "Construction.MRT"
aics[6,]$AIC <- AIC(constructionmrt.lmer)


# Make KC model - ArrowRelPosition MPFT (levels: A,B,C,F,G,H)
dat$KC.ArrowRelPosition.MPFT <- dat$ArrowRelPosition.MPFT
dat$KC.ArrowRelPosition.MPFT.Idx <- paste(dat$Subj,dat$KC.TArrowRelPosition.MPFT,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
group_by(KC.ArrowRelPosition.MPFT.Idx) %>%
mutate(KC.ArrowRelPosition.MPFT.Opp = cumsum(tmpVar)) # Opportunity 

arrowrelpositionmpft.lmer <- glmer(logRT~KC.ArrowRelPosition.MPFT +
  KC.ArrowRelPosition.MPFT:KC.ArrowRelPosition.MPFT.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[7,]$KCmodel <- "ArrowRelPosition.MPFT"
aics[7,]$AIC <- AIC(arrowrelpositionmpft.lmer)


# Make KC model - SquaresCut MPFT (levels: 1,2,3,4,5,6)
dat$KC.SquaresCut.MPFT <- dat$SquaresCut.MPFT
dat$KC.SquaresCut.MPFT.Idx <- paste(dat$Subj,dat$KC.SquaresCut.MPFT,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
group_by(KC.SquaresCut.MPFT.Idx) %>%
mutate(KC.SquaresCut.MPFT.Opp = cumsum(tmpVar)) # Opportunity 
squarescutmpft.lmer <- glmer(logRT~KC.SquaresCut.MPFT +
  KC.SquaresCut.MPFT:KC.SquaresCut.MPFT.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[8,]$KCmodel <- "SquaresCut.MPFT"
aics[8,]$AIC <- AIC(squarescutmpft.lmer)


# Make KC model - ArrowLocation MPFT (levels: blank, a, b, c, d, e)
dat$KC.ArrowLocation.MPFT <- dat$ArrowLocation.MPFT
dat$KC.ArrowLocation.MPFT.Idx <- paste(dat$Subj,dat$KC.ArrowLocation.MPFT,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
group_by(KC.ArrowLocation.MPFT.Idx) %>%
mutate(KC.ArrowLocation.MPFT.Opp = cumsum(tmpVar)) # Opportunity 

arrowlocationmpft.lmer <- glmer(logRT~KC.ArrowLocation.MPFT +
  KC.ArrowLocation.MPFT:KC.ArrowLocation.MPFT.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[9,]$KCmodel <- "ArrowLocation.MPFT"
aics[9,]$AIC <- AIC(arrowlocationmpft.lmer)


# Make KC model - Rotation MPFT (level: I, II, III, IV)
dat$KC.Rotation.MPFT <- dat$Rotation.MPFT
dat$KC.Rotation.MPFT.Idx <- paste(dat$Subj,dat$KC.Rotation.MPFT,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
group_by(KC.Rotation.MPFT.Idx) %>%
mutate(KC.Rotation.MPFT.Opp = cumsum(tmpVar)) # Opportunity 
rotationmpft.lmer <- glmer(logRT~KC.Rotation.MPFT +
  KC.Rotation.MPFT:KC.Rotation.MPFT.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[10,]$KCmodel <- "Rotation.MPFT"
aics[10,]$AIC <- AIC(rotationmpft.lmer)

# Make KC model - Difficulty (level: easy, medium, hard)
dat$KC.Difficulty <- ifelse(dat$Task == "MRT", 
						ifelse(dat$Rotation.MRT == 50,"Easy",
						ifelse(dat$Rotation.MRT == 100,"Medium",
						ifelse(dat$Rotation.MRT == 150,"Hard",NA
						))),
						ifelse(dat$Rotation.MPFT == "I", "Easy",
						ifelse(dat$Rotation.MPFT == "IV", "Hard",
						ifelse(dat$Rotation.MPFT == "II", "Medium",
						ifelse(dat$Rotation.MPFT == "III", "Medium",NA)))))
dat$KC.Difficulty.Idx <- paste(dat$Subj,dat$KC.Difficulty,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
group_by(KC.Difficulty.Idx) %>%
mutate(KC.Difficulty.Opp = cumsum(tmpVar)) # Opportunity 
difficulty.lmer <- glmer(logRT~KC.Difficulty +
  KC.Difficulty:KC.Difficulty.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[10,]$KCmodel <- "Difficulty"
aics[10,]$AIC <- AIC(difficulty.lmer)

# Make KC model - One KC
dat$KC.Single <- "Single"
dat$KC.Single.Idx <- paste(dat$Subj,dat$KC.Single,sep="-")
dat$tmpVar <- 1
dat <- dat %>% 
  group_by(KC.Single.Idx) %>%
  mutate(KC.Single.Opp = cumsum(tmpVar)) # Opportunity
single.lmer <- lmer(logRT~KC.Single +
  KC.Single:KC.Single.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

aics[11,]$KCmodel <- "Similarity"
aics[11,]$AIC <- AIC(similarity.lmer)


listKC <- names(dat)[grepl("KC.",names(dat))]
listKC <- listKC[!grepl(".Opp",listKC)]
listKC <- listKC[!grepl(".Idx",listKC)]

save(aics,file="./Data/aics.Rdat")
save(dat,file="./Data/datShiny.Rdat")
save(listKC,file="./Data/listKCShiny.Rdat")

i = 10
dat$tmpVar <- 1
for(KC in as.data.frame(combn(listKC,2))) {
	i = i+1
	KC1 <- gsub("KC.","",KC[1])
	KC2 <- gsub("KC.","",KC[2])
	newVar <- paste("KC.",KC1,"_",KC2,sep="")
	KC1vals <- dat[[as.character(KC[1])]]
	KC2vals <- dat[[as.character(KC[2])]]
	dat[[newVar]] <- paste(KC1vals,KC2vals,sep="_")
	dat[[paste(newVar,"Idx",sep=".")]] <- paste(dat$Subj,dat[[newVar]],sep="_")
	tmp <- tapply(dat$tmpVar, dat[[ncol(dat)]], cumsum))
	dat[[paste(newVar,"Opp",sep=".")]] <- NA

	dat %>% filter(KC.Rotation.MPFT_Difficulty.Idx==names(tmp)[1])
	for(idx in 1:length(names(tmp))) {
		dat[[ncol(dat)]] <- dat %>% filter(KC.Rotation.MPFT_Difficulty.Idx==names(tmp)[idx]) %>% tmp[idx]
	}

	model <- glmer(logRT~KC.Rotation.MPFT +
	  KC.Rotation.MPFT:KC.Rotation.MPFT.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression
	aics[i,]$KCmodel <- newVar
	aics[i,]$AIC <- AIC(model)
}



	dat$KC.Rotation.MPFT <- dat$Rotation.MPFT
	dat$KC.Rotation.MPFT.Idx <- paste(dat$Subj,dat$KC.Rotation.MPFT,sep="-")
	dat$tmpVar <- 1
	dat <- dat %>% 
	group_by(KC.Rotation.MPFT.Idx) %>%
	mutate(KC.Rotation.MPFT.Opp = cumsum(tmpVar)) # Opportunity 
	rotationmpft.lmer <- glmer(logRT~KC.Rotation.MPFT +
	  KC.Rotation.MPFT:KC.Rotation.MPFT.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression

}



load("datShiny.Rdat")

# Regression Model
similarity.lmer <- glmer(logRT~KC.Rotation.MRT +
 	KC.Rotation.MRT:KC.Rotation.MRT.Opp+(1|Subj),data=dat[!is.na(dat$logRT),])


# Learning Curves
similarity.plot <- with(dat, aggregate(list(logRT=logRT),list(Opportunity=KC.Similarity.Opp),mean,na.rm=TRUE))
plot(similarity.plot$Opportunity,similarity.plot$logRT, type="l")

problem.plot <- with(dat, aggregate(list(logRT=logRT),list(Opportunity=KC.Problem.Opp),mean,na.rm=TRUE))
plot(problem.plot$Opportunity,problem.plot$logRT, type="l")


# Let's have a look at the learning curve over problems












tmp$NewProblemName <- as.factor(tmp$NewProblemName)
cutpoints <- seq(0,max(tmp$Opportunity+25),by=25)
tmp$newOpportunity <-cut(tmp$Opportunity,cutpoints,include.lowest=TRUE,labels=FALSE)
tst <- with(tmp, aggregate(list(latency=latency),list(Opportunity=newOpportunity),mean,na.rm=TRUE))
plot(tst$Opportunity,tst$latency, type="l")


tmp$cosine <- NA
for(i in unique(tmp$newOpportunity)) {
	if(!is.na(i)) {
		if(i < max(tmp$newOpportunity,na.rm=TRUE)) {
			cos <- crossprod(as.vector(table(tmp[tmp$newOpportunity == i,]$NewProblemName)),as.vector(table(tmp[tmp$newOpportunity == i+1,]$NewProblemName)))
			tmp[tmp$newOpportunity == i,]$cosine <- as.numeric(cos)
		}
	}
}

tst <- with(tmp, aggregate(list(latency=latency),list(Opportunity=newOpportunity,cosine=cosine),mean,na.rm=TRUE))

symbols(x=tst$Opportunity, y=tst$latency, circles=tst$cosine, inches=1/10, ann=F, bg="steelblue2", fg="darkgray")



tmp$tmpVar <- 1
tmp <- tmp %>% 
  group_by(newOpportunity) %>%
  mutate(problemFreq= as.vector(table(NewProblemName))
h(tmp)