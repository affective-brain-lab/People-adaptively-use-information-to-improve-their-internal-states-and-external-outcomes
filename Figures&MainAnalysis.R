### FIGURES AND MAIN analysis _ cognition
# 17-03-22
# Irene Cogliati Dezza
##############################################################################
#clean up the global environment
rm(list=ls())

#load some packages to use later on
library(R.matlab)
library(MASS)
library(psycho)

##############################################################################
## Experiment 1: subjective card game main study
# analyses and figures are the same for the other experiment only the upoloaded "Utility" file and DEMO file changes
##############################################################################

################# ################# DEMOGRAPHICS ################# ################# ################# #################
DEMO<-read.csv('/Users/irenecogliatidezza/Documents/ABL_POSTDOC/CardGame_6Choices/Subjective Evaluation/subjectiveEvaluationsPsychBiases/RESULTS/DEMO_notorderwithMatlab.csv', header = TRUE)
round(mean(DEMO$Age_score),1)
round(sd(DEMO$Age_score),1)
sum(DEMO$Gender_score==1, na.rm = TRUE)

################# ################# MIXED MODELS ################# ################# ################# #################
Utility<-readMat('/Users/irenecogliatidezza/Documents/MATLAB/Model_inferenceTask/CardGame_6Choices/SubjectiveEvaluation/subjectiveEvaluationsPsychBiases/utilities/revision/Utilities_Lin.mat')

## utilities are zscore
#affect 
affect<-as.data.frame(Utility$Utilities[[6]][[1]])
#cognitive 
cognitve<-as.data.frame(Utility$Utilities[[7]][[1]])
#IU 
IU<-as.data.frame(Utility$Utilities[[8]][[1]]) # with agency
# objective agency
Agency<-as.data.frame(Utility$Utilities[[18]][[1]])  #objective agency
# expected mood
MoodBefore<-as.data.frame(Utility$Utilities[[16]][[1]])
#subjective uncertianty
KnowningBefore<-as.data.frame(Utility$Utilities[[15]][[1]]) #this is rescaled between 0 and -3
# IU without agency
IUnoAgency<-as.data.frame(Utility$Utilities[[17]][[1]]) # it is already with agency
# subjective agency
SubAgency<-as.data.frame(Utility$Utilities[[19]][[1]])  #objective agency
# IUsubjectiveagency
IUsubjectiveagency<-as.data.frame(Utility$Utilities[[20]][[1]])  #objective agency
# IU_EVmax with objective agency
IU_EVMax<-as.data.frame(Utility$Utilities[[31]][[1]])
# IU_EVmax with subjective agency
SubjIU_EVMax<-as.data.frame(Utility$Utilities[[33]][[1]])

# info choice
InfoChoice_original<-as.data.frame(Utility$Utilities[[4]][[1]])
InfoChoice<-InfoChoice_original

# different computations of uncertainty
Entropy<-as.data.frame(Utility$Utilities[[22]][[1]])
Entropy_zscore<-as.data.frame(Utility$Utilities[[25]][[1]])

# remove subjects
affect<-affect[-c(6, 12, 34, 46, 55, 58),]
cognitve<-cognitve[-c(6, 12, 34, 46, 55, 58),]
IU<-IU[-c(6, 12, 34, 46, 55, 58),]
Agency<-Agency[-c(6, 12, 34, 46, 55, 58),]
MoodBefore<-MoodBefore[-c(6, 12, 34, 46, 55, 58),]
KnowningBefore<-KnowningBefore[-c(6, 12, 34, 46, 55, 58),]
IUnoAgency<-IUnoAgency[-c(6, 12, 34, 46, 55, 58),]
SubAgency<-SubAgency[-c(6, 12, 34, 46, 55, 58),]
IUsubjectiveagency<-IUsubjectiveagency[-c(6, 12, 34, 46, 55, 58),]
InfoChoice<-InfoChoice[-c(6, 12, 34, 46, 55, 58),]
IU_EVMax<-IU_EVMax[-c(6, 12, 34, 46, 55, 58),]
SubjIU_EVMax<-SubjIU_EVMax[-c(6, 12, 34, 46, 55, 58),]
Entropy<-Entropy[-c(6, 12, 34, 46, 55, 58),]
Entropy_zscore<-Entropy_zscore[-c(6, 12, 34, 46, 55, 58),]

########################### create vectors for mixed models
InfoChoice_vect<-c()
for (i in 1:dim(InfoChoice)[1]) {
  vec<-t(InfoChoice[i,])
  InfoChoice_vect<-c(InfoChoice_vect, vec)
}
Affect_vect<-c()
for (i in 1:dim(affect)[1]) {
  vec1<-t(affect[i,])
  Affect_vect<-c(Affect_vect, vec1)
}
Cognitive_vect<-c()
for (i in 1:dim(cognitve)[1]) {
  vec2<-t(cognitve[i,])
  Cognitive_vect<-c(Cognitive_vect, vec2)
}
IU_vect<-c()
for (i in 1:dim(IU)[1]) {
  vec3<-t(IU[i,])
  IU_vect<-c(IU_vect, vec3)
}
Agency_vect<-c()
for (i in 1:dim(Agency)[1]) {
  vec5<-t(Agency[i,])
  Agency_vect<-c(Agency_vect, vec5)
}
Subject<-c()
for (i in 1:dim(IU)[1]) {
  vec4<-rep(i, 90)
  Subject<-c(Subject, vec4)
}

MoodBefore_vect<-c()
for (i in 1:dim(InfoChoice)[1]) {
  vec4<-t(MoodBefore[i,])
  MoodBefore_vect<-c(MoodBefore_vect, vec4)
}

KnowningBefore_vect<-c()
for (i in 1:dim(InfoChoice)[1]) {
  vec4<-t(KnowningBefore[i,])
  KnowningBefore_vect<-c(KnowningBefore_vect, vec4)
}

Trials<-c()
for (i in 1:dim(IU)[1]) {
  vec4<-c(1:90)
  Trials<-c(Trials, vec4)
}

IUnoAgency_vect<-c()
for (i in 1:dim(InfoChoice)[1]) {
  vec4<-t(IUnoAgency[i,])
  IUnoAgency_vect<-c(IUnoAgency_vect, vec4)
}

SubAgency_vect<-c()
for (i in 1:dim(InfoChoice)[1]) {
  vec4<-t(SubAgency[i,])
  SubAgency_vect<-c(SubAgency_vect, vec4)
}

IUSubAgency_vect<-c()
for (i in 1:dim(InfoChoice)[1]) {
  vec4<-t(IUsubjectiveagency[i,])
  IUSubAgency_vect<-c(IUSubAgency_vect, vec4)
}

IU_EVMax_vect<-c()
for (i in 1:dim(InfoChoice)[1]) {
  vec4<-t(IU_EVMax[i,])
  IU_EVMax_vect<-c(IU_EVMax_vect, vec4)
}

SubjIU_EVMax_vect<-c()
for (i in 1:dim(InfoChoice)[1]) {
  vec4<-t(SubjIU_EVMax[i,])
  SubjIU_EVMax_vect<-c(SubjIU_EVMax_vect, vec4)
}


Entropy_vect<-c()
for (i in 1:dim(Entropy_zscore)[1]) {
  vec1<-t(Entropy_zscore[i,])
  Entropy_vect<-c(Entropy_vect, vec1)
}

TRIALS_data<- data.frame(Subject, InfoChoice_vect, Affect_vect, Cognitive_vect, IU_vect, Agency_vect, IUnoAgency_vect, MoodBefore_vect, KnowningBefore_vect, SubAgency_vect, IUSubAgency_vect, IU_EVMax_vect, SubjIU_EVMax_vect, Entropy_vect)
colnames(TRIALS_data) <- c('Subject', 'InfoChoice', 'EV', 'SD', 'IUandAgency', 'ObjAgency', 'IUnoAgency', "ExpMood", "SubUnc", 'SubjAgency', 'IUSubAgency', 'IU_EVMax', 'SubjIU_EVMax', 'Entropy')

library(nlme)
library(MuMIn)

## Objective Model
model_mixed <- lme(InfoChoice~EV+SD+IU_EVMax, random=~1+EV+SD+IU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
## Subjective Model
model_mixed <- lme(InfoChoice~ExpMood+SubUnc+SubjIU_EVMax, random=~1+ExpMood+SubUnc+SubjIU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
# one or two factors subjective model
#ExpMood+SubUnc
model_mixed <- lme(InfoChoice~ExpMood+SubUnc, random=~1+ExpMood+SubUnc| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
#SubUnc+SubjIU_EVMax
model_mixed <- lme(InfoChoice~SubUnc+SubjIU_EVMax, random=~1+SubUnc+SubjIU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
#ExpMood+SubjIU_EVMax
model_mixed <- lme(InfoChoice~ExpMood+SubjIU_EVMax, random=~1+ExpMood+SubjIU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)

### model with one or 2 factors and mix factors
# OBJECTIVE MODEL 1 or 2 factors
# EV&SD
model_mixed <- lme(InfoChoice~EV+SD, random=~1+EV+SD| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
#EV&objAgency
model_mixed <- lme(InfoChoice~EV+IU_EVMax, random=~1+EV+IU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
#SD&objAgency
model_mixed <- lme(InfoChoice~SD+IU_EVMax, random=~1+SD+IU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)

# model with mix factors
# EV, subjective uncertainty, subjective IU
model_mixed <- lme(InfoChoice~EV+SubUnc+SubjIU_EVMax, random=~1+EV+SubUnc+SubjIU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
# ExpMood, SD, subjective IU
model_mixed <- lme(InfoChoice~ExpMood+SD+SubjIU_EVMax, random=~1+ExpMood+SD+SubjIU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
# ExpMood,  subjective uncertainty, objecive IU
model_mixed <- lme(InfoChoice~ExpMood+SubUnc+IU_EVMax, random=~1+ExpMood+SubUnc+SubjIU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
# EV, SD, subjective IU
model_mixed <- lme(InfoChoice~EV+SD+SubjIU_EVMax, random=~1+EV+SD+SubjIU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
# ExpMood, SD, objecive IU
model_mixed <- lme(InfoChoice~ExpMood+SD+IU_EVMax, random=~1+ExpMood+SD+SubjIU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
# EV,  subjective uncertainty, objecive IU
model_mixed <- lme(InfoChoice~EV+SubUnc+IU_EVMax, random=~1+EV+SubUnc+SubjIU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)

# only one factor model
#only exp mood
model_mixed <- lme(InfoChoice~ExpMood, random=~1+ExpMood| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
#only ubj uncertainty
model_mixed <- lme(InfoChoice~SubUnc, random=~1+SubUnc| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
#only subj IU
model_mixed <- lme(InfoChoice~SubjIU_EVMax, random=~1+SubjIU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
# only EV
model_mixed <- lme(InfoChoice~EV, random=~1+EV| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
# only SD
model_mixed <- lme(InfoChoice~SD, random=~1+SD| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
## only obj IU
model_mixed <- lme(InfoChoice~IU_EVMax, random=~1+IU_EVMax| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)
# only entropy
model_mixed <- lme(InfoChoice~Entropy, random=~1+Entropy| Subject, data=TRIALS_data, method ='ML', control=lmeControl(opt='optim'))
summary (model_mixed)

################# ################# INDIVIDUAL FIT ################# ################# ################# #################
# SUBJECTIVE MODEL
coef_cognitive_sbj<-c()
coef_affect_sbj<-c()
coef_IU_sbj<-c()
coef_costant_sbj<-c()
AIC_sbj<-c()
BIC_sbj<-c()

for (i in c(1:dim(affect)[1])) {
  # organize in a data frame for subject=i
  d<- data.frame(t(InfoChoice[i,]), t(MoodBefore[i,]), t(KnowningBefore[i,]), t(SubjIU_EVMax[i,]))
  colnames(d) <- c('InfoChoice', 'SubAffect', 'SubCognitive', 'IUsubAgency')
  
  ## LINEAR  REGRESSION
  model <- lm(InfoChoice ~SubAffect+SubCognitive+IUsubAgency, data = d)
  coef_affect_sbj[i]<- model$coefficients["SubAffect"]
  coef_cognitive_sbj[i] <- model$coefficients["SubCognitive"]
  coef_IU_sbj[i] <- model$coefficients["IUsubAgency"]
  
  coef_costant_sbj[i]<-model$coefficients[1]
  
  AIC_sbj[i]<-extractAIC(model)[2]
  BIC_sbj[i]<-extractAIC(model, k=log(dim(InfoChoice)[2]))[2]
}

coef_affect=coef_affect_sbj
coef_cognitive=coef_cognitive_sbj
coef_IU=coef_IU_sbj

################# ################# FIGURES ################# ################# ################# #################
# PLOT BIC
BIC_scores <- data.frame(18150,
                         18296,
                         18876,
                         18506,
                         18735,
                         19244,
                         18707,
                         19333,
                         18616,
                         18958,
                         19188,
                         18326,
                         18529,
                         18167,
                         18760,
                         18545,
                         18351,
                         19012,
                         19009,
                         19314,
                         19327,
                         19768,
                         19311,
                         19462)

# Create a matrix to represent the models
model_n <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 , 20, 21, 22, 23, 24)

m_has_AntiAffect <-   c(1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0)
m_has_SubUncertain <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0)
m_has_IUSubagency <-  c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0)

m_has_EV <-           c(0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0)
m_has_SD <-           c(0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0)
m_has_IUobjAg <-      c(0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0)

m_has_Entropy <-      c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

m_matrix <- data.frame(model_n, m_has_AntiAffect, m_has_SubUncertain, m_has_IUSubagency, m_has_EV, m_has_SD, m_has_IUobjAg, m_has_Entropy)

library(tidyr)
library(ggplot2)
m_matrix_long <- gather(m_matrix, variable, bool, m_has_AntiAffect:m_has_Entropy, factor_key=TRUE)
m_matrix_plot <- ggplot(m_matrix_long , aes(variable, model_n)) + geom_tile(aes(fill = bool),
                                                                            color="black") + theme_classic() +
  theme(legend.position="none") +
  scale_y_continuous(breaks=seq(1, 24, 1), labels=model_n) +
  scale_x_discrete(labels=c("AntiAffect", "SubUncertain", "IUSubagency", "EV", "SD", "IUobjAg", "Entropy")) +
  scale_fill_gradient(low="white", high="grey") +
  labs(x = "Fixed variable", y = "Model number")
m_matrix_plot
# Define argmin function for the plots
argmin <- function(x) {
  min <- min(x)
  for (i in 1:length(x)) {
    if (x[i] == min) {
      return (i)
    }
  }
}

m_BIC_dat <- data.frame(model_n, as.numeric(BIC_scores))
colnames(m_BIC_dat)<-c('model_n', 'BIC')
m_bIC_dat_plot <- ggplot(m_BIC_dat, aes(x = BIC, y = model_n)) +
  geom_vline(xintercept = min(m_BIC_dat$BIC), linetype="dashed") +
  theme_classic() +
  scale_y_continuous(breaks=seq(1, 24, 1),  limits=c(1, 24)) +
  scale_x_continuous(breaks=seq(18000, 19400, 250), limits=c(round(min(m_BIC_dat$BIC)), round(max(m_BIC_dat$BIC)))) +
  labs(x = "BIC score", y = "Model number")
for (n in model_n) {
  m_bIC_dat_plot <- m_bIC_dat_plot + geom_segment(x=18000, y=n, xend=m_BIC_dat$BIC[n], yend=n)
}
m_bIC_dat_plot <- m_bIC_dat_plot + geom_point(size=2, color='black')
m_bIC_dat_plot
library(gridExtra)
grid.arrange(m_matrix_plot, m_bIC_dat_plot, ncol=2)


### PLOT INDIVIDUAL COEFFICIENTS
library(ggplot2)
subject=length(coef_affect)
#without intercept
s_d<-c()
for (i in 1:length(coef_IU)) {
  sub<-paste("Subject", i, sep = "")
  s_d<-c(s_d, sub )
}
S_d<-rep(s_d,3)
v_d<- c(coef_affect, coef_cognitive, coef_IU)
c_d<- c(rep("BAffect", subject), rep("Cognitive", subject), rep("AInstrumental", subject))
d_d<-data.frame(subject_d=S_d,
                coeff=c_d,
                value_d=v_d)
p<-ggplot(d_d, aes(y=value_d, x=coeff, fill=coeff)) + 
  geom_violin()+
  geom_jitter(shape=16, size=2, position=position_jitter(0.2), color='grey70')+
  
  theme(axis.text=element_text(size=60),
        axis.title=element_text(size=60,face="bold")) +
  theme(panel.grid.minor = element_line(colour = "white"),
        
        panel.grid.major=element_line(colour = "white"),
        
        panel.background = element_rect(fill = "white") +
          theme(axis.text=element_text(size=70),
                axis.title=element_text(size=70,face="bold"))+
          theme(legend.justification=c(1,0),
                legend.position=c(1,0.4)),
        legend.title=element_text(size=25),
        legend.text=element_text(size=23) 
  )
p + ylab('Beta Estimate')+scale_fill_manual(values=c("seagreen4","red",'mediumblue') )+stat_summary(fun=mean, geom="point", size=2, color="black")+ylim(-1.7, 1.7)
###### STAT individual fit
t.test(mu=0, coef_affect)
t.test(mu=0, coef_cognitive)
t.test(mu=0, coef_IU)

###### ###### ###### ###### ###### PROXY ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ######

affectBeforeNozscore<-as.data.frame(Utility$Utilities[[1]][[1]])
affectBefore<-as.data.frame(Utility$Utilities[[6]][[1]])
affectAfter<-as.data.frame(Utility$Utilities[[12]][[1]])
#cognitive 
cognitveBeforeNozscore<-as.data.frame(Utility$Utilities[[2]][[1]])
cognitveBefore<-as.data.frame(Utility$Utilities[[7]][[1]])
cognitveAfter<-as.data.frame(Utility$Utilities[[13]][[1]])
#IU 
IUBefore<-as.data.frame(Utility$Utilities[[8]][[1]]) # it is already with agency

IUAfter<-as.data.frame(Utility$Utilities[[14]][[1]]) # it is already with agency

Agency<-as.data.frame(Utility$Utilities[[5]][[1]])/100

IUnoAgency<-as.data.frame(Utility$Utilities[[17]][[1]]) # it is already with agency

# info choice
InfoChoice_original<-as.data.frame(Utility$Utilities[[4]][[1]])
InfoChoice<-InfoChoice_original

# different computations of uncertainty
UncertantyDistributionLottery<-as.data.frame(Utility$Utilities[[22]][[1]])
UnceraintyRangeLottery<-as.data.frame(Utility$Utilities[[23]][[1]])
ExpectedReductionUncerainty<-as.data.frame(Utility$Utilities[[24]][[1]])
UncertaintyProportionGain<-as.data.frame(Utility$Utilities[[28]][[1]])

#### Subjective evaluations
MoodBefore<-readMat("/Users/irenecogliatidezza/Documents/MATLAB/Model_inferenceTask/CardGame_6Choices/SubjectiveEvaluation/subjectiveEvaluationsPsychBiases/MoodBefore.mat")
MoodBefore<-as.data.frame(MoodBefore$MoodBefore)


MoodAfter<-readMat("/Users/irenecogliatidezza/Documents/MATLAB/Model_inferenceTask/CardGame_6Choices/SubjectiveEvaluation/subjectiveEvaluationsPsychBiases/MoodAfter.mat")
MoodAfter<-as.data.frame(MoodAfter$MoodAfter)


KnowningBefore<-readMat("/Users/irenecogliatidezza/Documents/MATLAB/Model_inferenceTask/CardGame_6Choices/SubjectiveEvaluation/subjectiveEvaluationsPsychBiases/KnowningBefore.mat")
KnowningBefore<-as.data.frame(KnowningBefore$KnowningBefore)


KnowningAfter<-readMat("/Users/irenecogliatidezza/Documents/MATLAB/Model_inferenceTask/CardGame_6Choices/SubjectiveEvaluation/subjectiveEvaluationsPsychBiases/KnowningAfter.mat")
KnowningAfter<-as.data.frame(KnowningAfter$KnowningAfter)

SubjectiveAgency<-readMat("/Users/irenecogliatidezza/Documents/MATLAB/Model_inferenceTask/CardGame_6Choices/SubjectiveEvaluation/subjectiveEvaluationsPsychBiases/SubjectiveAgency.mat")
SubjectiveAgency<-as.data.frame(SubjectiveAgency$SubjectiveAgency)

### points
cost<-readMat('/Users/irenecogliatidezza/Documents/MATLAB/Model_inferenceTask/CardGame_6Choices/SubjectiveEvaluation/subjectiveEvaluationsPsychBiases/Cost.mat')
cost<-cost$Cost
# lotterychoice
lotterychoice<-readMat('/Users/irenecogliatidezza/Documents/MATLAB/Model_inferenceTask/CardGame_6Choices/SubjectiveEvaluation/subjectiveEvaluationsPsychBiases/LotteryChoice.mat')
lotterychoice<-lotterychoice$LotteryChoice
# outcomeoption
outcomeoption<-readMat('/Users/irenecogliatidezza/Documents/MATLAB/Model_inferenceTask/CardGame_6Choices/SubjectiveEvaluation/subjectiveEvaluationsPsychBiases/OutcomeOption.mat')
outcomeoption<-outcomeoption$OutcomeOption
# computerchoice
computerchoice<-readMat('/Users/irenecogliatidezza/Documents/MATLAB/Model_inferenceTask/CardGame_6Choices/SubjectiveEvaluation/subjectiveEvaluationsPsychBiases/ComputerChoice.mat')
computerchoice<-computerchoice$ComputerChoice

## remove subjects
affectBeforeNozscore<-affectBeforeNozscore[-c(6, 12, 34, 46, 55, 58),]
affectBefore<-affectBefore[-c(6, 12, 34, 46, 55, 58),]
affectAfter<-affectAfter[-c(6, 12, 34, 46, 55, 58),]
cognitveBeforeNozscore<-cognitveBeforeNozscore[-c(6, 12, 34, 46, 55, 58),]
cognitveBefore<-cognitveBefore[-c(6, 12, 34, 46, 55, 58),]
cognitveAfter<-cognitveAfter[-c(6, 12, 34, 46, 55, 58),]
IUBefore<-IUBefore[-c(6, 12, 34, 46, 55, 58),]
IUAfter<-IUAfter[-c(6, 12, 34, 46, 55, 58),]
Agency<-Agency[-c(6, 12, 34, 46, 55, 58),]
IUnoAgency<-IUnoAgency[-c(6, 12, 34, 46, 55, 58),]
InfoChoice<-InfoChoice[-c(6, 12, 34, 46, 55, 58),]
MoodBefore<-MoodBefore[-c(6, 12, 34, 46, 55, 58),]
MoodAfter<-MoodAfter[-c(6, 12, 34, 46, 55, 58),]
KnowningBefore<-KnowningBefore[-c(6, 12, 34, 46, 55, 58),]
KnowningAfter<-KnowningAfter[-c(6, 12, 34, 46, 55, 58),]
SubjectiveAgency<-SubjectiveAgency[-c(6, 12, 34, 46, 55, 58),]
#LINEAR<-LINEAR[-c(6, 12, 34, 46, 55, 58),] is laready removed in previous script
cost<-cost[-c(6, 12, 34, 46, 55, 58),]
lotterychoice<-lotterychoice[-c(6, 12, 34, 46, 55, 58),]
outcomeoption<-outcomeoption[-c(6, 12, 34, 46, 55, 58),]
computerchoice<-computerchoice[-c(6, 12, 34, 46, 55, 58),]
UncertantyDistributionLottery<-UncertantyDistributionLottery[-c(6, 12, 34, 46, 55, 58),]
UnceraintyRangeLottery<-UnceraintyRangeLottery[-c(6, 12, 34, 46, 55, 58),]
ExpectedReductionUncerainty<-ExpectedReductionUncerainty[-c(6, 12, 34, 46, 55, 58),]
UncertaintyProportionGain<-UncertaintyProportionGain[-c(6, 12, 34, 46, 55, 58),]

###### ###### ###### ###### ###### PROXY ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ######

########## correlations 
#EV before| Mood Before 
corr_EV_Mood<-c()
for (j in 1:dim(affectBefore)[1]) {
  corr<- cor.test(as.numeric(affectBeforeNozscore[j,]), as.numeric(MoodBefore[j,]))
  corr_EV_Mood[j]<-as.numeric(corr$estimate)
}
t.test(corr_EV_Mood, mu=0)
#SD before| Knowing Before ... 
#rescale the scale (0 in the center and same increasing values as going towards the extreme)
rowKnowningBefore<-KnowningBefore
KnowningBefore[KnowningBefore==3]<-'Zero'
KnowningBefore[KnowningBefore==4]<-'One'
KnowningBefore[KnowningBefore==2]<-'One'
KnowningBefore[KnowningBefore==5]<-'Two'
KnowningBefore[KnowningBefore==1]<-'Two'
KnowningBefore[KnowningBefore==0]<-'Three'
KnowningBefore[KnowningBefore==6]<-'Three'
#
KnowningBefore[KnowningBefore=="Zero"]<-0
KnowningBefore[KnowningBefore=="One"]<- -1
KnowningBefore[KnowningBefore=="Two"]<- -2
KnowningBefore[KnowningBefore=="Three"]<- -3
#
corr_SD_Know<-c()
rowmean<-c()
for (j in 1:dim(cognitveBefore)[1]) {
  corr<- cor.test(as.numeric(cognitveBeforeNozscore[j,]), as.numeric(KnowningBefore[j,]))
  rowmean[j]= mean(as.numeric(KnowningBefore[j,]))
  corr_SD_Know[j]<-as.numeric(corr$estimate)
}
t.test(corr_SD_Know, mu=0)

corr_SAg_Agency<-c()
for (j in 1:dim(affectBefore)[1]) {
  corr<- cor.test(as.numeric(SubjectiveAgency[j,]), as.numeric(Agency[j,]))
  corr_SAg_Agency[j]<-as.numeric(corr$estimate)
}
t.test(corr_SAg_Agency, mu=0)

###### PLOTS
MoodBefore_vect<-c()
for (i in 1:dim(MoodBefore)[1]) {
  vec4<-t(MoodBefore[i,])
  MoodBefore_vect<-c(MoodBefore_vect, vec4)
}
Affect_vect<-c()
for (i in 1:dim(affectBefore)[1]) {
  vec1<-t(affectBeforeNozscore[i,])
  Affect_vect<-c(Affect_vect, vec1)
}
Subject<-c()
for (i in 1:dim(affectBefore)[1]) {
  vec4<-rep(i, 90)
  Subject<-c(Subject, vec4)
}
dm<-data.frame(MoodBefore_vect, Affect_vect, Subject)
colnames(dm)<-c('AnticipatedAffect', 'EV', 'Subject')
library(ggplot2)
ggplot(dm, aes(x=AnticipatedAffect, y=EV)) +
  geom_point(col='red') + 
  geom_smooth(method=lm, colour='red', se=FALSE, fullrange=TRUE, aes( fill=as.factor(Subject)))+
  theme_classic()

KnowningBefore_vect<-c()
for (i in 1:dim(KnowningBefore)[1]) {
  vec1<-t(KnowningBefore[i,])
  KnowningBefore_vect<-c(KnowningBefore_vect, vec1)
}
cognitveBefore_vect<-c()
for (i in 1:dim(cognitveBefore)[1]) {
  vec1<-t(cognitveBeforeNozscore[i,])
  cognitveBefore_vect<-c(cognitveBefore_vect, vec1)
}
dm<-data.frame(as.numeric(KnowningBefore_vect), cognitveBefore_vect, Subject)
colnames(dm)<-c('SubjectiveUncertainty', 'SD', 'Subject')
library(ggplot2)
ggplot(dm, aes(x=SubjectiveUncertainty, y=SD)) +
  geom_point(col='mediumblue') + 
  geom_smooth(method=lm, colour='mediumblue', se=FALSE, fullrange=TRUE, aes( fill=as.factor(Subject)))+
  theme_classic()

SubjectiveAgency_vect<-c()
for (i in 1:dim(SubjectiveAgency)[1]) {
  vec1<-t(SubjectiveAgency[i,])
  SubjectiveAgency_vect<-c(SubjectiveAgency_vect, vec1)
}
Agency_vect<-c()
for (i in 1:dim(Agency)[1]) {
  vec1<-t(Agency[i,])
  Agency_vect<-c(Agency_vect, vec1)
}
dm<-data.frame(SubjectiveAgency_vect, Agency_vect, Subject)
colnames(dm)<-c('SubjectiveAgency', 'ObjectiveAgency', 'Subject')
library(ggplot2)
ggplot(dm, aes(x=SubjectiveAgency, y=ObjectiveAgency)) +
  geom_point(col='seagreen') + 
  geom_smooth(method=lm, colour='seagreen', se=FALSE, fullrange=TRUE, aes( fill=as.factor(Subject)))+
  theme_classic()

###### ###### ###### ###### ###### IMPACT OF INFORMATION ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ######
## including computer choices
pont<-matrix(c( rep(NA, dim(cost)[1]*dim(cost)[2]) ), ncol=dim(cost)[2])
infochoice<-InfoChoice
for (subject in 1:dim(cost)[1]) {
  
  for (i in 1:dim(cost)[2])  { 
    
    if (lotterychoice[subject,i]==0) { # if play the lottery
      if (infochoice[subject,i]>0) { #if info yes
        pont[subject,i]=outcomeoption[subject,i]-cost[subject,i]
      } else if (infochoice[subject,i]<0) {#if info no and play
        pont[subject,i]=outcomeoption[subject,i]
      }
    } else if (lotterychoice[subject,i]==1) { # if info yes and not play
      if (infochoice[subject,i]>0) {
        pont[subject,i]=0-cost[subject,i]
      } else if (infochoice[subject,i]<0) { # if info no and not play
        pont[subject,i]=0
      }
    } else if (lotterychoice[subject,i]==100) { #if computer plays
      if (infochoice[subject,i]>0) { #if info yes
        pont[subject,i]=outcomeoption[subject,i]-cost[subject,i]
      } else if (infochoice[subject,i]<0) { # if info no
        pont[subject,i]=outcomeoption[subject,i]
      }
    }
  }
}

#rescale the scale (0 in the center and same increasing values as going towards the extreme)
rowKnowningAfter<-KnowningAfter
KnowningAfter[KnowningAfter==3]<-'Zero'
KnowningAfter[KnowningAfter==4]<-'One'
KnowningAfter[KnowningAfter==2]<-'One'
KnowningAfter[KnowningAfter==5]<-'Two'
KnowningAfter[KnowningAfter==1]<-'Two'
KnowningAfter[KnowningAfter==0]<-'Three'
KnowningAfter[KnowningAfter==6]<-'Three'
#
KnowningAfter[KnowningAfter=="Zero"]<-0
KnowningAfter[KnowningAfter=="One"]<- -1
KnowningAfter[KnowningAfter=="Two"]<- -2
KnowningAfter[KnowningAfter=="Three"]<- -3

# mood after
meanMoodInfo<-c()
meanMoodNoInfo<-c()
for (i in 1:dim(MoodAfter)[1]){
  x=MoodAfter[i,]
  y=InfoChoice[i,]
  meanMoodInfo[i]<-mean(x[y>0])
  meanMoodNoInfo[i]<-mean(x[y<0])
}
t.test(meanMoodInfo, meanMoodNoInfo, paired=TRUE)

###### change subjective uncertainty
changeKnowing<-KnowningAfter
for (i in 1: dim(KnowningAfter)[1]){
  changeKnowing[i,]<-as.numeric(KnowningAfter[i,])-as.numeric(KnowningBefore[i,])
}
meanKnowingChangeInfo<-c()
meanKnowingChangeNoInfo<-c()
for (i in 1:dim(changeKnowing)[1]){
  x=changeKnowing[i,]
  y=InfoChoice[i,]
  meanKnowingChangeInfo[i]<-mean(as.numeric(x[y>0]))
  meanKnowingChangeNoInfo[i]<-mean(as.numeric(x[y<0]))
}
t.test(meanKnowingChangeInfo, meanKnowingChangeNoInfo, paired=TRUE)

##

#after points
meanPointsAfterInfo<-c()
meanPointsAfterNoInfo<-c()
for (i in 1:dim(pont)[1]){
  x=pont[i,]
  y=InfoChoice[i,]
  meanPointsAfterInfo[i]<-mean(x[y>0], na.rm=TRUE)
  meanPointsAfterNoInfo[i]<-mean(x[y<0], na.rm=TRUE)
}
t.test(meanPointsAfterInfo, meanPointsAfterNoInfo, paired=TRUE)

### plot differences
mooddiffrence=meanMoodInfo-meanMoodNoInfo
pwr.t.test(d=(0-mean(mooddiffrence))/sd(c(mooddiffrence)),power=0.95,sig.level=0.05,type="paired",alternative="two.sided")

chageuncdifference=meanKnowingChangeInfo-meanKnowingChangeNoInfo
differencepoints=meanPointsAfterInfo-meanPointsAfterNoInfo
t.test(mooddiffrence, mu=0)
t.test(chageuncdifference, mu=0)
t.test(differencepoints, mu=0)

d<-data.frame(c(mooddiffrence), c( rep("ExpMood", length(meanMoodInfo))))
colnames(d)<-c('Value', 'Type')

ggplot(d, aes(x=Type, y=Value, fill=Type)) +
  geom_violin()+
  geom_jitter(shape=16, size=1, position=position_jitter(0.2),color='grey70' )+
  theme(axis.text=element_text(size=60),
        axis.title=element_text(size=60,face="bold")) +
  theme(panel.grid.minor = element_line(colour = "white"),
        
        panel.grid.major=element_line(colour = "white"),
        
        panel.background = element_rect(fill = "white") +
          theme(axis.text=element_text(size=70),
                axis.title=element_text(size=70,face="bold"))+
          theme(legend.justification=c(1,0),
                legend.position=c(1,0.4)),
        legend.title=element_text(size=25),
        legend.text=element_text(size=23)
        
  )+ylab('Difference info-noinfo')+scale_fill_manual(values=c("red"))+stat_summary(fun=mean, geom="point", size=2, color="black")


d<-data.frame(c(chageuncdifference), c( rep("SubUnc", length(meanMoodInfo))))
colnames(d)<-c('Value', 'Type')

ggplot(d, aes(x=Type, y=Value, fill=Type)) +
  geom_violin()+
  geom_jitter(shape=16, size=1, position=position_jitter(0.2),color='grey70' )+
  theme(axis.text=element_text(size=60),
        axis.title=element_text(size=60,face="bold")) +
  theme(panel.grid.minor = element_line(colour = "white"),
        
        panel.grid.major=element_line(colour = "white"),
        
        panel.background = element_rect(fill = "white") +
          theme(axis.text=element_text(size=70),
                axis.title=element_text(size=70,face="bold"))+
          theme(legend.justification=c(1,0),
                legend.position=c(1,0.4)),
        legend.title=element_text(size=25),
        legend.text=element_text(size=23)
        
  )+ylab('Difference info-noinfo')+scale_fill_manual(values=c("mediumblue"))+stat_summary(fun=mean, geom="point", size=2, color="black")

d<-data.frame(c(differencepoints), c(rep("Points",length(meanPointsAfterNoInfo)) ))
colnames(d)<-c('Value', 'Type')

ggplot(d, aes(x=Type, y=Value, fill=Type)) +
  geom_violin()+
  geom_jitter(shape=16, size=1, position=position_jitter(0.2),color='grey70' )+
  theme(axis.text=element_text(size=60),
        axis.title=element_text(size=60,face="bold")) +
  theme(panel.grid.minor = element_line(colour = "white"),
        
        panel.grid.major=element_line(colour = "white"),
        
        panel.background = element_rect(fill = "white") +
          theme(axis.text=element_text(size=70),
                axis.title=element_text(size=70,face="bold"))+
          theme(legend.justification=c(1,0),
                legend.position=c(1,0.4)),
        legend.title=element_text(size=25),
        legend.text=element_text(size=23)
        
  )+ylab('Difference info-noinfo')+scale_fill_manual(values=c( "seagreen4"))+stat_summary(fun=mean, geom="point", size=2, color="black")

