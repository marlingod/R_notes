setwd("C:/Users/haj02/Documents/ICO")
getwd()

library(ggplot2)
library(lmtest)
library(car)
library(xtable)
library(ggplot2)
require(ggplot2)
require(GGally)
require(CCA)
require(lattice)
require(reshape2)
library(mice)
library(VIM)

# loading the csv file
jet0725t=read.csv("jet0725t.csv",head=TRUE,sep=",")

summary(jet0725t)
drawHistDensity<-function(df,var,xlabel){
  hist<-ggplot(data=df,aes(var))+
    geom_histogram(aes(y=..density..))+
    labs(x=xlabel,y="Density")
  hist+stat_function(fun=dnorm,args=list(mean=mean(var,na.rm=TRUE),sd=sd(var,na.rm = TRUE)),color="blue",size=1)+geom_density()
}

drawHistCount<-function(df,var,xlabel){
  hist<-ggplot(data=df,aes(var))+
    geom_histogram(aes(y=..count..))+
    labs(x=xlabel,y="Count")
  hist
}

drawHistDensity(jet0725t,jet0725t$underwriting_time,"Underwriting_time")
drawHistCount(jet0725t,jet0725t$underwriting_time,"Underwriting_time")

jet0725t$FINAL_ACTION_MONTH=as.factor(jet0725t$FINAL_ACTION_MONTH)
jet0725t$APPL_SEX_CDE=as.factor(jet0725t$APPL_SEX_CDE)
jet0725t$ELECTRONIC_APP_IND=as.factor(jet0725t$ELECTRONIC_APP_IND)
jet0725t$MHQ_IND=as.factor(jet0725t$MHQ_IND)
jet0725t$COMMISSION_AUTHORIZE_IND=as.factor(jet0725t$COMMISSION_AUTHORIZE_IND)
jet0725t$REQ_GRID_NUM=as.factor(jet0725t$REQ_GRID_NUM)

# Pattern of missings
md.pattern(jet0725t)
aggr_plot <- aggr(jet0725t, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(jet0725t), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

subset_jet = jet0725t[, c(3,8,27)]
ggpairs(subset_jet)

z <- cor(subset_jet)
levelplot(z,aspect = "iso",scale=list(x=list(rot=45)))

cor.matrix <- round(cor(subset_jet, use = "pairwise.complete.obs", method = "spearman"), digits = 2)
cor.matrix
cor.dat <- melt(cor.matrix)
cor.dat <- data.frame(cor.dat)
ggplot(cor.dat, aes(Var2, Var1, fill = value)) + 
  geom_tile() + 
  geom_text(aes(Var2, Var1, label = value), color = "#073642", size = 4) +
  scale_fill_gradient(name=expression("Spearman" * ~ rho), low = "#fdf6e3", high = "steelblue",
                      breaks=seq(0, 1, by = 0.2), limits = c(0.3, 1)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "", y = "") + 
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top",
                               title.hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.9, 0.7),
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", 
                               title.hjust = 0.5))

# Complete Cases
jet0725t_complete=jet0725t[complete.cases(jet0725t),]
jet0725t_complete=jet0725t_complete[jet0725t_complete$underwriting_time>0,]
jet0725t_complete$loguw_time<-log(jet0725t_complete$underwriting_time)
jet0725t_complete$sqrtuw_time=sqrt(jet0725t_complete$underwriting_time)


drawHistDensity(jet0725t_complete,jet0725t_complete$loguw_time,"log_Underwriting_time")

drawHistDensity(jet0725t_complete,jet0725t_complete$sqrtuw_time,"sqrt_Underwriting_time")

drawHistCount(jet0725t_complete,jet0725t_complete$loguw_time,"log_Underwriting_time")
# performing the linear regression
lm.fit<-lm(underwriting_time  ~
             FINAL_ACTION_MONTH+
             AGE+
             HEIGHT_FEET_AND_INCHES+
             WEIGHT+
             APPL_SEX_CDE+
             UNDERWRITE_AMT+
             ELECTRONIC_APP_IND+
             AGENT_LOS+
             MHQ_IND+
             COMMISSION_AUTHORIZE_IND+
             NUM_WB_TOUCHES+
             NUM_WB_ACTIVITIES+
             NUM_REQ_AGENT_OBTAIN_INFO+
             NUM_REQ_ORDERED+
             NUM_LINKGRAMS_SENT+
             NUM_FOLLOWUPS+
             NUM_CASE_ACTIVTY_FOLLOWUPS+
             REQ_GRID_NUM+
             NUM_DISC_REQ 
           ,data=jet0725t_complete)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
#performing the box-cox transformation
par(mfrow=c(1,1))
lm.fit.b=boxcox(underwriting_time  ~
                  FINAL_ACTION_MONTH+
                  AGE+
                  HEIGHT_FEET_AND_INCHES+
                  WEIGHT+
                  APPL_SEX_CDE+
                  UNDERWRITE_AMT+
                  ELECTRONIC_APP_IND+
                  AGENT_LOS+
                  MHQ_IND+
                  COMMISSION_AUTHORIZE_IND+
                  NUM_WB_TOUCHES+
                  NUM_WB_ACTIVITIES+
                  NUM_REQ_AGENT_OBTAIN_INFO+
                  NUM_REQ_ORDERED+
                  NUM_LINKGRAMS_SENT+
                  NUM_FOLLOWUPS+
                  NUM_CASE_ACTIVTY_FOLLOWUPS+
                  REQ_GRID_NUM+
                  NUM_DISC_REQ 
                ,data=jet0725t_complete)

lambda=0.15
jet0725t_complete$boxcox=jet0725t_complete$underwriting_time^lambda
drawHistDensity(jet0725t_complete,jet0725t_complete$boxcox,"boxcox_Underwriting_time")
lm.fit.box=lm(boxcox ~
                FINAL_ACTION_MONTH+
                AGE+
                HEIGHT_FEET_AND_INCHES+
                WEIGHT+
                APPL_SEX_CDE+
                UNDERWRITE_AMT+
                ELECTRONIC_APP_IND+
                AGENT_LOS+
                MHQ_IND+
                COMMISSION_AUTHORIZE_IND+
                NUM_WB_TOUCHES+
                NUM_WB_ACTIVITIES+
                NUM_REQ_AGENT_OBTAIN_INFO+
                NUM_REQ_ORDERED+
                NUM_LINKGRAMS_SENT+
                NUM_FOLLOWUPS+
                NUM_CASE_ACTIVTY_FOLLOWUPS+
                REQ_GRID_NUM+
                NUM_DISC_REQ-1 
              ,data=jet0725t_complete )
summary(lm.fit.box)
par(mfrow=c(2,2))
plot(lm.fit.box)
#splitting the box cox into high and low

jet0725t_complete_low_box=jet0725t_complete[(jet0725t_complete$boxcox<=0.875),]
drawHistDensity(jet0725t_complete_low_box,jet0725t_complete_low_box$boxcox,"boxcox_Underwriting_time")
jet0725t_complete_high_box=jet0725t_complete[(jet0725t_complete$boxcox>0.875),]
drawHistDensity(jet0725t_complete_high_box,jet0725t_complete_high_box$boxcox,"boxcox_Underwriting_time")

lm.fit.box_low=lm(boxcox ~
                    FINAL_ACTION_MONTH+
                    AGE+
                    HEIGHT_FEET_AND_INCHES+
                    WEIGHT+
                    APPL_SEX_CDE+
                    UNDERWRITE_AMT+
                    ELECTRONIC_APP_IND+
                    AGENT_LOS+
                    MHQ_IND+
                    COMMISSION_AUTHORIZE_IND+
                    NUM_WB_TOUCHES+
                    NUM_WB_ACTIVITIES+
                    NUM_REQ_AGENT_OBTAIN_INFO+
                    NUM_REQ_ORDERED+
                    NUM_LINKGRAMS_SENT+
                    NUM_FOLLOWUPS+
                    NUM_CASE_ACTIVTY_FOLLOWUPS+
                    REQ_GRID_NUM+
                    NUM_DISC_REQ 
                  ,data=jet0725t_complete_low_box )
summary(lm.fit.box_low)
par(mfrow=c(2,2))
plot(lm.fit.box_low)

lm.fit.box_high=lm(boxcox ~
                     FINAL_ACTION_MONTH+
                     AGE+
                     HEIGHT_FEET_AND_INCHES+
                     WEIGHT+
                     APPL_SEX_CDE+
                     UNDERWRITE_AMT+
                     ELECTRONIC_APP_IND+
                     AGENT_LOS+
                     MHQ_IND+
                     COMMISSION_AUTHORIZE_IND+
                     NUM_WB_TOUCHES+
                     NUM_WB_ACTIVITIES+
                     NUM_REQ_AGENT_OBTAIN_INFO+
                     NUM_REQ_ORDERED+
                     NUM_LINKGRAMS_SENT+
                     NUM_FOLLOWUPS+
                     NUM_CASE_ACTIVTY_FOLLOWUPS+
                     REQ_GRID_NUM+
                     NUM_DISC_REQ 
                   ,data=jet0725t_complete_high_box )
summary(lm.fit.box_high)
par(mfrow=c(2,2))
plot(lm.fit.box_high)

# splitting the log into two domains

jet0725t_complete_low_log=jet0725t_complete[(jet0725t_complete$loguw_time<=-1),]

jet0725t_complete_high_log=jet0725t_complete[(jet0725t_complete$loguw_time>-1),]

drawHistDensity(jet0725t_complete_low_log,jet0725t_complete_low_log$loguw_time,"log_Underwriting_time")

drawHistDensity(jet0725t_complete_high_log,jet0725t_complete_high_log$loguw_time,"log_Underwriting_time")

lm.fit_low_log<-lm(underwriting_time  ~
                     FINAL_ACTION_MONTH+
                     AGE+
                     HEIGHT_FEET_AND_INCHES+
                     WEIGHT+
                     APPL_SEX_CDE+
                     UNDERWRITE_AMT+
                     ELECTRONIC_APP_IND+
                     AGENT_LOS+
                     MHQ_IND+
                     COMMISSION_AUTHORIZE_IND+
                     NUM_WB_TOUCHES+
                     NUM_WB_ACTIVITIES+
                     NUM_REQ_AGENT_OBTAIN_INFO+
                     NUM_REQ_ORDERED+
                     NUM_LINKGRAMS_SENT+
                     NUM_FOLLOWUPS+
                     NUM_CASE_ACTIVTY_FOLLOWUPS+
                     REQ_GRID_NUM+
                     NUM_DISC_REQ 
                   ,data=jet0725t_complete_low_log)
summary(lm.fit_low_log)
par(mfrow=c(2,2))
plot(lm.fit_low_log)

lm.fit_high_log<-lm(underwriting_time  ~
                      FINAL_ACTION_MONTH+
                      AGE+
                      HEIGHT_FEET_AND_INCHES+
                      WEIGHT+
                      APPL_SEX_CDE+
                      UNDERWRITE_AMT+
                      ELECTRONIC_APP_IND+
                      AGENT_LOS+
                      MHQ_IND+
                      COMMISSION_AUTHORIZE_IND+
                      NUM_WB_TOUCHES+
                      NUM_WB_ACTIVITIES+
                      NUM_REQ_AGENT_OBTAIN_INFO+
                      NUM_REQ_ORDERED+
                      NUM_LINKGRAMS_SENT+
                      NUM_FOLLOWUPS+
                      NUM_CASE_ACTIVTY_FOLLOWUPS+
                      REQ_GRID_NUM+
                      NUM_DISC_REQ 
                    ,data=jet0725t_complete_high_log)
summary(lm.fit_high_log)
par(mfrow=c(2,2))
plot(lm.fit_high_log)


glm.fit<-glm(underwriting_time  ~
               FINAL_ACTION_MONTH+
               AGE+
               HEIGHT_FEET_AND_INCHES+
               WEIGHT+
               APPL_SEX_CDE+
               UNDERWRITE_AMT+
               ELECTRONIC_APP_IND+
               AGENT_LOS+
               MHQ_IND+
               COMMISSION_AUTHORIZE_IND+
               NUM_WB_TOUCHES+
               NUM_WB_ACTIVITIES+
               NUM_REQ_AGENT_OBTAIN_INFO+
               NUM_REQ_ORDERED+
               NUM_LINKGRAMS_SENT+
               NUM_FOLLOWUPS+
               NUM_CASE_ACTIVTY_FOLLOWUPS+
               REQ_GRID_NUM+
               NUM_DISC_REQ 
             ,data=jet0725t_complete, family=binomial())
summary(glm.fit)
par(mfrow=c(2,2))
plot(glm.fit)

##Deision tree


output.tree <- rpart(
  underwriting_time  ~
    FINAL_ACTION_MONTH+
    AGE+
    HEIGHT_FEET_AND_INCHES+
    WEIGHT+
    APPL_SEX_CDE+
    UNDERWRITE_AMT+
    ELECTRONIC_APP_IND+
    AGENT_LOS+
    MHQ_IND+
    COMMISSION_AUTHORIZE_IND+
    NUM_WB_TOUCHES+
    NUM_WB_ACTIVITIES+
    NUM_REQ_AGENT_OBTAIN_INFO+
    NUM_REQ_ORDERED+
    NUM_LINKGRAMS_SENT+
    NUM_FOLLOWUPS+
    NUM_CASE_ACTIVTY_FOLLOWUPS+
    REQ_GRID_NUM+
    NUM_DISC_REQ, 
  data = jet0725t_complete)
VI_F=importance(fit)
summary(output.tree)
printcp(output.tree)
par(mfrow = c(1,2), xpd = NA)
plot(output.tree, uniform=TRUE, main="Regression Tree ")
text(output.tree, use.n=TRUE, all=TRUE, cex=.8)

# loading the csv file with the reeive month instead of the final month 
jet0808t=read.csv("jet0808t.csv",head=TRUE,sep=",")

jet0808t$HO_RECEIVE_MONTH=as.factor(jet0808t$HO_RECEIVE_MONTH)
jet0808t$APPL_SEX_CDE=as.factor(jet0808t$APPL_SEX_CDE)
jet0808t$SRC_APP_IND=as.factor(jet0808t$SRC_APP_IND)
jet0808t$MHQ_IND=as.factor(jet0808t$MHQ_IND)
jet0808t$COMMISSION_AUTHORIZE_IND=as.factor(jet0808t$COMMISSION_AUTHORIZE_IND)
jet0808t$REQ_GRID_NUM=as.factor(jet0808t$REQ_GRID_NUM)

# Complete Cases
jet0808t_complete=jet0808t[complete.cases(jet0808t),]
# performing the linear regression on cycle time
lm.fit<-lm(CYCLE_TIME  ~
             HO_RECEIVE_MONTH+
             AGE+
             HEIGHT_FEET_AND_INCHES+
             WEIGHT+
             APPL_SEX_CDE+
             UNDERWRITE_AMT+
             SRC_APP_IND+
             AGENT_LOS+
             MHQ_IND+
             COMMISSION_AUTHORIZE_IND+
             NUM_WB_TOUCHES+
             NUM_WB_ACTIVITIES+
             NUM_REQ_AGENT_OBTAIN_INFO+
             NUM_REQ_ORDERED+
             NUM_LINKGRAMS_SENT+
             NUM_FOLLOWUPS+
             NUM_CASE_ACTIVTY_FOLLOWUPS+
             REQ_GRID_NUM+
             NUM_DISC_REQ 
           ,data=jet0808t_complete)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
#finding the best box cox transformation 
lm.fit.b=boxcox(CYCLE_TIME  ~
                  HO_RECEIVE_MONTH+
                  AGE+
                  HEIGHT_FEET_AND_INCHES+
                  WEIGHT+
                  APPL_SEX_CDE+
                  UNDERWRITE_AMT+
                  SRC_APP_IND+
                  AGENT_LOS+
                  MHQ_IND+
                  COMMISSION_AUTHORIZE_IND+
                  NUM_WB_TOUCHES+
                  NUM_WB_ACTIVITIES+
                  NUM_REQ_AGENT_OBTAIN_INFO+
                  NUM_REQ_ORDERED+
                  NUM_LINKGRAMS_SENT+
                  NUM_FOLLOWUPS+
                  NUM_CASE_ACTIVTY_FOLLOWUPS+
                  REQ_GRID_NUM+
                  NUM_DISC_REQ 
                ,data=jet0808t_complete)

# creating the box ox transformed column
lambda=0.25
jet0808t_complete$boxcox=jet0808t_complete$CYCLE_TIME^lambda
# Making a log column
jet0808t_complete$log_cycle_time=log(jet0808t_complete$CYCLE_TIME)
# density of box cox
drawHistDensity(jet0808t_complete,jet0808t_complete$boxcox,"boxcox_cycle_time")

drawHistDensity(jet0808t_complete,jet0808t_complete$log_cycle_time,"log_cycle_time")
# performing the regression on box-cox transformed
lm.fit.box=lm(boxcox ~
                HO_RECEIVE_MONTH+
                AGE+
                HEIGHT_FEET_AND_INCHES+
                WEIGHT+
                APPL_SEX_CDE+
                UNDERWRITE_AMT+
                SRC_APP_IND+
                AGENT_LOS+
                MHQ_IND+
                COMMISSION_AUTHORIZE_IND+
                NUM_WB_TOUCHES+
                NUM_WB_ACTIVITIES+
                NUM_REQ_AGENT_OBTAIN_INFO+
                NUM_REQ_ORDERED+
                NUM_LINKGRAMS_SENT+
                NUM_FOLLOWUPS+
                NUM_CASE_ACTIVTY_FOLLOWUPS+
                REQ_GRID_NUM+
                NUM_DISC_REQ-1
              ,data=jet0808t_complete )
summary(lm.fit.box)
par(mfrow=c(2,2))
plot(lm.fit.box)
# performing the regression on log_cycle_time
lm.fit.log=lm(log_cycle_time ~
                HO_RECEIVE_MONTH+
                AGE+
                HEIGHT_FEET_AND_INCHES+
                WEIGHT+
                APPL_SEX_CDE+
                UNDERWRITE_AMT+
                SRC_APP_IND+
                AGENT_LOS+
                MHQ_IND+
                COMMISSION_AUTHORIZE_IND+
                NUM_WB_TOUCHES+
                NUM_WB_ACTIVITIES+
                NUM_REQ_AGENT_OBTAIN_INFO+
                NUM_REQ_ORDERED+
                NUM_LINKGRAMS_SENT+
                NUM_FOLLOWUPS+
                NUM_CASE_ACTIVTY_FOLLOWUPS+
                REQ_GRID_NUM+
                NUM_DISC_REQ-1
              ,data=jet0808t_complete )
summary(lm.fit.log)
par(mfrow=c(2,2))
plot(lm.fit.log)

##Deision tree for cycle time 


output.tree_2 <- rpart(
  CYCLE_TIME  ~
    HO_RECEIVE_MONTH+
    AGE+
    HEIGHT_FEET_AND_INCHES+
    WEIGHT+
    APPL_SEX_CDE+
    UNDERWRITE_AMT+
    SRC_APP_IND+
    AGENT_LOS+
    MHQ_IND+
    COMMISSION_AUTHORIZE_IND+
    NUM_WB_TOUCHES+
    NUM_WB_ACTIVITIES+
    NUM_REQ_AGENT_OBTAIN_INFO+
    NUM_REQ_ORDERED+
    NUM_LINKGRAMS_SENT+
    NUM_FOLLOWUPS+
    NUM_CASE_ACTIVTY_FOLLOWUPS+
    REQ_GRID_NUM+
    NUM_DISC_REQ, 
  data = jet0808t)

summary(output.tree_2)
printcp(output.tree_2)
par(mfrow = c(1,2), xpd = NA)
plot(output.tree_2, uniform=TRUE, main="Regression Tree ")
text(output.tree_2, use.n=TRUE, all=TRUE, cex=.8)