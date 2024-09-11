library(semtree)

if (!dir.exists("data")) dir.create("data")
if (!file.exists("data/DASS_data_21.02.19.zip")) download.file("https://openpsychometrics.org/_rawdata/DASS_data_21.02.19.zip",destfile="data/DASS_data_21.02.19.zip")
if (!exists("DASS")) DASS<-read.csv("data/DASS_data_21.02.19/data.csv",sep="\t")
names(DASS)

# data cleaning
DASS <- DASS[DASS$age<=120, ]
DASS <- DASS[DASS$gender %in% c(1,2,3),]
DASS <- DASS[DASS$hand %in% c(1,2,3),]
DASS <- DASS[DASS$urban %in% c(1,2,3),]
DASS <- DASS[DASS$voted %in% c(1,2),]

items <- DASS[,c((1:42)-1)*3+1 ]
sumscore <- rowSums(items)

predictors <- DASS[,c("education",
                      "urban",
                      "gender",
                      #"engnat",
                      "age",
                      "hand",
                      "familysize",
                      "screensize",
                      "voted"
                    #  "married")
                   )]

DASS_sum <- cbind( sumscore, predictors)
DASS_sum <- DASS_sum[complete.cases(DASS_sum),]

DASS_sum$education <- ordered(DASS_sum$education, levels=1:4,
                              labels=c(
                                "Less than high school", "High school", "University degree", "Graduate degree"))
DASS_sum$urban <- factor(DASS_sum$urban, levels=c(1,2,3),
                         labels=c("Rural (country side)", "Suburban", "Urban (town, city)"))
DASS_sum$gender <- factor(DASS_sum$gender,levels = c(1,2,3),
                          labels=c("male","female","other"))
#DASS_sum$engnat <- factor(DASS_sum$engnat,levels=c(1,2),
#                          labels=c("yes","no"))
DASS_sum$hand <- factor(DASS_sum$hand, levels=1:3,
                        labels=c("right","left","both"))
DASS_sum$voted <- factor(DASS_sum$voted, levels=1:2,labels=c("yes","no"))
#DASS_sum$married <- factor(DASS_sum$married,levels=1:3,
#                           labels=c("never","currently","previously"))
#DASS_sum$religion <- factor(DASS_sum$religion)
DASS_sum$screensize <- factor(DASS_sum$screensize,levels=1:2,
                              labels=c("small screen","big screen"))

DASS_sum <- DASS_sum[complete.cases(DASS_sum),]

# subsample
DASS_sum <- DASS_sum[sample(1:nrow(DASS_sum),size = 5000, TRUE)]

manifests<-c("sumscore")
latents<-c()
model <- mxModel("Simple Model", 
                 type="RAM",
                 manifestVars = manifests,
                 latentVars = latents,
                 mxPath(from="one",to=manifests,
                        free=c(TRUE), 
                        value=c(1.0) , arrows=1, 
                        label=c("mu") 
                 ),
                 mxPath(from=manifests,to=manifests,
                        free=c(TRUE), value=c(1.0) , 
                        arrows=2, 
                        label=c("var") 
                 ),
                 mxData(DASS_sum, type = "raw")
);
model <- mxRun(model)

library(semtree)

tree<-semtree(model, DASS_sum, 
              control=semtree_control(method="score", max.depth=4,verbose=TRUE))

plot(tree)

# forest dauert 20 Minuten pro Stück... etwa 4min/tree... warum?! 
library(future)
plan(multisession, workers = 15)
boruta_results <- boruta(model, DASS_sum, 
                         control=semforest_score_control(num.trees=25,
                           control=semtree_control(max.depth=4,method="score",alpha=1)),
                         maxRuns = 11)

if (!dir.exists("results/ex3")) dir.create("results/ex3")
saveRDS(tree, "results/ex3/tree.rds")
saveRDS(boruta_results,"results/ex3/boruta_results.rds")


# to compute chi2 effect size, use a tree
library(semtree)
tree <- semtree(model, DASS_sum[,c("sumscore","screensize")])
chi2 <- tree$lr
sqrt(chi2/nrow(DASS_sum))

 ggplot(aes(y=sumscore,x=age))+
  geom_point()+geom_smooth(method="lm")
