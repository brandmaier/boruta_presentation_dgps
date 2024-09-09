if (!exists("DASS")) DASS<-read.csv("data/DASS_data_21.02.19/data.csv",sep="\t")
names(DASS)

items <- DASS[,c((1:42)-1)*3+1 ]
sumscore <- rowSums(items)

predictors <- DASS[,c("education","urban","gender",
                      "engnat","age","hand","voted","married")]

DASS_sum <- cbind( sumscore, predictors)
DASS_sum <- DASS_sum[complete.cases(DASS_sum),]

DASS_sum$education <- ordered(DASS_sum$education, levels=1:4,
                              labels=c(
                                "Less than high school", "High school", "University degree", "Graduate degree"))
DASS_sum$urban <- factor(DASS_sum$urban, levels=c(1,2,3),
                         labels=c("Rural (country side)", "Suburban", "Urban (town, city)"))
DASS_sum$gender <- factor(DASS_sum$gender,levels = c(1,2,3),
                          labels=c("male","female","other"))
DASS_sum$engnat <- factor(DASS_sum$engnat,levels=c(1,2),
                          labels=c("yes","no"))
DASS_sum$hand <- factor(DASS_sum$hand, levels=1:3,
                        labels=c("right","left","both"))
DASS_sum$voted <- factor(DASS_sum$voted, levels=1:2,labels=c("yes","no"))
DASS_sum$married <- factor(DASS_sum$married,levels=1:3,
                           labels=c("never","currently","previously"))
#DASS_sum$religion <- factor(DASS_sum$religion)

DASS_sum <- DASS_sum[complete.cases(DASS_sum),]
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

# forest dauert 20 Minuten pro Stück... etwa 4min/tree... warum?! 
library(future)
#plan(multisession, workers = 4)
boruta_results <- boruta(model, DASS_sum, 
                         control=semforest_score_control(),
                         maxRuns = 11)

if (!dir.exists("results/ex3")) dir.create("results/ex3")
saveRDS(tree, "results/ex3/tree.rds")
saveRDS(boruta_results,"results/ex3/boruta_results.rds")