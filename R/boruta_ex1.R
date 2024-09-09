diabetes <- read.csv("data-downloaded/diabetes.csv")
diabetes$Outcome <- ordered(diabetes$Outcome)
diabetes$Pregnancies <- ordered(diabetes$Pregnancies)
diabetes <- diabetes[diabetes$BloodPressure>=20,]
names(diabetes)[names(diabetes) == "BloodPressure"] <- "BP"

library(semtree)
library(lavaan)
model <- "BP ~~ BP
           BP ~ 1"
fitted_model <- sem(model, diabetes)
summary(fitted_model)


tree <- semtree(fitted_model, diabetes, semtree_control(method="score"))

plot(prune(tree,2))

tt<- toTable(prune(tree,2))

di2 <- diabetes
di2$grp <- 0
di2$grp[di2$Age<26.5 & di2$BMI<39.25]<-1 # ADIPOSITAS II -> III
di2$grp[di2$Age>=26.5 & di2$BMI<39.05]<-2
di2$grp[di2$Age<26.5 & di2$BMI>=39.25]<-3
di2$grp[di2$Age>=26.5 & di2$BMI>=39.05]<-4
di2$grp <- factor(di2$grp)
library(ggplot2)
ggplot2::ggplot(di2, aes(group=grp,fill=grp,y=BP,x=grp))+
  geom_violin()+geom_boxplot(width=.3)+
  scale_fill_discrete(name="Leaf")+xlab("")


sf<-semforest(fitted_model, diabetes, 
              control=semforest_score_control(num.trees=20))

vi <- semtree::varimp(sf)

plot(vi)

library(future)
plan(multisession, workers = 4)
plan(sequential)
boruta_result<-boruta(fitted_model,
                      diabetes, 
                      semforest_score_control(num.trees=30),
                      nruns=11)

# save everything
dir.create("results/ex1")
saveRDS(tree, file="results/ex1/tree.rds")
saveRDS(vi, file="results/ex1/vi.rds")
saveRDS(sf, file="results/ex1/sf.rds")
saveRDS(boruta_result, file="results/ex1/boruta_result.rds")
