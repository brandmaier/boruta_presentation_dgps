
library(semtree)
library(OpenMx)


library(psychTools)
data(affect)

affect$Film <- factor(affect$Film, ordered = FALSE, 
                      labels=c("Frontline", "Halloween", "Nat. Geographic","Parenthood"))


tree.data <- affect[,c("Film","neur","ext","soc","traitanx","NA1","PA1")] 
tree.data$DeltaPA <- affect$PA2-affect$PA1

#knitr::kable(head(tree.data))


manifests<-c("DeltaPA")
latents<-c()
model <- mxModel("Simple Model", 
                 type="RAM",
                 manifestVars = manifests,
                 latentVars = latents,
                 mxPath(from="one",to=manifests, free=c(TRUE), value=c(1.0) , arrows=1, label=c("mu") ),
                 mxPath(from=manifests,to=manifests, free=c(TRUE), value=c(1.0) , arrows=2, label=c("sigma2") ),
                 mxData(tree.data, type = "raw")
);

result <- mxRun(model)
summary(result)

ctrl <- semtree_control(method="score",min.N=100)

ctrl_forest <- semforest_control(control=ctrl, num.trees=50)

tree = semtree( model = result, 
                data = tree.data, 
                control=ctrl)

boruta_result <- boruta( model = result, 
                         data=tree.data,
                         control = ctrl_forest,
                         maxRuns = 11)

library(dplyr)
library(ggplot2)
semtree:::plot.boruta(boruta_result,type=1) #+ ggplot2::ylim(-20, 100)

dir.create("results/ex2")
saveRDS(tree, "results/ex2/tree.rds")
