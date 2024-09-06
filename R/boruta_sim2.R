library(OpenMx)#
library(semtree)

N <- 1000
x1 <- sample(c(0,1,2),size=N, TRUE)
x2 <- sample(c(0,1,2),size=N, TRUE)
x3 <- sample(c(0,1,2),size=N, TRUE)
x4 <- sample(c(0,1,2),size=N, TRUE)
x5 <- sample(c(0,1,2),size=N, TRUE)
x6 <- sample(c(0,1,2),size=N, TRUE)
y <- x1+5*x2+0.1*x6+rnorm(N,0,0.01)

tree.data <- data.frame(y,x1=ordered(x1),x2=ordered(x2),
                        x3=ordered(x3),x4=ordered(x4),x5=ordered(x5),
                        x6=ordered(x6))

set.seed(2349)

manifests<-c("y")
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
                        free=c(FALSE), value=c(1.0) , 
                        arrows=2, 
                        label=c("sigma2") 
                 ),
                 mxData(tree.data, type = "raw")
);
fitted <- mxRun(model)
ctrl = semtree.control(
  method="score", 
  bonferroni = TRUE)
tree = semtree( model = model, 
                data = tree.data, 
                control=ctrl)


tctrl <- semforest.control(control = semtree.control(method="score",alpha=0.05))

library(future)
plan(multisession, workers = 4)
boruta_result <- semtree::boruta(model=fitted,
                                 control=tctrl, 
                                 data = tree.data)

boruta_result

semtree:::plot.boruta(boruta_result)

dir.create("results/sim2")
saveRDS(boruta_result, file="results/sim2/boruta_result.rds")
