#plots

#correlation

library('corrplot')

df <- read.csv('C:/Users/user/Desktop/1/correlation.csv', sep=',')
sub_df = subset(df, select = -c(X) )
corrmatr <- cor(sub_df)
corr_mtest <- cor.mtest(sub_df, conf.level = .99)
corrplot(corrmatr, p.mat = corr_mtest$p, insig = "blank")

#tree1

loaddata <- read.csv('C:/Users/user/Desktop/1/binary.csv', sep=',', encoding = 'UTF-8')

library(rpart)
library(rpart.plot)
tree1 <- rpart(VERB~A1.anim+GOAL+PART+A2.anim+FIN+INF+GERUND, data=Tloaddata)

rpart.plot(tree1, uniform=TRUE,
           main="Classification Tree")
text(tree1, use.n=T, all=T, cex=.7)

#tree2

library(party)
tree2 <- ctree(VERB~A1.anim+GOAL+PART+A2.anim+FIN+INF+GERUND, data=loaddata)
plot(tree2)

# tree3
library(party)
tree3 <- ctree(VERB~A1.anim+GOAL+PART+A2.anim, data=loaddata)
plot(tree3)

#forest

fit_forest <- randomForest(factor(VERB)~., data=loaddata, importance = TRUE)
importance(fit_forest)
varImpPlot(fit_forest, type=2)

# accuracy

t_pred1 = predict(tree1,subset(loaddata, select = -c(VERB)),type="class")
t_pred2 = predict(tree2,subset(loaddata, select = -c(VERB)),type="response")
t_pred3 = predict(tree3,subset(loaddata, select = -c(VERB)),type="response")

cmatrix1 <- table(loaddata$VERB,t_pred1)
cmatrix2 <- table(loaddata$VERB,t_pred2)
cmatrix3 <- table(loaddata$VERB,t_pred3)

accuracy1 <- sum(diag(cmatrix1))/sum(cmatrix1)
accuracy2 <- sum(diag(cmatrix2))/sum(cmatrix2)
accuracy3 <- sum(diag(cmatrix3))/sum(cmatrix3)