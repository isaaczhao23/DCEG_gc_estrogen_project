


setwd("~/Desktop/nci_summer_project/project")
load("./data/df_all.RDA")

library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggpubr)


estrogen_names = c("E1", "E2", "4OHE1", "4ME1","4ME2","2OHE1",
	"3ME1" ,"2OHE2","2ME1","2ME2","16aE1","17epiE3","E3","16KE2","16epiE3") 


data = df %>% select(origin, case, age, bmi, alcohol, smoking, relativegc, education, paste0("t.",estrogen_names)) %>%
select(-t.4OHE1) %>% na.omit()


a = ggplot(data %>% select(case,age,bmi) %>% 
		gather(variable,value,-case) %>% mutate(case = ifelse(case==1,"Case","Control")),
	aes(case,value))+
	geom_boxplot(fill="lightblue") +
	stat_compare_means(aes(group = case), method="t.test")+
	theme_bw()+
	ylab("")+
	xlab("")+
	facet_wrap(~variable,scales="free")

b = ggplot(data %>% select(case,alcohol,smoking,relativegc) %>% 
		gather(variable,value,-case) %>% mutate(case = ifelse(case==1,"Case","Control"))%>%
			filter(value != 9999),
	aes(case))+
	geom_bar(aes(fill=value),position="fill",color="black") +
	  scale_fill_brewer("", labels=c("No","Yes"))+
	theme_bw()+
	scale_y_continuous("",labels=scales::percent)+
	xlab("")+
	facet_wrap(~variable,scales="free")

ggarrange(a,b,widths=c(1,2))
	


data[sapply(data,function(x) !is.numeric(x))] = sapply(data[sapply(data,function(x) !is.numeric(x))], make.names)
# data$origin = as.factor(data$origin)
# data$case = as.factor(data$case)
# data$alcohol = as.factor(data$alcohol)
# data$smoking = as.factor(data$smoking)
# data$relativegc = as.factor(data$relativegc)
# train the model
algorithms = data.frame(model = c("CART","Logistic Regression",
	"Bagged CART","Random Forest",
	"Boosted Logistic Regression","AdaBoosted Tree"),
	code =  c("rpart","glm","treebag","rf","LogitBoost","adaboost"))

model=list()
for(i in 1:nrow(algorithms)){
set.seed(1)
	# can change method to "boot" for bootstrap. use number=100
model[[i]] <- train(case~., data=data, 
	trControl=trainControl(method="cv", number=10, allowParallel=TRUE,savePredictions = T,
		classProbs=T), 
	method=algorithms$code[i])
	# trControl=trainControl(method="cv", number=10, allowParallel=TRUE,
	# 	classProbs=T, summaryFunction=twoClassSummary, savePredictions = T), 
	# method=algorithms$code[i], metric="ROC")
}
names(model) = algorithms$model


color_df = data.frame(model = algorithms$model, color_label = c(rep("Neither",2),
	rep("Bagging",2),rep("Boosting",2)))

 results=resamples(model)$values %>% select(contains("Accuracy"))
 colnames(results) = algorithms$model
 accuracy = results %>% gather(model,accuracy) %>% inner_join(color_df,by="model")
 
 stats = accuracy %>% group_by(model) %>%
  	dplyr::summarize(accuracy_mean = mean(accuracy), accuracy_sd = sd(accuracy)) %>% arrange(accuracy_mean) %>%
 	inner_join(color_df,by="model")
 # order in graph by mse
 accuracy$model = factor(accuracy$model,levels=rev(stats$model))
# graph mse
    ggplot(accuracy, aes(model,accuracy)) +
	geom_boxplot(aes(fill=color_label),color="black",alpha=0.7)+
 	#geom_errorbar(aes(ymin=MSE_mean-MSE_sd, ymax=MSE_mean+MSE_sd), width=.2)+
	scale_y_continuous("Accuracy",pretty(accuracy$accuracy, n = 10)) +
	xlab("")+
	scale_fill_brewer("",palette="Blues")+
	theme_classic()+
 	ggtitle("Tertile Estrogen + Demographics")+
	theme(legend.position="bottom",
		axis.text.x = element_text(angle = 30, hjust = 1,face="bold"))
