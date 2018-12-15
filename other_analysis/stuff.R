country.df = list()
model2.coef= list()
for (j in 1:4){ # change to 1:5 to add korea snu
  country.df[[j]] = df %>% filter(origin==country_list[j]) %>% select(origin,case,t.estrogen_names,age,bmi,alcohol,smoking,relativegc,education)
  model2.coef[[j]] = data.frame(origin=country_list[j],estrogen = c(estrogen_names),estimate=rep(NA,15), variance=rep(NA,15), lower = rep(NA,15), upper = rep(NA,15))
  
  if (j==1){ range=c(1,2,4:15)} else{ range=1:15  }
  for (k in range){ # gets estimate for each estrogen
    if(j==1){   # model for germany
      model = summary(glm(as.formula(paste0("case", " ~ ", (paste(t.estrogen_names[k], "age","bmi","smoking", sep="+")))), data=country.df[[j]],  family=binomial(link="logit")))$coef
    }else if (j==2){ # model for japan
      model = summary(glm(as.formula(paste0("case", " ~ ", (paste(t.estrogen_names[k], "age","bmi","smoking","relativegc", sep="+")))), data=country.df[[j]],  family=binomial(link="logit")))$coef 
    }else if (j==3){ # model for korea (kmcc)
      model = summary(glm(as.formula(paste0("case", " ~ ", (paste(t.estrogen_names[k], "age","bmi","smoking","education", sep="+")))), data=country.df[[j]],  family=binomial(link="logit")))$coef 
    }else if (j==4){ # model for iran
      model = summary(glm(as.formula(paste0("case", " ~ ", (paste(t.estrogen_names[k], "age","bmi","smoking","education", sep="+")))), data=country.df[[j]],  family=binomial(link="logit")))$coef 
    }else if (j==5){
      model = summary(glm(as.formula(paste0("case", " ~ ", (paste(t.estrogen_names[k], "age","bmi","smoking","relativegc","education", sep="+")))), data=country.df[[j]],  family=binomial(link="logit")))$coef  
    }
    model2.coef[[j]]$estimate[k] = model[2,1]
    model2.coef[[j]]$variance[k] = (model[2,2])**2
    model2.coef[[j]]$lower[k] = model[2,1]-1.96*model[2,2]
    model2.coef[[j]]$upper[k] = model[2,1]+1.96*model[2,2]
  }
}