# Codigos, shorcuts y demases confeccionado y mantenido por Joshua Kunst
# jbkunst@gmail.com
# SET MEMORY TO USE

# Recuerde que es SIEMPRE recomendable tener la ?ltima versi?n de
# softwares (R, RStudio) y librer?as (ggplot2, pylr, etc) 

# 2012 09 25: - Change name ScaleProbToScore to ScoreScaleProb
#             - Creation of ScoreBenchamark function needs (data, response, scores, split.var (optional))

library(plyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(lubridate)
library(stringr)
library(ROCR)
library(rJava)
#library(xlsx)
library(rstudio)
library(sqldf)

h <- utils::head
n <- base::names
  


options(stringsAsFactors = FALSE)
options("repos"="http://dirichlet.mat.puc.cl/")

# LoadPackages
LoadPackages <- function(){
  cran.packages <- c("xtable","sqldf","ROCR","RODBC","ggplot2","grid","googleVis","rjson",
                    "gridExtra","xlsx","foreign","scales","plyr","sas7bdat","RColorBrewer",
                     "lubridate")
  # paste(cran.packages, collapse= ",")  
  for(p in cran.packages) { 
  if(!suppressWarnings(require(p, character.only = TRUE, quietly = TRUE))) {
      cat(paste(p, "missing, will attempt to install\n"))
  install.packages(p, dependencies = T, type = "source")
  } else {
      cat(paste(p, paste(rep(" ", times  = 10-nchar(p)), collapse=""), "installed OK\n"))
    }
  }
  rm(p,cran.packages)
}

# FUNCTIONS TO VALIDATION SCORECARDS
SummaryScorecard <- function(score,good.label){ 
  
  if(!is.numeric(good.label)) good.label <- as.numeric(good.label)-1
  
  res <- c(N = length(score),
           N.good = length(score[good.label == 1]),
           N.bad = length(score[good.label == 0]),
           BR = length(score[good.label == 0])/length(score),
           KS(score,good.label),
           AUCROC(score,good.label),
           Gini(score,good.label),
           #Divergence(score,good.label),
           Gain = Gain(score,good.label))
  res <- data.frame(t(res))
  names(res) <- gsub("\\.", "", names(res))
  res
}
  
KS <- function(score, good.label){
  library(ROCR)
  pred <- prediction(score,good.label)
  perf <- performance(pred,"tpr","fpr")
  KS <- max(abs(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]]))
  return(c(KS = as.numeric(KS)))
}

AUCROC <- function(score, good.label){
  library(ROCR)
  pred <- prediction(score,good.label)
  perf <- performance(pred,"tpr","fpr")
  auc <- attr(performance(pred,"auc"),"y.values")[[1]]
  return(c(AUCROC = auc ))
}

Gini <- function(score, good.label){
  return( c(GINI = 2*as.numeric(AUCROC(score, good.label)) - 1))
}

Divergence <- function(score, good.label){
  s.good <- score[good.label == 1]
  s.bad <- score[good.label == 0]
  return(c(DIVERGENCE = (mean(s.good) - mean(s.bad))^2/(var(s.good) + var(s.bad))*2))
}

Gain <- function(score, good.label, percents = c(0.10, 0.20, 0.30, 0.40, 0.50)){
  g <- ecdf(score[good.label==0])(quantile(score,percents,na.rm=T))
  names(g) <- percent(percents)
  g
}

OddsTable <- function(score, good.label, min = min(score), max = max(score), cuts = NULL,
                      nclass = 10, round = 0, quantile = T, format.2 = T){
  if(missing(cuts) & quantile){
    cuts <- unique(round(quantile(score, seq( 0, 1, length = nclass + 1)), digits=round))
  } 
  if(missing(cuts) & !quantile) {
    cuts <- unique(round(seq( from = min, to = max, length = nclass + 1), digits=round))
  }
  
  t <- table(cut(score, cuts, include.lowest=T), good.label)
  t <- t[(length(cuts)-1):1,]
  nclass <- dim(t)[1]
  N <- sum(t)
  t2 <- data.frame(class  = row.names(t),
                   n      = (t[,1]+t[,2]),
                   p      = (t[,1]+t[,2])/N,
                   p_acum = cumsum((t[,1]+t[,2])/N),
                   p_desacum  = c(1,((sum(t[,1]+t[,2])-cumsum(t[,1]+t[,2]))/N)[1:(nclass-1)]),
                   n_bad  = t[,1],
                   p_bad  = t[,1]/sum(t[,1]),
                   p_bad_acum = cumsum(t[,1]/sum(t[,1])),
                   p_bad_desacum = c(1,((sum(t[,1])-cumsum(t[,1]))/sum(t[,1]))[1:(nclass-1)]),
                   br     = t[,1]/(t[,1]+t[,2]),
                   br_acum= cumsum(t[,1])/cumsum((t[,1]+t[,2])),
                   br_desacum = c((cumsum(t[,1])/cumsum((t[,1]+t[,2])))[nclass],((sum(t[,1])-cumsum(t[,1]))/(sum(t[,1]+t[,2])-cumsum(t[,1]+t[,2])))[1:(nclass-1)]),
                   odds   =  t[,2]/ t[,1])
  rownames(t2) <- NULL

  #min <- min(score)
  #max <- max(score)
    
  if(format.2) t2$class <- paste(c(1,cuts[2:(nclass)]+1),c(cuts[2:nclass],999), sep = "-")[nclass:1]
   
  list(oddstable = t2, cuts = cuts)
}


CreateData <- function(n.size = 5000, br = 0.3, noise = 0.) {
  
  n.noise <- round(n.size*noise)
  n.size <- n.size - n.noise
  
  good.label <- rbinom(n=n.size, size=1, prob=1-br)
  score <- 1000*invlogit(rnorm(n.size) + good.label - .5)
    
  DistChart(score,good.label)
  data <- rbind(data.frame(score = score, good.label),
                data.frame(score = runif(n.noise), good.label = rbinom(n=n.noise, size=1, prob=.5)))
  
  data$score <- round(data$score)
  
  return(data)
}


CreateDataReport <- function(clean.rate.lims = c(.1,.4), n.size = 50000,
                             until = Sys.Date(), n.per = 12) {
  library(lubridate)
  dates <- seq(until, length = n.per, by = "-1 month")
  day(dates) <- 01
  
  clean.rates <- as.vector(arima.sim(n = n.per, list(ar = c(0.8))))
  clean.rates <- clean.rates - min(clean.rates)
  clean.rates <- clean.rates/max(clean.rates)
  clean.rates <- clean.rates*abs(diff(clean.rate.lims)) + min(clean.rate.lims)
   
  data <- ldply(1:n.per, function(id){
    cbind(FECHA_CALC = dates[id], CreateData(n.size=n.size, br=clean.rates[id]))
    })
  
  names(data)[2:3] <-  c("SCORE","CLEAN")
  
  return(data)
}


# CHART FUNCTIONS
SmoothChart <- function(variable, label, ...){
  daux <- data.frame(variable, label)
  p <- ggplot(daux, aes(variable,1-label))
  p <- p + scale_y_continuous("Proporcion de casos", labels = percent_format(), limits = c(0, 1), ...)
  p <- p + stat_smooth(fill = "red", colour = "darkred", size = 1.0, alpha = 0.25, se = T)
  p <- p + geom_point(aes(variable,label*0), shape = 20)
  p
}

# BRChart <- function(variable, label, n.cats = 10, br.size = 4,...){
#  
#   t <- TableBivariate(CatVar(variable, n.cats=n.cats), label)
#   
#   p <- ggplot(t, aes(x = variable)) +  geom_bar(aes(y =Percent))
#   p <- p + labs(x = paste("Categorias\n", "(n = ",prettyNum(length(variable), big.mark = "."), ")", sep = "" ), fill = NULL)
#   p <- p + scale_y_continuous('Proporcion de casos', labels = percent_format() )
#   p <- p + geom_line(aes(x = Id, y = BadRate), colour = "red", size = 1)
#   p <- p + geom_point(aes(x = Id, y = BadRate), colour = "darkred", size = 3)
#   p <- p + geom_text(aes(x = Id, y = BadRate, label = percent(BadRate)), colour = "red", size = br.size, hjust = 0, vjust = -1)
#   p
# }

BRChart <- 
  function(variable, gb.label, gb.label2 = NA, gb.label3 = NA,n.cats = 10, br.size = 4,acum = F,...){
    
    variable <- CatVar(variable, n.cats=n.cats)
    
    daux <- ddply(data.frame(variable,gb = gb.label, gb2 = gb.label2, gb3 = gb.label3),.(variable), summarise,
                  BadRate = (length(gb)-sum(gb))/length(gb),
                  BadRate2 = (length(gb2)-sum(gb2))/length(gb2),
                  BadRate3 = (length(gb3)-sum(gb3))/length(gb3),
                  Percent = length(gb))
    
    daux$Id <- 1:nrow(daux)
    daux$Percent <- daux$Percent/length(gb.label)
    daux$Acum <- cumsum(daux$Percent)
    
    
    t <- daux
    
    p <- ggplot(t, aes(x = variable)) +  geom_bar(aes(y =Percent))
    p <- p + labs(x = paste("Categorias\n", "(n = ",prettyNum(length(variable), big.mark = "."), ")", sep = "" ), fill = NULL)
    p <- p + scale_y_continuous('Proporcion de casos', labels = percent_format() )
    p <- p + geom_line(aes(x = Id, y = BadRate), colour = "red", size = 1)
    p <- p + geom_point(aes(x = Id, y = BadRate), colour = "darkred", size = 3)
    p <- p + geom_text(aes(x = Id, y = BadRate, label = percent(BadRate)), colour = "red", size = br.size, hjust = 0, vjust = -1)
    
    if(is.na(gb.label2)==F){
    p <- p + geom_line(aes(x = Id, y = BadRate2), colour = "orange", size = 1,hjust = 0, vjust = -2)
    p <- p + geom_point(aes(x = Id, y = BadRate2), colour = "darkorange", size = 3,hjust = 0, vjust = -2)
    p <- p + geom_text(aes(x = Id, y = BadRate2, label = percent(BadRate2)), colour = "darkorange", size = br.size, hjust = 0, vjust = -0.8)
    }
    
    if(is.na(gb.label3)==F){
      p <- p + geom_line(aes(x = Id, y = BadRate3), colour = "darkgreen", size = 1,hjust = 0, vjust = -4)
      p <- p + geom_point(aes(x = Id, y = BadRate3), colour = "darkgreen", size = 3,hjust = 0, vjust = -4)
      p <- p + geom_text(aes(x = Id, y = BadRate3, label = percent(BadRate3)), colour = "darkgreen", size = br.size, hjust = 0, vjust = -0.8)
    }
    
    if(acum == T){
      p <- p + geom_line(aes(x = Id, y = Acum), colour = "orange", size = 1,hjust = 0, vjust = -2)
      p <- p + geom_point(aes(x = Id, y = Acum), colour = "darkorange", size = 3,hjust = 0, vjust = -2)
      p <- p + geom_text(aes(x = Id, y = Acum, label = percent(round(Acum,2))), colour = "darkorange", size = br.size, hjust = 0, vjust = -0.8)
      
    }
    
    p
  }

BRBubble <- function(var.x, var.y, gb.label){
  daux <- data.frame(X = var.x, Y = var.y, GB = gb.label)
  daux <- ddply(daux, .(X, Y), summarize, BadRate = 1-mean(GB), Size = length(GB))
  
  p <- ggplot(daux) + geom_point(aes(x=X, y = Y, colour = BadRate, size = Size)) +
    scale_colour_gradient2(low = "green", high = "red") + scale_area(range=c(10,60)) +
    geom_text(aes(x=X, y = Y, label=percent(BadRate)), size = 4.5, vjust = -1) + 
    geom_text(aes(x=X, y = Y, label=prettyNum(Size, big.mark = ".")), size = 4.5, vjust = 1) +
    xlab(NULL) + ylab(NULL) 
  p
}

BRHeatMap <- function(var.x, var.y, good.label){

  daux <- ddply(data.frame(var.x, var.y, good.label),
              .(var.x, var.y), summarise, BR = (1-mean(good.label)))
  
  p <- ggplot(daux, aes(var.x, var.y, fill = BR, label = percent(BR))) + 
    geom_tile() + scale_fill_gradient2(low = "blue", high = "red") + geom_text() +
    xlab(NULL) + ylab(NULL)
  p
}


DistChart <- function(score, good.label, ...) {
  Type <- as.factor(good.label)
  levels(Type) <- c("Bad","Good")
  daux <- data.frame(Score = score, Type)
  
  p <- ggplot(daux, aes(Score, fill = Type)) + geom_density(alpha=0.60, size = 0.8) + labs(fill = "Type")
  p <- p + scale_fill_manual(values = c("red","blue"))
  p  
}

KSChart <- function(score, good.label, class = 500 , ...) {
  # 2012-09-07: Draw the segment where occurs the max separation
  ecd.bad <- ecdf(score[good.label == 0])
  ecd.good <- ecdf(score[good.label == 1])
  cuts <- round(seq(min(score), max(score), length.= class))
  
  df <- data.frame( score = c(cuts,cuts),
                    ecd = c(ecd.good(cuts),ecd.bad(cuts)),
                    Type = as.factor(rep( c("Good","Bad"), length = 2*length(cuts), each = length(cuts))))
  
  cut <- cuts[abs(ecd.good(cuts) - ecd.bad(cuts)) == max(abs(ecd.good(cuts) - ecd.bad(cuts)))]  
      
  p <- ggplot(df, aes(score, ecd, colour = Type))  + geom_line(size = 1.2) + labs(x = "Score", y = "ECDF")
  p <- p + scale_colour_manual(values=c("red","blue"))
  #p <- p + geom_segment(aes(x = x, y = y), colour = grey(.6), size = 1.0)
  p
}

GainChart <- function(score, good.label, n.breaks = 50, ...){

  df <- data.frame(percentiles = seq(0, 1, length = n.breaks),
                   gain = Gain(score, good.label, seq(0, 1, length = n.breaks)))

  p <-  ggplot(df, aes(percentiles, gain))  + geom_line(size = 1.2, colour = "darkred")
  p <- p + geom_line(aes(x = c(0,1), y = c(0,1)), colour = "gray", size = 0.7)
  p <- p + scale_x_continuous("Sample Percentiles", labels = percent_format(), limits = c(0, 1))
  p <- p + scale_y_continuous("Cumulative Percents of Bads", labels = percent_format(), limits = c(0, 1))
  p
}

ROCChart <- function(score, good.label, ...){
  pred <- prediction(score,good.label)
  perf <- performance(pred,"tpr","fpr")
  auc <- attr(performance(pred,"auc"),"y.values")[[1]]
  
  df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  
  p <- ggplot(df, aes(x, y))  + geom_line(size = 1.2, colour = "darkred")
  p <- p + geom_path(data= data.frame(x = c(0,1), y = c(0,1)), colour = "gray", size = 0.7)
  p <- p + scale_x_continuous("False Positive Rate (1 - Specificity)", labels = percent_format(), limits = c(0, 1))
  p <- p + scale_y_continuous("True Positive Rate (Sensivity or Recall)", labels = percent_format(), limits = c(0, 1))
  p
}

SummaryScorecardPlot <- function(score,good.label, save.plots = F, prefix = "Score", extension = "png",...){
  library(gridExtra)
  
  if(!is.numeric(good.label)) good.label <- as.numeric(good.label)-1
  
  p1 <- DistChart(score,good.label)
  p2 <- KSChart(score,good.label)
  p3 <- GainChart(score,good.label)
  p4 <- ROCChart(score, good.label)
  p5 <- SmoothChart(score, good.label) + labs(x = "Score") +
    scale_y_continuous("Bad Rate",labels = percent_format(), limits = c(0, 1))
  p6 <- BRChart(score, good.label, n.cats=5) + labs(x = "Score")
  
  if(save.plots){
    ggsave(plot=p1, filename=paste(prefix,"_DistChart.",extension, sep = ""),...)
    ggsave(plot=p2, filename=paste(prefix,"_KSChart.",extension, sep = ""),...)
    ggsave(plot=p3, filename=paste(prefix,"_GainChart.",extension, sep = ""),...)
    ggsave(plot=p4, filename=paste(prefix,"_ROCChart.",extension, sep = ""),...)
    ggsave(plot=p5, filename=paste(prefix,"_SmoothChart.",extension, sep = ""),...)
    ggsave(plot=p6, filename=paste(prefix,"_BRChart.",extension, sep = ""),...)
  }
  grid.arrange(p1, p3, p5, p2, p4, p6, ncol = 3, ...)
}


Barplot <- function(variable, values = T, sort.by.count = F, color = "darkred", transpose = F, ...){
  variable <- as.factor(variable)
  if(any(is.na(variable))){
    variable <- addNA(variable)
  }
  if(sort.by.count){
    if(transpose){
      variable <- factor(variable, levels=names(sort(table(variable), decreasing=F)))
    } else {
      variable <- factor(variable, levels=names(sort(table(variable), decreasing=T)))
    }  
  }
  daux <- data.frame(variable)
  taux <- table(data.frame(variable))
  
  p <- ggplot(daux, aes(x = variable))
  p <- p + geom_bar(aes(y = (..count..)/sum(..count..)), fill=color)
  p <- p + scale_y_continuous('Proporcion de casos', labels = percent_format())
  p <- p + labs(x = paste("(n = ",prettyNum(length(variable), big.mark = "."),")", sep = ""), fill = NULL)
  
  if(transpose) p <- p + coord_flip()
  
  if(values){
    frame <- data.frame(id = 1:length(levels(variable)),
                        cat = levels(variable),
                        n = prettyNum(as.vector(taux), big.mark = "."),
                        per = as.vector(prop.table(taux)),
                        label = paste(round(100*as.vector(prop.table(taux)),2), "%", sep = ""),
                        label2 = paste(paste(round(100*as.vector(prop.table(taux)),2), "%", sep = ""),
                                       paste0("(", gsub(" ", "", prettyNum(as.vector(taux), big.mark = ".")), ")")))
    
    
    
    
    if(transpose){
      p <- p + geom_text(aes(id, per, label = label2), data = frame, colour = "black", size = 4, hjust = -.1, vjust = 0)
    } else {
      p <- p + geom_text(aes(id, per, label = label), data = frame, colour = "black", size = 4, hjust = .5, vjust = -2.5)                            
      p <- p + geom_text(aes(id, per, label = n), data = frame, colour = "black", size = 3.8, hjust = .5, vjust = -1)
    }   
    p <- p + scale_y_continuous('Proporcion de casos', labels = percent_format(), limits = c(0,max(frame$per)+.07))
  }
  p 
}
Histogram <- function(variable, count = T, color = "#1E90FF",...){
  p <- qplot(variable, geom = "blank")
  if(count){
    p <- p + geom_histogram(aes(y = ..count..), fill = color, colour = "black",
                            binwidth=diff(range(variable))/30)
  } else {
    p <- p + geom_histogram(aes(y = ..density..), fill = color, colour = "black",
                            binwidth=diff(range(variable))/30) 
  }
  p + xlab(NULL) + ylab(NULL) 
}



Density <- function(variable, color = "blue", alpha = 0.6, ...){
   ggplot(data.frame(variable),aes(variable)) + geom_density(fill=color, alpha = alpha) +
     xlab(NULL) + ylab(NULL) 
}

ParetoChart <- function(variable, prop = T, ...){
  
  daux <- as.data.frame(if(prop) table(variable)/length(variable) else table(variable), stringsAsFactors=F)
  names(daux) <- c("category",if(prop) "percent" else "frequency")
  daux <- daux[order(daux[,2], decreasing=TRUE), ]
  daux$category <- factor(daux$category, levels=daux$category)
  daux$cum <- cumsum(daux[,2])
  
  p <- ggplot(daux, aes(x=category))
  if(prop){
    p <- p + geom_bar(aes(y=percent)) + scale_y_continuous(labels = percent_format())
  } else {
    p <- p + geom_bar(aes(y=frequency))
  }
  p <- p + geom_point(aes(y=cum)) + geom_path(aes(y=cum, group=1)) +
    labs(x = paste("(n = ",prettyNum(length(variable), big.mark = "."),")", sep = ""), fill = NULL)
  p
}

ModelGraphWeights <- function(model, exclude = c(""), sort.weigths = T,  ...){
  
  data <- ModelCoefficientsSummary(model)
  data <- subset(data, !Coefficient %in% c("(Intercept)", exclude), select = c("Coefficient","Estimate"))
  
  if(sort.weigths){
    p <- ggplot(data, aes(x=reorder(Coefficient, -Estimate)))
  } else {
    p <- ggplot(data, aes(x=Coefficient))
  }
  
  p <- p + geom_bar(aes(y=Estimate), fill="yellow2", colour="darkred",alpha = 0.65, size = .7) +
    xlab('Coefficient') + ylab('Value') + coord_flip()
  p
}
    



# FUNCTIONS TO UNIVARIATE ANALISYS
Univariate <- function(data, save.output = T, file.output = "Univariate.xlsx",
                       plots = F, save.plots = F, folder.plots = "plots",
                       return.results = F, quantile_sup_hist = 1){
  library(xlsx)
  library(plyr)

  pc <- ProcContent(data)
  pm <- ProcMeans(data)
  
  View(pc,"ProcContent")
  View(pm,"ProcMeans")
  
  if(save.output){
    print('Creating file')
    write.xlsx2(pc, file.output,  row.names = F, sheetName="proc_content")
    write.xlsx2(pm, file.output,  row.names = F, sheetName="proc_means", append=T)
  
    if(length(grep("RUT", names(data)))>1) data <- subset(data, select=names(data)[-grep("RUT", names(data))])
    
    print('Writing tables')
    llply(names(data)[!laply(data, is.numeric)],
          function(var){write.xlsx2(TablePercent(data[[var]]), file.output,  row.names = F, sheetName=var, append=T)},
          .progress = "text")
  }
  
  if(plots){
    faux <- function(var){
      if(!is.numeric(data[[var]]) | length(unique(data[[var]])) < 10){
        p <- Barplot(data[[var]], title = var)
      } else {      
        p <- Histogram(Truncate(data[[var]], sup=quantile(data[[var]], .95)), title = var)
      }
      return(p)
    }
    
    print('Calculating plots')
    plots <- llply(names(data), function(x) faux(x), .progress='text')
      
    if(save.plots){
      print('Saving plots')
      dir.create(folder.plots)
      ldply(names(data),
            function(x){ cat(x); ggsave(plot=faux(x)+ggtitle(x), filename = file.path(folder.plots,paste("uni_",x,".png", sep = "")), width=12, height=7)},
            .progress='text')
    }
  }
  if(return.results){
    return(list(proc_content = pc, proc_means = pm, plots = plots))
  } else {
    return(print("Done it!"))  
  }
 
}


# FUNCTIONS TO BIVARIATE ANALISYS
Bivariate <- function(data, response = 'GB', save.output = F, file.output = "Bivariate.xlsx",
                      plots = F, save.plots = F, folder.plots = "plots",
                      return.results = F,  n.cat = 10, dict = NULL){

  response.var <- data[[response]]
  data[[response]] <- NULL
  data <- subset(data, select = laply(data, function(x) length(unique(x)) > 1))
  
  f1aux <- function(namevar){
    cat(paste(namevar, paste(rep(" ", 10), collapse=" ")))
    model <- glm(response.var ~ data[[namevar]], family = binomial(link = logit))
    p.value <- anova.glm(glm(response.var ~ 1, family = binomial(link = logit)), model, test = "Chisq")[2,5]
    sign <- "c"
    if(is.numeric(data[[namevar]]))  sign <- if(model$coefficients[2]<0) "-" else "+" 
    data.frame(variable = namevar,
               KS = KS(predict(model, type = "response"), response.var),
               #IV = IV(data[[namevar]],response.var),
               p.value = p.value,
               signifcodes = SignifCodes(p.value),
               default.prop = f3aux(namevar),
               sign = sign)
  }
   
  f2aux <- function(namevar, response.var){
    var.aux <- data[[namevar]]
    if(is.numeric(var.aux) & length(unique(var.aux))>10){
      var.aux <- Truncate(var.aux, sup=quantile(var.aux,.9))
    }
    BRChart(var.aux, response.var, n.cat=n.cat) +
      ggtitle(paste(namevar, "\n(KS=", round(as.numeric(f1aux(namevar)$KS)*100,2), "%)", sep = ""))    
  }
  
  f3aux <- function(namevar){
    v <- data[[namevar]]
    if(is.numeric(v)) length(v[v==0])/length(v) else length(v[v==""])/length(v)
  }
  
  print('Calculating summary bivariate table')
  bivariate_ranking <- ldply(names(data), f1aux, .progress='text')
  if(!missing(dict)){
    print('Merging dicctionaty')
    bivariate_ranking <- merge(bivariate_ranking, dict, by.x = "variable", by.y = names(dict)[1], all.x=T)
  } 
  bivariate_ranking <- bivariate_ranking[order(bivariate_ranking$KS,decreasing=T),]
  
  print('Calculating bivariates tables')
  bivariate_tables <- ldply(names(data),
                            function(namevar) cbind(namevar = namevar, TableBivariate(data[[namevar]],response.var)),
                            .progress='text')
  
  View(bivariate_ranking,"bivariate_ranking")
  View(bivariate_tables,"bivariate_tables")
  
  
  if(save.output){
    library(xlsx)
    write.xlsx2(bivariate_ranking, file.output,  row.names = F, sheetName="bivariate_ranking")
    write.xlsx2(bivariate_tables, file.output,  row.names = F, sheetName="bivariate_tables", append=T)
  }
  
  if(plots){
    print('Calculating plots')
    plots <- llply(names(data), function(x) f2aux(x, response.var), .progress='text')
    
    if(save.plots){
      print('Saving plots')
      dir.create(folder.plots)
      ldply(names(data),
            function(x) ggsave(plot=f2aux(x, response.var), filename = file.path(folder.plots,paste("biv_",x,".png", sep = "")), width=12, height=7),
            .progress='text')
    }
  }
  
  if(return.results){
    return(list(bivariate_ranking = bivariate_ranking, bivariate_tables = bivariate_tables, plots = plots))
  } else {
    return(print("Done it!"))  
  }  
}

TableBivariate <- function(variable, gb.label){
  
  if(length(unique(variable))<=10){
    variable <- factor(variable)
  }
  if(flag <- is.numeric(variable)){
    cuts <- unique(quantile(variable, c(0,.10,.25,.50,.75,.90,1)))
    variable <- cut(variable, breaks=cuts, include.lowest=T)
    levels(variable) <- PrettyLevels(levels(variable))
  }
  variable <- as.factor(variable)
  
  if(any(is.na(variable))) variable <- addNA(variable)
  
  daux <- ddply(data.frame(variable,gb = gb.label),.(variable), summarise,
                N_Good = sum(gb),
                N_Bad = length(gb)-sum(gb),
                N_Total = length(gb),
                BMratio = sum(gb)/(length(gb)-sum(gb)),
                MBratio = (length(gb)-sum(gb))/sum(gb),
                P_Good = sum(gb),
                P_Bad = length(gb)-sum(gb),
                WOE = 0,
                BadRate = (length(gb)-sum(gb))/length(gb),
                Percent = length(gb))
  daux$P_Good <- daux$P_Good/sum(daux$P_Good)
  daux$P_Bad <- daux$P_Bad/sum(daux$P_Bad)
  daux$WOE <- ifelse(daux$P_Bad*daux$P_Good == 0, 0, log(daux$P_Good/daux$P_Bad))
  daux$Percent <- daux$Percent/length(gb.label)
  daux$Id <- 1:nrow(daux)
  daux
}

IV <- function(variable, good.label){
  t <- TableBivariate(variable, good.label)
  IV <- sum((t$P_Good-t$P_Bad)*t$WOE)
  IV
}

GroupVariable <- function(response, variable, name.output, ...){
  
  library(party)
  library(plyr)
  
  if(!is.numeric(variable)) variable <- as.factor(variable)
  
  daux <- data.frame(response = response, variable = variable)
  
  tree <- ctree(factor(response)~variable, data = daux, ...)  
  plot(tree)
  
  daux$newvar <- factor(tree@where)
  levels(daux$newvar) <- paste0("CLUS_", 1:length(unique(tree@where)))
  
  d <- as.data.frame.matrix(table(daux$variable,daux$newvar))

  if(is.numeric(variable)){
    res <- llply(d, function(x) c(min = min(as.numeric(rownames(d)[x!=0])),
                                  max = max(as.numeric(rownames(d)[x!=0]))))
    
    cod <- ldply(res, function(x) data.frame(x[[1]],x[[2]]))
    names(cod) <- c("Cluster", "Minima", "Maxima")
    
  } else{
    res <- llply(d, function(x) rownames(d)[x!=0])
    cod <- ldply(d, function(x) data.frame(rownames(d)[x!=0]))
    names(cod) <- c("Cluster", "Element")
    
  }
  
  if(!missing(name.output)) ExportTable(cod, name.output)
  
  list(newvar = daux$newvar, cod = cod, cod2 = res)
}


SignifCodes <- function(pvalue){
  # 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  pvalue[is.na(pvalue)] <- 1
  codes <- rep('',length(pvalue))
  codes <- ifelse(pvalue > 0.1   & pvalue <= 1    , ' '   , codes)
  codes <- ifelse(pvalue > 0.05  & pvalue <= 0.1  , '.'   , codes)
  codes <- ifelse(pvalue > 0.01  & pvalue <= 0.5  , '*'   , codes)
  codes <- ifelse(pvalue > 0.001 & pvalue <= 0.01 , '**'  , codes)
  codes <- ifelse(pvalue >= 0    & pvalue <= 0.001, '***' , codes)
  codes  
}

ConfMatrix <- function(true.values, predictions) {
  t <- table( true = true.values, prediction = predictions)
  # http://www2.cs.uregina.ca/~dbd/cs831/notes/confusion_matrix/confusion_matrix.html
  #                     Prediction
  #                 NegPred   PosPred
  # real NegOutcome
  # real PosOutcome
  
  AC <- sum(diag(t))/sum(t) #Accuracy (AC) is the he proportion of the total number of predictions that were correct.
  TP <- t[2,2]/sum(t[2,])   #Recall or true positive rate (TP) is the proportion of positive cases that were correctly identified. (BB)
  FP <- t[1,2]/sum(t[1,])   #False positive rate (FP) is the proportion of negatives cases that were incorrectly classified as positive
  TN <- t[1,1]/sum(t[1,])   #True negative rate (TN) is defined as the proportion of negatives cases that were classified correctly (MM)
  FN <- t[2,1]/sum(t[2,])   #False negative rate (FN) is the proportion of positives cases that were incorrectly classified as negative
  P <- t[2,2]/sum(t[,2])    #Precision (P) is the proportion of the predicted positive cases that were correct
  return(list(confusion.matrix = t,
              Accuracy = AC,
              "True Positive rate (BB)" = TP,
              "False Positive rate" = FP,
              "True Negative rate (MM)" = TN,
              "False Negative rate" = FN,
              Precision = P))
}



PSI <- function(ori_var, new_var, ncats = 5, suffixes=c("_ori","_new")){
#   
#   if(is.numeric(ori_var) & length(unique(ori_var)) > ncats
#      if(is.numeric(ori_var) & length(unique(ori_var)) > ncats)
#        breaks= unique(seq(min(variable),max(variable),length.out=n.cat+1))
    
     levs <- levels(ori_var)
     t <- merge(data.frame( levels = levels(ori_var), n = as.vector(table(ori_var)), p = prop.table(as.vector(table(ori_var)))),
                data.frame( levels = levels(new_var), n = as.vector(table(new_var)), p = prop.table(as.vector(table(new_var)))),
                by = "levels", all.x = T, suffixes=suffixes)
     t  
}

DiffVectors <- function(v1,v2, length.out = 10, base = exp(1),tittle1="Var 1",tittle2="Var 2"){
  difs <- abs(v1-v2)
  cuts <- unique(round(c(0,base**seq(logb(1,base),logb(max(difs)), length.out=length.out))))
  t <- ldply(cuts, function(c) c("Cut<=" = c,N = sum(difs<=c)), .progress='text')
  t$Percent <- t$N/length(difs)
  
  library(gridExtra)
  grid.arrange(Histogram(v1-v2, count=F) + labs(title = paste0("Diferencia", "\n",tittle1," menos ",tittle2)),
               Histogram(abs(v1-v2), count=F) + labs(title = "Diferencia absoluta"))

  return(list(summary=summary(difs), tablesummary = t))
}
 


CompareScores <- function(good.label, lift = T,...){
  # the "..." arguments must be scores, the first score is the reference to calculate lifts
  arglist <- match.call(expand.dots = FALSE)$...
  var.names <- sapply(arglist, deparse)
  arglist <- lapply(arglist, eval.parent, n = 2)
  # For each score obtain the indicators
  list <- lapply(arglist, function(x) SummaryScorecard(x, good.label))
  # The list turn to data.frame
  out <- ldply(list, data.frame)
  names(out)[1] <- "SCORE"
  
  # Calculating lifts
  if(lift){
    nc <- ncol(out)
    nr <- nrow(out)
    out2 <- data.frame(matrix(0, nrow = (2*nr)-1, ncol = nc))
    out2[c(1,seq(2:nr)*2),] <- out
    out2[seq(2:nr)*2+1,2:nc] <- (out[2:nr,2:nc] - out[rep(1,nr-1),2:nc])/out[rep(1,nr-1),2:nc]
    out2[seq(2:nr)*2+1,1] <- paste("LIFT", out2[(seq(2:nr)*2),1])
    names(out2) <- names(out)
    out <- out2
  }
  
  out
}


# MODEL AND SCORECARDS FUNCTIONS
ModelPredictiveVariables <- function(model){
  list(predvars = unlist(strsplit(as.character(formula(model))[3], " \\+ ")),
       response = as.character(formula(model))[2])
}

ModelCoefficientsSummary <- function(model){ 
  coeffs <- data.frame(summary(model)$coefficients)
  coeffs$sigcodes <- SignifCodes(coeffs[,4])
  coeffs$sign <- ifelse(coeffs$Estimate >= 0, '+', '-')
  coeffs <- data.frame(Coefficient = rownames(coeffs), coeffs)
  coeffs$Coefficient <- as.character(coeffs$Coefficient)
  rownames(coeffs) <- NULL
  names(coeffs) <- str_first_upper(gsub("\\.", "", names(coeffs)))
  return(coeffs)
}

ModelFormula <- function(model, round = 3){
  formula <- paste(round(coefficients(model),round), names(coefficients(model)), sep = " * ", collapse= " + ")
  formula <- gsub("\\s\\*\\s\\(Intercept\\)", "", formula)
  formula  <- paste(paste(as.character(formula(model))[2], " = ", sep = ""), formula, sep = " ")
  formula
}

ModelCorVars <- function(model){
  data <- subset(model$data, select = ModelPredictiveVariables(model)$predvars)
  cor <- cor(data[,laply(data, is.numeric)])
  cor
}

ModelHarcode <- function(model, name = "hardcode.txt"){
  t <- ModelCoefficientsSummary(model)
  aux <- paste( c("(",rep("", times=(nrow(t)-1))),
                format(t$Estimate, width = 10, scientific = FALSE),
                c("",rep("*", times=(nrow(t)-1))),
                gsub("\\(Intercept\\)","",t$Coefficient),
                c(rep(" +", times=(nrow(t)-1)),""),
                c(rep("", times=(nrow(t)-1)),")"),
                sep = "")
  data.frame(harcode = aux)
  ExportTable(aux, name)          
}

ModelCrossValidation <- function(model, K = 10, seed = 10, ...){
  set.seed(seed)
  data <- model$data
  data$FOLD <- MakePartition(nrow(data), probs = c(rep(1,K)), labels = c(1:K))
  
  formula <- formula(model)
  response <- as.character(formula(model))[2]
  
  faux <- function(k){
    m <- glm(formula, data = subset(data, FOLD != k), family = binomial(link = "logit"))  
    SummaryScorecard(ScoreScaleProb(predict.glm(m, type="response", newdata=subset(data, FOLD == k))),subset(data, FOLD == k)[[response]])
  }
  
  print("Calculating coefficients")
  coeffs <- ldply(1:K, function(k) glm(formula, data = subset(data, FOLD != k), family = binomial(link = "logit"))$coefficients, .progress='text')
  
  print("Calculating indicators")
  inds <- ldply(1:K, function(k) faux(k), .progress='text')
  
  names(coeffs) <- names(model$coefficients)
  
  list(Coefficients = coeffs,  Indicators = inds, Dist.folds = TablePercent(data$FOLD))
}

ModelReport <- function(model, newdata, off.set = 0, report.folder = "model_report"){
  
  library(car)
  library(xlsx)
  
  dir.create(report.folder)
  wb <- createWorkbook()
  style1 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()
  
  cuts <- c(seq(0,9, length = 10),10)*100
  response <- ModelPredictiveVariables(model)$response
  
  desdata <- model$data
  desdata$SCORE <- ScoreScaleProb(predict(model, type = "response"), off.set=off.set)

  
  t_model_sum <- ModelCoefficientsSummary(model)
  t_cor_predvars <- ModelCorVars(model)
  
  t_vif <- as.data.frame(vif(model))
  t_vif$variable <- rownames(t_vif)
  
  biv <- Bivariate(subset(desdata, select = unlist(ModelPredictiveVariables(model))),
                   response=response,save.output=F,return.results=T)
  t_biv_tables <- biv$bivariate_tables
  
  t_pred_vars <- join(biv$bivariate_ranking, t_vif)
  
  
  # Indicators
  if(!missing(newdata)){
    newdata$SCORE <- ScoreScaleProb(predict(model, newdata = newdata, type = "response"), off.set=off.set)
    t_ind <- rbind(cbind(SAMPLE = "development", SummaryScorecard(predict(model, type = "response"), desdata[[response]])),
                   cbind(SAMPLE = "validation", SummaryScorecard(predict(model, newdata = newdata,type = "response"), newdata[[response]])))
  } else {
    t_ind <- SummaryScorecard(predict(model, type = "response"), desdata[[response]])
  }
  
  # Distribution GB
  if(!missing(newdata)){    
    taux <- as.data.frame(rbind(cbind("validation",newdata[[response]]),
                                cbind("development",desdata[[response]])),
                          stringsAsFactors=F)
    t_dist_bg <- TableBivariate(taux$V1, as.numeric(taux$V2))[,c(1,2,3,4,10,11)]
  } else {
    t_dist_bg <- TablePercent(ifelse(desdata[[response]],"good","bad"), title="Behavior")
  }

  addDataFrame(t_model_sum, createSheet(wb, sheetName="Coeffs"), colnamesStyle=style1, row.names=FALSE)
  addDataFrame(t_ind, createSheet(wb, sheetName="Indicators"), colnamesStyle=style1, row.names=FALSE)
  addDataFrame(t_dist_bg, createSheet(wb, sheetName="GB_Distribution"), colnamesStyle=style1, row.names=FALSE)
  addDataFrame(t_pred_vars, createSheet(wb, sheetName="Variables"), colnamesStyle=style1, row.names=FALSE)
  addDataFrame(t_cor_predvars, createSheet(wb, sheetName="Correlations"), colnamesStyle=style1, row.names=TRUE)
  
  addDataFrame(t_odds <- OddsTable(desdata$SCORE, desdata[[response]])$oddstable,
               createSheet(wb, sheetName="Odds_Table_DEV"), colnamesStyle=style1, row.names=FALSE)

  # Odds table
  if(!missing(newdata)){
    addDataFrame(t_odds <- OddsTable(newdata$SCORE, newdata[[response]])$oddstable,
                 createSheet(wb, sheetName="Odds_Table_VAL"), colnamesStyle=style1, row.names=FALSE)
  }
  
  saveWorkbook(wb, file.path(report.folder,"model_report.xlsx"))
  
  # Plots png
  SummaryScorecardPlot(desdata$SCORE, desdata[[response]], save.plots=T, prefix=paste(report.folder, "dev", sep = "/"))
  if(!missing(newdata)){
    SummaryScorecardPlot(newdata$SCORE, newdata[[response]], save.plots=T, prefix=paste(report.folder, "val", sep = "/"))
  }
  
  
  if(!missing(newdata)){
    p1 <- GVisBRChart(cut(newdata$SCORE, cuts), newdata[[response]], title = "Distribution Score Validation", width = 1000, legend="bottom")
    p2 <- GVisDist(newdata$SCORE, newdata[[response]], title ="GB Distributions Validation", width = 1000, legend="bottom")
    p3 <- GVisKS(newdata$SCORE, newdata[[response]], width = 1000, legend="bottom",
                 title = paste("KS validation (", percent(KS(newdata$SCORE, newdata[[response]])), ")", sep =""))
    
  } else {
    p1 <- GVisBRChart(cut(desdata$SCORE, cuts), desdata[[response]], title = "Distribution Score", width = 1000, legend="bottom")
    p2 <- GVisDist(desdata$SCORE, desdata[[response]], title ="GB Distributions", width = 1000, legend="bottom")
    p3 <- GVisKS(desdata$SCORE, desdata[[response]], width = 1000, legend="bottom",
                 title = paste("KS (", percent(KS(desdata$SCORE, desdata[[response]])), ")", sep =""))
  }
   
  list_graphics <- list(GVisTable(t_dist_bg, title = "", height = 300, width = 300),
                        GVisTable(t_model_sum, title = "", height = 300, width = 1000),
                        GVisTable(t_ind, title = "", height = 300, width = 1000),
                        GVisTable(t_pred_vars, title = "", height = 300, width = 1000),
                        GVisTable(t_odds, title = "", height = 300, width = 1000),
                        p1,
                        p2,
                        p3,
                        GVisTable(t_biv_tables, title = "", height = 600, width = 1200))
  
  pr <- Reduce(gvisMerge, list_graphics)
  plot(pr)
  GVisExportHtml(pr,name.file=paste(report.folder, "report", sep = "/"))
  
  return(print("done it!"))  
}


ModelTopReasons <- function(model, data, top = 3){
  # Calculating top 3 variables affecting Credit Score Function in R.
  # In credit scoring per regulation lenders are required to provide the top 3 reasons
  # impacting the credit decision when a loan fails to be pass the credit score (Velez, 2008).
  
  # Get results of terms in regression
  g <- predict(model, newdata=data, type='terms')
  
  # Function to pick top 3 reasons works by sorting coefficient terms in equation
  # and selecting top 3 in sort for each loan scored
  
  ftopk<- function(x,top=3)
  {
    res <- names(x)[order(x, decreasing = FALSE)][1:top]
    paste(res,collapse=";",sep="")
  }
  
  # Application of the function using the top 3 rows
  
  topk <- apply(g, 1, ftopk, top = top)
  
  # add reason list to scored tets sample
  topk
}  
  
  
ModelScorecardAlling <- function(model, nclass = 10,
								odds = 1/1, points.at.odds = 500, pdo = 20){
  # The model must be a model with dummy variables.
  # The model must be a model with factors.
  
  # First we want: score = offset + factor*ln(odds)
  # but ln(odds) = alpha + beta*Lx
  # So, we find alpha and beta and calculate the
  # factor and offset (in source, page 114)
  # Source: Credit Risk - Scorecards Developing and
  # Implementing Intelligent Credit Scoring
  # by Naeem Siddiqi
  
  t <- TableBivariate(cut_interval(predict(model, type="link"), n = nclass),
                      model$data[[as.character(as.formula(model)[2])]])
  t$lx <- laply(levels(t$variable), function(x){mean(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)","",x),split=","))))})
  
  modellx <- lm(log(BMratio)~lx, data = t, weights=sqrt(t$N_Bad*(t$N_Total - t$N_Bad)/t$N_Total))
  
  alpha <- as.numeric(modellx$coefficients[2])
  beta <- as.numeric(modellx$coefficients[1])
  
  factor <- pdo/log(2)
  offset <- points.at.odds - factor*log(odds)
  
  parameters <- c(alpha = alpha, beta = beta, factor = factor, offset = offset)
  
  t <- ModelCoefficientsSummary(model)
  t$Points <- round(ifelse(t$Coefficient!="(Intercept)", factor*alpha*t$Estimate, factor*(alpha+beta)+offset+t$Estimate))

  formula <- gsub("\\*\\(Intercept\\)", "", paste(t$Points,t$Coefficient, sep = "*", collapse=" + "))
  
  sum.sccrd <- SummaryScorecard(with(data=as.data.frame(model.matrix(model)), {eval(parse(text = formula))}),
                                model$data[as.character(formula(model))[2]])
  
  list(scorecard = t, formula = formula, parameters = parameters, summary.scorecard = sum.sccrd)
}


# SCORE FUNCTIONS
ScoreScaleProb <- function(p.good, off.set = 0){
  Truncate(round(1000*invlogit(logit(Truncate(p.good,1e-16,1-1e-16)) + off.set)),inf=1,sup=999)
}

ScoreBenchmark <- function(data, responses = str_pattern(names(data),"^GB"), scores = str_pattern(names(data),"^SCORE"), split.var){
  if(!missing(split.var)){  
    bench <- 
      ldply(responses, function(response){
        ldply(scores, function(score){
          ldply(sort(as.character(unique(data[[split.var]]))), function(cat){
            daux <- data[data[[split.var]] == cat, ]
            cbind(RESPONSE = response, SCORE = score, cat, SummaryScorecard(daux[[score]],daux[[response]]))
            })
          })
      })
    
    names(bench)[3] <- split.var
    
  } else {
    bench <- 
      ldply(responses, function(response){
        ldply(scores, function(score){
          cbind(RESPONSE = response, SCORE = score, SummaryScorecard(data[[score]],data[[response]]))
          })
        })
    
  }
  bench  
}


# RUT FUCNTIONS
# RUT '16019432K'
# RUTNUM 16019432
# RUT10 '016019432K'
# DV 'K'
RUTDV <- function(rutnum){
  digits <- as.numeric(unlist(strsplit(as.character(format(rutnum, scientific = F)), '')))
  mult <- rep(7:2,2)[(13-nchar(rutnum)):12]
  dv <- 11-sum(digits*mult)%%11
  if(dv==10) dv <- "K"
  if(dv==11) dv <- "0"
  return(as.character(dv))
}

RUT_to_RUT10 <- function(rut){
  RUT10 <- paste(paste(rep(0,10), collapse=""), as.character(rut), sep = "")
  RUT10 <- substring(RUT10, nchar(RUT10) - 9, nchar(RUT10) - 0)
  RUT10 <- gsub('k', 'K', RUT10)
  return(RUT10)
}

RUT10_to_RUTNUM <- function(RUT10){
 rutnum <- as.character(RUT10) # rut debe venir con digito verificador
 rutnum <- as.numeric(substring(rutnum, 0, nchar(rutnum) - 1))
 return(rutnum)
}

RUTNUM_to_RUT10 <- function(rutnum){
  RUT_to_RUT10(paste(format(rutnum, scientific = F),RUTDV(rutnum), sep =""))
}


# SOME OTHER FUCNTIONS
LoadingBar <- function(segs = 120, ...){
  l_ply(1:segs,
        function(x) Sys.sleep(1),
        .progress = if(exists("winProgressBar")) progress_win(...) else progress_tk(...))
}

Alarm <- function(n=10){
  library(tcltk)
  for(i in 1:n){ Sys.sleep(runif(1)/2); tkbell()}  
}

Multi.Sapply <- function(...) {
  arglist <- match.call(expand.dots = FALSE)$...
  var.names <- sapply(arglist, deparse)
  has.name <- (names(arglist) != "")
  var.names[has.name] <- names(arglist)[has.name]
  arglist <- lapply(arglist, eval.parent, n = 2)
  x <- arglist[[1]]
  arglist[[1]] <- NULL
  result <- sapply(arglist, function (FUN, x) sapply(x, FUN), x)
  colnames(result) <- var.names[-1]
  return(result)
}

ProcMeans <- function(data, ...){
  res <- Multi.Sapply(data[,laply(data, is.numeric)],
                      N = length,
                      N.miss = function(x) sum(is.na(x, ...)),
                      N.uniques = function(x) length(unique(x, ...)),
                      Min = function(x) min(x, na.rm = T),
                      Qu.1st = function(x) quantile(x, .25, na.rm = T,...),
                      Median = function(x) median(x, na.rm = T),
                      Mean = function(x) mean(x, na.rm = T),
                      Qu.3rd = function(x) quantile(x, .75, na.rm = T,...),
                      Max = function(x) max(x, na.rm = T),
                      StdDev = function(x) sd(x, na.rm = T),
                      ...)
  res <- data.frame(res)
  res$VARIABLE <- names(data[,laply(data, is.numeric)])
  res <- subset(res, select = unique(c("VARIABLE",names(res))))
  res
}

ProcContent <- function(data, ...){
  res <- Multi.Sapply(data,
                      type = class,
                      Len = function(x) max(nchar(as.character(x))),
                      N.miss = function(x) sum(is.na(x)),
                      ...)
  res <- data.frame(res)
  res$VARIABLE <- names(data)
  res <- subset(res, select = unique(c("VARIABLE",names(res))))
  res
}


TablePercent <- function(factor, sort.by.count = F, title){
  factor <- as.factor(factor)
  if(any(is.na(factor))) factor <- addNA(factor)
  Counts <- as.vector(table(factor))
  
  aux <- data.frame(Levels = levels(factor),
                    Counts,
                    Percents = prop.table(Counts))
  if(sort.by.count) aux <- aux[order(aux$Counts, decreasing=T),]
  aux$Acum <- cumsum(aux$Percents)
  
  Levels <- c(as.character(aux$Levels),"Total")
  Counts <- c(aux$Counts,sum(aux$Counts))
  Percents <- c(aux$Percents,1)
  Acum <- c(aux$Acum,NA)
  result <- data.frame(Levels, Counts, Percents, Acum)
  if(!missing(title)) names(result)[1] <- title
  result 
}

CatVar <- function(variable, n.cats = 10){
  if(is.numeric(variable) & length(unique(variable)) > n.cats){
    variable <- cut(variable, breaks= unique(seq(min(variable),max(variable),length.out=n.cats+1)), include.lowest=T)
    levels(variable) <- PrettyLevels(levels(variable))
  }
  factor(variable)
}

Truncate <- function(x, inf, sup){
  if(missing(inf)) return(ifelse(x<sup,x,sup))
  if(missing(sup)) return(ifelse(x>inf,x,inf))
  x <- ifelse(x<sup,x,sup)
  x <- ifelse(x>inf,x,inf)
  return(x)
}

StringFirstToUpper <- function(characters){
  paste(toupper(substring(characters, 1, 1)),substring(characters, 2, nchar(characters)), sep = "")
}

str_first_upper <- function(characters){
  paste(toupper(substring(characters, 1, 1)),substring(characters, 2, nchar(characters)), sep = "")
}


str_pattern <- function(string, pattern){
  library(stringr)
  string[str_detect(string, pattern)]
}

str_capitalize <- function(strings){
  laply(strings, function(x){
    x <- tolower(x)
    x <- str_split(x, " ")
    paste(laply(x, str_first_upper), collapse=" ")
  })
}

StringsWith <- function(strings, patterns, complement = F){
  # From a vector of string, return the string which have the pattern.
  # If complement is T, return the string which not have the pattern.
  if(!complement){
    for(pattern in patterns){
      strings <- strings[(1:length(strings) %in% grep(x=strings, pattern=pattern))]
    }
  } else {
    for(pattern in patterns){
      strings <- strings[!(1:length(strings) %in% grep(x=strings, pattern=pattern))]
    }
  }
  strings
}

StringsWith2 <- function(strings, include.pattern, exclude.pattern){
  # From a vector of string, return the string which have the include.pattern
  # and exclude the string which have the exclude.patterns
  if(!missing(include.pattern)){
    for(pattern in include.pattern){
      strings <- strings[(1:length(strings) %in% grep(x=strings, pattern=pattern))]
    }
  }
  if(!missing(exclude.pattern)){
    for(pattern in exclude.pattern){
      strings <- strings[!(1:length(strings) %in% grep(x=strings, pattern=pattern))]
    }
  }  
  strings
}


Lematiza <- function(frase){
	require(XML)
	base.url <- paste("http://cartago.lllf.uam.es/grampal/grampal.cgi?m=etiqueta&e=",
                    gsub(" ", "+", frase), sep = "" )
	tmp <- readLines(base.url, encoding = 'utf8')
	tmp <- iconv(tmp, "utf-8")
	tmp <- gsub("&nbsp;", " ", tmp )
	tmp <- readHTMLTable(tmp)
	tmp <- as.character( tmp[[1]]$V3 )
	tmp <- do.call(rbind, strsplit( tmp, " " ) )[,4]
  tmp <- tolower(tmp)
	tmp
}

logit <- function(x){
  return(log(x/(1-x)))
} 
  
invlogit <- function(x){
  return(1/(1+exp(-x)))
}



SplitVector <- function(x, n, force.number.of.groups = TRUE, len = length(x), groups = trunc(len/n), overflow = len%%n) { 
   if(force.number.of.groups) {
      f1 <- as.character(sort(rep(1:n, groups)))
      f <- as.character(c(f1, rep(n, overflow)))
   } else {
      f1 <- as.character(sort(rep(1:groups, n)))
      f <- as.character(c(f1, rep("overflow", overflow)))
   }
   split(x, f)
}


# DATE FUCNTIONS
DayWeek <- function(date){
  return(as.numeric(strftime(as.Date(as.character(date), format = "%Y%m%d"), format = "%w")))
}

DayWed <- function(date){
  # Return the past Wednsday
  if(DayWeek(date) %in% c(0,1,2)){
    r <- as.Date(as.character(date), "%Y%m%d") - as.difftime(DayWeek(date)+4, units = "days")
  } else {
    r <- as.Date(as.character(date), "%Y%m%d") + as.difftime(3-DayWeek(date), units = "days")
  }
  return(as.numeric(format(r, format="%Y%m%d")))
}

AddDate <- function(dates, ...){
  # AddDate(dates, tim = 365, units = "days")
  return(as.numeric(format(as.Date(as.character(dates), "%Y%m%d") + as.difftime(...), "%Y%m%d")))
}

DiffDates <- function(dates1, dates2, units = "days", ...){
  dates <- cbind(format(as.Date(as.character(dates1), "%Y%m%d")),
                 format(as.Date(as.character(dates2), "%Y%m%d")))
  round(as.numeric(lapply(vector=1:nrow(dates), function(i, ...){  try(difftime(dates[i,1], dates[i,2], ...))  })))  
}

LastDayOfMonth <- function(p){
  p <- as.character(p)
  if(substring(p,5,8) != "12"){
    p <- as.numeric(p) + 1
    p <- paste(p, "01", sep = "")
  } else {
    p <- paste(as.numeric(substring(p, 0 ,4))+1,"0101", sep = "")
  }
  p <- as.Date(p, format="%Y%m%d")
  p <- p - as.difftime(tim=1,units="days")
  as.numeric(format(p, "%Y%m%d"))
}

as.date <- function(x){
  as.Date(as.character(x), format="%Y%m%d")
}

Table <- function(v1,v2){
  if(missing(v2)){
    as.data.frame(table(v1))
  } else {
    as.data.frame.matrix(table(v1,v2))
  }
}

ReadTable <- function(files, ...){
  library(plyr)
  # 20120314: Use of the function ldply to load more than 1 file
  read.table.aux <- function(name, ...){
    ext <- tolower(unlist(strsplit(name, "\\."))[length(unlist(strsplit(name, "\\.")))])
    if(!ext%in%c("txt","csv","xlsx","xls","dbf","sas7bdat","sav","tsv","dat")) stop("No posible format (extension)")
    if(ext %in% c("txt","tsv","dat")) return(read.table(name, sep = "\t", h = T, comment.char = "", quote = "",  ...))
    if(ext == "csv") return(read.table(name, sep = ";", h = T,  comment.char = "", quote = "", dec=",", ...))
    if(ext == "sav"){
      library(foreign)
      return(read.spss(name, to.data.frame = T))
    }
    if(ext == "sas7bdat"){ library(sas7bdat); return(read.sas7bdat(name))}
    if(ext %in% c("xlsx","xls")){ library(xlsx); return(read.xlsx(name, sheetIndex=1, ...))}
    if(ext == "dbf"){
      library(foreign)
      write.table(read.dbf(name, as.is=TRUE), "data.txt", quote = F, row.names = FALSE, col.names = TRUE, append = FALSE, sep = "\t")
      write.table(read.table("data.txt", sep = "\t", h = T, dec = ","), "data.txt", dec = ".", sep = "\t", col.names = TRUE, quote = FALSE, row.names = F)
      d <- read.table("data.txt", h = T, sep = "\t", stringsAsFactors=F)    
      shell("del data.txt")    
      if(any(names(d)=="RUT") ){ if(is.numeric(d$RUT)){d$RUT <- laply(d$RUT, RUT_to_RUT10)}}    
      return(d)
    }
  }
  # This function not necessary require all data with the same structure
  ldply(files, function(f) read.table.aux(f, ...), .progress = "text")
}

ExportTable <- function(data, name = "data.txt",  row.names = F,...){
  if(class(data)=="table") data <- as.data.frame.matrix(data)
  if(length(unlist(strsplit(name, "\\.")))==1){
    name <- paste(name, ".txt", sep = "")
  }
  ext <- tolower(unlist(strsplit(name, "\\."))[length(unlist(strsplit(name, "\\.")))])
  if(!ext %in% c("txt","csv","xlsx","xls")) stop("No posible extension")
  if(ext == "txt") write.table(data, name, dec = ".", sep = "\t", col.names = TRUE, quote = FALSE, row.names = row.names, ...)
  if(ext == "csv") write.table(data, name, dec = ",", sep = ";", col.names = TRUE, quote = FALSE, row.names = row.names, ...)
  if(ext %in% c("xlsx","xls")){
    library(xlsx)
    wb <- createWorkbook()
    style1 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()
    addDataFrame(data, createSheet(wb), colnamesStyle=style1, row.names=row.names)
    saveWorkbook(wb, name)
  }
}

UploadDataToDatamart <- function(data,name){
  library(RODBC)
  channel <- odbcConnect( dsn = "DNS_SQL_TASREP", uid = "igonzalez", pwd = "Efx$2013")
  sqlQuery(channel,  paste("DROP TABLE", name, sep =  " "))
  sqlSave(channel, data, tablename = name, rownames = FALSE)
}

DownloadDataFromDatamart <- function(name, ...){
  # This function return the table as data.frame object
  library(RODBC)
  return(sqlFetch(channel <- odbcConnect( dsn = "DNS_SQL_TASREP", uid = "igonzalez", pwd = "Efx$2013"), name, ...))
}

DownloadDataFromDatamart2 <- function(nametable, query){
  # This function download the table to the working directory as txt file via sqlcmd
  tot <- 'sqlcmd -S CLPRWXDM01 -s "\t", -U igonzalez -P Efx$2013 -Q 
  "set nocount on;set ansi_warnings off; query"
  -W | findstr /V /C:"-" /B > nameout.txt'
  
  if(!missing(nametable)){
    tot <- gsub("\n", "", tot)
    tot <- gsub("query", "SELECT * FROM CLMSDBTASREP.dbo.table", tot)
    tot <- gsub("table", nametable, tot)
    tot <- gsub("nameout", nametable, tot)
  } else {
    tot <- gsub("\n", "", tot)
    tot <- gsub("query", query, tot)
    tot <- gsub("nameout", gsub("\\*", "AST", gsub(" ", "_", query)), tot)
    
  }
  print(tot)
  shell(tot)
}


DownloadDataFromDatamart3 <- function(name, ...){
  # This function download the table to the working directory as txt file via bcp
  tot <- gsub("/","\\\\", paste(paste('BCP "[CLMSDBTASREP].[dbo].', name,'"', sep = ''),
                                paste('out "',paste(getwd(), '\\',name,'.txt"', sep = ""), sep = ""),
                                paste('-c -S CLPRWXDM01 -U igonzalez -P Efx$2013')))
  print(tot)
  shell(tot)
}



DataMartTables <- function(){
  library(RODBC)
  channel <- odbcConnect( dsn = "DNS_SQL_TASREP", uid = "igonzalez", pwd = "Efx$2013")
  tables <- sqlTables(channel)$TABLE_NAME
  return(tables)
}

DDA_CLI_COMP <- function(RUT,FECHA){
  UploadDataToDatamart(data.frame(RUT=RUT, FECHA=as.character(FECHA)), "CLI_COMP_T_KUNST")
}

OpenWD <- function(){
  # Open working directory
  return(shell("start ."))
 }

CLS <- function() {
  # Clear screen
  cat("\014")
}

ObjectSizes <- function (pos = 1, pattern, order.by = "Size", p.max = 0.01, decreasing=TRUE, head = TRUE, n = 10) {
  # based on postings by Petr Pikal and David Hinds to the r-help list in 2004
  # modified by: Dirk Eddelbuettel (http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session) 
  # I then gave it a few tweaks (show size as megabytes and use defaults that I like)
  # a data frame of the objects and their associated storage needs.
  napply <- function(names, fn) sapply(names, function(x)
          fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size) / 10^6 # megabytes
  obj.percent <- round(as.numeric(obj.size/sum(obj.size)),4)
  obj.dim <- t(napply(names, function(x)
            as.numeric(dim(x))[1:2]))
  
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(names, obj.type, obj.size, obj.percent, obj.dim)
  
  names(out) <- c("Object", "Type", "Size", "Percent", "Rows", "Columns")
  out <- out[order(out[[order.by]], decreasing=decreasing), ]
  out$Object <- as.character(out$Object)
  out$Type <- as.character(out$Type)
  rownames(out) <- NULL
  
  outaux2 <- subset(out, Percent < p.max)
  outaux2 <- c(Object = "Others", Type = "Others", Size=sum(outaux2$Size), Percent=sum(outaux2$Percent), Rows = NA, Columns = NA)
  outaux <- rbind(subset(out, Percent >= p.max),
                   outaux2)
  outaux$Object <- factor(outaux$Object, levels = outaux[order(outaux$Percent, decreasing=decreasing), ]$Object)
  p <- ggplot(outaux, aes(x=Object,y=Percent)) + geom_bar(fill="darkred")
      
  print(p)
   
  if (head)
    out <- head(out, n)
  names(out) <- str_first_upper(names(out))
  out
}

cut2 <- function(...){
  var <- cut(...)
  levels(var) <- PrettyLevels(levels(var))
  var
}

PrettyLevels <- function(levels){

  laply(levels, function(aux){
    liminf <- substring(aux,1,1)
    limsup <- substring(aux,nchar(aux),nchar(aux))
    nums <- as.numeric(unlist(strsplit(substring(aux, 2, nchar(aux) - 1 ), ",")))
    n1 <- prettyNum(format(nums[1],scientific=F), big.mark = ".")
    n2 <- prettyNum(format(nums[2],scientific=F), big.mark = ".")
    paste(liminf, paste(n1,n2, sep = ",")  ,limsup, sep = "")
  })

}


PrettyLevels2 <- function(levels){
  levels <-  gsub("\\,", "\\_",gsub("\\]", "",gsub("\\)", "",gsub("\\(", "",gsub("\\[", "", levels)))))
  levels
}


MakeID <- function(size){
  set.seed(1)
  sample(1:size)
}

MakePartition <- function(size, probs = c(.5,.5), seed = 1, labels = c("des","val")){
  set.seed(seed)
  sample(labels, size = size, prob = probs, replace = T)
}

MissingData <- function(data, factor.replace = ""){
  vars <- names(data)
  for(var in vars){
    varaux <- data[[var]]
    # print(paste(paste(round(which(var==vars)/length(vars),3)*100,"%",sep=""),var,class(varaux), "| nas:", sum(is.na(varaux))))
    if(any(is.na(varaux))){  
      if(is.numeric(varaux)){
        varaux[is.na(varaux)] <- 0
      } else {
        varaux <- addNA(varaux)
        levels(varaux)[is.na(levels(varaux))] <- factor.replace
      }
    }    
    if(!is.numeric(varaux)){
        varaux <- toupper(gsub('^+ | +$','',varaux))
    }
    data[[var]] <- varaux
  }
  return(data)
}

PHPClean <- function(data, upload.to.datamart = T, missing.data = T, performance = T,no.icom = F, ...){
  
  names(data) <- toupper(names(data))

  #-------------------------------
  cat("\nDeleting trash variables")
  data <- subset(data, select = names(data)[!(1:ncol(data) %in% grep(x=names(data), pattern="ORIG_"))])
  data <- subset(data, select = names(data)[!(1:ncol(data) %in% grep(x=names(data), pattern="FORMULA_2"))])
  data <- subset(data, select = names(data)[!(1:ncol(data) %in% grep(x=names(data), pattern="FORMULA_3"))])
  vars_to_del <- c("MTO_CITI12","MPROT_CITI","FVEN_CITI","NDIRCIT_18","COD_CIUCIT","CLUS4_CITI","PREDICTOR2","PREDICTOR3")
  data <- subset(data, select = setdiff(names(data),vars_to_del))
  
  #-------------------------------
  if(upload.to.datamart){
    cat("\nUploading data to JBK_CLI")
    UploadDataToDatamart(unique(data.frame( RUT = data$RUT, FECHA_CALC = data$FECHA_CALC)),name='JBK_CLI')
  }

    
  
  tables <- c("TAS_VEHI_RESUMEN_HIS","TAS_TEL_RESUMEN","TAS_GSE_RESUMEN",
              "TAS_MPN_RESUMEN","TAS_VDOM_RESUMEN","TAS_MARCA_BORRON","TAS_ULTIMA_DIRECCION",
              "TAS_AVALUO","TAS_RUBROS_RESUMEN","TAS_PHP_ICOM") 
  
  if(no.icom){
  tables <- c("TAS_VEHI_RESUMEN_HIS","TAS_TEL_RESUMEN","TAS_GSE_RESUMEN",
              "TAS_MPN_RESUMEN","TAS_VDOM_RESUMEN","TAS_MARCA_BORRON","TAS_ULTIMA_DIRECCION",
              "TAS_RUBROS_RESUMEN", "TAS_AVALUO") 
  }
  
  for(t in tables){
    cat(paste("\nObtaining", t, "data"))
    tabla <- DownloadDataFromDatamart(t, stringsAsFactor = F)
    tabla$RUT <- RUT_to_RUT10(tabla$RUT)
    data <- join(data, tabla, by = c("RUT","FECHA_CALC"), type="left", match="first")
  }
  
  if(performance){
    tables <- c("TAS_PERFORMANCE_BED_6M_MONTDOC","TAS_PERFORMANCE_BED_12M_MONTDOC","TAS_PERFORMANCE_BOLCOM_6M_MONTDOC","TAS_PERFORMANCE_BOLCOM_12M_MONTDOC")
    for(t in tables){
      cat(paste("\nObtaining", t, "data"))
      data <- join(data, DownloadDataFromDatamart(t, stringsAsFactor = F), by = c("RUT","FECHA_CALC"),
                   type="left", match="first")
    }

    #-------------------------------
    cat("\nFixing GBs")
    data$BED_HIT_6M <- ifelse(is.na(data$BED_HIT_6M), 1, data$BED_HIT_6M)
    data$BED_HIT_12M <- ifelse(is.na(data$BED_HIT_12M), 1, data$BED_HIT_12M)
    data$BOLCOM_HIT_6M <- ifelse(is.na(data$BOLCOM_HIT_6M), 1, data$BOLCOM_HIT_6M)
    data$BOLCOM_HIT_12M <- ifelse(is.na(data$BOLCOM_HIT_12M), 1, data$BOLCOM_HIT_12M)
    data$GB_6M <- data$BED_HIT_6M*data$BOLCOM_HIT_6M
    data$GB_12M <- data$BED_HIT_12M*data$BOLCOM_HIT_12M
  	data$BED_MAX_DMORA_6M[is.na(data$BED_MAX_DMORA_6M)] <- 0
  	data$BOLCOM_MAX_DMORA_6M[is.na(data$BOLCOM_MAX_DMORA_6M)] <- 0
  	data$BED_MAX_DMORA_12M[is.na(data$BED_MAX_DMORA_12M)] <- 0
  	data$BOLCOM_MAX_DMORA_12M[is.na(data$BOLCOM_MAX_DMORA_12M)] <- 0
  	data$GB_60_6M <- ifelse(data$BED_MAX_DMORA_6M > 60 | data$BOLCOM_MAX_DMORA_6M > 60 ,0,1)
  	data$GB_60_12M <- ifelse(data$BED_MAX_DMORA_12M > 60 | data$BOLCOM_MAX_DMORA_12M > 60 ,0,1)
  }
  
  
  #-------------------------------
  cat("\nObtaining RIESGO_COMUNA data")
  riesgo_comuna <- sqlQuery(channel <- odbcConnect( dsn = "DNS_SQL_TASREP", uid = "igonzalez", pwd = "Efx$2013"),
                            "SELECT DES_ULTCOM, RIESGO_ZONA, RIESGO_TAMAÑO, RIESGO FROM RIESGO_COMUNA",
                            stringsAsFactor = F)
  data <- join(data, riesgo_comuna, by = "DES_ULTCOM", type="left", match="first")
  rm(riesgo_comuna)
  
  #-------------------------------
  cat("\nCreating variables before 'missing data'")
  data$ESTADO_LEY <- ifelse(data$ESTADO %in% c("L","M","LM"), "BEN","NO_BEN")
  data$ESTADO_LEY <- ifelse(is.na(data$ESTADO), "NUEVO", data$ESTADO_LEY)
  data$ACTIVIDAD_IND <- ifelse(data$ACTIVIDAD == ""| data$CODIGO_ACTI == 24, 0, 1)
  data$PROFESION_IND <- ifelse(data$PROFESION == "" | data$CODIGO_PROF == 24, 0, 1)
  data$SEXO_DESC        <- ifelse(data$SEXO == 0, 'F', 'M')
  data$SEXO_DESC        <- ifelse(data$SEXO == 2, 'SI', data$SEXO_DESC)
  data$SEXO_IND_F       <- ifelse(data$SEXO == 0, 1, 0)
  data$SEXO_IND_M       <- ifelse(data$SEXO == 1, 1, 0)
  data$TIP_ULTDIR_DESC  <- 'SI' # Sin Informacion
  data$TIP_ULTDIR_DESC  <- ifelse(data$TIP_ULTDIR == 1, 'P', data$TIP_ULTDIR_DESC) # Particular
  data$TIP_ULTDIR_DESC  <- ifelse(data$TIP_ULTDIR == 2, 'C', data$TIP_ULTDIR_DESC) # Comercial
  data$TIP_ULTDIR_DESC  <- ifelse(data$TIP_ULTDIR == 3, 'CM', data$TIP_ULTDIR_DESC) # Casa Matriz
  data$TIP_ULTDIR_DESC  <- ifelse(data$TIP_ULTDIR == 4, 'S', data$TIP_ULTDIR_DESC) # Sucursal
  data$TIP_ULTDIR_DESC  <- ifelse(data$TIP_ULTDIR == 5, 'O', data$TIP_ULTDIR_DESC) # Otro
  

  if(missing.data){
    cat("\nTreating missing data")
    data <- MissingData(data,...)
  }
  
  cat("\nCreating variables")
  #-------------------------------
  data$RUT <- as.character(data$RUT)
  data$RUTNUM <- RUT10_to_RUTNUM(data$RUT)
  data$PERIODO <- substring(data$FECHA_CALC,0,6)
  data$RUT_TIPO <- ifelse(data$RUTNUM>50000000, "E", "PN")
  data$EDAD_CALC <- 0
  data$EDAD_CALC <- as.numeric(trunc(difftime(as.Date(as.character(data$FECHA_CALC), "%Y%m%d"), as.Date(as.character(data$FNAC_26), "%Y%m%d"),units="days")/365.25))
  data$EDAD_CALC[is.na(data$EDAD_CALC)] <- 0
  data$AVALUO_IND       <- ifelse(data$AVALUO>0, 1, 0)                        #  Avaluo 0
  data$NACR_02_IND      <- ifelse(data$NACR_02!=0, 1, 0)                      #  NACR
  data$NCCC_IND         <- ifelse(data$NCCC!=0, 1, 0)                         #  NCCC
  data$NDIR_18_06_IND   <- ifelse(data$NDIR_18_06!=0, 1, 0)                   #  NDIR_18_06
  data$BED_IND          <- ifelse(data$TDOC_02 == 0, 0, 1)                    #  TMOUF - BedSicom
  data$BOLCOM_IND       <- ifelse(data$TDOC_12 == 0, 0, 1)                    #  TMOT_12 - Bolcom
  data$CLEAN            <- ifelse(data$TDOC_12+data$TDOC_02 == 0, 1, 0)       #  CLEAN
  if(no.icom==F){
  data$CLEAN2           <- ifelse(data$TDOC_12+data$TDOC_02+data$TDOC_39 == 0, 1, 0) #  CLEAN
  }
  data$TOT_DOCS         <- data$TDOC_02 + data$TDOC_12                        #  TOTAL DOCUMENTOS
  data$TOT_MONT         <- data$TMOT_12 + data$TMOUF_02                       #  TOTAL MONTO
  data$SUMC_IND         <- ifelse(data$SUMC!=0, 1, 0)                         #  SUMC              
  data$NCHEQUE_12_IND   <- ifelse(data$NCHEQUE_12!=0, 1, 0)                   #  NCHEQUE
  data$PROT_IND         <- ifelse(data$MES_ULTPRO!=0, 1, 0)                   #  INDICADOR PROTESTOS
  data$MES_ULTPRO_2     <- ifelse(data$MES_ULTPRO==0, 60, data$MES_ULTPRO)
  data$UBIC_CIU_IND_N   <- ifelse(data$UBIC_CIU == "N", 1, 0)                 #  UBIC_CIU
  data$UBIC_CIU_IND_S   <- ifelse(data$UBIC_CIU == "S", 1, 0)                 #  UBIC_CIU
  data$UBIC_CIU_IND_C   <- ifelse(data$UBIC_CIU == "C", 1, 0)                 #  UBIC_CIU
  data$MOTI_PROTE_DESC <- 'SI' # Motivo Protesto Mas Reciente, tengo valores 0 : Otro motivo, 1: Cuenta cerrada y 2: Falta de fondos.
  data$MOTI_PROTE_DESC <- ifelse(data$MOTI_PROTE == 0, 'OM', data$MOTI_PROTE_DESC)
  data$MOTI_PROTE_DESC <- ifelse(data$MOTI_PROTE == 1, 'CC', data$MOTI_PROTE_DESC)
  data$MOTI_PROTE_DESC <- ifelse(data$MOTI_PROTE == 2, 'FF', data$MOTI_PROTE_DESC)
      
  if(length(names(data)[names(data)=="GSE"]) == 1){
    cat("\nCreating GSE variables")
    data$GSE_IND_ABC1 <- ifelse(data$GSE == "ABC1", 1, 0)
    data$GSE_IND_DE   <- ifelse(data$GSE %in% c("D","E"), 1, 0) 
    
    cat("\nCreating EST_CIVIL variables")
    data$ESTCIV_IND_CASADO  <- ifelse(data$EST_CIVIL == "CASADO", 1, 0)
    data$ESTCIV_IND_SOLTERO <- ifelse(data$EST_CIVIL == "SOLTERO", 1, 0)
    
    cat("\nCreating REGION variables")
    data$REGION_N <- ''
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('IQUIQUE'), '01', data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('ANTOFAGASTA', 'CALAMA', 'MEJILLONES', 'TOCOPILLA'), '02',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('CALDERA', 'CHANARAL', 'COPIAPO', 'HUASCO', 'VALLENAR'), '03',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('COQUIMBO', 'ILLAPEL', 'LA SERENA', 'OVALLE'), '04',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('HANGA ROA', 'JUAN FERNANDEZ', 'LA LIGUA', 'LIMACHE', 'LOS ANDES', 'QUILLOTA', 'SAN ANTONIO', 'SAN FELIPE', 'VALPARAISO', 'VINA DEL MAR'), '05',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('PICHILEMU', 'RANCAGUA', 'SAN FERNANDO', 'SANTA CRUZ'), '06',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('CAUQUENES', 'CURICO', 'LINARES', 'PARRAL', 'TALCA'), '07',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('ARAUCO', 'CHILLAN', 'CONCEPCION', 'CORONEL', 'LEBU', 'LOS ANGELES', 'LOTA', 'TALCAHUANO'), '08',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('ANGOL', 'TEMUCO', 'VICTORIA', 'VILLARRICA'), '09',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('ANCUD', 'CASTRO', 'CHAITEN', 'OSORNO', 'PUERTO MONTT', 'PUERTO VARAS'), '10',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('AYSEN', 'CHILE CHICO', 'COCHRANE', 'COYHAIQUE'), '11',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('PORVENIR', 'PUERTO NATALES', 'PUERTO WILLIAMS', 'PUNTA ARENAS'), '12',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('COLINA', 'MELIPILLA', 'PUENTE ALTO', 'SAN BERNARDO', 'SANTIAGO', 'TALAGANTE'), 'RM',data$REGION_N)
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('ARICA','PUTRE' ), '14',data$REGION_N) # Used to be 01
    data$REGION_N <- ifelse(data$DES_ULTCIU %in% c('VALDIVIA', 'LA UNION'), '15',data$REGION_N)
    data$REGION_N <- as.factor(data$REGION_N)
    
    data$REGION <- ''
    data$REGION <- ifelse(data$REGION_N == '01','DE TARAPACA',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '02','DE ANTOFAGASTA',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '03','DE ATACAMA',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '04','DE COQUIMBO',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '05','DE VALPARAISO',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '06','DEL LIBERTADOR BERNARDO OHIGGINS',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '07','DEL MAULE',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '08','DEL BIO BIO',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '09','DE LA ARAUCANIA',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '10','DE LOS LAGOS',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '11','AYSEN DEL GENERAL CARLOS IBANEZ',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '12','DE MAGALLANES Y ANTARTICA CHILENA',data$REGION)
    data$REGION <- ifelse(data$REGION_N == 'RM','METROPOLITANA DE SANTIAGO',data$REGION)
    data$REGION <- ifelse(data$REGION_N == '14','DE ARICA y PARINACOTA',data$REGION) 
    data$REGION <- ifelse(data$REGION_N == '15','DE LOS RIOS',data$REGION)
    data$REGION <- as.factor(data$REGION)
    
    data$REGION_IND_RM <- ifelse(data$REGION %in% c("METROPOLITANA DE SANTIAGO"), 1, 0)    
    
    
    cat("\nCreating NACIONALIDAD variables")
    data$TIPONAC_IND_CHI <- ifelse(data$TIPO_NACIONALIDAD == "CHILENO", 1, 0)
    data$TIPONAC_IND_EX   <- ifelse(data$TIPO_NACIONALIDAD %in% c("NACIONALIZADO","EXTRANJERO"), 1, 0) 
  }
  
  return(data)  
}

PHPDiferidos <- function(data,ejecutivo="", cliente = "",codigos, interno = F){
  
  d <- unique(subset(data, select = c("RUT","FECHA_CALC")))
  
  
  msg <- "EJECUTIVO,
  Para CLIENTE se requiere el PHP con codigo CODIGO. Son REGISTROS registros y FECHAS fechas unicas. INTERNO.
  Muchas gracias.
  
  Saludos."
  
  msg <- gsub("EJECUTIVO", ejecutivo, msg)
  msg <- gsub("CLIENTE", cliente, msg)
  msg <- gsub("CODIGO", paste(codigos, collapse = " y "), msg)
  msg <- gsub("REGISTROS", prettyNum(dim(unique(d))[1], big.mark = "."), msg)
  msg <- gsub("FECHAS", prettyNum(length(unique(d$FECHA_CALC)), big.mark = "."), msg)
  
  if(interno==T){
    msg <- gsub("INTERNO.", "\n  Al ser un desarrollo interno de producto las consultas no deben quedar registradas.", msg)
    
  }
  if(interno==F){
    msg <- gsub("INTERNO.", "", msg)
    
  }
  cat(msg)
  
  ExportTable(d, paste(cliente, "PHP", paste(codigos, collapse = "_"), Sys.Date(), dim(unique(d))[1],sep = "_"))  
  ExportTable(msg,paste0(paste(cliente, "PHP", paste(codigos, collapse = "_"), Sys.Date(), sep = "_"),"_Mensaje.txt"))
  
}


# GoogleVisualizarion Shortcuts
GVisExportHtml <- function(p ,name.file){
  cat(unlist(p$html), file=paste(name.file, "html", sep = "."))
}

GVisBarplot <- function(variable, name.html = "barplot", export = F, ...){
  library(googleVis)
  p <- gvisColumnChart(data.frame(Categories = names(table(variable)), Count = as.vector(table(variable))),
                       , options=list(...))
  if(export) GVisExportHtml(p, name.html)
  return(p)
}

GVisBarStacked <- function(val1, val2, name.html = "bar_stacked_chart", export = F, ...){
  library(googleVis)
  
  df <- as.data.frame.matrix(table(val1,val2))
  vals <- names(df)
  df$xvar <- rownames(df)
  names(df)
  
  p <- gvisColumnChart(df, xvar="xvar", yvar=vals,
                       options=list(isStacked=TRUE, ...))
  
  if(export) GVisExportHtml(p, name.html)
  return(p)
}

GVisPieChart <- function(variable, name.html = "pie_chart", export = F, ...){
  library(googleVis)
  p <- gvisPieChart(data.frame(Categories = names(table(variable)), Count = as.vector(table(variable))),
                    options = list(...))
  if(export) GVisExportHtml(p, name.html)
  return(p)
}

GVisTable <- function(data, name.html = "table", export = F, ...){
  library(googleVis)
  p <- gvisTable(data, options=list(...))
  if(export) GVisExportHtml(p, name.html)
  p
}

GVisBRChart <- function(variable, label, n.cats = 10, name.html = "br_chart", export = F,...){
  library(googleVis)
  t <- TableBivariate(CatVar(variable, n.cats=n.cats), label)
  
  p <- gvisComboChart(t, xvar="variable",yvar=c("Percent", "BadRate"),
                      options=list(seriesType="bars",
                                   series='{1: {type:"line"}}',
                                   vAxis="{format:'#,##%'}", ...))
  if(export) GVisExportHtml(p, name.html)
  p
}

GVisKS <- function(score, good.label, name.html = "ks_chart", export = F,...){
  library(googleVis)
  ecd.bad <- ecdf(score[good.label == 0])
  ecd.good <- ecdf(score[good.label == 1])
  
  d <- data.frame(score = seq(0,1000, by = 5))
  d$Bad <- ecd.bad(d$score)
  d$Good <- ecd.good(d$score)
  
  p <- gvisLineChart(d, xvar = "score", yvar = c("Bad","Good"),
                     options=list(curveType="function", vAxis="{format:'#,##%'}",
                                  hAxis='{minValue:0, maxValue:1000}', ...))  
  if(export) GVisExportHtml(p, name.html)
  p
}

GVisDist <- function(score, good.label, name.html = "dist_chart", export = F,...){
  library(googleVis)
  cuts <- c(seq(0,9, length = 10),9.99)*100
  t <- Table(cut(score, cuts),good.label)
  t <- t[,c(2,1)]
  names(t) <- c("Good","Bad")
  t$score <- cuts[-length(cuts)] + 50
  t <- rbind(c(0,0,0), t, c(0,0,1000))

  p <- gvisLineChart(t, xvar = "score", yvar = c("Bad","Good"), options=list(curveType="function", ...))
  
  if(export) GVisExportHtml(p, name.html)
  p
}

GVisScatter <- function(var.x, var.y, name.html = "scatter_", export = F, ...){
  library(googleVis)
  p <-  gvisScatterChart(data.frame(var.x, var.y),
                         options=list(legend="none", pointSize=2, ...))
  if(export) GVisExportHtml(p, name.html)
  p
}


DownloadRenta <- function(data,upload.to.datamart= T ,...){
  
  data$EDAD_CALC        <- 0
  data$EDAD_CALC        <- as.numeric(trunc(difftime(as.Date(as.character(data$FECHA_CALC), "%Y%m%d"), as.Date(as.character(data$FNAC_26), "%Y%m%d"),units="days")/365.25))
  data$EDAD_CALC[is.na(data$EDAD_CALC)] <- 0
  data$TRAMO_EDAD_LAB <- cut(data$EDAD_CALC, c(0,29,44,64,Inf), include.lowest=T, labels=F)
  data$SEXO_IND_M       <- ifelse(data$SEXO == 1, 1, 0)
  data$CLEAN            <- ifelse(data$TDOC_12+data$TDOC_02 == 0, 1, 0) #  CLEAN
  data$TOT_MONT         <- data$TMOT_12 + data$TMOUF_02                 #  TOTAL MONTO
  
  if(upload.to.datamart){
  cat("\nUploading data to RENTA_TEMP")
  UploadDataToDatamart(unique(data.frame( RUT = data$RUT, FECHA_CALC = data$FECHA_CALC, SEXO = data$SEXO, TRAMO_EDAD_LAB = data$TRAMO_EDAD_LAB, DES_ULTCOM = data$DES_ULTCOM)),name='RENTA_TEMP')
  }
  
  
  #RENTA_CTA_CTE
  dres <- data.frame(RUT = data$RUT[1],FECHA_CALC = data$FECHA_CALC[1], IND_CTA_CTE = 0)
  UploadDataToDatamart(dres,'RENTA_CTA_CTE_AUX')
  #"RENTA_RUBROS"
  dres <- data.frame(RUT = data$RUT[1],CLUSTER_RUBRO_MEAN = 300856.223)
  UploadDataToDatamart(dres,'RENTA_RUBROS_AUX')
  #"RENTA_PROF_ACTI"
  dres <- data.frame(RUT = data$RUT[1],CLUSTER_ACTIV_MEAN = 307721.623, CLUSTER_PROF_MEAN = 313978.425)
  UploadDataToDatamart(dres,'RENTA_PROF_ACTI_AUX')
  #"RENTA_TEL" 
  dres <- data.frame(RUT = data$RUT[1],FECHA_CALC = data$FECHA_CALC[1], CANT_TEL = 0)
  UploadDataToDatamart(dres,'RENTA_TEL_AUX')
  #"RENTA_MEAN_SAMP"
  dres <- data.frame(RUT = data$RUT[1],RENTA_MEAN_SAMP_AUX = 145000)
  UploadDataToDatamart(dres,'RENTA_MEAN_SAMP_AUX')
  #"RENTA_VEHI"
  dres <- data.frame(RUT = data$RUT[1],VEHI_AVALUO = 0, VEHI_ANTIG = 80,VEHI_MAX_AÑO = 0,VEHI_MIN_AÑO = 0,VEHI_CANTIDAD =0 )
  UploadDataToDatamart(dres,'RENTA_VEHI_AUX')
  #"RENTA_CONSULTAS_NO_SBIF"
  dres <- data.frame(RUT = data$RUT[1],N_NO_SBIF = 0)
  UploadDataToDatamart(dres,'RENTA_CONSULTAS_NO_SBIF_AUX')
  #"RENTA_CONSULTAS_SBIF"
  dres <- data.frame(RUT = data$RUT[1],N_SBIF = 0)
  UploadDataToDatamart(dres,'RENTA_CONSULTAS_SBIF_AUX')
  
  
  data <- subset(data, select = c("RUT","FECHA_CALC","SEXO","TRAMO_EDAD_LAB","SEXO_IND_M", "CLEAN","TOT_MONT", "DES_ULTCOM"))
  
  tablas <- c("RENTA_CTA_CTE", "RENTA_RUBROS", "RENTA_PROF_ACTI","RENTA_TEL", "RENTA_MEAN_SAMP",
              "RENTA_VEHI","RENTA_CONSULTAS_NO_SBIF", "RENTA_CONSULTAS_SBIF")
  
  for(tabla in tablas){
    cat(paste0("\n",tabla))
    t <- DownloadDataFromDatamart(tabla, stringsAsFactor = F) 
    if(nrow(t) ==0){
      t <- DownloadDataFromDatamart(paste0(tabla,"_AUX"), stringsAsFactor = F) 
      t$RUT <- RUT_to_RUT10(t$RUT)
    }
    if(nrow(t) >0){
      t$RUT <- RUT_to_RUT10(t$RUT)
    }
        
    data <- join(data, t, type="left", match="first")
  }
  
  
  data$N_NO_SBIF[is.na(data$N_NO_SBIF)] <- 0 # Consultas no SBIF
  data$RENTA_MEAN_SAMP[is.na(data$RENTA_MEAN_SAMP)] <- 145000 # Renta media de la muestra
  data$VEHI_AVALUO[is.na(data$VEHI_AVALUO)] <- 0 # AValuo fiscal vehiculos
  data$CLUSTER_PROF_MEAN[is.na(data$CLUSTER_PROF_MEAN)] <- 313978.425 # Renta cluster profesion
  data$N_SBIF[is.na(data$N_SBIF)] <- 0 # Consultas SBIF
  data$CLUSTER_ACTIV_MEAN[is.na(data$CLUSTER_ACTIV_MEAN)] <- 307721.623 # Renta cluster actividad
  data$VEHI_ANTIG[is.na(data$VEHI_ANTIG)] <- 80 # Antiguedad vehiculos
  data$CANT_TEL[is.na(data$CANT_TEL)] <- 0 # Cantidad de telefonos
  data$CLUSTER_RUBRO_MEAN[is.na(data$CLUSTER_RUBRO_MEAN)] <- 300856.223 # cluster Rubro
  data$IND_CTA_CTE[is.na(data$IND_CTA_CTE)] <- 0 # Indicacdor cuenta corriente
  data$TOT_MONT[is.na(data$TOT_MONT)] <- 0 # monto total bolcom+sicom
  
  ######################################## Modelo ########################################
  data$RENTA_PRED <- -377241.40 + 
    3365.71*data$N_NO_SBIF + 
    0.61*data$RENTA_MEAN_SAMP + 
    0.01*data$VEHI_AVALUO + 
    0.66*data$CLUSTER_PROF_MEAN + 
    45041.34*data$N_SBIF + 
    0.42*data$CLUSTER_ACTIV_MEAN - 
    31.56*data$VEHI_ANTIG + 
    10069.22*data$CANT_TEL + 
    0.28*data$CLUSTER_RUBRO_MEAN + 
    18597.84*data$SEXO_IND_M + 
    40566.54*data$IND_CTA_CTE + 
    229.64*data$TOT_MONT + 
    11285.28*data$CLEAN
  
  ########################################Ajuste lineal########################################
  data$RENTA_AJUST <- 0
  
  data$RENTA_AJUST <- ifelse(data$RENTA_PRED <= 236686,
                             ((Truncate(data$RENTA_PRED,45634) - 45634) / (236686 - 45634) * (192886  - 72126)) + 72126, 
                             data$RENTA_AJUST)
  
  data$RENTA_AJUST <- ifelse(data$RENTA_PRED > 236686 & data$RENTA_PRED <= 304659,
                             ((data$RENTA_PRED - 236686) / (304659 - 236686) * (300283  - 192886 )) + 192886,
                             data$RENTA_AJUST)
  
  data$RENTA_AJUST <- ifelse(data$RENTA_PRED > 304659 & data$RENTA_PRED <= 382839,
                             ((data$RENTA_PRED - 304659) / (382839 - 304659) * (422848 - 300283 )) + 300283,
                             data$RENTA_AJUST)
  
  data$RENTA_AJUST <- ifelse(data$RENTA_PRED > 382839 & data$RENTA_PRED <= 475029,
                             ((data$RENTA_PRED - 382839) / (475029 - 382839) * (558709 - 422848)) + 422848,
                             data$RENTA_AJUST)
  
  data$RENTA_AJUST <- ifelse(data$RENTA_PRED > 475029 & data$RENTA_PRED <= 583058,
                             ((data$RENTA_PRED - 475029) / (583058 - 475029) * (708570 - 558709)) + 558709,
                             data$RENTA_AJUST)
  
  data$RENTA_AJUST <- ifelse(data$RENTA_PRED > 583058 & data$RENTA_PRED <= 704333,
                             ((data$RENTA_PRED - 583058) / (704333 - 583058) * (869289 - 708570)) + 708570,
                             data$RENTA_AJUST)
  
  data$RENTA_AJUST <- ifelse(data$RENTA_PRED > 704333 & data$RENTA_PRED <= 846740,
                             ((data$RENTA_PRED - 704333) / (846740 - 704333) * (1038262 - 869289)) + 869289,
                             data$RENTA_AJUST)
  
  data$RENTA_AJUST <- ifelse(data$RENTA_PRED > 846740,
                             ((data$RENTA_PRED - 846740) / (1141371 - 846740) * (1206805 - 1038262)) + 1038262,
                             data$RENTA_AJUST)
  

  return(data)
}

# Extra
'%notin%' <- Negate('%in%') 




PlotMap <- function(table, santiago.only = T, label.comuna = T, color = "darkblue"){
  #download.file("http://rex.webfactional.com/media/data/coord_rm.RData", "coord_rm.RData")
  load(file="~/coord_rm.RData")
  coords_rm <- join(coords_rm, table, by = "COMUNA", type = "left")
  
  # unique(coords_rm$COMUNA[coords_rm$PROVINCIA != "SANTIAGO"])
  if(santiago.only){
    coords_rm <- subset(coords_rm, PROVINCIA == "SANTIAGO" | COMUNA %in% c("PUENTE ALTO","PADRE HURTADO"))  
  }
  
  coords_rm <- join(coords_rm,
                    ddply(coords_rm, .(COMUNA), summarize, center.lat = mean(lat), center.long = mean(long)),
                    type = "left")
  
  g <- ggplot(coords_rm) + theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank())
  g <- g + geom_polygon(aes(x=long, y =lat, group = group, alpha = VAR), fill = color, color = "white")
  
  if(label.comuna){
    g <- g + geom_text(aes( x= center.long, y=center.lat, label = COMUNA), size = 2, color = "white")
  }
  g  
}

DataNameUnique <- function(data){
  names(data)[laply(data, function(x) length(unique(x)) > 1)]
}

ModeloRenta <- function(data){
  
  vars <- c("RUT","FECHA_CALC", "SEXO","FORMULA_18", "TMOT_12", "NCHEQUE_12","NCCC",
            "TOT_PRE_08","TMOUF_02","NPAG_06_02","CANT_TEL", "CANT_VEHIC","NDIR_18",
            "DES_ULTCIU","DES_ULTCOM","NACR_02_06")
  
  agr <- ReadTable("C:/Users/igonzalez/Desktop/Proyectos/SCRIPTS/RENTA_AGREGADOS.txt", stringsAsFactors=F)
  daux <- subset(data, select = vars)
  
  #------------------------------------------------------------
  # RENTA GEO MEDIAN
  daux$SEXO_DESC <- ""
  daux$SEXO_DESC <- ifelse(daux$SEXO == 0, "F", daux$SEXO_DESC)
  daux$SEXO_DESC <- ifelse(daux$SEXO == 1, "M", daux$SEXO_DESC)
  daux$SEXO <- daux$SEXO_DESC
  
  daux$EDAD2 <- ifelse(daux$FORMULA_18<18, 18, daux$FORMULA_18)
  
  daux$RANGO_ED <- "."
  daux$RANGO_ED <- ifelse(daux$EDAD >= 18 & daux$EDAD <= 24, '1', daux$RANGO_ED)
  daux$RANGO_ED <- ifelse(daux$EDAD >= 25 & daux$EDAD <= 34, '2', daux$RANGO_ED)
  daux$RANGO_ED <- ifelse(daux$EDAD >= 35 & daux$EDAD <= 50, '3', daux$RANGO_ED)
  daux$RANGO_ED <- ifelse(daux$EDAD > 50, '4', daux$RANGO_ED)
  daux$SEXO_M <- as.numeric(daux$SEXO=="M")
  
  daux <- join(daux, agr, type="left", match="first")
  rm(agr)
  
  daux$RENTA_MEDIAN <- ifelse(is.na(daux$RENTA_MEDIAN), 325000, daux$RENTA_MEDIAN)
  daux$RENTA_GEO <- ifelse(is.na(daux$RENTA_GEO), 321408, daux$RENTA_MEDIAN)
  
  #------------------------------------------------------------
  # SEGMENTACION
  daux$SEGMENTO <- ""
  daux$SEGMENTO <- ifelse(daux$EDAD2 >=18 & daux$EDAD2 <= 24, "L", daux$SEGMENTO)
  
  
  daux$SEGMENTO <- ifelse(daux$EDAD2 >=25 & daux$EDAD2 <= 34 & daux$RENTA_GEO < 350000, "L", daux$SEGMENTO)
  daux$SEGMENTO <- ifelse(daux$EDAD2 >=25 & daux$EDAD2 <= 34 & daux$RENTA_GEO >= 350000 & daux$RENTA_GEO < 430092, "M", daux$SEGMENTO)
  daux$SEGMENTO <- ifelse(daux$EDAD2 >=25 & daux$EDAD2 <= 34 & daux$RENTA_GEO >= 430092, "H", daux$SEGMENTO)
  
  daux$SEGMENTO <- ifelse(daux$EDAD2 >=35 & daux$EDAD2 <= 50 & daux$RENTA_GEO < 321539, "L", daux$SEGMENTO)
  daux$SEGMENTO <- ifelse(daux$EDAD2 >=35 & daux$EDAD2 <= 50 & daux$RENTA_GEO >= 321539 & daux$RENTA_GEO < 372595, "M", daux$SEGMENTO)
  daux$SEGMENTO <- ifelse(daux$EDAD2 >=35 & daux$EDAD2 <= 50 & daux$RENTA_GEO >= 372595, "H", daux$SEGMENTO)
  
  daux$SEGMENTO <- ifelse(daux$EDAD2 >=51 & daux$SEXO_M == 0, "M", daux$SEGMENTO)
  daux$SEGMENTO <- ifelse(daux$EDAD2 >=51 & daux$SEXO_M == 1, "H", daux$SEGMENTO)
  
  #------------------------------------------------------------
  # VARS
  daux$EDAD_LOG <- log(daux$EDAD2)
  daux$TMOT12_10K <- as.numeric(daux$TMOT_12>100)
  daux$NCHEQUE_12_1 <- as.numeric(daux$NCHEQUE_12>0)
  daux$NCCC <- ifelse(is.na(daux$NCCC), 0, daux$NCCC)
  daux$NCCC_LOG <-  log(0.1 + ifelse(daux$NCCC>3, 3, daux$NCCC)) - log(0.1)
  daux$PRE08 <- as.numeric(daux$TOT_PRE_08>0)
  daux$TMOUF_02 <- ifelse(is.na(daux$TMOUF_02), 0, daux$TMOUF_02)
  daux$TMOT02_5K <- Truncate(daux$TMOUF_02, inf=50, sup=200) - 50
  daux$TMOT02_7K <- as.numeric(daux$TMOUF_02>70)
  daux$NPAG02_06_1 <- as.numeric(daux$NPAG_06_02>0)
  daux$NACR02_06_2 <- as.numeric(daux$NACR_02_06>1)
  daux$CANT_VEHIC <- ifelse(is.na(daux$CANT_VEHIC), 0, daux$CANT_VEHIC)
  daux$CANT_VEHIC <- ifelse(daux$CANT_VEHIC > 3, 3, daux$CANT_VEHIC)
  daux$CANT_VEHIC_LOG <- log(0.1 + daux$CANT_VEHIC) - log(0.1)
  daux$CANT_TEL <- ifelse(is.na(daux$CANT_TEL), 0, daux$CANT_TEL)
  daux$CANT_TEL <- ifelse(daux$CANT_TEL > 5, 5, daux$CANT_TEL)
  daux$NDIR_18 <- ifelse(is.na(daux$NDIR_18), 0, daux$NDIR_18)
  daux$NDIR_18 <- ifelse(daux$NDIR_18 > 4, 4, daux$NDIR_18)
  
  #------------------------------------------------------------
  # HIGH
  daux$RENTA_PRED_H <- -111575.5443283 + 
    daux$NCHEQUE_12_1 *                                 81301.90509376 + 
    daux$NCCC_LOG *                                   109616.43296914 +
    daux$SEXO_M *                                      59706.21113045 +
    daux$PRE08 *                                       86566.22069103 +
    daux$TMOT02_5K *                                     873.21321509 +
    daux$NPAG02_06_1 *                                 50887.44436953 +
    daux$NACR02_06_2 *                                 68141.69656388 +
    daux$CANT_VEHIC_LOG *                              40647.76964683 +
    daux$CANT_TEL *                                    32368.99830932 +
    daux$RENTA_GEO *                                       0.60954873 +
    daux$RENTA_MEDIAN *                                    0.54883889
  
  #------------------------------------------------------------
  # MED
  daux$RENTA_PRED_M <- -184414.3831292 + 
    daux$NCHEQUE_12_1 *                                 88837.87414942 +
    daux$NCCC_LOG *                                    83569.20457441 +
    daux$PRE08 *                                      136108.66588215 +
    daux$TMOT02_5K *                                    1002.64569204 +
    daux$NPAG02_06_1 *                                 50131.72981305 +
    daux$CANT_VEHIC_LOG *                              39616.17903894 +
    daux$RENTA_GEO *                                       0.63711121 +
    daux$RENTA_MEDIAN *                                    0.82544253 +
    daux$CANT_TEL *                                    25765.20469733 +
    daux$NDIR_18 *                                     12169.34351322
  
  #------------------------------------------------------------
  # LOW
  daux$RENTA_PRED_L <- -210822.1573807 + 
    daux$EDAD_LOG *                                    43305.46768851 + 
    daux$TMOT12_10K *                                  23072.71721126 +
    daux$NCHEQUE_12_1 *                                 92499.90833708 +
    daux$NCCC_LOG *                                    96917.21839068 +
    daux$SEXO_M *                                      11654.70216597 +
    daux$PRE08 *                                      140267.68091992 +
    daux$TMOT02_5K *                                     451.51821310 +
    daux$TMOT02_7K *                                   74855.60926484 +
    daux$CANT_TEL *                                    10616.38711067 +
    daux$RENTA_GEO *                                       0.50246223 +
    daux$RENTA_MEDIAN *                                    0.77375254 +
    daux$CANT_VEHIC_LOG *                              34494.63436759
  
  #------------------------------------------------------------
  # AJUSTES  
  
  daux$RENTA_PRED <- ifelse(daux$SEGMENTO == "L", daux$RENTA_PRED_L, daux$RENTA_PRED_M)
  daux$RENTA_PRED <- ifelse(daux$SEGMENTO == "H", daux$RENTA_PRED_H, daux$RENTA_PRED)
  
  daux$RENTA_PRED <- ifelse(daux$RENTA_PRED <= 298978,
                            (Truncate(daux$RENTA_PRED, inf = 165838) - 165838) / (298978 - 165838) * (182000 - 64000) + 64000,
                            daux$RENTA_PRED)
  
  daux$RENTA_PRED <- ifelse(daux$RENTA_PRED > 298978 & daux$RENTA_PRED <= 328646,
                            (daux$RENTA_PRED - 298978) / (328646 - 298978) * (225000 - 182000) + 182000,
                            daux$RENTA_PRED)
  
  daux$RENTA_PRED <- ifelse(daux$RENTA_PRED > 328646 & daux$RENTA_PRED <= 359147,
                            (daux$RENTA_PRED - 328646) / (359147 - 328646) * (275000 - 225000) + 225000,
                            daux$RENTA_PRED)
  
  daux$RENTA_PRED <- ifelse(daux$RENTA_PRED > 359147 & daux$RENTA_PRED <= 389773,
                            (daux$RENTA_PRED - 359147) / (389773 - 359147) * (350000 - 275000) + 275000,
                            daux$RENTA_PRED)
  
  daux$RENTA_PRED <- ifelse(daux$RENTA_PRED > 389773 & daux$RENTA_PRED <= 455622,
                            (daux$RENTA_PRED - 389773) / (455622 - 389773) * (505000 - 350000) + 350000,
                            daux$RENTA_PRED)
  
  daux$RENTA_PRED <- ifelse(daux$RENTA_PRED > 455622 & daux$RENTA_PRED <= 516199,
                            (daux$RENTA_PRED - 455622) / (516199 - 455622) * (645000 - 505000) + 505000,
                            daux$RENTA_PRED)
  
  daux$RENTA_PRED <- ifelse(daux$RENTA_PRED > 516199 & daux$RENTA_PRED <= 653492,
                            (daux$RENTA_PRED - 516199) / (653492 - 516199) * (1000000 - 645000) + 645000,
                            daux$RENTA_PRED)
  
  daux$RENTA_PRED <- ifelse(daux$RENTA_PRED > 653492,
                            (daux$RENTA_PRED - 653492) / (1608495 - 653492) * (2000000 - 1000000) + 1000000,
                            daux$RENTA_PRED)
  daux[is.na(daux$RENTA_PRED),]
  TablePercent(is.na(daux$RENTA_PRED))
  
  
  return(daux)
}

CreateFolder <- function(Create =T,...) {
  
  if(Create){
    dir.create("00. Cliente/")
    dir.create("00. Cliente/00.1 Recibidos")
    dir.create("00. Cliente/00.2 Enviados")
    dir.create("01. Diferidos/")
    dir.create("01. Diferidos/01.1 Enviados")
    dir.create("01. Diferidos/01.2 Recibidos")
    dir.create("02. Procesados/")
    dir.create("02. Procesados/02.1 Data")
    dir.create("02. Procesados/02.2 Plots")
  }
}

OUTPUT <- function(data.proc = data,data.or.plot = 'data', file.name = '' ,extension = 'txt',...){
  
  data.proc <- data.proc
  data.or.plot <- data.or.plot
  file.name <- file.name
  extension <- extension
  
  if(data.or.plot == 'data'){
    OUT <- paste0("02. Procesados/02.1 Data/",as.character(format(Sys.Date(), "%Y%m%d")),"_",file.name,"_",nrow(data.proc),".",extension)
  }
  if(data.or.plot == 'plot'){
    
    OUT <- paste0("02. Procesados/02.2 Plots/",as.character(format(Sys.Date(), "%Y%m%d")),"_",file.name,"_",nrow(data.proc),".png")
  }
  
  return(OUT)
}

PHPDesc <- function(data){
  
  daux <- subset(data, select = c("RUT","FECHA_CALC"))
  dres <- rbind(c("Registros",prettyNum(length(daux$RUT), big.mark=".")),
                c("Ruts unicos", prettyNum(length(unique(daux$RUT)), big.mark=".")),
                c("Rut/fechas unicas", prettyNum(nrow(unique(daux)), big.mark=".")),
                c("Fechas unicas", prettyNum(length(unique(daux$FECHA_CALC)), big.mark=".")),
                c("Periodos", prettyNum(length(unique(substring(daux$FECHA_CALC,0,6))), big.mark=".")),
                c("Minima fecha", format(ymd(min(daux$FECHA_CALC)), format="%d %B %Y")),
                c("Maxima fecha", format(ymd(max(daux$FECHA_CALC)), format="%d %B %Y"))
  )
  
  
  dres <- as.data.frame(dres, row.names=NULL)
  names(dres) <- c("Descripcion", "Valor")
  dres
}


ModelosFIN <- function(data,...){
  
  # /** CREACIoN DE LOS SEGMENTOS PRELIMINARES **/
  
  data$SEG_IND_02_NSA <- ifelse(data$TDOC_02 > 0, 1,0)
  data$SEG_IND_12_NSA <- ifelse(data$TDOC_12 > 0, 1,0)
  data$SEG_IND_16_SA_R <- ifelse((data$DDA_VIG + data$DDA_VIG12 + data$DDA_MOR + data$DDA_MOR12 + data$DDA_VEN +
                                    data$DDA_VEN12 + data$DDA_CCONS+ data$DDA_CCONS2+ data$DDA_CCONS3+ data$NACR +
                                    data$DDA_CHIPOT+ data$DDA_CCIAL+ data$DDA_CCIAL2+ data$DDA_CCIAL3+ data$DDA_IVIG +
                                    data$DDA_IVENC+ data$DDA_IVENC2+ data$DDA_IVENC3+ data$DDA_IVENC4+ data$DDA_DCAST +
                                    data$DDA_DCAS12+ data$DDA_ICAST+ data$DDA_ICAST2+ data$DDA_ICAST3+ data$DDA_ICAST4 +
                                    data$LCNU12+data$LCNU12_2) >0 , 1 , 0)
  
  # /** definicion para dirty **/
  
  data$DIRTY <- ifelse((data$DDA_MOR+ data$DDA_VEN+ data$DDA_DCAST+ data$DDA_DCAS12+ data$DDA_IVENC+ data$DDA_ICAST +
                          data$DDA_ICAST2+ data$DDA_ICAST3+ data$DDA_ICAST4 + data$SEG_IND_12_NSA + data$SEG_IND_02_NSA) > 0,1,0)
  
  # /*** NUEVO SEGMENTO USADO POR PUC - VERSION IV ***/
  
  data$SEG_PUC_IV <- "CLEAN"
  data$SEG_PUC_IV <- ifelse((data$SEG_IND_02_NSA + data$SEG_IND_12_NSA + data$SEG_IND_16_SA_R) == 0,"NO INFO",data$SEG_PUC_IV)
  data$SEG_PUC_IV <- ifelse(data$DIRTY == 1,"DIRTY",data$SEG_PUC_IV)
  
  # /** Edad - formula_09 **/
  data$formula_EDAD <- Truncate(data$EDAD_CALC,0,64)
  
  ############################ CALCULO DEL SCORE EXPRESS
  
  ###### PREDICTOR NO INFO
  data$SCORE_EXPRESS_FIN <- 999
  
  ###### PREDICTOR CLEAN
  
  data$FORMULA_01_AUX <- ifelse((data$MESDDACA12 + data$MESDDAVE12) > 0,1,0)
  data$FORMULA_02_AUX <- Truncate(ifelse(data$NACR <=2,0,data$NACR),0,8)
  data$FORMULA_03_AUX <- ifelse((data$PU3_LCNU/100) > 254092667,254092667,(data$PU3_LCNU/100))
  data$FORMULA_04_AUX <- ifelse(data$MESDDAVI12 > 0,data$MESDDAVI12,0 )
  data$FORMULA_05_AUX <- Truncate(data$MESDDAMO12,0,11)
  data$FORMULA_06_AUX <- ifelse((data$MAXUTIL_12/100) > 0.75 , 1, 0)
  data$FORMULA_07_AUX <- data$RELLCNUP12/100
  data$FORMULA_08_AUX <- data$RELCOMEP_3/100
  data$FORMULA_09_AUX <- data$RELCONSP_3/100
  
  data$AUX_CLEAN <- 2.4066434948 +
    -0.2000000000  *	data$FORMULA_01_AUX	+
    -0.2393239779	*	data$FORMULA_02_AUX	+
    0.0017006257	*	data$FORMULA_03_AUX	+
    0.0512599389	*	data$FORMULA_04_AUX	+
    -0.3095723717	*	data$FORMULA_05_AUX	+
    -0.3444115391	*	data$FORMULA_06_AUX	+
    0.5110251280	*	data$FORMULA_07_AUX	+
    -0.3043846340	*	data$FORMULA_08_AUX	+
    -1.4768658754	*	data$FORMULA_09_AUX	
  
  data$AUX_CLEAN <- round(1000/(1+exp(-data$AUX_CLEAN)))
  data$AUX_CLEAN <- Truncate(data$AUX_CLEAN,1,999)
  data$AUX_CLEAN <- round(data$AUX_CLEAN,0)
  
  ###### PREDICTOR DIRTY
  
  data$FORMULA_01_AUX <- ifelse(data$TMUF_02_06 > 0,1,0)
  data$FORMULA_02_AUX <- ifelse(data$TDOC_12_06 > 0,1,0)
  data$FORMULA_03_AUX <- Truncate(data$TMOUF_02,0,100.28)
  data$FORMULA_04_AUX <- Truncate(data$NACR_02,0,4)
  data$FORMULA_05_AUX <- Truncate(data$TDOC_12,0,50)
  data$FORMULA_06_AUX <- Truncate(data$TDOC_02_03,0,16)
  data$FORMULA_07_AUX <- Truncate(data$TDOC_12_03,0,4)
  data$FORMULA_08_AUX <- Truncate(data$NTCR_02_06,0,5)
  data$FORMULA_09_AUX <- Truncate(ifelse(data$NACR <= 1,0,data$NACR),0,9)
  data$FORMULA_10_AUX <- ifelse((data$LCNU/100) > 3139.33,3139.33,(data$LCNU/100))
  data$FORMULA_11_AUX <- data$MESDDAMO12
  data$FORMULA_12_AUX <- data$MESDDAVE12
  data$FORMULA_13_AUX <- data$MESDDACA12
  
  
  data$AUX_DIRTY <-  1.3093615469					+
    -0.5702604251	*	data$FORMULA_01_AUX	+
    -0.7240384872	*	data$FORMULA_02_AUX	+
    -0.0086161805 *	data$FORMULA_03_AUX	+
    -0.3552099122	*	data$FORMULA_04_AUX	+
    -0.0172142711	*	data$FORMULA_05_AUX	+
    -0.1993775751 *	data$FORMULA_06_AUX	+
    -0.6299104229	*	data$FORMULA_07_AUX	+
    -0.6670502439	*	data$FORMULA_08_AUX	+
    -0.3467330853	*	data$FORMULA_09_AUX	+
    0.0018553451	*	data$FORMULA_10_AUX	+
    -0.1642482153	*	data$FORMULA_11_AUX	+
    -0.1254214261	*	data$FORMULA_12_AUX	+
    -0.1524904498	*	data$FORMULA_13_AUX	
  
  data$AUX_DIRTY <- round(1000/(1+exp(-data$AUX_DIRTY)))
  data$AUX_DIRTY <- Truncate(data$AUX_DIRTY,1,999)
  data$AUX_DIRTY <- round(data$AUX_DIRTY,0)
  
  
  data$SCORE_EXPRESS_FIN <- ifelse(data$SEG_PUC_IV == "DIRTY",data$AUX_DIRTY ,data$SCORE_EXPRESS_FIN)
  data$SCORE_EXPRESS_FIN <- ifelse(data$SEG_PUC_IV == "CLEAN",data$AUX_CLEAN ,data$SCORE_EXPRESS_FIN)
  
  data$FORMULA_01_AUX <- NULL
  data$FORMULA_02_AUX <- NULL
  data$FORMULA_03_AUX <- NULL
  data$FORMULA_04_AUX <- NULL
  data$FORMULA_05_AUX <- NULL
  data$FORMULA_06_AUX <- NULL
  data$FORMULA_07_AUX <- NULL
  data$FORMULA_08_AUX <- NULL
  data$FORMULA_09_AUX <- NULL
  data$FORMULA_10_AUX <- NULL
  data$FORMULA_11_AUX <- NULL
  data$FORMULA_12_AUX <- NULL
  data$FORMULA_13_AUX <- NULL
  data$AUX_CLEAN <- NULL
  data$AUX_DIRTY <- NULL
  
  #####MODELO PENTA
  
  #Vars Bureau
  data$FORMULA_04_AUX <- ifelse(data$NDIR_18 > 0,1,0)
  data$FORMULA_07_AUX <- Truncate(data$AVALUO,0,2714515152)/(2000*22800)
  data$FORMULA_09_AUX <- ifelse(data$TMUF_02_06 > 0,1,0)
  data$FORMULA_10_AUX <- Truncate(data$NACR_02,0,4)
  data$FORMULA_17_AUX <- Truncate(data$MORP_02_06,0,110)
  data$FORMULA_18_AUX <- ifelse(Truncate(data$TMUF_02_06,0,100.28) == 0,0,Truncate(data$TMUF_02_03,0,100.28)/Truncate(data$TMUF_02_06,0,100.28))
  data$FORMULA_26_AUX <- Truncate(data$TCMO_12,0,10)
  data$FORMULA_28_AUX <- Truncate(data$TPROSA_12,0,12)
  data$FORMULA_29_AUX <- Truncate(data$TOT_PRE_08,0,5)
  
  #Vars SocioDem
  
  data$ABC1_AUX <- ifelse(data$ISE_EQUIFAX=="ABC1",1,0)
  data$C2_AUX <- ifelse(data$ISE_EQUIFAX=="C2",1,0)
  data$C3_AUX <- ifelse(data$ISE_EQUIFAX=="C3",1,0)
  data$EDAD_R_AUX <- data$EDAD_CALC
  data$DIVORCIADO_AUX <- ifelse(data$EST_CIVIL =="DIVORCIADO",1,0)
  data$RISK_COM_ALTO_AUX <- ifelse(data$RIESGO_COM =="A",1,0)
  data$RISK_COM_BAJO_AUX <- ifelse(data$RIESGO_COM =="B",1,0)
  data$SEXO_M_AUX <- ifelse(data$SEXO ==1,1,0)
  data$SEXO_F_AUX <- ifelse(data$SEXO ==0,1,0)
  data$COUNT_VEHIC_AUX <- data$CANT_VEHIC
  
  #Vars Com
  data$COM_1_AUX <- ifelse(data$DES_ULTCOM == 'LAS CONDES',1,0)
  data$COM_2_AUX <- ifelse(data$DES_ULTCOM == 'ANTOFAGASTA',1,0)
  data$COM_4_AUX <- ifelse(data$DES_ULTCOM == 'PROVIDENCIA',1,0)
  data$COM_6_AUX <- ifelse(data$DES_ULTCOM == 'MAIPU',1,0)
  data$COM_8_AUX <- ifelse(data$DES_ULTCOM == 'VINA DEL MAR',1,0)
  data$COM_10_AUX <- ifelse(data$DES_ULTCOM == 'CONCEPCION',1,0)
  data$COM_18_AUX <- ifelse(data$DES_ULTCOM %in% c("QUILICURA","HUECHURABA","RECOLETA","CONCHALI", "INDEPENDENCIA"),1,0)
  data$COM_20_AUX <- ifelse(data$DES_ULTCOM %in% c("PENALOLEN","MACUL"),1,0)
  data$COM_26_AUX <- ifelse(data$DES_ULTCOM %in% c("LOS ANDES", "SAN ESTEBAN", "CALLE LARGA", "RINCONADA", "LA LIGUA", "ZAPALLAR", "PETORCA", "PAPUDO", "CABILDO", "QUILLOTA", "LIMACHE", "NOGALES", "LA CALERA", "LA CRUZ", "OLMUE", "HIJUELAS", "ALGARROBO", "QUILPUE", "VILLA ALEMANA", "CONCON", "CASABLANCA", "QUINTERO", "PUCHUNCAVI", "JUAN FERNANDEZ", "ISLA DE PASCUA","SAN ANTONIO", "SANTO DOMINGO", "CARTAGENA", "EL QUISCO", "EL TABO", "SAN FELIPE", "LLAY-LLAY","PUTAENDO", "CATEMU", "SANTA MARIA", "PANQUEHUE"),1,0)
  
  
  
  data$SCORE_PENTA <- 1.279 +
    0.1519 * data$COM_4_AUX + 
    0.1735 * data$COM_1_AUX + 
    -0.2134 * data$COM_2_AUX + 
    0.4552 * data$COM_8_AUX + 
    -0.1433 * data$COM_6_AUX + 
    0.0953 * data$COM_26_AUX + 
    -0.0292 * data$COM_20_AUX + 
    -0.1437 * data$COM_18_AUX + 
    0.0545 * data$COM_10_AUX + 
    1.159  * data$ABC1_AUX + 
    1.0078 * data$C2_AUX + 
    0.9974 * data$C3_AUX + 
    -0.1685 * data$DIVORCIADO_AUX + 
    0.0091 * data$EDAD_R_AUX + 
    -0.0916 * data$RISK_COM_ALTO_AUX + 
    0.0462 * data$RISK_COM_BAJO_AUX + 
    -0.0965 * data$SEXO_M_AUX + 
    0.0136 * data$SEXO_F_AUX + 
    0.0549 * data$COUNT_VEHIC_AUX + 
    0.1251 * data$FORMULA_04_AUX + 
    0.086  * data$FORMULA_07_AUX + 
    -0.7597 * data$FORMULA_09_AUX + 
    -1.1636 * data$FORMULA_10_AUX + 
    -0.0287 * data$FORMULA_17_AUX + 
    -1.7329 * data$FORMULA_18_AUX + 
    -0.2549 * data$FORMULA_26_AUX + 
    -0.271  * data$FORMULA_28_AUX + 
    -0.3564 * data$FORMULA_29_AUX + 
    -2 
  
  
  data$SCORE_PENTA <- round(1000/(1+exp(-data$SCORE_PENTA)))
  data$SCORE_PENTA <- Truncate(data$SCORE_PENTA,1,999)
  data$SCORE_PENTA <- round(data$SCORE_PENTA,0)
  
  data$FORMULA_04_AUX <- NULL
  data$FORMULA_07_AUX <- NULL
  data$FORMULA_09_AUX <- NULL
  data$FORMULA_10_AUX <- NULL
  data$FORMULA_17_AUX <- NULL
  data$FORMULA_18_AUX <- NULL
  data$FORMULA_26_AUX <- NULL
  data$FORMULA_28_AUX <- NULL
  data$FORMULA_29_AUX <- NULL
  data$ABC1_AUX <- NULL
  data$C2_AUX <- NULL
  data$C3_AUX <- NULL
  data$EDAD_R_AUX <- NULL
  data$DIVORCIADO_AUX <- NULL
  data$RISK_COM_ALTO_AUX <- NULL
  data$RISK_COM_BAJO_AUX <- NULL
  data$SEXO_M_AUX <- NULL
  data$SEXO_F_AUX <- NULL
  data$COUNT_VEHIC_AUX <- NULL
  data$COM_1_AUX <- NULL
  data$COM_2_AUX <- NULL
  data$COM_4_AUX <- NULL
  data$COM_6_AUX <- NULL
  data$COM_8_AUX <- NULL
  data$COM_10_AUX <- NULL
  data$COM_18_AUX <- NULL
  data$COM_20_AUX <- NULL
  data$COM_26_AUX <- NULL
  
  return(data)
  
}


DataMartQuery <- function(query){
  # This function download the required query
  library(RODBC)
  channel <- odbcConnect( dsn = "DNS_SQL_TASREP", uid = "igonzalez", pwd = "Efx$2013")
  sqlQuery(channel,  gsub("\n"," ",query))
}


DiffVectors <- function(v1,v2, length.out = 10, base = exp(1),tittle1="Var 1",tittle2="Var 2"){
  difs <- abs(v1-v2)
  cuts <- unique(round(c(0,base**seq(logb(1,base),logb(max(difs)), length.out=length.out))))
  t <- ldply(cuts, function(c) c("Cut<=" = c,N = sum(difs<=c)), .progress='text')
  t$Percent <- t$N/length(difs)
  
  library(gridExtra)
  grid.arrange(Histogram(v1-v2, count=F) + labs(title = paste0("Diferencia", "\n",tittle1," menos ",tittle2)),
               Histogram(abs(v1-v2), count=F) + labs(title = "Diferencia absoluta"))
  
  return(list(summary=summary(difs), tablesummary = t))
}



#Removes the defined patterns in all variables replacing its by blanks
CleanVars <- function(dataset, pattern){
  
  vars <- names(dataset)
  
  for(var in vars){
    dataset[[var]]  <-  gsub("\'","",gsub(" ","",gsub(pattern,"",dataset[[var]] )))
  }
  return(dataset)
}

################
#Convert the var type by the defined in the vector of types asked
ConvertTypes <- function(dataset,vectTypes){
  
  vars <- names(dataset)
  i <- 1
  for(var in vars){
    if(vectTypes[i] %in% c("T","X")){  dataset[[var]]  <- as.character(dataset[[var]])}
    if(vectTypes[i] == "D"){  dataset[[var]]  <- as.numeric(dataset[[var]]) }
    if(vectTypes[i] == "N"){  dataset[[var]]  <- as.numeric(dataset[[var]]) }
    i <- i+1
  }
  return(dataset)
}


##########
#Plot a boxplot given a vector of vars to compare, if the length of vars to compare is greater than one then
#compares the values of the vars, if the length is equal to one the var must have at least 2 levels to generate the plot
#additionally need an 'objective' var to ilustrate the stats for each category
Boxplot <- function(dataset,varstocomp, objective.var,...){
  
  if(length(varstocomp) > 1){
    
    df <- data.frame(VALUE = as.numeric(),LEGEND = as.character())
    for(var in varstocomp){
      
      df <- rbind(df,data.frame(VALUE = dataset[[var]], LEGEND = var))
      p <- qplot(LEGEND, VALUE, data=df, geom=c("boxplot"), fill=LEGEND, xlab="",...)
    }    
  }  
  
  if(length(varstocomp) == 1){
    
    dataset$LEGEND <- as.factor(dataset[[varstocomp]] )
    dataset$VALUE <- as.numeric(dataset[[objective.var]])
    p <- qplot(LEGEND, VALUE, data=dataset, geom=c("boxplot"), fill=LEGEND, xlab="",...)
    
  }
  
  plot(p)
}


###############

Trim <- function(variable,factor=" ",...){
  
  #Removes trailing and leading character(s) given in the functions argument
  #from a given vector(variable)
  
  variable <- gsub(paste0("^",factor,"+"),"",variable)
  variable <- gsub(paste0(factor,"+$"),"",variable)
  
  return(variable)
  
}


# Removes leading and trailing factor from all dataset
TrimAll <- function(dataset,...){
  
  vars <- names(dataset)
  
  for(var in vars){
    dataset[[var]]  <- Trim(dataset[[var]] )
  }
  return(dataset)
}


############################
#Computes the optimal cutpoint where KS maximizes the distance between distributions
CutPoint_KS <- function(data, score,gb,plot.title="KS segun Score",...){
  
  daux <- subset(data,select=c(score,gb))
  names(daux) <- c("score","gb")
  
  goods <- sum(daux$gb)
  bads <- nrow(daux)-sum(daux$gb)
  
  tabla <- ddply(daux,.(score),summarize,.progress="text",
                 N=length(score),
                 BADS = length(score) - sum(gb),
                 GOOD = sum(gb))
  rm(daux)
  
  tabla$B_ACUM <- cumsum(tabla$BADS)
  tabla$B_ACUM_PER <- round((tabla$B_ACUM / bads)*100,10)
  
  tabla$G_ACUM <- cumsum(tabla$GOOD)
  tabla$G_ACUM_PER <- round((tabla$G_ACUM / goods)*100,10)
  
  tabla$KS <- tabla$B_ACUM_PER - tabla$G_ACUM_PER
  
  p <- ggplot(data=tabla, aes(x=score, y=KS)) + geom_line(colour="#0099FF") +    
    geom_vline(xintercept = tabla$score[tabla$KS == max(tabla$KS)], colour="darkgreen") +
    labs(title = paste0(plot.title,"\n")) +
    theme(plot.title=element_text(lineheight=.8,face="bold")) +
    scale_x_continuous("Score")
  
  plot(p)
  return( tabla$score[tabla$KS == max(tabla$KS)])
  
}

