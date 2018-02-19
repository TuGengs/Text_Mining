library("openxlsx")
library("rvest")
library("tibble")
library("qdap")
library("sentimentr")
library("gplots")
library("dplyr")
library("tm")
library("syuzhet")
library("factoextra")
library("beeswarm")
library("scales")
library("RColorBrewer")
library("RANN")
library("tm")
library("topicmodels")
library("corrplot")
library("rJava")
library("Rwordseg")
library("wordcloud")
library("RYoudaoTranslate")
library("neuralnet")

## sheetIndex table = sheet(1)
data = read.xlsx("data/InaugurationInfo.xlsx", sheet = 1)

## Set the current working directory
setwd("data/InauguralSpeeches")

## return File Vector
ups = unique(data$File)

## paste0 Division of " "
## paste string with Division of sep
## return character Vector
files = paste0("inaug",paste(data$File, data$Term, sep = "-"),".txt")

speech.list = NULL
sp = NULL
ssp = NULL
d2 = NULL

## the numbers of files
for(i in 1:length(files)){
            ## read lines         ## 3.31 jump empty   ## choose sep
  sp = paste(readLines(files[i],n=-1, skipNul=TRUE),collapse=" ")
                  ##speech.list filename
  speech.list  = c(speech.list,sp)
}

for(i in 1:length(files)){
  
  sp = paste(paste(readLines(files[i],n=-1, skipNul=TRUE),collapse=" "),sp,collapse = " ")

}

                ## fulltext -> col
speech.list = data.frame(fulltext = speech.list)

## generate list of sentences

sentence.list=NULL
for(i in 1:nrow(speech.list)){
            ##str_detect           ## returns a character vector of sentences split on endmark.
  sentences=sent_detect(speech.list$fulltext[i],
                        endmarks = c("?", ".", "!", "|",";"))
  if(length(sentences)>0){
              ## emotion type
    emotions=get_nrc_sentiment(sentences)
              ## numbers of sentences
    word.count=word_count(sentences)
    # colnames(emotions)=paste0("emo.", colnames(emotions))
    # in case the word counts are zeros?
    ##left diag *，diag \ * row.
    ##* right diag，diag \ * col.
                                    ## matrix
    emotions=diag(1/(word.count+0.01))%*%as.matrix(emotions)
    sentence.list=rbind(sentence.list, 
                        cbind(speech.list[i,-ncol(speech.list)],
                              sentences=as.character(sentences),
                              word.count,
                              emotions,
                              sent.id=1:length(sentences), File = data$File[i],
                              Term = data$Term[i]
                        )
    )
  }
}

sentence.list=
  sentence.list%>%
  filter(!is.na(word.count))


#length of sentences

sel.comparison=c("DonaldJTrump","JohnMcCain", "GeorgeBush", "MittRomney", "GeorgeWBush",
                 "RonaldReagan","AlbertGore,Jr", "HillaryClinton","JohnFKerry", 
                 "WilliamJClinton","HarrySTruman", "BarackObama", "LyndonBJohnson",
                 "GeraldRFord", "JimmyCarter", "DwightDEisenhower", "FranklinDRoosevelt",
                 "HerbertHoover","JohnFKennedy","RichardNixon","WoodrowWilson", 
                 "AbrahamLincoln", "TheodoreRoosevelt", "JamesGarfield", 
                 "JohnQuincyAdams", "UlyssesSGrant", "ThomasJefferson",
                 "GeorgeWashington", "WilliamHowardTaft", "AndrewJackson",
                 "WilliamHenryHarrison", "JohnAdams")

#### First term

pos = which(sentence.list$Term==1 & sentence.list$File%in%sel.comparison)
#sel.comparison=levels(sentence.list$FileOrdered)
sentence.list.sel= sentence.list[pos,]
sentence.list.sel$File= factor(sentence.list.sel$File)

sentence.list.sel$FileOrdered=reorder(sentence.list.sel$File, 
                                      sentence.list.sel$word.count, 
                                      mean, 
                                      order=T)



## Clustering of emotions

emo.means=colMeans(select(sentence.list, anger:trust)>0.01)
col.use=c("red2", "darkgoldenrod1", 
          "chartreuse3", "blueviolet",
          "darkgoldenrod2", "dodgerblue3", 
          "darkgoldenrod1", "darkgoldenrod1")

presid.summary=tbl_df(sentence.list)%>%
  subset(File%in%sel.comparison)%>%
  #group_by(paste0(type, File))%>%
  group_by(File)%>%
  summarise(
    anger=mean(anger),  ##mean 算数平均值
    anticipation=mean(anticipation),
    disgust=mean(disgust),
    fear=mean(fear),
    joy=mean(joy),
    sadness=mean(sadness),
    surprise=mean(surprise),
    trust=mean(trust)
    #negative=mean(negative),
    #positive=mean(positive)
  )

presid.summary=as.data.frame(presid.summary)
rownames(presid.summary)=as.character((presid.summary[,1]))
km.res=kmeans(presid.summary[,-1], iter.max=200,5)

youdaoTranslate<-function(word){
  url = getURL(youdaoUrl(word))
  greout3 = gregexpr("<",url)
  if(length(greout3[[1]])<2){
  obj = fromJSON(url)
  result=paste0(obj$translation[1],collapse="；")
  return(result)
  }else{
    return("")
  }
}

youdaoUrl<-function(word){
  paste("http://fanyi.youdao.com/openapi.do?keyfrom=fy1991--421fy&key=282671603&type=data&doctype=json&version=1.1&q=",word,sep="")
}

greout<-gregexpr("\\.",sp)
ssp_nums = sample(2:length(greout[[1]]), size = 10)

for(i in 2:length(ssp_nums)-1){
    
    ssp_num = ssp_nums[i]
    print(ssp_num)
    
    if(nchar(substring(sp, greout[[1]][ssp_num]+1,last=greout[[1]][ssp_num+1]-1))>1){
    if(nchar(substring(sp, greout[[1]][ssp_num]+1,last=greout[[1]][ssp_num+1]-1))<200){
    sspp = youdaoTranslate(Trim(substring(sp, greout[[1]][ssp_num]+1,last=greout[[1]][ssp_num+1]-1)))
    ssp = paste(ssp,sspp,sep=' ')}}
  
}

nnet=sentence.list%>%select(anger:positive)
concrete_train <- nnet[1:4000,]
concrete_test <- nnet[4500:5000,]
print("Waiting...")
concrete_model <- neuralnet(positive ~ anger+anticipation+disgust+fear+joy+sadness+surprise+trust,data=concrete_train,hidden=8)
print("End...")

model_results <- compute(concrete_model,concrete_test[1:8])
predicted_positive <- model_results$net.result
print("The Result...")

shinyServer(function(input, output) {
  
 
output$fig  <-  renderPlot({   
  if(as.integer(input$v3) == 1) {
    
    beeswarm(word.count~FileOrdered, 
             data=sentence.list.sel,
             horizontal = TRUE, 
             pch=16, col=alpha(brewer.pal(9, "Set1"), 0.6), 
             cex=0.55, cex.axis=0.8, cex.lab=0.8,
             spacing=5/nlevels(sentence.list.sel$FileOrdered),
             las=2, xlab="Number of words in a sentence.", ylab="",
             main="Inaugural speeches")
    
    
    
  }else if(as.integer(input$v3) == 2) {
    par(mar=c(4, 11, 2, 2))
    heatmap.2(cor(sentence.list%>%select(anger:trust)), 
              scale = "none", 
              col = bluered(100),  margin=c(6, 6), key=F,
              trace = "none", density.info = "none")
    
  }else if(as.integer(input$v3) == 3) {
    
    par(mar=c(4, 6, 2, 1))
    barplot(emo.means[order(emo.means)], las=2, col=col.use[order(emo.means)], horiz=T, main="Inaugural Speeches")
    
    
  }else if(as.integer(input$v3) == 4) {
    
    fviz_cluster(km.res, 
                 stand=F, repel= TRUE,
                 data = presid.summary[,-1], xlab="", xaxt="n",
                 show.clust.cent=FALSE)
    
    
  }else if(as.integer(input$v3) == 5) {
    
    barplot(km.res$cluster, main = "number of clusters")
    
  }else if(as.integer(input$v3) == 6){
    
    r =cor(sentence.list%>%select(anger:trust))
    corrplot(r)
    
  }else if(as.integer(input$v3) == 7){
    
    ssp2 = segmentCN(ssp,returnType="tm")
    words2=unlist(lapply(X=ssp2, FUN=segmentCN))
    word2=lapply(X=words2, FUN=strsplit, " ")
    v2=table(unlist(word2))
    v2=rev(sort(v2))
    d2=data.frame(词汇=names(v2), 词频=v2)
    d2=subset(d2, nchar(as.character(d2$词汇))>1 & d2$词频.Freq>=3)
    
    mycolors <- brewer.pal(12,"Paired")
    wordcloud(d2$词频.Var1,d2$词频.Freq,random.order=FALSE,random.color=TRUE,colors=mycolors,family="STXihei")
    
  }else if(as.integer(input$v3) == 8){
      plot(concrete_model)
      print(cor(predicted_positive,concrete_test$positive))
  }
  
})


})



