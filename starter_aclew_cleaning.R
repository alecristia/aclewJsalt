library(tidyverse)
library(forcats)

# allData_long <- read_delim("~/Dropbox/aclew/starter_aclew_r/data/allData_long.txt", 
#                       "\t", escape_double = FALSE, col_names = FALSE, 
#                       trim_ws = TRUE)
# allData_long <- allData_long %>% 
#   rename(tier = X1,
#          metaname = X2,
#          onset = X3,
#          offset = X4,
#          duration = X5,
#          annotation = X6,
#          filename = X7)

allData <- read_delim("~/Dropbox/aclew/starter_aclew_r/data/allData_wide.txt", 
                           "\t", escape_double = FALSE, col_names = T, 
                           trim_ws = TRUE)
allData <- allData %>% 
#  dplyr::select(-X31) %>% 
  mutate(`xds@CHI2` = factor(`xds@CHI2`),
         `vcm@CHI`  = factor (`vcm@CHI`),
         `mwu@CHI`  = factor (`mwu@CHI`),
         `xds@MA1` = factor (`xds@MA1`),
         `lex@CHI` = factor (`lex@CHI`),
         `xds@EE1` = factor (`xds@EE1`),
         `xds@FA4` = factor (`xds@FA4`),
         `xds@FA3` = factor (`xds@FA3`),
         `xds@FA2` = factor (`xds@FA2`),
         `xds@MC1` = factor (`xds@MC1`),
         `xds@FC1` = factor (`xds@FC1`),
         `xds@FC3` = factor (`xds@FC3`),
         `xds@FC2` = factor (`xds@FC2`),
         `xds@FA1` = factor (`xds@FA1`)
 q  separate(File, c("db-corpus","aclewid","age","recnum","s_intofullfile-coder.ftype"),
           sep = "_", remove = F) %>% 
  separate(`db-corpus`, c("dbgroup","corpus"), sep = "-") %>% 
  separate(`s_intofullfile-coder.ftype`, c("s_intofull","coder.ftype"), sep = "-") %>% 
  separate(coder.ftype, c("coder","ftype"), sep = "\\.") %>% 
  mutate(dbgroup = factor(dbgroup),
         corpus = factor(corpus),
         aclewid = factor(aclewid),
         age = as.numeric(as.character(age)),
         recnum = as.numeric(as.character(recnum)),
         s_intofull = as.numeric(as.character(s_intofull)),
         coder = factor(coder),
         ftype = factor(ftype))

allData=data.frame(allData)
allData$uniqueID=paste(allData$File,allData[,"Begin.Time...ss.msec"],allData[,"End.Time...ss.msec"],sep="_")
#head(allData$uniqueID)

cleanContents <- read_delim("~/Dropbox/aclew/starter_aclew_r/data/cleanContents.txt",
                            "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)
cleanContents<-data.frame(cleanContents)
names(cleanContents)<-c("File","beg","end","content")
cleanContents$uniqueID=paste(cleanContents[,1],cleanContents[,2],cleanContents[,3],sep="_")
head(cleanContents$uniqueID)

merge(allData,cleanContents,all.x=T,all.y=T)->allData 
allData$length=sapply(gregexpr("\\W+", allData$content), length)

summary(allData)

names(allData)[names(allData)=="CHI2"]<-"FC4"
#names(allData)[names(allData)=="EE1"]<-"MAE"

write.table(allData,"~/Dropbox/aclew/starter_aclew_r/data/allData_wide_wlex.txt")

#checks for key info for JSALT being there
adults=names(allData)[grep("^[A-Z]A[0-9]$",names(allData),perl=T)]
allData$xdsOK=NA
for(thisppnt in adults){
  xds=paste0("xds.",thisppnt)
  allData[!is.na(allData[,thisppnt]) & !is.na(allData[,xds]),"xdsOK"]<-1
  allData[!is.na(allData[,thisppnt]) & is.na(allData[,xds]),"xdsOK"]<-0
}
table(allData$xdsOK)
#write.table(allData[allData$xdsOK==0 & !is.na(allData$xdsOK),c("File","beg","end")],"fix_xds.txt",sep="\t")

#these are in fact all ok because kid is old
allData$vcmOK=NA
  allData[!is.na(allData[,"CHI"]) & !is.na(allData[,"vcm.CHI"]),"vcmOK"]<-1
  allData[!is.na(allData[,"CHI"]) & is.na(allData[,"vcm.CHI"]),"vcmOK"]<-0
  table(allData$xdsOK)
#write.table(allData[allData$vcmOK==0 & !is.na(allData$vcmOK),c("File","beg","end")],"fix_vcm.txt",sep="\t")
  
  allData[is.na(allData[,"CHI"]) & !is.na(allData[,"vcm.CHI"]),c("CHI","content")]<-"0."

    allData$uniqueID=paste0(allData$File,allData$beg,allData$end)
  allData[allData[,"uniqueID"]=="ROS_8340_17_01_03600-CG.eaf112.31113.6",c("FA1","content")]<-"&ataratarara."
  allData[allData[,"uniqueID"]=="ROS_8340_17_01_03600-CG.eaf190.46191.7",c("FA2","content")]<-"&piiiprrrr."
  
#Dataset for VAD+Speaker ID:
  #target: file beg end talker
ppnts=names(allData)[grep("^[A-Z][A-Z].$",names(allData),perl=T)]
cbind(allData[,c("File","beg","end")],stack(allData[,ppnts]))->vad
vad[!is.na(vad$values),]->vad
names(vad)[5]<-"speakerID"
write.table(vad[,c("File","beg","end","speakerID")],"~/Dropbox/aclew/starter_aclew_r/data/vad_speakerID.txt",row.names=F,sep="\t")

allData[,c("File","beg","end","vcm.CHI")]->vcm
vcm[!is.na(vcm$vcm.CHI),]->vcm
write.table(vcm,"~/Dropbox/aclew/starter_aclew_r/data/vocalMaturityCHI.txt")

xdsppnts=names(allData)[grep("^xds.[A-Z][A-Z].$",names(allData),perl=T)]
allData$addressee=NA
for(thiscol in xdsppnts) {
  allData[,thiscol]<-gsub("^T$","C",allData[,thiscol])
  allData$addressee=paste0(allData$addressee,allData[,thiscol])
  }
allData$addressee=gsub("NA","",allData$addressee)
allData[allData$addressee!="",c("File","beg","end","addressee")]->xds
write.table(xds,"~/Dropbox/aclew/starter_aclew_r/data/addressee.txt",row.names=F,sep="\t")
