data=read.table('20170301_play.log',fill=TRUE,sep = "\t",quote = "",stringsAsFactors = FALSE)
#find empty rows and delete
row_na=apply(data,1,function(x) sum(x==""))
length(which(row_na!=0))/dim(data)[1]
data1=data[-which(row_na!=0),]
#check last column
unique(data1[,9])
table(data1[,9])
sum(data1[,9]==0)/length(rownames(data1))
#decide to remove last column, since no useful info it provides
data1=data1[,-9]
#name
colnames(data1) <- c("uid","os","rid","type","name","singer","play_time","track_time")
#sort by uid
data2=data1[with(data1, order(uid)),]


#process track_time
sum(with(data2,track_time=="0 "))/dim(data2)[1]
data3=data2[-which(data2["track_time"]=="0 "),]
data3$play_time<-as.numeric(data3$play_time)
data3$track_time<-as.numeric(data3$track_time)
data3=data3[-which(is.na(data3$play_time)),]
# data3=data3[-which(is.na(data3$track_time)),] #no na after last command
data3$play_ratio<-data3$play_time/data3$track_time
length(which(data3$play_ratio>=10))/dim(data3)[1] #0.01803919
length(which(data3$play_ratio>=100))/dim(data3)[1] #0.01469108
data3=data3[-which(data3$play_ratio>=100),] #remove unreasonable play_ratio


#look uid
length(which(table(data3$uid)>=100))/dim(data3)[1] #0.001294829


#process name
data4<-data3
data3$name<-gsub('(-\\(.*\\))|(\\(.*\\))|(\\{.*\\})|(（.*）)', '', data3$name)
data3$name<-gsub('\\s*$','',data3$name)
data3$name[1:100]

length(grep('-',data3$name))/dim(data3)[1]
# data3$name[grep('-',data3$name)][1:100]
data3<-data3[-grep('-',data3$name),]

length(grep('^[0-9]+$',data3$name))/dim(data3)[1]
data3<-data3[-grep('^[0-9]+$',data3$name),]

length(grep('\\.+.*\\.+',data3$name))/dim(data3)[1]
data3<-data3[-grep('\\.+.*\\.+',data3$name),]

data3$name<-gsub('(\\(.*）)|(（.*\\))','',data3$name)

length(grep('\\s*(\\(|（).*',data3$name))/dim(data3)[1]
cc<-grep('\\s*(\\(|（).*',data3$name)
data3$name<-gsub('\\s*(\\(|（).*','',data3$name)
data44<-data3[cc,]

data3$name<-gsub('\\[.*\\]','',data3$name)

length(grep('【|】',data3$name))/dim(data3)[1]
data3<-data3[-grep('【|】',data3$name),]


#process singer
data3$singer[1:100]
data3$singer<-gsub('\\s*$','',data3$singer)

length(grep('未知歌手',data3$singer))/dim(data3)[1]
data3<-data3[-grep('未知歌手',data3$singer),]

length(which(data3$singer==" "))/dim(data3)[1]
data3<-data3[-which(data3$singer==" "),]

data3[which(data3$uid=="154408074 "),]$singer

data3$singer<-gsub('\\[.*\\]','',data3$singer)

### do the same thing for singer !!

### more thing i can do since do much data: for the accuracy of recommendation, remove all the user who involve in deleting his/her music


### if play ratio <1/9, means the user don't like it

which(data1$track_time %in% c(0))

table(data3$type)[2]/dim(data3)[1]

data4<-data3[which(data3$type=="2 "),]

