library(jsonlite)
library(purrr)
library(dplyr)
salary107 <- fromJSON("/Users/laichunming/Desktop/Salary107_json/107年各教育程度別初任人員每人每月經常性薪資─按大職類分.json")
salary104 <- fromJSON("/Users/laichunming/Desktop/Salary104.json")

salary104$大職業別 <- gsub("、","_",salary104$大職業別)
salary104$大職業別 <- gsub("部門","",salary104$大職業別)
salary107$大職業別 <- gsub("出版、影音製作、傳播及資通訊服務業","資訊及通訊傳播業",
                       salary107$大職業別)
salary107$大職業別 <- gsub("工程","業",salary107$大職業別)
salary107$大職業別 <- gsub("建","造",salary107$大職業別)
salary104$大職業別 <- gsub("教育服務業","教育業",salary104$大職業別)
salary104$大職業別 <- gsub("醫療保健服務業","醫療保健業",salary104$大職業別)
compare <- cbind(salary104$大職業別,salary107$大職業別)

salary104$`經常性薪資-薪資` <- gsub("—|…","",salary104$`經常性薪資-薪資`)
salary104$`經常性薪資-女/男` <- gsub("—|…","",salary104$`經常性薪資-女/男`)
salary104$`國中及以下-薪資` <- gsub("—|…","",salary104$`國中及以下-薪資`)
salary104$`國中及以下-女/男` <- gsub("—|…","",salary104$`國中及以下-女/男`)
salary104$`高中或高職-薪資` <- gsub("—|…","",salary104$`國中及以下-女/男`)
salary104$`高中或高職-女/男` <- gsub("—|…","",salary104$`國中及以下-女/男`)
salary104$`專科-薪資` <- gsub("—|…","",salary104$`國中及以下-女/男`)
salary104$`專科-女/男` <- gsub("—|…","",salary104$`國中及以下-女/男`)
salary104$`大學-薪資` <- gsub("—|…","",salary104$`大學-薪資`)
salary104$`大學-女/男`<- gsub("—|…","",salary104$`大學-女/男`)
salary104$`研究所及以上-薪資` <- gsub("—|…","",salary104$`研究所及以上-薪資`)
salary104$`研究所及以上-女/男`<- gsub("—|…","",salary104$`研究所及以上-女/男`)

names(salary107)[13] <- '研究所及以上-薪資'
names(salary107)[14] <- '研究所及以上-女/男'

salary107$`經常性薪資-薪資` <- gsub("—|…","",salary107$`經常性薪資-薪資`)
salary107$`經常性薪資-女/男` <- gsub("—|…","",salary107$`經常性薪資-女/男`)
salary107$`國中及以下-薪資` <- gsub("—|…","",salary107$`國中及以下-薪資`)
salary107$`國中及以下-女/男` <- gsub("—|…","",salary107$`國中及以下-女/男`)
salary107$`高中或高職-薪資` <- gsub("—|…","",salary107$`國中及以下-女/男`)
salary107$`高中或高職-女/男` <- gsub("—|…","",salary107$`國中及以下-女/男`)
salary107$`專科-薪資` <- gsub("—|…","",salary107$`國中及以下-女/男`)
salary107$`專科-女/男` <- gsub("—|…","",salary107$`國中及以下-女/男`)
salary107$`大學-薪資` <- gsub("—|…","",salary107$`大學-薪資`)
salary107$`大學-女/男`<- gsub("—|…","",salary107$`大學-女/男`)
salary107$`研究所及以上-薪資` <- gsub("—|…","",salary107$`研究所及以上-薪資`)
salary107$`研究所及以上-女/男`<- gsub("—|…","",salary107$`研究所及以上-女/男`)

salary104 <- salary104[,c(1,2,11,12,13,14)]
salary107 <- salary107[,c(1,2,11,12,13,14)]
joinData <- inner_join(salary104,salary107,by="大職業別")

joinData$`大學-薪資.x` <- as.numeric(joinData$`大學-薪資.x`)
joinData$`大學-薪資.y` <- as.numeric(joinData$`大學-薪資.y`)
joinData <- mutate(joinData,大學薪資比=`大學-薪資.y` / `大學-薪資.x`)
#排序
joinDataOrder <- joinData[order(joinData$大學薪資比,decreasing = T),]
head(joinDataOrder,10)
#篩選提高比例大於5%
DataAws <- filter(joinDataOrder,大學薪資比>1.05)
#主要的職業種類
DataAws$splitData <- strsplit(DataAws$大職業別,"-")
DataAws$splitData <- lapply(DataAws$splitData, "[", 1)
DataAws$splitData<-unlist(DataAws$splitData)
View(table(DataAws$splitData))

###########################################################
salary104$`大學-女/男` <- as.numeric(salary104$`大學-女/男`)
View(salary104[order(salary104$`大學-女/男`),])
View(salary104[order(salary104$`大學-女/男`,decreasing = T),])
#男生的薪資比女生多104
menSalary104 <- filter(salary104,`大學-女/男`<100)
head(menSalary104[order(menSalary104$`大學-女/男`,decreasing = T),],10)
#女生的薪資比男生多104
womenSalaryH104 <- filter(salary104,`大學-女/男`>=100)
womenSalaryH104[order(womenSalaryH104$`大學-女/男`,decreasing = T),]

salary107$`大學-女/男` <- as.numeric(salary107$`大學-女/男`)
View(salary107[order(salary107$`大學-女/男`),])
View(salary107[order(salary107$`大學-女/男`,decreasing = T),])
#男生的薪資比女生多107
menSalary107 <- filter(salary107,`大學-女/男`<100)
head(menSalary107[order(menSalary107$`大學-女/男`,decreasing = T),],10)
#女生的薪資比男生多107
womenSalaryH107 <- filter(salary107,`大學-女/男`>=100)
womenSalaryH107[order(womenSalaryH107$`大學-女/男`,decreasing = T),]

###########################################################

#哪一個職業念研究所比較划算
salary107$`研究所及以上-薪資` <- as.numeric(salary107$`研究所及以上-薪資`)
salary107$`大學-薪資` <- as.numeric(salary107$`大學-薪資`)
salary107$Study <- salary107$`研究所及以上-薪資`/salary107$`大學-薪資`
#joinData$`研究所及以上-薪資.y` <- as.numeric(joinData$`研究所及以上-薪資.y`)
salary107$Study <- salary107$`研究所及以上-薪資`/salary107$`大學-薪資`
#View(joinData[order(joinData$Study.x,decreasing = T),])
View(head(salary107[order(salary107$Study,decreasing = T),],10))
#joinData$Study.y <- joinData$`研究所及以上-薪資.y`/joinData$`大學-薪資.y`
#View(joinData[order(joinData$Study.y,decreasing = T),])
#View(head(joinData[order(joinData$Study.y,decreasing = T),],10))

###########################################################################

job <- salary107[grepl("資訊",salary107$大職業別),]
job <- job[1:4,]
job$jobDifferent <- job$`研究所及以上-薪資` - job$`大學-薪資`




