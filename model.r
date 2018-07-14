#Tablet price prediction

rm(list = ls(all=T))

#Reading train and test data
train_data=read.csv(file = "train.csv",header = T)
test_data=read.csv(file = "test.csv",header = T,fileEncoding="latin1")
test_data$Price=0


#selecting columns with less number of NAs
colu=c("Brand","Brand.Name","Bluetooth","Bluetooth.4.0","Color","Colour","Condition","Depth","Display.Screen.Type",
       "Features","Graphics.Processing.Type","HDMI","Hard.Drive.Capacity","Hardware.Connectivity","MPN",
       "Memory","Memory..RAM.","Memory.RAM","Memory.Size","Memory.Technology","Model","Name","Operating.System",
       "Operating.System.Edition","Price","Processor.Speed","Processor.Type","Product.Line","Release.Year","Resolution",
       "Screen.Size","Seller.Notes","Storage.Type","Type","UPC","Unnamed..0.1","Unnamed..0.1.1")

#Preparing train and test data for pre-processing with selected variables
test_data1=test_data[,colu]
train_data1=train_data[,colu]
train_data1=rbind(train_data1,test_data1)
train_data1$Price[10053:13403]=0
train_data1$Price=as.numeric(train_data1$Price)

#Pre-processing variable "Brand" by selecting brands which occured more often
table(train_data1$Brand)
which(table(train_data1$Brand)>30)
train_data1$Brand=as.character(train_data1$Brand)
train_data1$Brand=tolower(train_data1$Brand)
brands=c("Acer","Apple","Asus","Compaq","DDMS","Dell","Fujitsu","Gateway","HP","IBM","Intel","Lenovo","LG",
         "Microsoft","none","Panasonic","Samsung","Sony","Toshiba","Unbranded")
brands=tolower(brands)
train_data1$Brand[grepl(pattern = "acer",x = train_data1$Brand)]="acer"
train_data1$Brand[grepl(pattern = "apple",x = train_data1$Brand)]="apple"
train_data1$Brand[grepl(pattern = "asus",x = train_data1$Brand)]="asus"
train_data1$Brand[grepl(pattern = "compaq",x = train_data1$Brand)]="compaq"
train_data1$Brand[grepl(pattern = "ddms",x = train_data1$Brand)]="ddms"
train_data1$Brand[grepl(pattern = "dell",x = train_data1$Brand)]="dell"
train_data1$Brand[grepl(pattern = "fujitsu",x = train_data1$Brand)]="fujitsu"
train_data1$Brand[grepl(pattern = "gateway",x = train_data1$Brand)]="gateway"
train_data1$Brand[grepl(pattern = "hp",x = train_data1$Brand)]="hp"
train_data1$Brand[grepl(pattern = "ibm",x = train_data1$Brand)]="ibm"
train_data1$Brand[grepl(pattern = "intel",x = train_data1$Brand)]="intel"
train_data1$Brand[grepl(pattern = "lenovo",x = train_data1$Brand)]="lenovo"
train_data1$Brand[grepl(pattern = "lg",x = train_data1$Brand)]="lg"
train_data1$Brand[grepl(pattern = "microsoft",x = train_data1$Brand)]="microsoft"
train_data1$Brand[grepl(pattern = "panasonic",x = train_data1$Brand)]="panasonic"
train_data1$Brand[grepl(pattern = "samsung",x = train_data1$Brand)]="samsung"
train_data1$Brand[grepl(pattern = "sony",x = train_data1$Brand)]="sony"
train_data1$Brand[grepl(pattern = "toshiba",x = train_data1$Brand)]="toshiba"
train_data1$Brand[grepl(pattern = "unbranded",x = train_data1$Brand)]="unbranded"
train_data1$Brand[grepl(pattern = "none",x = train_data1$Brand)]="none"
train_data1$Brand[which(train_data1$Brand=="")]="none"
table(train_data1$Brand)
train_data1$Brand[which(!train_data1$Brand %in% brands)]="unbranded"
train_data1$Brand=as.factor(train_data1$Brand)
table(train_data1$Brand)

#Removing furthur more variables with most NA values
train_data1$Brand.Name=NULL
train_data1$Bluetooth=NULL
train_data1$Bluetooth.4.0=NULL

#Pre-processing variable "Color"
train_data1$Color
train_data1$Color=as.character(train_data1$Color)
train_data1$Color=tolower(train_data1$Color)
table(train_data1$Color)
which(table(train_data1$Color)>30)
train_data1$Color[which(train_data1$Color=="")]="none"
colur=c("Black","Blue","Gold","Grey","None","Red","Silver","White")
colur=tolower(colur)
train_data1$Color[grepl(pattern = "black",x = train_data1$Color)]="black"
train_data1$Color[grepl(pattern = "blue",x = train_data1$Color)]="blue"
train_data1$Color[grepl(pattern = "gold",x = train_data1$Color)]="gold"
train_data1$Color[grepl(pattern = "grey",x = train_data1$Color)]="grey"
train_data1$Color[grepl(pattern = "gray",x = train_data1$Color)]="grey"
train_data1$Color[grepl(pattern = "red",x = train_data1$Color)]="red"
train_data1$Color[grepl(pattern = "silver",x = train_data1$Color)]="silver"
train_data1$Color[grepl(pattern = "white",x = train_data1$Color)]="white"
table(train_data1$Color)
train_data1$Color[which(!train_data1$Color %in% colur)]="Unique_colors"

#Removing variables with more NA values
train_data1$Colour=NULL

#Pre-processing variable "Condition"
train_data1$Condition
table(train_data1$Condition)
which(table(train_data1$Condition)>30)
train_data1$Condition=as.character(train_data1$Condition)
train_data1$Condition=tolower(train_data1$Condition)
train_data1$Condition[grepl(pattern = "manufacturer refurbished",x = train_data1$Condition)]="manufacturer refurbished"
train_data1$Condition[grepl(pattern = "new",x = train_data1$Condition)]="new"
train_data1$Condition[grepl(pattern = "seller refurbished",x = train_data1$Condition)]="seller refurbished"
train_data1$Condition[grepl(pattern = "used",x = train_data1$Condition)]="used"
train_data1$Condition[grepl(pattern = "not working",x = train_data1$Condition)]="not working"
table(train_data1$Condition)
condi=c("not working","Manufacturer refurbished","New","Seller refurbished","Used")
condi=tolower(condi)
train_data1$Condition[which(!train_data1$Condition %in% condi)]="not_specified"
table(train_data1$Condition)

#Removing variables with many NA values
train_data1$Depth=NULL
train_data1$Display.Screen.Type=NULL
train_data1$HDMI=NULL
train_data1$MPN=NULL

#Preprocessing variable "Memory"
library(dplyr)
train_data1$Memory
train_data1$Memory=as.character(train_data1$Memory)
train_data1$Memory=tolower(train_data1$Memory)
which(table(train_data1$Memory)>50)
mem_rows=which(grepl(pattern = "gb",x = train_data1$Memory))
train_data1[mem_rows,"Memory"]
juju=sapply(train_data1[mem_rows,"Memory"], function(y) paste(unlist(strsplit(y, "gb"))[1], collapse = " "))
class(juju)
juju[1]
names(juju)
juju=unname(juju)
juju=as.numeric(juju)
train_data1[mem_rows,"Memory"]=juju
table(train_data1$Memory)
train_data1[mem_rows,"Memory"]=sprintf('%sgb', train_data1[mem_rows,"Memory"])
table(train_data1$Memory)
train_data1$Memory[grepl(pattern = "512mb ",x = train_data1$Memory)]="512mb"
train_data1$Memory[grepl(pattern = "512 mb ",x = train_data1$Memory)]="512mb"
train_data1$Memory[grepl(pattern = "512",x = train_data1$Memory)]="512mb"
train_data1$Memory[grepl(pattern = "does not apply",x = train_data1$Memory)]="none"
train_data1$Memory[grepl(pattern = "no",x = train_data1$Memory)]="none"
train_data1$Memory[grepl(pattern = "none",x = train_data1$Memory)]="none"
mem_s=c("512mb","1gb","2gb","3gb","4gb","6gb","8gb","12gb","16gb","unknow","none")
train_data1$Memory[which(train_data1$Memory=="")]="unknow"
train_data1$Memory[which(!train_data1$Memory %in% mem_s)]="unknow"
table(train_data1$Memory)

#Removing variables with many NA values
train_data1$Name=NULL
train_data1$Model=NULL
train_data1$Memory.Technology=NULL
train_data1$Memory.Size=NULL
train_data1$Memory.RAM=NULL
train_data1$Memory..RAM.=NULL
train_data1$Unnamed..0.1.1=NULL
train_data1$Unnamed..0.1=NULL
train_data1$UPC=NULL
train_data1$Seller.Notes=NULL
train_data1$Resolution=NULL

#Pre-processing variable "Type"
train_data1$Type
table(train_data1$Type)
which(table(train_data1$Type)>30)
train_data1$Type=as.character(train_data1$Type)
train_data1$Type=tolower(train_data1$Type)
train_data1$Type[grepl(pattern = "ultra",x = train_data1$Type)]="ultrabook"
train_data1$Type[grepl(pattern = "tablet/laptop",x = train_data1$Type)]="tablet/laptop"
train_data1$Type[grepl(pattern = "notebook / laptop",x = train_data1$Type)]="notebook/laptop"
train_data1$Type[grepl(pattern = "notebook laptop",x = train_data1$Type)]="notebook/laptop"
train_data1$Type[grepl(pattern = "notebook/ laptop",x = train_data1$Type)]="notebook/laptop"
train_data1$Type[grepl(pattern = "notebook/laptop",x = train_data1$Type)]="notebook/laptop"
train_data1$Type[grepl(pattern = "notebook//laptop",x = train_data1$Type)]="notebook/laptop"
train_data1$Type[grepl(pattern = "netbook",x = train_data1$Type)]="notebook"
train_data1$Type[grepl(pattern = "nb",x = train_data1$Type)]="notebook"
train_data1$Type[grepl(pattern = "laptop/notebook",x = train_data1$Type)]="notebook/laptop"
train_data1$Type[grepl(pattern = "laptop notebook",x = train_data1$Type)]="notebook/laptop"
train_data1$Type[grepl(pattern = "laptop / notebook",x = train_data1$Type)]="notebook/laptop"
train_data1$Type[grepl(pattern = "laptop/tablet",x = train_data1$Type)]="tablet/laptop"
train_data1$Type[which(train_data1$Type=="")]="unspecified"
types=c("laptop","notebook","notebook/laptop","tablet/laptop","ultrabook","unspecified")
train_data1$Type[which(!train_data1$Type %in% types)]="unspecified"


#Preprocessing variable "Screen.Size"
train_data1$Screen.Size
table(train_data1$Screen.Size)
which(table(train_data1$Screen.Size)>30)
train_data1$Screen.Size=as.character(train_data1$Screen.Size)
train_data1$Screen.Size=tolower(train_data1$Screen.Size)
train_data1$Screen.Size[grepl(pattern = "10",x = train_data1$Screen.Size)]="10''"
train_data1$Screen.Size[grepl(pattern = "11",x = train_data1$Screen.Size)]="11''"
train_data1$Screen.Size[grepl(pattern = "12",x = train_data1$Screen.Size)]="12''"
train_data1$Screen.Size[grepl(pattern = "13",x = train_data1$Screen.Size)]="13''"
train_data1$Screen.Size[grepl(pattern = "14",x = train_data1$Screen.Size)]="14''"
train_data1$Screen.Size[grepl(pattern = "15",x = train_data1$Screen.Size)]="15''"
train_data1$Screen.Size[grepl(pattern = "16",x = train_data1$Screen.Size)]="16''"
train_data1$Screen.Size[grepl(pattern = "17",x = train_data1$Screen.Size)]="17''"
sizes=c("10''","11''","12''","13''","14''","15''","16''","17''","18''")
train_data1$Screen.Size[which(!train_data1$Screen.Size %in% sizes)]="unknown/abnormal"
table(train_data1$Screen.Size)

#Pre-processing variable "Release.Year"
train_data1$Release.Year
table(train_data1$Release.Year)
train_data1$Release.Year=as.character(train_data1$Release.Year)
train_data1$Release.Year= gsub('[[:punct:] ]+',' ',train_data1$Release.Year)
library(stringr)
str_replace_all(string=train_data1$Release.Year, pattern=" ", repl="")
train_data1$Release.Year
mat=regmatches(train_data1$Release.Year, gregexpr("[[:digit:]]+", train_data1$Release.Year))
unique(mat)
for(i in 1:nrow(train_data1)){
  train_data1$Release.Year[i]=mat[[i]][1]
}
unique(train_data1$Release.Year)
which(table(train_data1$Release.Year)>30)
years=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017")
train_data1$Release.Year[which(!train_data1$Release.Year %in% years)]="unknown"
table(train_data1$Release.Year)

train_data1$Product.Line=NULL

#Preprocessing variable "Processor.Type"
train_data1$Processor.Type
table(train_data1$Processor.Type)
which(table(train_data1$Processor.Type)>30)
train_data1$Processor.Type=as.character(train_data1$Processor.Type)
train_data1$Processor.Type=tolower(train_data1$Processor.Type)
train_data1$Processor.Type[grepl(pattern = "pentium",x = train_data1$Processor.Type)]="pentium"
train_data1$Processor.Type[grepl(pattern = "i7",x = train_data1$Processor.Type)]="i7"
train_data1$Processor.Type[grepl(pattern = "i5",x = train_data1$Processor.Type)]="i5"
train_data1$Processor.Type[grepl(pattern = "i3",x = train_data1$Processor.Type)]="i3"
train_data1$Processor.Type[grepl(pattern = "duo",x = train_data1$Processor.Type)]="duo"
train_data1$Processor.Type[grepl(pattern = "celeron",x = train_data1$Processor.Type)]="celeron"
train_data1$Processor.Type[grepl(pattern = "atom",x = train_data1$Processor.Type)]="atom"
train_data1$Processor.Type[grepl(pattern = "amd",x = train_data1$Processor.Type)]="amd"
train_data1$Processor.Type[which(train_data1$Processor.Type=="")]="unknownn"
processs=c("amd","atom","celeron","duo","i3","i5","i7","pentium","unknownn")
train_data1$Processor.Type[which(!train_data1$Processor.Type %in% processs)]="unique_ones"
table(train_data1$Processor.Type)

train_data1$Processor.Speed=NULL

#Pre-processing variable "Operating.System"
train_data1$Operating.System
which(table(train_data1$Operating.System)>30)
table(train_data1$Operating.System)
train_data1$Operating.System=as.character(train_data1$Operating.System)
train_data1$Operating.System=tolower(train_data1$Operating.System)
train_data1$Operating.System[grepl(pattern = "xp",x = train_data1$Operating.System)]="windows xp"
train_data1$Operating.System[grepl(pattern = "vista",x = train_data1$Operating.System)]="windows vista"
train_data1$Operating.System[grepl(pattern = "8.1",x = train_data1$Operating.System)]="windows 8.1"
train_data1$Operating.System[grepl(pattern = "8",x = train_data1$Operating.System)]="windows 8"
train_data1$Operating.System[grepl(pattern = "7",x = train_data1$Operating.System)]="windows 7"
train_data1$Operating.System[grepl(pattern = "xp",x = train_data1$Operating.System)]="windows xp"
train_data1$Operating.System[grepl(pattern = "10",x = train_data1$Operating.System)]="windows 10"
train_data1$Operating.System[grepl(pattern = "xp",x = train_data1$Operating.System)]="windows xp"
train_data1$Operating.System[grepl(pattern = "no",x = train_data1$Operating.System)]="no OS"
train_data1$Operating.System[grepl(pattern = "mac",x = train_data1$Operating.System)]="mac os"
train_data1$Operating.System[grepl(pattern = "chrome",x = train_data1$Operating.System)]="chrome os"
train_data1$Operating.System[which(train_data1$Operating.System=="")]="Unknown OS"
oses=c("chrome os","no OS","Unknown OS","windows 10","windows 7","windows 8","windows vista","windows xp","mac os")
train_data1$Operating.System[which(!train_data1$Operating.System %in% oses)]="uniq"
table(train_data1$Operating.System)

train_data1$Operating.System.Edition=NULL

#Pre-processing variable "Hardware.Connectivity"
train_data1$Hardware.Connectivity
train_data1$Hardware.Connectivity=as.character(train_data1$Hardware.Connectivity)
comp_split=strsplit(x = train_data1$Hardware.Connectivity,split = ",",fixed = T)
as.character(length(comp_split[[995]]))
for(k in 1:nrow(train_data1)){
  train_data1$Hardware.Connectivity[k]=as.character(length(comp_split[[k]]))
}
train_data1$Hardware.Connectivity
train_data1$Hardware.Connectivity=as.numeric(train_data1$Hardware.Connectivity)

#Pre-processing variable "Hard.Drive.Capacity"
train_data1$Hard.Drive.Capacity
table(train_data1$Hard.Drive.Capacity)
which(table(train_data1$Hard.Drive.Capacity)>50)
train_data1$Hard.Drive.Capacity=as.character(train_data1$Hard.Drive.Capacity)
train_data1$Hard.Drive.Capacity=tolower(train_data1$Hard.Drive.Capacity)
train_data1$Hard.Drive.Capacity[grepl(pattern = "1 tb",x = train_data1$Hard.Drive.Capacity)]="1tb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "1tb",x = train_data1$Hard.Drive.Capacity)]="1tb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "500 gb",x = train_data1$Hard.Drive.Capacity)]="512gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "500gb",x = train_data1$Hard.Drive.Capacity)]="512gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "512gb",x = train_data1$Hard.Drive.Capacity)]="512gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "512 gb",x = train_data1$Hard.Drive.Capacity)]="512gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "80gb",x = train_data1$Hard.Drive.Capacity)]="80gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "80 gb",x = train_data1$Hard.Drive.Capacity)]="80gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "750 gb",x = train_data1$Hard.Drive.Capacity)]="750gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "750gb",x = train_data1$Hard.Drive.Capacity)]="750gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "60gb",x = train_data1$Hard.Drive.Capacity)]="60gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "60 gb",x = train_data1$Hard.Drive.Capacity)]="60gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "128gb",x = train_data1$Hard.Drive.Capacity)]="128gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "128 gb",x = train_data1$Hard.Drive.Capacity)]="128gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "120 gb",x = train_data1$Hard.Drive.Capacity)]="128gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "120gb",x = train_data1$Hard.Drive.Capacity)]="128gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "160 gb",x = train_data1$Hard.Drive.Capacity)]="128gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "160 gb",x = train_data1$Hard.Drive.Capacity)]="128gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "250 gb",x = train_data1$Hard.Drive.Capacity)]="256gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "250gb",x = train_data1$Hard.Drive.Capacity)]="256gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "256gb",x = train_data1$Hard.Drive.Capacity)]="256gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "256 gb",x = train_data1$Hard.Drive.Capacity)]="256gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "320gb",x = train_data1$Hard.Drive.Capacity)]="256gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "no",x = train_data1$Hard.Drive.Capacity)]="none"
train_data1$Hard.Drive.Capacity[grepl(pattern = "64 gb",x = train_data1$Hard.Drive.Capacity)]="64gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "64gb",x = train_data1$Hard.Drive.Capacity)]="64gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "60 gb",x = train_data1$Hard.Drive.Capacity)]="64gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "60gb",x = train_data1$Hard.Drive.Capacity)]="64gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "32 gb",x = train_data1$Hard.Drive.Capacity)]="32gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "32gb",x = train_data1$Hard.Drive.Capacity)]="32gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "30 gb",x = train_data1$Hard.Drive.Capacity)]="32gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "30gb",x = train_data1$Hard.Drive.Capacity)]="32gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "40 gb",x = train_data1$Hard.Drive.Capacity)]="32gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "40gb",x = train_data1$Hard.Drive.Capacity)]="32gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "80 gb",x = train_data1$Hard.Drive.Capacity)]="64gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "80gb",x = train_data1$Hard.Drive.Capacity)]="64gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "16gb",x = train_data1$Hard.Drive.Capacity)]="16gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "16 gb",x = train_data1$Hard.Drive.Capacity)]="16gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "4 gb",x = train_data1$Hard.Drive.Capacity)]="4gb"
train_data1$Hard.Drive.Capacity[grepl(pattern = "4gb",x = train_data1$Hard.Drive.Capacity)]="4gb"
train_data1$Hard.Drive.Capacity[which(train_data1$Hard.Drive.Capacity=="")]="Not_specified"
mem=c("128gb","16gb","1tb","256gb","32gb","512gb","64gb","4gb","512gb","750gb","none","Not_specified")
train_data1$Hard.Drive.Capacity[which(!train_data1$Hard.Drive.Capacity %in% mem)]="Not_specified"
table(train_data1$Hard.Drive.Capacity)

train_data1$Features=NULL

#Pre-processing variable "Graphics.Processing.Type"
train_data1$Graphics.Processing.Type
which(table(train_data1$Graphics.Processing.Type)>50)
train_data1$Graphics.Processing.Type=as.character(train_data1$Graphics.Processing.Type)
train_data1$Graphics.Processing.Type=tolower(train_data1$Graphics.Processing.Type)
train_data1$Graphics.Processing.Type[grepl(pattern = "dedicated",x = train_data1$Graphics.Processing.Type)]="dedicated"
train_data1$Graphics.Processing.Type[grepl(pattern = "hybrid",x = train_data1$Graphics.Processing.Type)]="hybrid"
train_data1$Graphics.Processing.Type[grepl(pattern = "integrated",x = train_data1$Graphics.Processing.Type)]="integrated"
train_data1$Graphics.Processing.Type[grepl(pattern = "intel",x = train_data1$Graphics.Processing.Type)]="intel"
train_data1$Graphics.Processing.Type[grepl(pattern = "no",x = train_data1$Graphics.Processing.Type)]="none"
train_data1$Graphics.Processing.Type[which(train_data1$Graphics.Processing.Type=="")]="Unkonwn"
gpus=c("dedicated","hybrid","integrated","intel","none","Unknown")
train_data1$Graphics.Processing.Type[which(!train_data1$Graphics.Processing.Type %in% gpus)]="Unknown"
table(train_data1$Graphics.Processing.Type)

#Pre-processing variable "Storage.Type"
train_data1$Storage.Type
table(train_data1$Storage.Type)
which(table(train_data1$Storage.Type)>30)
train_data1$Storage.Type=as.character(train_data1$Storage.Type)
train_data1$Storage.Type=tolower(train_data1$Storage.Type)
train_data1$Storage.Type[which(train_data1$Storage.Type=="hdd + ssd")]="hdss"
train_data1$Storage.Type[which(train_data1$Storage.Type=="hdd+ssd")]="hdss"
train_data1$Storage.Type[which(train_data1$Storage.Type=="hdd  ssd")]="hdss"
train_data1$Storage.Type[grepl(pattern = "solid state drive",x = train_data1$Storage.Type)]="ssd"
train_data1$Storage.Type[grepl(pattern = "ssd",x = train_data1$Storage.Type)]="ssd"
train_data1$Storage.Type[grepl(pattern = "hdd",x = train_data1$Storage.Type)]="hdd"
train_data1$Storage.Type[grepl(pattern = "no",x = train_data1$Storage.Type)]="none"
train_data1$Storage.Type[which(train_data1$Storage.Type=="")]="not_specified"
drives=c("hdd","ssd","none","hdss","not_specified")
train_data1$Storage.Type[which(!train_data1$Storage.Type %in% drives)]="not_specified"
table(train_data1$Storage.Type)



#Converting all datatypes of variables
str(train_data1)
train_data1$Color=as.factor(train_data1$Color)
train_data1$Condition=as.factor(train_data1$Condition)
train_data1$Graphics.Processing.Type=as.factor(train_data1$Graphics.Processing.Type)
train_data1$Hard.Drive.Capacity=as.factor(train_data1$Hard.Drive.Capacity)
train_data1$Memory=as.factor(train_data1$Memory)
train_data1$Operating.System=as.factor(train_data1$Operating.System)
train_data1$Processor.Type=as.factor(train_data1$Processor.Type)
train_data1$Release.Year=as.factor(train_data1$Release.Year)
train_data1$Screen.Size=as.factor(train_data1$Screen.Size)
train_data1$Storage.Type=as.factor(train_data1$Storage.Type)
train_data1$Type=as.factor(train_data1$Type)


trainny=train_data1[1:10052,]
testy=train_data1[10053:13403,]

#Removing rows with NAs in variable "Price"
which(is.na(trainny$Price))
trainny=trainny[-c(1605,4359,5842), ]

#Model Building using Random Forest by ensembling 250 trees and tuning mtry
library(randomForest)
model1=tuneRF(x = trainny[,-9],y = trainny$Price,ntreeTry = 250,improve = T,trace = T,plot = T,doBest = T)
varImpPlot(model1)
plot(model1)

#Using mtry=4
pred1=predict(object = model1,newdata = testy)
names(pred1)=NULL

final_result1=data.frame("Index"=test_data$index,"Price"=pred1)

#writing final predictions to csv file
write.csv(x = final_result1,file = "final_result.csv")
