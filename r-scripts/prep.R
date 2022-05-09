#----DATASET----
raw<-raw24
#---------------

raw_cat<-raw[c(4:nrow(raw)),c(6, 7, 8)] #removing csv header info and dropping unnecessary columns
colnames(raw_cat)<-c("time", "value", "metric") #renaming columns
row.names(raw_cat)<-1:nrow(raw_cat) #changing indexes from 4:end back to 1:end-3
kable(head(raw_cat)) #Checking outcome is as desired

length(unique(raw_cat$metric)) #checking no. of metics, should be 141
length(unique(grep(glob2rx("fields_*"), raw_cat$metric, value=TRUE))) #checking that there are 134 metrics available before cleaning
df_cat <- with(raw_cat, subset(raw_cat, grepl(glob2rx("fields_*"), raw_cat$metric))) #dropping metrics that don't begin with fields_
length(unique(df_cat$metric)) #checking removal of anomalous metrics

df<-df_cat %>% pivot_wider(names_from = metric, values_from = value) #pivoting the table to get metrics from single column list to column headers

date_time<-as.matrix(str_split_fixed(df$time, "T", 2)) #splitting time column into date and time
timecat<-substr(date_time[,2], 1, 8) #tidying the time by rounding up to seconds


df1<-na.omit(as.data.frame(cbind(date_time[,1], timecat, df[-1])))#dropping the columns with _NA_ in rows and replacing time with new date and time columns
names(df1)[1]<-"Date"
names(df1)[2]<-"Time"


if (as.numeric(as.character(df1$`fields_General_0_Part Count`[1])) > 15){
  x<-as.numeric(as.character(df1$`fields_General_0_Part Count`[1]))
  df1$`fields_General_0_Part Count`<-(as.numeric(as.character(df1$`fields_General_0_Part Count`))-x)
}

#-----------------------------------------------
# Getting tool names and filtering for chosen 2
#-----------------------------------------------

ch1ToolNames <-as.factor(unique(df1$fields_Tool_0_tool_name))
ch1ToolNames

#3 tools of interest:
fullRad <- filter(df1, fields_Tool_0_tool_name == "FULL_RAD_118_TC")
finishSplines <- filter(df1, fields_Tool_0_tool_name == "FINISH_SPLINES_TC")
chamdrill8D <- filter(df1, fields_Tool_0_tool_name == "11.5MM_8D_CHAMDRILL_TC")
chamdrill3D <- filter(df1, fields_Tool_0_tool_name == "11.5MM_3D_CHAMDRILL_TC")
gundrill <- filter(df1, fields_Tool_0_tool_name == "11.8MM_GUNDRILL_TC")
partoff <- filter(df1, fields_Tool_0_tool_name == "PART_OFF_TC")



