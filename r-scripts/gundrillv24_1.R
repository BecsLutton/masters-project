
#-------------------------------------------------------------------
#checking what RPM bands the tools operate in and no of occurrences
#-------------------------------------------------------------------
gundrill2<-filter(gundrill,
                  `fields_General_0_Part Count` == 2,   #filtering for one part (number 2)
                  `fields_Channels_0_Program Status`==3) #filtering for active program (status = 2)

gundrill2_rpm <- gundrill2[, grepl("RPM", names(gundrill2))] #sort for columns with RPM in column title for partcount = 2

unique(gundrill2_rpm$`fields_S1_0_Desired RPM`)
unique(gundrill2_rpm$`fields_S4_0_Desired RPM`)
table(gundrill2_rpm$`fields_S1_0_Desired RPM`)
table(gundrill2_rpm$`fields_S4_0_Desired RPM`)

table(gundrill$fields_Tool_0_tool_no)
gundrill$`fields_General_0_Part Count`[6475]
table(gundrill2$fields_Channels_0_Msg)
table(gundrill2$`fields_S1_0_Desired Feed Rate`)
table(gundrill2$`fields_General_0_Axes Status`)
table(gundrill2$`fields_Channels_0_Line Number`)

Gundrill<-data.frame(group_by(gundrill, as.factor(gundrill$`fields_General_0_Part Count`)))

gundrill300<-filter(
    Gundrill,
    fields_S4_0_Desired.RPM== -600,
    fields_Channels_0_Msg == "Drilling",
    fields_S1_0_Desired.Feed.Rate =="108000",
    fields_Channels_0_Program.Status==3,
    fields_General_0_Axes.Status==0,
    fields_Channels_0_Line.Number == 251,
    as.numeric(fields_General_0_Part.Count)<38)

length(table(gundrill300$`fields_General_0_Part.Count`)) # checking that all parts have experienced this RPM (54)


#-----------------------------
#  DRIVE VARIABLES
#-----------------------------
g_300<-setNames(data.frame(gundrill300$Date), "Date")
g_300$Time<-gundrill300$Time
g_300$PartCount<-as.numeric(gundrill300$fields_General_0_Part.Count)
g_300$DrivePower <- as.numeric(gundrill300$fields_Channels_0_DrivePower)
g_300$DriveCurrent <- as.numeric(gundrill300$fields_Channels_0_DriveCurrent)
g_300$DriveTorque <- as.numeric(gundrill300$fields_Channels_0_DriveTorque)
g_300$DriveLoad <- as.numeric(gundrill300$fields_Channels_0_DriveLoad)
g_300$DriveTemp<-as.numeric(gundrill300$fields_Channels_0_Drive.Temperature)
g_300$Name<-as.factor(gundrill300$fields_Tool_0_tool_name)
g_300$ToolNo<-as.factor(gundrill300$fields_Tool_0_tool_no)
g_300$LineNo <-as.numeric(gundrill300$fields_Channels_0_Line.Number)
g_300$Type<-as.factor("Mill")
g_300$Life<-as.numeric(gundrill300$fields_Tool_0_tool_life_min)
g_300$X<-as.numeric(gundrill300$fields_Channels_0_X_Coord)
g_300$Y<-as.numeric(gundrill300$fields_Channels_0_Y_Coord)
g_300$Z<-as.numeric(gundrill300$fields_Channels_0_Z_Coord)

#-----------------------------
#  S1 VARIABLES
#-----------------------------
g_300_s1<-setNames(data.frame(as.factor(matrix("S1", nrow = nrow(g_300)))), "Spindle")
g_300_s1$DRPM<-as.numeric(gundrill300$fields_S1_0_Desired.RPM)
g_300_s1$RPM<-as.numeric(gundrill300$fields_S1_0_RPM)
g_300_s1$Load<-as.numeric(gundrill300$fields_S1_0_Load)
g_300_s1$Power<-as.numeric(gundrill300$fields_S1_0_Power)
g_300_s1$Torque<-as.numeric(gundrill300$fields_S1_0_Torque)
g_300_s1$Current<-as.numeric(gundrill300$fields_S1_0_Current)
g_300_s1$FR<-as.numeric(gundrill300$fields_S1_0_Feed.Rate)
g_300_s1$DFR<-as.numeric(gundrill300$fields_S1_0_Desired.Feed.Rate)
g_300_s1$DCL<-as.numeric(gundrill300$fields_S1_0_Desired.RPM)/as.numeric(gundrill300$fields_S1_0_Desired.Feed.Rate)
g_300_s1$CL<-as.numeric(gundrill300$fields_S1_0_RPM)/as.numeric(gundrill300$fields_S1_0_Feed.Rate)

g_300_s1$DCL[is.nan(g_300_s1$DCL)] <-0
g_300_s1$CL[is.nan(g_300_s1$CL)] <-0

#-----------------------------
#  S4 VARIABLES
#-----------------------------
g_300_s4<-setNames(data.frame(as.factor(matrix("S4", nrow = nrow(g_300)))), "Spindle")
g_300_s4$DRPM<-as.numeric(gundrill300$fields_S4_0_Desired.RPM)
g_300_s4$RPM<-as.numeric(gundrill300$fields_S4_0_RPM)
g_300_s4$Load<-as.numeric(gundrill300$fields_S4_0_Load)
g_300_s4$Power<-as.numeric(gundrill300$fields_S4_0_Power)
g_300_s4$Torque<-as.numeric(gundrill300$fields_S4_0_Torque)
g_300_s4$Current<-as.numeric(gundrill300$fields_S4_0_Current)
g_300_s4$DFR<-as.numeric(gundrill300$fields_S4_0_Desired.Feed.Rate)
g_300_s4$FR<-as.numeric(gundrill300$fields_S4_0_Feed.Rate)
g_300_s4$DCL<-as.numeric(gundrill300$fields_S4_0_Desired.RPM)/as.numeric(gundrill300$fields_S4_0_Desired.Feed.Rate)
g_300_s4$CL<-as.numeric(gundrill300$fields_S4_0_RPM)/as.numeric(gundrill300$fields_S4_0_Feed.Rate)

g_300_s4$DCL[is.nan(g_300_s4$DCL)] <-0
g_300_s4$CL[is.nan(g_300_s4$CL)] <-0

g_300_all <- cbind(g_300, g_300_s1, g_300_s4)
#write.csv(g_300_all,"~/onedrive/Scenic/data/indexG220/ml/gundrill_3.csv", row.names = FALSE)

g_300_Drive<-g_300
g_300_S1<-cbind(g_300, g_300_s1)
g_300_S4<-cbind(g_300, g_300_s4)


drop<-c("Date", "Time", "Name", "ToolNo", "Type", "Spindle", "X", "Y")
s1_sum_mean<-g_300_S1[,!(names(g_300_S1) %in% drop)] %>%
  group_by(PartCount) %>%
  summarise(across(everything(), list(mean)))

#write.csv(s1_sum_mean,"~/onedrive/Scenic/data/indexG220/ml/s1_sum_mean_1.csv", row.names = FALSE)

#---------------------------------------------------------------------------------------------------------------
# SUMMARISING DATA
#-----------------------------

#----------- DRIVE

g_DPower<-g_300%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(DrivePower),
    Median  = median(DrivePower),
    Mode = Mode(DrivePower),
    Q1 = quantile(DrivePower, 0.25),
    Q3 = quantile(DrivePower, 0.75),
    IQR = Q3-Q1,
    SD = sd(DrivePower),
    SD_trim = sd_trim(DrivePower, trim=0.1),
    mad=mad(DrivePower, constant = 1.4826),
    size = length(DrivePower))

g_DCurrent<-g_300%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(DriveCurrent),
    Median  = median(DriveCurrent),
    Mode = Mode(DriveCurrent),
    Q1 = quantile(DriveCurrent, 0.25),
    Q3 = quantile(DriveCurrent, 0.75),
    IQR = Q3-Q1,
    SD = sd(DriveCurrent),
    SD_trim = sd_trim(DriveCurrent, trim=0.1),
    mad=mad(DriveCurrent, constant = 1.4826),
    size = length(DriveCurrent))

g_DLoad<-g_300%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(DriveLoad),
    Median  = median(DriveLoad),
    Mode = Mode(DriveLoad),
    Q1 = quantile(DriveLoad, 0.25),
    Q3 = quantile(DriveLoad, 0.75),
    IQR = Q3-Q1,
    SD = sd(DriveLoad),
    SD_trim = sd_trim(DriveLoad, trim=0.1),
    mad=mad(DriveLoad, constant = 1.4826),
    size = length(DriveLoad))

g_DTorque<-g_300%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(DriveTorque),
    Median  = median(DriveTorque),
    Mode = Mode(DriveTorque),
    Q1 = quantile(DriveTorque, 0.25),
    Q3 = quantile(DriveTorque, 0.75),
    IQR = Q3-Q1,
    SD = sd(DriveTorque),
    SD_trim = sd_trim(DriveTorque, trim=0.1),
    mad=mad(DriveTorque, constant = 1.4826),
    size = length(DriveTorque))

g_DTemp<-g_300%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(DriveTemp),
    Median  = median(DriveTemp),
    Mode = Mode(DriveTemp),
    Q1 = quantile(DriveTemp, 0.25),
    Q3 = quantile(DriveTemp, 0.75),
    IQR = Q3-Q1,
    SD = sd(DriveTemp),
    SD_trim = sd_trim(DriveTemp, trim=0.1),
    mad=mad(DriveTemp, constant = 1.4826),
    size = length(DriveTemp))

summarised_Drive_mean <- setNames(data.frame(cbind(g_DPower$PartCount , g_DPower$Mean, g_DCurrent$Mean, g_DLoad$Mean, g_DTorque$Mean, g_DTemp$Mean)),
                                  c("PartCount", "DPow.mean", "DCurr.mean", "DLoad.mean", "DTorque.mean", "DTemp.mean"))

summarised_Drive_median <- setNames(data.frame(cbind(g_DPower$PartCount , g_DPower$median, g_DCurrent$median, g_DLoad$median, g_DTorque$median, g_DTemp$median)),
                                  c("PartCount", "DPow.median", "DCurr.median", "DLoad.median", "DTorque.median", "DTemp.median"))

summarised_Drive_mad <- setNames(data.frame(cbind(g_DPower$PartCount , g_DPower$mad, g_DCurrent$mad, g_DLoad$mad, g_DTorque$mad, g_DTemp$mad)),
                                  c("PartCount", "DPow.mad", "DCurr.mad", "DLoad.mad", "DTorque.mad", "DTemp.mad"))


#----------- S1

g_Spow<-g_300_S1%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(abs(Power)),
    Mean_trim= mean(abs(Power), trim = 0.1),
    Median  = median(abs(Power)),
    Mode = Mode(abs(Power)),
    Q1 = quantile(Power, 0.25),
    Q3 = quantile(Power, 0.75),
    IQR = Q3-Q1,
    SD = sd(Power),
    SD_trim = sd_trim(Power, trim=0.1),
    mad=mad(Power, constant = 1.4826),
    size = length(Power))

g_Scurr<-g_300_S1%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(abs(Current)),
    Mean_trim=mean(abs(Current), trim =0.1),
    Median  = median(abs(Current)),
    Mode = Mode(abs(Current)),
    Q1 = quantile(Current, 0.25),
    Q3 = quantile(Current, 0.75),
    IQR = Q3-Q1,
    SD = sd(Current),
    SD_trim = sd_trim(Current, trim=0.1),
    mad=mad(Current, constant = 1.4826),
    size = length(Current))

g_Sload<-g_300_S1%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(Load),
    Mean_trim=mean(abs(Load), trim =0.1),
    Median  = median(Load),
    Mode = Mode(Load),
    Q1 = quantile(Load, 0.25),
    Q3 = quantile(Load, 0.75),
    IQR = Q3-Q1,
    SD = sd(Load),
    SD_trim = sd_trim(Load, trim=0.1),
    mad=mad(Load, constant = 1.4826),
    size = length(Load))

g_STorque<-g_300_S1%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean((Torque)),
    Mean_trim=mean(abs(Torque), trim =0.1),
    Median  = median((Torque)),
    Mode = Mode(Torque),
    Q1 = quantile(Torque, 0.25),
    Q3 = quantile(Torque, 0.75),
    IQR = Q3-Q1,
    SD = sd(Torque),
    SD_trim = sd_trim(Torque, trim=0.1),
    mad=mad(Torque, constant = 1.4826),
    size = length(Torque))

summarised_S1_mean <- setNames(data.frame(cbind(g_Spow$PartCount , g_Spow$Mean, g_Scurr$Mean, g_Sload$Mean, g_STorque$Mean)),
                               c("PartCount", "S1Pow.mean", "S1Curr.mean", "S1Load.mean", "S1Torque.mean"))

#write.csv(summarised_S1_mean,"~/onedrive/Scenic/data/indexG220/ml/summarised_S1_mean_3.csv", row.names = FALSE)

summarised_S1_Median <- setNames(data.frame(cbind(g_Spow$PartCount , g_Spow$Median, g_Scurr$Median, g_Sload$Median, g_STorque$Median)),
                              c("PartCount", "S1Pow.Median", "S1Curr.Median", "S1Load.Median", "S1Torque.Median"))

summarised_S1_mad <- setNames(data.frame(cbind(g_Spow$PartCount , g_Spow$mad, g_Scurr$mad, g_Sload$mad, g_STorque$mad)),
                               c("PartCount", "S1Pow.mad", "S1Curr.mad", "S1Load.mad", "S1Torque.mad"))



ggplot(summarised_S1_mean,
       aes(x=abs(S1Torque.mean), y=abs(S1Load.mean), color = as.factor(PartCount))) +
  geom_point()+
  labs(x = expression(bold("S1 Torque")), y=expression(bold("S1 Load")), title = expression(bold("Mean Load vs Torque Pairwise plot"))) +
  theme(plot.title=element_text(hjust=0.5, size=18))+
  theme(legend.position = "none")


plot_ly(summarised_S1_mean, x=~abs(S1Torque.mean), y=~abs(S1Load.mean), color=~PartCount, text=~PartCount) %>%
  add_markers() %>%
  add_text(textposition = "top right")


#----------- S4

g_S4_pow<-g_300_S4%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(abs(Power)),
    Mean_trim=mean(abs(Power), trim =0.1),
    Median  = median(abs(Power)),
    Mode = Mode(abs(Power)),
    Q1 = quantile(Power, 0.25),
    Q3 = quantile(Power, 0.75),
    IQR = Q3-Q1,
    SD = sd(Power),
    SD_trim = sd_trim(Power, trim=0.1),
    mad=mad(Power, constant = 1.4826),
    size = length(Power))

g_S4_curr<-g_300_S4%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(abs(Current)),
    Mean_trim=mean(abs(Current), trim =0.1),
    Median  = median(abs(Current)),
    Mode = Mode(Current),
    Q1 = quantile(Current, 0.25),
    Q3 = quantile(Current, 0.75),
    IQR = Q3-Q1,
    SD = sd(Current),
    SD_trim = sd_trim(Current, trim=0.1),
    mad=mad(Current, constant = 1.4826),
    size = length(Current))

g_S4_load<-g_300_S4%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(Load),
    Mean_trim=mean(abs(Load)),
    Median  = median(Load),
    Mode = Mode(Load),
    Q1 = quantile(Load, 0.25),
    Q3 = quantile(Load, 0.75),
    IQR = Q3-Q1,
    SD = sd(Load),
    SD_trim = sd_trim(Load, trim=0.1),
    mad=mad(Load, constant = 1.4826),
    size = length(Load))

g_S4_Torque<-g_300_S4%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(abs(Torque)),
    Mean_trim=mean(abs(Torque), trim =0.1),
    Median  = median(abs(Torque)),
    Mode = Mode(Torque),
    Q1 = quantile(Torque, 0.25),
    Q3 = quantile(Torque, 0.75),
    IQR = Q3-Q1,
    SD = sd(Torque),
    SD_trim = sd_trim(Torque, trim=0.1),
    mad=mad(Torque, constant = 1.4826),
    size = length(Torque))

summarised_S4_mean <- setNames(data.frame(cbind(g_Spow$PartCount , g_S4_pow$Mean, g_S4_curr$Mean, g_S4_load$Mean, g_S4_Torque$Mean)),
                               c("PartCount", "S4Pow.mean", "S4Curr.mean", "S4Load.mean", "S4Torque.mean"))

#write.csv(summarised_S4_mean,"~/onedrive/Scenic/data/indexG220/ml/summarised_S4_mean_3.csv", row.names = FALSE)

summarised_S4_Median <- setNames(data.frame(cbind(g_Spow$PartCount , g_S4_pow$Median, g_S4_curr$Median, g_S4_load$Median, g_S4_Torque$Median)),
                              c("PartCount", "S4Pow.Median", "S4Curr.Median", "S4Load.Median", "S4Torque.Median"))

summarised_S4_mad <- setNames(data.frame(cbind(g_Spow$PartCount , g_S4_pow$mad, g_S4_curr$mad, g_S4_load$mad, g_S4_Torque$mad)),
                              c("PartCount", "S4Pow.mad", "S4Curr.mad", "S4Load.mad", "S4Torque.mad"))


#--------------------------------
#       Plot setup
#--------------------------------


Dpow <- list(
  text = "<b>Drive Power</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "top",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

Dcu <- list(
  text = "<b>Drive Current</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "top",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

Dlo <- list(
  text = "<b>Drive Load</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

Dto <- list(
  text = "<b>Drive Torque</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s1pow <- list(
  text = "<b>S1 Power</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "top",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s1cu <- list(
  text = "<b>S1 Current</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "top",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s1lo <- list(
  text = "<b>S1 Load</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s1to <- list(
  text = "<b>S1 Torque</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s4pow <- list(
  text = "<b>S4 Power</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "top",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s4cu <- list(
  text = "<b>S4 Current</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "top",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s4lo <- list(
  text = "<b>S4 Load</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s4to <- list(
  text = "<b>S4 Torque</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)


pow_title <- list(
  text = "<b>Power</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "top",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

curr_title <- list(
  text = "<b>Current</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "top",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

lo_title <- list(
  text = "<b>Load</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

to_title <- list(
  text = "<b>Torque</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)


t <- list(
  family = "Calibri",
  size = 25,
  color = 'Black')

#--------------------------------
#        DRIVE
#--------------------------------

Dpow_plot<-plot_ly(g_DPower, x = ~PartCount, y = ~Median, type = 'scatter', mode = 'lines+markers') %>%
  layout(annotations = Dpow, title = "Median Values of Drive Power Draw over 37 parts using Gundrill Tool")
Dpow_plot

Dcurr_plot<-plot_ly(g_DCurrent, x = ~PartCount, y = ~abs(Median), type = 'scatter', mode = 'lines+markers') %>%
  layout( annotations = Dcu, title = "Median Values of Drive Current Draw over 37 parts using Gundrill Tool")
Dcurr_plot

Dload_plot<-plot_ly(g_DLoad, x = ~PartCount, y = ~Median, type = 'scatter', mode = 'lines+markers') %>%
  layout(annotations = Dlo, title = "Median Values of Drive Load over 37 parts using Gundrill Tool")
Dload_plot

Dtorque_plot<-plot_ly(g_STorque, x = ~PartCount, y = ~abs(Mean), type = 'scatter',mode = 'lines+markers', name="Mean") %>%
  layout(annotations = Dto, title = "Median Values of Drive Torque over 37 parts using Gundrill Tool")
Dtorque_plot

#--------------------------------
#      SPINDLE 1
#--------------------------------

s1pow_plot<-plot_ly(g_Spow, x = ~PartCount, y = ~Median, type = 'scatter', mode = 'lines+markers') %>%
  layout(annotations = s1pow, title = "Median Values of Current Draw in Spindle over 37 parts using Gundrill Tool")
s1pow_plot

s1curr_plot<-plot_ly(g_Scurr, x = ~PartCount, y = ~abs(Mean), type = 'scatter', mode = 'lines+markers') %>%
  layout(annotations = s1cu, title = "Median Values of Current Draw in Spindle over 37 parts using Gundrill Tool")
s1curr_plot

s1load_plot<-plot_ly(g_Sload, x = ~PartCount, y = ~Median, type = 'scatter', mode = 'lines+markers') %>%
  layout(annotations = s1lo, title = "Median Values of Load on Spindle over 37 parts using Gundrill Tool")
s1load_plot

s1torque_plot<-plot_ly(g_STorque, x = ~PartCount, y = ~abs(Mean), type = 'scatter',mode = 'lines+markers', name="Mean") %>%
  layout(annotations = s1to, title = "Median Values of Torque on Spindle over 37 parts using Gundrill Tool")
s1torque_plot

#------------------------------------ messing around --------------------------------------


plot_ly(summarised_S1_mean, x=~abs(S1Torque.mean), y=~abs(S1Load.mean), color=~PartCount, text=~PartCount) %>%
  add_markers() %>%
  add_text(textposition = "top right")

plot_ly(g_STorque, x = ~PartCount, y = ~abs(Mean),
        type = 'scatter',
        mode = 'lines',
        name = "Mean Torque S1") %>%
  add_trace(y = ~abs(Median),
            type = 'scatter',
            mode = 'lines',
            name = 'Median Torque S1')  %>%
  add_trace(y = ~abs(mad),
            type = 'scatter',
            mode = 'lines',
            name = 'MAD')

#--------------------------------
#      SPINDLE 4
#--------------------------------

s4pow_plot<-plot_ly(g_S4_pow, x = ~PartCount, y = ~Median, type = 'scatter', mode = 'lines+markers') %>%
  layout( annotations = s4pow, title = "Median Values of Power Draw in Chuck over 37 parts using Gundrill Tool")
s4pow_plot

s4curr_plot<-plot_ly(g_S4_curr, x = ~PartCount, y = ~Median, type = 'scatter', mode = 'lines+markers') %>%
  layout(annotations = s4cu, title = "Median Values of Current Draw in Chuck over 37 parts using Gundrill Tool")
s4curr_plot

s4load_plot<-plot_ly(g_S4_load, x = ~PartCount, y = ~Median, type = 'scatter', mode = 'lines+markers') %>%
  layout(annotations = s4lo, title = "Median Values of Load on Chuck over 37 parts using Gundrill Tool")
s4load_plot

s4torque_plot<-plot_ly(g_S4_Torque, x = ~PartCount, y = ~Median, type = 'scatter', mode = 'lines+markers') %>%
  layout(annotations = s4to, title = "Median Values of Torque on Chuck over 37 parts using Gundrill Tool")
s4torque_plot


#--------------------------------
#      POWER
#--------------------------------

power_mean<-setNames(data.frame(cbind(g_DPower$PartCount, g_DPower$Mean, g_Spow$Mean, g_S4_pow$Mean)), c("PartCount", "Drive", "S1", "S4"))
current_mean<-setNames(data.frame(cbind(g_DCurrent$PartCount, g_DCurrent$Mean, g_Scurr$Mean, g_S4_curr$Mean)), c("PartCount", "Drive", "S1", "S4"))
load_mean<-setNames(data.frame(cbind(g_DLoad$PartCount, g_DLoad$Mean, g_Sload$Mean, g_S4_load$Mean)), c("PartCount", "Drive", "S1", "S4"))
torque_mean<-setNames(data.frame(cbind(g_DTorque$PartCount, g_DTorque$Mean, g_STorque$Mean, g_S4_Torque$Mean)), c("PartCount", "Drive", "S1", "S4"))

power_median<-setNames(data.frame(cbind(g_DPower$PartCount, g_DPower$Median, g_Spow$Median, g_S4_pow$Median)), c("PartCount", "Drive", "S1", "S4"))
current_median<-setNames(data.frame(cbind(g_DCurrent$PartCount, g_DCurrent$Median, g_Scurr$Median, g_S4_curr$Median)), c("PartCount", "Drive", "S1", "S4"))
load_median<-setNames(data.frame(cbind(g_DLoad$PartCount, g_DLoad$Median, g_Sload$Median, g_S4_load$Median)), c("PartCount", "Drive", "S1", "S4"))
torque_median<-setNames(data.frame(cbind(g_DTorque$PartCount, g_DTorque$Median, g_STorque$Median, g_S4_Torque$Median)), c("PartCount", "Drive", "S1", "S4"))

power_mad<-setNames(data.frame(cbind(g_DPower$PartCount, g_DPower$mad, g_Spow$mad, g_S4_pow$mad)), c("PartCount", "Drive", "S1", "S4"))
current_mad<-setNames(data.frame(cbind(g_DCurrent$PartCount, g_DCurrent$mad, g_Scurr$mad, g_S4_curr$mad)), c("PartCount", "Drive", "S1", "S4"))
load_mad<-setNames(data.frame(cbind(g_DLoad$PartCount, g_DLoad$mad, g_Sload$mad, g_S4_load$mad)), c("PartCount", "Drive", "S1", "S4"))
torque_mad<-setNames(data.frame(cbind(g_DTorque$PartCount, g_DTorque$mad, g_STorque$mad, g_S4_Torque$mad)), c("PartCount", "Drive", "S1", "S4"))

mean_power_plot<-plot_ly(power_mean, x = ~PartCount, y = ~abs(Drive),
                        type = 'scatter',
                        mode = 'lines+markers',
                        name = "Drive Power") %>%
  add_trace(y = ~abs(S1),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S1 Power')  %>%
  add_trace(y = ~abs(S4),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S4 Power') %>%
  layout(annotations = pow_title, showlegend = TRUE,
         yaxis = list(title = "abs(Current)", range = c(50,500)),
         title = "Mean Values of Power Draw over 37 parts using Gundrill Tool")

mean_current_plot<-plot_ly(current_mean, x = ~PartCount, y = ~abs(Drive),
                          type = 'scatter',
                          mode = 'lines+markers',
                          name = "Drive Current") %>%
  add_trace(y = ~abs(S1),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S1 Current')  %>%
  add_trace(y = ~abs(S4),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S4 Current')%>%
  layout(annotations = curr_title, showlegend = TRUE,
         yaxis = list(title = "abs(Current)", range = c(0,5)),
         title = "Mean Values of Current Draw over 37 parts using Gundrill Tool")

mean_load_plot<-plot_ly(load_mean, x = ~PartCount, y = ~abs(Drive),
                       type = 'scatter',
                       mode = 'lines+markers',
                       name = "Drive Load") %>%
  add_trace(y = ~abs(S1),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S1 Load')  %>%
  add_trace(y = ~abs(S4),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S4 Load') %>%
  layout(annotations = lo_title, showlegend = TRUE,
         yaxis = list(title = "abs(Current)", range = c(0,10)),
         title = "Mean Values of Load over 37 parts using Gundrill Tool")

mean_torque_plot<-plot_ly(torque_mean, x = ~PartCount, y = ~abs(Drive),
                         type = 'scatter',
                         mode = 'lines+markers',
                         name = "Drive Torque") %>%
  add_trace(y = ~abs(S1),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S1 Torque')  %>%
  add_trace(y = ~abs(S4),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S4 Torque') %>%
  layout(annotations = to_title, showlegend = TRUE,
         yaxis = list(title = "abs(Current)", range = c(0,8)),
         title = "Mean Values of Torque over 37 parts using Gundrill Tool")


mad_power_plot<-plot_ly(power_mad, x = ~PartCount, y = ~abs(Drive),
        type = 'scatter',
        mode = 'lines+markers',
        name = "Drive Power") %>%
  add_trace(y = ~abs(S1),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S1 Power')  %>%
  add_trace(y = ~abs(S4),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S4 Power') %>%
  layout(annotations = pow_title, showlegend = TRUE,
         yaxis = list(title = "abs(Current)"),
         title = "MAD values of Power Draw over 37 parts using Gundrill Tool")

mad_current_plot<-plot_ly(current_mad, x = ~PartCount, y = ~abs(Drive),
        type = 'scatter',
        mode = 'lines+markers',
        name = "Drive Current") %>%
  add_trace(y = ~abs(S1),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S1 Current')  %>%
  add_trace(y = ~abs(S4),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S4 Current')%>%
  layout(annotations = curr_title, showlegend = TRUE,
         yaxis = list(title = "abs(Current)"),
         title = "MAD Values of Current Draw over 37 parts using Gundrill Tool")

mad_load_plot<-plot_ly(load_mad, x = ~PartCount, y = ~abs(Drive),
        type = 'scatter',
        mode = 'lines+markers',
        name = "Drive Load") %>%
  add_trace(y = ~abs(S1),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S1 Load')  %>%
  add_trace(y = ~abs(S4),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S4 Load') %>%
  layout(annotations = lo_title, showlegend = TRUE,
         yaxis = list(title = "abs(Current)"),
         title = "MAD Values of Load over 37 parts using Gundrill Tool")

mad_torque_plot<-plot_ly(torque_mad, x = ~PartCount, y = ~abs(Drive),
        type = 'scatter',
        mode = 'lines+markers',
        name = "Drive Torque") %>%
  add_trace(y = ~abs(S1),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S1 Torque')  %>%
  add_trace(y = ~abs(S4),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S4 Torque') %>%
  layout(annotations = to_title, showlegend = TRUE,
         yaxis = list(title = "abs(Current)"),
         title = "MAD Values of Torque over 37 parts using Gundrill Tool")


#---------------
#    PLOTS
#---------------

fig1 <- subplot(s1pow_plot,
               s4pow_plot,
               Dpow_plot,
               s1curr_plot,
               s4curr_plot,
               Dcurr_plot,
               s1torque_plot,
               s4torque_plot,
               Dtorque_plot,
               s1load_plot,
               s4load_plot,
               Dload_plot,
               nrows = 4,
               shareX = TRUE) %>%
  layout(title = list(text="<b>Median Values for Gundrill Tool over 'Life'(37 parts)</b>",
                      font = t,
                      y = 1, x = 0.5,
                      xanchor = 'center',
                      yanchor =  'top'),
         showlegend =FALSE,
         plot_bgcolor='#e5ecf6')


fig2 <- subplot(mean_power_plot,
                mean_current_plot,
                mean_load_plot,
                mean_torque_plot,
                nrows = 2,
                shareX = TRUE) %>%
  layout(title = list(text="<b>Mean Values for Gundrill Tool over 'Life'(37 parts)</b>",
                      font = t,
                      y = 1, x = 0.5,
                      xanchor = 'center',
                      yanchor =  'top'),
         showlegend =TRUE,
         plot_bgcolor='#e5ecf6')


fig3 <- subplot(mad_power_plot,
                mad_current_plot,
                mad_load_plot,
                mad_torque_plot,
                nrows = 2,
                shareX = TRUE) %>%
  layout(title = list(text="<b>MAD Values for Gundrill Tool over 'Life'(37 parts)</b>",
                      font = t,
                      y = 1, x = 0.5,
                      xanchor = 'center',
                      yanchor =  'top'),
         showlegend =TRUE,
         plot_bgcolor='#e5ecf6')


fig1
fig2
fig3

#-----------------------------------------

df_g_s1<-g_300_S1
df_g_s4<-g_300_S4

count1 <-c()
for (val in df_g_s1$PartCount){
  if(val<6){
    count1<-append(count1,5)
  }else if(val>=6 & val<11){
    count1<-append(count1,10)
  }else if(val>=11 & val<16){
    count1<-append(count1,15)
  }else if(val>=16 & val<21){
    count1<-append(count1,20)
  }else if(val>=21 & val<26){
    count1<-append(count1,25)
  }else if(val>=26 & val<31){
    count1<-append(count1,30)
  }else if(val>=31 & val<36){
    count1<-append(count1,35)
  }else if(val>=36 & val<41){
    count1<-append(count1,40)
  }else if(val>=41 & val<46){
    count1<-append(count1,45)
  }else if(val>=46 & val<51){
    count1<-append(count1,50)
  }else if(val>=51 & val<56){
    count1<-append(count1,55)
  }else if(val>=56 & val<61){
    count1<-append(count1,60)
  }else if(val>=61 & val<66){
    count1<-append(count1,65)
  }else if(val>=66 & val<71){
    count1<-append(count1,70)
  }else if(val>=71 & val<76){
    count1<-append(count1,75)
  }
}

#--------
# S1
#--------

length(df_g_s1)
nrow(df_g_s1)


df_g_s1$PartCount <- count1
unique(df_g_s1$PartCount)
length(df_g_s1$PartCount)

dftime<-which(df_g_s1$PartCount != dplyr::lag(df_g_s1$PartCount))
timez<-df_g_s1[dftime,]$Time
time<-append(df_g_s1$Time[1], timez)
time

#Power
g_s1_power_plot <-
  ggplot(df_g_s1, aes(PartCount, Power, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.5)+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(df_g_s1$Power, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Power (W)")),
#      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_c1_power_plot <-
  ggplot(df_g_s1, aes(PartCount, DrivePower, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.5)+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(df_g_s1$DrivePower, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Power (W)")),
#      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_s1_current_plot <-
  ggplot(df_g_s1, aes(PartCount, abs(Current), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(abs(df_g_s1$Current), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Current (A)")),
#      title=expression(bold("S1 Current for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_c1_current_plot <-
  ggplot(df_g_s1, aes(PartCount, abs(DriveCurrent), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(abs(df_g_s1$DriveCurrent), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Current (A)")),
#      title=expression(bold("S1 Current for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))



# Torque
g_s1_torque_plot <-
  ggplot(df_g_s1, aes(PartCount, abs(Torque), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(abs(df_g_s1$Torque), c(0.01, 0.99)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Torque (Nm)")),
#      title=expression(bold("S1 Torque for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_c1_torque_plot <-
  ggplot(df_g_s1, aes(PartCount, abs(DriveTorque), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(abs(df_g_s1$DriveTorque), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Torque (Nm)")),
#      title=expression(bold("S1 Torque for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


# Load
g_s1_load_plot <-
  ggplot(df_g_s1, aes(PartCount, Load, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(df_g_s1$Load, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
#+ labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Load (%)")),
#      title=expression(bold("S1 Load for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Load
g_c1_load_plot <-
  ggplot(df_g_s1, aes(PartCount, DriveLoad, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(df_g_s1$DriveLoad, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
#+ labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Load (%)")),
#      title=expression(bold("S1 Load for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


# RPM
g_s1_rpm_plot <-
  ggplot(df_g_s1, aes(PartCount, RPM, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(df_g_s1$RPM, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
#+labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 RPM")),
#      title=expression(bold("S1 RPM for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Feed Rate
g_s1_fr_plot <-
  ggplot(df_g_s1, aes(PartCount, FR, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_g_s1$FR, c(0.01, 0.99)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Feed Rate (inch/min)")),
#      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Chip Load
g_s1_cl_plot <-
  ggplot(df_g_s1, aes(PartCount, CL, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(df_g_s1$FR, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Feed Rate (inch/min)")),
#      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_s1_cl_spread <-
  df_g_s1 %>%
  ggplot(aes(x =CL , fill= factor(PartCount))) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~PartCount, ncol=1) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set1") + scale_x_continuous(limits = c(0.002755, 0.002795)) +
  labs(x=expression(bold("Chip Load")),
       y=expression(bold("Density")),
       title=expression(bold("Theoretical Chip Load Density Plots for Finish Splines Tool over 1 shift on 11/02/22"))) +
  theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  guides(color = guide_legend(override.aes = list(size = 3)))




#--------
# S4
#--------

count2 <-c()
for (val in df_g_s4$PartCount){
  if(val<22){
    count2<-append(count2,16)
  }else if(val>=22 & val<28){
    count2<-append(count2,22)
  }else if(val>=28 & val<34){
    count2<-append(count2,28)
  }else if(val>=34 & val<40){
    count2<-append(count2,34)
  }else if(val>=40 & val<46){
    count2<-append(count2,40)
  }else if(val>=46 & val<52){
    count2<-append(count2,46)
  }else if(val>=52 & val<58){
    count2<-append(count2,52)
  }else if(val>=58 & val<64){
    count2<-append(count2,58)
  }else if(val>=64 & val<70){
    count2<-append(count2,64)
  }
}

length(df_g_s4)
unique(df_g_s4$PartCount)
df_g_s4$PartCount <- count2
unique(df_g_s4$PartCount)
length(df_g_s4$PartCount)


#Power
g_s4_power_plot <-
  ggplot(df_g_s4, aes(PartCount, Power, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)),  position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(df_g_s4$Power, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")
  #scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Power (W)")),
#      title=expression(bold("S1 Power for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_s4_current_plot <-
  ggplot(df_g_s4, aes(PartCount, abs(Current), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(abs(df_g_s4$Current), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Current (A)")),
#      title=expression(bold("S1 Current for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))



# Torque
g_s4_torque_plot <-
  ggplot(df_g_s4, aes(PartCount, abs(Torque), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(abs(df_g_s4$Torque), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Torque (Nm)")),
#      title=expression(bold("S1 Torque for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


# Load
g_s4_load_plot <-
  ggplot(df_g_s4, aes(PartCount, Load, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(df_g_s4$Load, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
#+ labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Load (%)")),
#      title=expression(bold("S1 Load for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


# RPM
g_s4_rpm_plot <-
  ggplot(df_g_s4, aes(PartCount, RPM, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(df_g_s4$RPM, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
#+labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 RPM")),
#      title=expression(bold("S1 RPM for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Feed Rate
g_s4_fr_plot <-
  ggplot(df_g_s4, aes(PartCount, FR, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(df_g_s4$FR, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Feed Rate (inch/min)")),
#      title=expression(bold("S1 Power for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Chip Load
g_s4_cl_plot <-
  ggplot(df_g_s4, aes(PartCount, CL, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  # scale_y_continuous(limits = quantile(df_g_s1$FR, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Feed Rate (inch/min)")),
#      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


g_s4_cl_spread <-
  df_g_s4 %>%
  ggplot(aes(x =CL , fill= factor(PartCount))) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~PartCount, ncol=1) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") + scale_x_continuous(limits = c(0.002776, 0.00278))


#--------
# TEST
#--------

test<-ggarrange(g_s1_current_plot, g_s1_torque_plot, ncol=1)
ggarrange(g_s1_cl_spread, test, widths = c(1.5,2))


