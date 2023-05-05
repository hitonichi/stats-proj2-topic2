setwd("D:/HCMUT/HK222/XSTK/Assignment/Main/Project2")

library(readr)
library(ggcorrplot)
library(corrr)
library(corrplot)
library(data.table)
library(summarytools)
library(dplyr)

options(max.print = 150)

#Reading data from source file
data <- read_tsv("transcoding_mesurment.tsv")

####################### DATA CLEANING ########################
# Check whether there's NA value in the dataset
sum(is.na(data))
# Checked - No NA value
##############################################################

# Data Wrangling - Turn width & height into 1 column - resolution
data$i_resolution <- paste(as.numeric(data$width * data$height))

data$o_resolution <- paste(as.numeric(data$o_width * data$o_height))

transcodeData <- data
transcodeData = transcodeData[,-1]
# transcodeData = transcodeData[,-3:-4]
# transcodeData <- data[,c('duration','i_resolution', 'o_resolution','framerate', 'codec','o_codec','bitrate','i','i_size','p','p_size','b','b_size','utime')]
transcodeData$i_resolution <- as.numeric(transcodeData$i_resolution)

transcodeData$o_resolution <- as.numeric(transcodeData$o_resolution)

# Add new field - is_same_codec:
transcodeData$is_same_codec <- ifelse(transcodeData$codec == transcodeData$o_codec, 1, 0)

transcodeData$is_same_res <- ifelse(transcodeData$i_resolution == transcodeData$o_resolution, 1, 0)

################### Dealing with Outlier #######################
# The plotting is highly affected by outliers so we remove them
Q1 <- quantile(transcodeData$utime, .25)
Q3 <- quantile(transcodeData$utime, .75)
IQR <- IQR(transcodeData$utime)

nrow(subset(transcodeData, utime > (Q1 - 1.5*IQR) & utime < (Q3 + 1.5*IQR)))

nrow(transcodeData)

no_outliers <- subset(transcodeData, 
                      transcodeData$utime > (Q1 - 1.5*IQR) &
                        transcodeData$utime < (Q3 + 1.5*IQR))

transcodeData <- transcodeData[!(transcodeData$duration>=3000),]
transcodeData <- transcodeData[!(transcodeData$utime>=70),]

#transcodeData <- data[,c('id','duration','o_codec','bitrate','framerate','frames','size','utime')]

#Rename the target variable
colnames(transcodeData)[which(names(transcodeData) == "utime")] <- "Transcode_time"

#View data
transcodeData
sum(is.na(transcodeData$i_resolution))
sum(is.na(transcodeData$o_resolution))
descr(as.data.frame(transcodeData), transpose = TRUE, stats = c('mean', 'sd', 'min', 'max', 'med', 'Q1', 'IQR', 'Q3'))

######################## HISTOGRAM #############################
# par(mfrow=c(3,2))
# hist(transcodeData$duration,main="Duration",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$resolution,main="Resolution",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$framerate,main="Framerate",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$bitrate,main="Bitrate",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$i,main="No. i",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$i_size,main="i Size",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$p,main="No. p",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$p_size,main="p Size",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$utime,main="Transcoding Time",
#       xlab="", col="cyan", breaks=50)
################################################################

######################## PAIR PLOT #############################
# Get a copy
tmpDF <- transcodeData

# Making Dummy Vars
codec_is_mpeg4 <- ifelse(tmpDF$codec == 'mpeg4', 1, 0)
codec_is_vp8 <- ifelse(tmpDF$codec == 'vp8', 1, 0)
codec_is_flv <- ifelse(tmpDF$codec == 'flv', 1, 0)
codec_is_h264 <- ifelse(tmpDF$codec == 'h264', 1, 0)

o_codec_is_mpeg4 <- ifelse(tmpDF$o_codec == 'mpeg4', 1, 0)
o_codec_is_vp8 <- ifelse(tmpDF$o_codec == 'vp8', 1, 0)
o_codec_is_flv <- ifelse(tmpDF$o_codec == 'flv', 1, 0)
o_codec_is_h264 <- ifelse(tmpDF$o_codec == 'h264', 1, 0)

tmpDF$codec_is_mpeg4 <- codec_is_mpeg4
tmpDF$codec_is_vp8 <- codec_is_vp8
tmpDF$codec_is_flv <- codec_is_flv
tmpDF$codec_is_h264 <- codec_is_h264

tmpDF$o_codec_is_mpeg4 <- o_codec_is_mpeg4
tmpDF$o_codec_is_vp8 <- o_codec_is_vp8
tmpDF$o_codec_is_flv <- o_codec_is_flv
tmpDF$o_codec_is_h264 <- o_codec_is_h264

tmpDF$codec <- NULL
tmpDF$o_codec <- NULL
# 
# tmpDF
# 
# pairs(tmpDF[, 1:5])
################################################################

######################## BOX PLOT #############################
# # With outliers
# png("boxplot_dur_i_codec_out.png", width = 1000, height = 700)
# boxplot(transcodeData$Transcode_time ~ transcodeData$codec,
#         main = "Duration by input codec (with outliers)", xlab = "Input Codec", ylab = "Duration (seconds)",
#         col = 2:8, las = 1)
# dev.off()
# 
# png("boxplot_dur_o_codec_out.png", width = 1000, height = 700)
# boxplot(transcodeData$Transcode_time ~ transcodeData$o_codec,
#         main = "Duration by output codec (with outliers)", xlab = "Output Codec", ylab = "Duration (seconds)",
#         col = 2:8, las = 1)
# dev.off()
# 
# png("boxplot_dur_same_codec_out.png", width = 1000, height = 700)
# boxplot(transcodeData$Transcode_time ~ transcodeData$is_same_codec,
#         main = "Duration by codec accordance (with outliers)", xlab = "Codec types", ylab = "Duration (seconds)",
#         col = 2:8, las = 1)
# dev.off()
# 
# png("boxplot_dur_same_res_out.png", width = 1000, height = 700)
# boxplot(transcodeData$Transcode_time ~ transcodeData$is_same_res,
#         main = "Duration by resolution accordance (with outliers)", xlab = "Resolution", ylab = "Duration (seconds)",
#         col = 2:8, las = 1)
# dev.off()
# 
# 
# # Without outliers
# png("boxplot_dur_i_codec_no_out.png", width = 1000, height = 700)
# boxplot(no_outliers$utime ~ no_outliers$codec,
#         main = "Duration by input codec (without outliers)", xlab = "Input Codec", ylab = "Duration (seconds)",
#         col = 2:8, las = 1)
# dev.off()
# 
# png("boxplot_dur_o_codec_no_out.png", width = 1000, height = 700)
# boxplot(no_outliers$utime ~ no_outliers$o_codec,
#         main = "Duration by output codec (without outliers)", xlab = "Output Codec", ylab = "Duration (seconds)",
#         col = 2:8, las = 1)
# dev.off()
# 
# png("boxplot_dur_same_codec_no_out.png", width = 1000, height = 700)
# boxplot(no_outliers$utime ~ no_outliers$is_same_codec,
#         main = "Duration by codec accordance (without outliers)", xlab = "Codec types", ylab = "Duration (seconds)",
#         col = 2:8, las = 1)
# dev.off()
# 
# png("boxplot_dur_same_res_no_out.png", width = 1000, height = 700)
# boxplot(no_outliers$utime ~ no_outliers$is_same_res,
#         main = "Duration by resolution accordance (without outliers)", xlab = "Resolution", ylab = "Duration (seconds)",
#         col = 2:8, las = 1)
# dev.off()
################################################################

######################## CORRR MAP #############################
# Correlation heatmap
CR <- cor(tmpDF)
View(CR)
png("corr_plot.png", width = 6000, height = 6000)
corrplot(CR, addCoef.col = 1, cl.cex = 6,
         tl.cex = 3, number.cex = 0.01)
dev.off()

################################################################
