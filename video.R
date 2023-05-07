setwd("D:/HCMUT/HK222/XSTK/Assignment/Main/Project2")

library(readr)
library(ggcorrplot)
library(corrr)
library(corrplot)
library(data.table)
library(summarytools)
library(dplyr)
library("ggpubr")
library("ggplot2")
library("GGally")

options(max.print = 150)

#Reading data from source file
data <- read_tsv("transcoding_mesurment.tsv")

####################### DATA CLEANING ########################
# Check whether there's NA value in the dataset
sum(is.na(data))
# Checked - No NA value

# Check internal structure
str(data)
##############################################################

# Data Wrangling - Turn width & height into 1 column - resolution
data$i_resolution <- paste(as.numeric(data$width * data$height))

data$o_resolution <- paste(as.numeric(data$o_width * data$o_height))

n_distinct(data$height)

# Get DF and drop uneccessary fields (proved)
transcodeData <- data
transcodeData$id <- NULL
transcodeData$width <- NULL
transcodeData$height <- NULL
transcodeData$o_width <- NULL
transcodeData$o_height <- NULL
transcodeData$b <- NULL
transcodeData$b_size <- NULL

transcodeData$i_resolution <- as.numeric(transcodeData$i_resolution)
transcodeData$o_resolution <- as.numeric(transcodeData$o_resolution)

# Add new field - is_same_codec:
transcodeData$is_same_codec <- ifelse(transcodeData$codec == transcodeData$o_codec, 1, 0)

# Add new field - is_same_res:
transcodeData$is_same_res <- ifelse(transcodeData$i_resolution == transcodeData$o_resolution, 1, 0)

# Testing to remove width, heigth
# sum(transcodeData$is_same_res == 1)
# sum(transcodeData$is_same_res == 1
#     & transcodeData$width == transcodeData$o_width)
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

# transcodeData <- transcodeData[!(transcodeData$duration>=3000),]
# transcodeData <- transcodeData[!(transcodeData$utime>=70),]

#transcodeData <- data[,c('id','duration','o_codec','bitrate','framerate','frames','size','utime')]

#Rename the target variable
colnames(transcodeData)[which(names(transcodeData) == "utime")] <- "Transcode_time"

#View data
# transcodeData
sum(is.na(transcodeData$i_resolution))
sum(is.na(transcodeData$o_resolution))

n_distinct(transcodeData$i_resolution)
n_distinct(transcodeData$o_resolution)

descr(as.data.frame(transcodeData), transpose = TRUE, stats = c('mean', 'sd', 'min', 'max', 'med', 'Q1', 'IQR', 'Q3'))

######################## CORRR MAP #############################
# Correlation heatmap
tmpDF <- transcodeData
tmpDF$codec <- NULL
tmpDF$o_codec <- NULL
CR <- cor(tmpDF)
# View(CR)
png("corr_plot.png", width = 4500, height = 4500)
corrplot(CR, addCoef.col = 1, cl.cex = 6,
         tl.cex = 3, number.cex = 0.01)
dev.off()

################################################################

######################## HISTOGRAM #############################
# png("hist_1.png", width = 1000, height = 400)
# par(mfrow=c(1,2))
# hist(transcodeData$i_resolution,main="Input Resolution",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$o_resolution,main="Output Resolution",
#       xlab="", col="cyan", breaks=50)
# dev.off()
# 
# png("hist_2.png", width = 1000, height = 400)
# par(mfrow=c(1,2))
# hist(transcodeData$bitrate,main="Input Bitrate",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$o_bitrate,main="Output Bitrate",
#       xlab="", col="cyan", breaks=50)
# dev.off()
# 
# png("hist_3.png", width = 1000, height = 400)
# par(mfrow=c(1,2))
# hist(transcodeData$framerate,main="Input Framerate",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$o_framerate,main="Output Framerate",
#       xlab="", col="cyan", breaks=50)
# dev.off()
# 
# png("hist_4.png", width = 1000, height = 400)
# par(mfrow=c(1,2))
# hist(transcodeData$umem,main="Memory",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$size,main="Size",
#       xlab="", col="cyan", breaks=50)
# dev.off()
# 
# png("hist_5.png", width = 1000, height = 400)
# par(mfrow=c(1,2))
# hist(transcodeData$duration,main="Duration",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$Transcode_time,main="Transcoding Time",
#       xlab="", col="cyan", breaks=50)
# dev.off()
# 
# png("hist_6.png", width = 1000, height = 400)
# par(mfrow=c(1,2))
# hist(transcodeData$i,main="Number of i-frames",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$i_size,main="Total i-frame size",
#       xlab="", col="cyan", breaks=50)
# dev.off()
# 
# png("hist_7.png", width = 1000, height = 400)
# par(mfrow=c(1,2))
# hist(transcodeData$p,main="Number of p-frames",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$p_size,main="Total p-frame size",
#       xlab="", col="cyan", breaks=50)
# dev.off()
# 
# png("hist_8.png", width = 500, height = 400)
# par(mfrow=c(1,1))
# hist(log10(transcodeData$frames),main="Frames",
#      xlab="", col="cyan", breaks=50)
# dev.off()
# 
# #############
# # The below histograms are only used for demo - use Box Cox later on
# png("hist_5a.png", width = 1000, height = 400)
# par(mfrow=c(1,2))
# hist(log10(transcodeData$duration),main="Duration",
#      xlab="", col="cyan", breaks=50)
# hist(log10(transcodeData$Transcode_time),main="Transcoding Time",
#      xlab="", col="cyan", breaks=50)
# dev.off()
# 
# png("hist_6a.png", width = 1000, height = 400)
# par(mfrow=c(1,2))
# hist(log10(transcodeData$i),main="Number of i-frames",
#       xlab="", col="cyan", breaks=50)
# hist(log10(transcodeData$i_size),main="Total i-frame size",
#       xlab="", col="cyan", breaks=50)
# dev.off()
# 
# png("hist_7a.png", width = 1000, height = 400)
# par(mfrow=c(1,2))
# hist(log10(transcodeData$p),main="Number of p-frames",
#      xlab="", col="cyan", breaks=50)
# hist(log10(transcodeData$p_size),main="Total p-frame size",
#      xlab="", col="cyan", breaks=50)
# dev.off()
# 
# png("hist_size_transformed.png", width = 500, height = 400)
# par(mfrow=c(1,1))
# hist(log10(transcodeData$size),main="Size",
#       xlab="", col="cyan", breaks=50)
# dev.off()
# 
# # Counting distinct vals
# n_distinct(transcodeData$i_resolution)
# n_distinct(transcodeData$o_resolution)
# n_distinct(transcodeData$o_bitrate)
# n_distinct(transcodeData$o_framerate)
# 
# unique(transcodeData$i_resolution)
# unique(transcodeData$o_resolution)
# unique(transcodeData$o_bitrate)
# unique(transcodeData$o_framerate)

# png("qq_test.png", width = 900, height = 900)
# ggqqplot(transcodeData$Transcode_time)
# dev.off()

# hist(transcodeData$framerate,main="Framerate",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$i,main="No. i",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$i_size,main="i Size",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$p,main="No. p",
#       xlab="", col="cyan", breaks=50)
# hist(transcodeData$p_size,main="p Size",
#       xlab="", col="cyan", breaks=50)
################################################################

######################## PAIR PLOT #############################
# 
# # Get a copy
# reducedDF <- transcodeData
# # Eliminate highly correlated vars
# reducedDF$codec <- NULL
# reducedDF$o_codec <- NULL
# reducedDF$bitrate <- NULL
# reducedDF$frames <- NULL
# reducedDF$i <- NULL
# reducedDF$p <- NULL
# reducedDF$p_size <- NULL
# 
# # Making Dummy Vars
# codec_is_mpeg4 <- ifelse(reducedDF$codec == 'mpeg4', 1, 0)
# codec_is_vp8 <- ifelse(reducedDF$codec == 'vp8', 1, 0)
# codec_is_flv <- ifelse(reducedDF$codec == 'flv', 1, 0)
# codec_is_h264 <- ifelse(reducedDF$codec == 'h264', 1, 0)
# 
# o_codec_is_mpeg4 <- ifelse(reducedDF$o_codec == 'mpeg4', 1, 0)
# o_codec_is_vp8 <- ifelse(reducedDF$o_codec == 'vp8', 1, 0)
# o_codec_is_flv <- ifelse(reducedDF$o_codec == 'flv', 1, 0)
# o_codec_is_h264 <- ifelse(reducedDF$o_codec == 'h264', 1, 0)
# 
# reducedDF$codec_is_mpeg4 <- codec_is_mpeg4
# reducedDF$codec_is_vp8 <- codec_is_vp8
# reducedDF$codec_is_flv <- codec_is_flv
# reducedDF$codec_is_h264 <- codec_is_h264
# 
# reducedDF$o_codec_is_mpeg4 <- o_codec_is_mpeg4
# reducedDF$o_codec_is_vp8 <- o_codec_is_vp8
# reducedDF$o_codec_is_flv <- o_codec_is_flv
# reducedDF$o_codec_is_h264 <- o_codec_is_h264
# 
# reducedDF$codec <- NULL
# reducedDF$o_codec <- NULL
# 
# reducedDF <- reducedDF %>% select(-Transcode_time, Transcode_time)
# 
# # 
# # reducedDF
# # 
# png("pairs_plot_alt_1.png", width = 1000, height = 1000)
# ggpairs(reducedDF[, 1:7],
#         mapping = ggplot2::aes(color = "sex"))
# dev.off()
# 
# png("pairs_plot_alt_2.png", width = 1000, height = 1000)
# ggpairs(reducedDF[, 8:14],
#       mapping = ggplot2::aes(color = "sex"))
# dev.off()
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
