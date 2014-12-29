setwd('~/STAT215A')
ImageSave=FALSE

library(ggplot2)
library(dplyr)

# Load the images
image1 <- read.table('image1.txt', header = F)
image2 <- read.table('image2.txt', header = F)
image3 <- read.table('image3.txt', header = F)

# Give the columns the appropriate labels
column.labels <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- column.labels
names(image2) <- column.labels
names(image3) <- column.labels

# Add label to each image so we can combine them to work on one data set
image1 <- mutate(image1, Image = 1)
image2 <- mutate(image2, Image = 2)
image3 <- mutate(image3, Image = 3)

# Combine
all.images <- rbind(image1, image2, image3)

# Plot unlabelled images according to AN radiance
raw.eda <- ggplot(all.images, aes(x = x, y = y, color = AN)) + 
  geom_point() + 
  facet_wrap(~ Image) +
  ggtitle('Unlabelled Images with AN Radiances') +
  scale_color_continuous(name='AN', low = '#3366CC', high = '#FFFFFF' )

# Plot images with their expert labels
experts.eda <- ggplot(all.images, aes(x = x, y = y, color = factor(label))) + 
  geom_point() + facet_wrap(~ Image) +
  ggtitle('Expertly Labelled Images')+ 
  scale_color_discrete(name = '',breaks = c("-1", "0", "1"),
                       labels = c("Clear", "Unknown", "Cloudy"))

# Plot images with NDAI
ndai.eda <- ggplot(all.images, aes(x=x, y=y, color = NDAI)) + 
  geom_point() + 
  facet_wrap(~ Image) +
  ggtitle('Mapped NDAI Readings') + 
  scale_color_continuous(name='NDAI', low = '#3366CC', high = '#FFFFFF')

# Plot images with SD
sd.eda <- ggplot(all.images, aes(x = x, y = y, color = SD)) + 
  geom_point() + 
  facet_wrap(~ Image) + 
  ggtitle('Mapped SD Readings') + 
  scale_color_continuous(name = 'SD', low = '#3366CC', high = '#FFFFFF')

# Plot images with CORR
corr.eda <- ggplot(all.images, aes(x = x, y = y, color = CORR)) + 
  geom_point() + 
  facet_wrap(~ Image) + 
  ggtitle('Mapped CORR Readings') + 
  scale_color_continuous(name='CORR', low = '#3366CC', high = '#FFFFFF' )

# Look at radiance densities for image 3
an.eda <- ggplot(image3) + geom_density(aes(x = AN, group = factor(label), 
                                            fill = factor(label)), alpha = 0.5)
af.eda <- ggplot(image3) + geom_density(aes(x = AF, group = factor(label), 
                                            fill = factor(label)), alpha = 0.5)
bf.eda <- ggplot(image3) + geom_density(aes(x = BF, group = factor(label), 
                                            fill = factor(label)), alpha = 0.5)
cf.eda <- ggplot(image3) + geom_density(aes(x = CF, group = factor(label), 
                                            fill = factor(label)), alpha = 0.5)
df.eda <- ggplot(image3) + geom_density(aes(x = DF, group = factor(label), 
                                            fill = factor(label)), alpha = 0.5)

###############################################################################
# -Pairwise EDA- 
# 
# This script generates the ggplot objects containing the scatterplots in our
# EDA section

labeled.image1 <- filter(image1, label!=0)

corr.ndai.eda <- ggplot(labeled.image1) + 
  geom_jitter(aes(x=CORR, y=NDAI, 
                  group=factor(label), colour=factor(label),
                  alpha=0.5)) +
  ggtitle("CORR vs. NDAI EDA") +
  theme(legend.position="none", aspect.ratio=1)


ndai.sd.eda <- ggplot(labeled.image1) + 
  geom_jitter(aes(x=NDAI, y=SD, 
                  group=factor(label), colour=factor(label),
                  alpha=0.5)) +
  ggtitle("NDAI vs. SD EDA") +
  theme(legend.position="none", aspect.ratio=1)

corr.sd.eda <- ggplot(labeled.image1) + 
  geom_jitter(aes(x=CORR, y=SD, 
                  group=factor(label), colour=factor(label),
                  alpha=0.5)) +
  ggtitle("CORR vs. SD EDA") +
  theme(legend.position="none", aspect.ratio=1)

###############################################################################
# - Image Files  -
#
# If you want to save the images, ImageSave to TRUE

if (ImageSave){

ggsave(filename = "RAWEDA.png", path="../figures/", plot=raw.eda,
       height=5, width=5)
ggsave(filename = "EXPERTSEDA.png", path="../figures/", plot=experts.eda,
       height=3, width=8)
ggsave(filename = "NDAIEDA.png", path="../figures/", plot=ndai.eda,
       height=3, width=8)
ggsave(filename = "SDEDA.png", path="../figures/", plot=sd.eda,
       height=3, width=8)
ggsave(filename = "CORREDA.png", path="../figures/", plot=corr.eda,
       height=3, width=8)
ggsave(filename = "ANEDA.png", path="../figures/", plot=an.eda,
       height=5, width=5)
ggsave(filename = "AFEDA.png", path="../figures/", plot=af.eda,
       height=5, width=5)
ggsave(filename = "BFEDA.png", path="../figures/", plot=bf.eda,
       height=5, width=5)
ggsave(filename = "CFEDA.png", path="../figures/", plot=cf.eda,
       height=5, width=5)
ggsave(filename = "DFEDA.png", path="../figures/", plot=df.eda,
       height=5, width=5)
ggsave(filename="CORR_vs_NDAI.png", path="../figures/", plot=corr.ndai.eda,
       height=5, width=5)
ggsave(filename="NDAI_vs_SD.png", path="../figures/", plot=ndai.sd.eda,
       height=5, width=5)
ggsave(filename="CORR_vs_SD.png", path="../figures/", plot=corr.sd.eda,
       height=5, width=5)

}