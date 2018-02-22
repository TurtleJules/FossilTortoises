library(dplyr)
library(ggplot2)
library(multcompView)

set.seed(0)
lev <- gl(3, 10)
y <- c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)

a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)

generate_label_df <- function(HSD, flev){
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- HSD$flev[,4]
  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- ddply(d, flev, function (x) max(fivenum(x$y)) + 0.2)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  return(labels.df)
}


p_base <- ggplot(d, aes(x=lev, y=y)) + geom_boxplot() +
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels))



#########################################################

#PleiPlioCL$EpochBins = WaterConDryMass$ChillTime

PleiPlioCL <- PleiPlioCL %>%
  filter(!is.na(CL))

Model4 <- aov(CL~EpochBins, data=PleiPlioCL)
tHSD <- TukeyHSD(Model4, ordered = FALSE, conf.level = 0.95)
plot(tHSD , las=1 , col="brown" )


generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

model=lm(PleiPlioCL$CL~PleiPlioCL$EpochBins)
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'PleiPlioCL$EpochBins', conf.level=0.95)

labels<-generate_label_df(TUKEY , "PleiPlioCL$EpochBins")#generate labels using function

names(labels)<-c('Letters','EpochBins')#rename columns for merging

#yvalue<-aggregate(.~CL, data=PleiPlioCL, mean)# obtain letter position for y axis using means

yvalue <- PleiPlioCL %>%
  filter(!is.na(CL)) %>%
  group_by(EpochBins) %>%
  select(EpochBins, CL) %>%
  dplyr::summarise(CL=median(CL), mean=mean(CL))




final<-merge(labels,yvalue) #merge dataframes

ggplot(PleiPlioCL, aes(x = EpochBins, y = CL)) +
  geom_blank() +
  theme_bw() +
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
 # labs(x = 'Age', y = 'CL [mm]') +
#  ggtitle(expression(atop(bold("CL"), atop(italic("(Dry Mass)"), "")))) +
#  theme(plot.title = element_text(hjust = 0.5, face='bold')) +
#  annotate(geom = "rect", xmin = 1.5, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "grey90") +
  geom_boxplot(fill = 'green2', stat = "boxplot") +
  geom_text(data = final, aes(x = EpochBins, y = CL, label = Letters),vjust=-1) +  #,hjust=-.5
#  geom_vline(aes(xintercept=4.5), linetype="dashed") +
  theme(plot.title = element_text(vjust=-0.6)) +
  geom_text(data = final, aes(x = EpochBins, y = mean, label = "*")) 




## continental (excluding insular species)

### genera (continental)

################################################

#paleoTS plot with genus mean, excluding island species


GenusMeanIs <- PleiPlioCL %>%
  merge(Continents) %>%
  filter(EpochBins != "Modern" & Island == "n" & Continent!= "Europe") %>%
  filter(!is.na(CL)) %>%
  group_by(EpochBins, Genus) %>%
  # filter(EpochBins != "Aquitanian") %>%
  dplyr::summarise(meanCL = mean(CL), n=n()) %>%
  #  ungroup() %>%
  #  group_by(EpochBins) %>%
  dplyr::summarise(mm = mean(meanCL), nn=n(), vv=var(meanCL)) %>%
  merge(BINSMio) %>%
  select(mm, nn, vv, tt=MeanBins)

#na <- PleiPlioCL[!complete.cases(PleiPlioCL),]



GenusModernIs <- PleiPlioCL %>%
  filter(EpochBins == "Modern" & Island == "n") %>%
  filter(!is.na(CL)) %>%
  group_by(Genus) %>%
  dplyr::summarise(meanCL=mean(CL), sdCL=sd(CL), n=n(), Age=mean(Age), EpochBins=unique(EpochBins), MeanBins=unique(MeanBins)) #%>%
#  summarise(mm=mean(meanCL), nn=n(), vv=var(meanCL), tt=mean(MeanBins))



#sumTort <- read.csv("tortoises_summary.csv", sep=";", header=TRUE)
#colnames(sumTort)[7] <- "meanCL"
#colnames(sumTort)[8] <- "sdCL"


sumTortoises <- sumTort %>%
  dplyr::filter(Island == "n") %>%
  mutate(Genus= as.character(Genus)) %>%
  mutate(MeanBins=(Mamin+Mamax)/2) %>%
  select(Genus, MeanBins,  meanCL, sdCL, n) %>% #n,
  bind_rows(GenusModernIs)


SumTort <- sumTortoises %>%
  group_by(Genus) %>%
  mutate(tt=MeanBins, vv=sdCL^2, nn=n, mm=meanCL) %>%
  dplyr::select(mm, nn, vv, tt, Genus) %>%
  mutate(nx = nn*mm) %>%
  mutate(mmall=sum(nx)/sum(nn)) %>%
  mutate(SD=sqrt(nx), d=mm-mmall) %>%
  mutate(nsd=((nx^2+d^2)*nn)) %>%
  mutate(varall=sum(nsd)/sum(nn), n=sum(nn)) %>%
  dplyr::select(mm=mmall, vv=varall, nn=n, tt, Genus) %>%
  unique() %>%
  dplyr::select(CL=mm, n=nn, var=vv, tt, Genus)

#write.table(SumTort,file="SumTortModernGenus.txt", sep="\t", row.names = FALSE)

#kable(SumTort, caption="Overview over body size means per time bin on genus level, excluding island species.")

#boxplot(SumTort$CL, caption="Body size distribution in time bin 'modern'.")


modernMeanGenusIs <- SumTort %>%
  ungroup() %>%
  select(CL, n, tt) %>%
  filter( !is.na(CL)) %>%
  group_by(tt) %>%
  dplyr::summarise(mm = mean(CL), vv=var(CL), nn=n())

#bis hier alle modernen taxa zusammengefasst (summarised and MFN_testudinidae)
var(SumTortAll$CL)

GenusPaleoIs <- modernMeanGenusIs %>%
  bind_rows(GenusMeanIs)%>%
  arrange(tt)

GenusPaleoIs[is.na(GenusPaleoIs)] <- 0

kable(GenusPaleoIs, caption="paleoTS object, continental")


paleoGenIs <-as.paleoTS(GenusPaleoIs$mm, GenusPaleoIs$vv, GenusPaleoIs$nn, GenusPaleoIs$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode, Holocene-Upper Miocene, genus mean CL", reset.time=TRUE)

plot(paleoGenIs)


fit3models(paleoGenIs, silent=FALSE, method="AD", pool=FALSE)

PaleoGenIsFit <- (fit3models(paleoGenIs, silent=FALSE, method="AD", pool=FALSE))
