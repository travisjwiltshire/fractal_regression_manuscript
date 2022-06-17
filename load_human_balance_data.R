# Creating a .Rdata file for the analysis

path <- "C:/Users/wiltshir/Documents/Human Balance Data/"
file_names<-list.files(path=path)
file_names<-file_names[-c(1931:1934)] #remove non data files from file_names
BDSinfo <- read.delim("~/Human Balance Data/BDSinfo.txt")

library(fractalRegression)
library(segmented)
library(dplyr)

# Run a loop to append all the data files
for (i in 1:length(files_names)) {
  patterns <- c(".txt|~/Human Balance Data/")
  if (i == 1){
  file = paste("~/Human Balance Data/", file_names[i],sep = '')
  BDS_Dataset <- read.delim(file = file) %>%
  select(-c(2:7))
  BDS_Dataset$Trial <- rep_len(stringr::str_remove_all(string=file,pattern=patterns), length.out = length(BDS_Dataset$Time.s.))
  print(file)
  } else {
  file = paste("~/Human Balance Data/", file_names[i],sep = '')
  BDS_temp <-read.delim(file = file) %>%
    select(-c(2:7))
  BDS_temp$Trial <- rep_len(stringr::str_remove_all(string=file,pattern=patterns),  length.out = length(BDS_temp$Time.s.))
  BDS_Dataset <- rbind(BDS_Dataset,BDS_temp)
  print(file)
  }

}

# Remove unwanted columns
BDSinfo <- select(BDSinfo,-c(7:ncol(BDSinfo)))

# Merge datasets
BDS_test <- merge(BDS_Dataset,BDSinfo,by="Trial")

# Save as RDS
saveRDS(BDS_test, file="Human_Balance_Data.rds")

## This is from eyes open on firm surface
BDS00001 <- read.delim("~/Human Balance Data/BDS00001.txt")
scales <- logscale(scale_min = 16, scale_max=length(BDS00001$COPx.cm)/4,scale_ratio =1.1)
balance.test.dfa <- dfa(diff(BDS00001$COPx.cm), order = 1, scales=scales, verbose =1)

dfa.plot(balance.test.dfa)

# here we are checking what the breakpoints are due to the cross-over
dfa.mod <- lm(log_rms~log_scales, data=balance.test.dfa)
seg <- segmented(dfa.mod, seg.Z = ~log_scales)
plot(balance.test.dfa$log_scale,balance.test.dfa$log_rms, pch=16, ylab="logF(s)", xlab = "log(s)")
plot(seg,add=T, lwd=2,col="red")
slope(seg)

# run mfdfa
balance.test.mfdfa <- mfdfa(x = diff(BDS00001$COPx.cm), q = c(-5:5), order = 1, scales=scales)

mfdfa.plot(balance.test.mfdfa)

balance.test.dcca <- dcca(diff(BDS00001$COPx.cm),diff(BDS00001$COPy.cm),order=1,scales=scales)
plot(balance.test.dcca$scales,balance.test.dcca$rho,type='l')

dcca.plot(rhos=balance.test.dcca,order = 1, ci = TRUE, iterations = NULL, return.ci = TRUE)

balance.test.mra.xy <- mra(diff(BDS00001$COPx.cm),diff(BDS00001$COPy.cm),order=1,scales=scales)
balance.test.mra.yx <- mra(diff(BDS00001$COPy.cm),diff(BDS00001$COPx.cm),order=1,scales=scales)

mra.plot(balance.test.mra.xy,order = 1, ci = TRUE, iterations = NULL, return.ci = TRUE)
mra.plot(balance.test.mra.yx,order = 1, ci = TRUE, iterations = NULL, return.ci = TRUE)



plot(balance.test.mra.yx$scales,balance.test.mra.yx$betas,type='l', ylim = c(0,3), xlab="Scales", ylab="Betas", main = "Eyes Open Firm")
lines(balance.test.mra.xy$scales,balance.test.mra.xy$betas, col='blue')
legend("topright", legend = c("COPy>COPx", "COPx>COPy"), col=c('black','blue'),lty=1)

plot(balance.test.mra.xy$scales,balance.test.mra.xy$betas,type='l')

# Eyes open on foam surface
BDS00007 <- read.delim("~/Human Balance Data/BDS00007.txt")
balance.foam.dfa <- dfa(diff(BDS00007$COPx.cm), order = 1, scales=scales, verbose =1)

dfa.plot(balance.foam.dfa)

balance.foam.mfdfa <- mfdfa(x = diff(BDS00007$COPx.cm), q = c(-5:5), order = 1, scales=scales)

mfdfa.plot(balance.foam.mfdfa)

balance.foam.dcca <- dcca(diff(BDS00007$COPx.cm),diff(BDS00007$COPy.cm),order=1,scales=scales)
plot(balance.foam.dcca$scales,balance.foam.dcca$rho,type='l')


dcca.plot(rhos=balance.foam.dcca,order = 1, ci = TRUE, iterations = NULL, return.ci = TRUE)


balance.foam.mra.xy <- mra(diff(BDS00007$COPx.cm.),diff(BDS00007$COPy.cm.),order=1,scales=scales)
balance.foam.mra.yx <- mra(diff(BDS00007$COPy.cm.),diff(BDS00007$COPx.cm.),order=1,scales=scales)


mra.plot(balance.foam.mra.xy,order = 1, ci = TRUE, iterations = NULL, return.ci = TRUE)

mra.plot(balance.test.mra.yx,order = 1, ci = TRUE, iterations = NULL, return.ci = TRUE)


plot(balance.foam.mra.yx$scales,balance.foam.mra.yx$betas,type='l', xlab="Scales", ylab="Betas",ylim = c(0,3), main = "Eyes Open Foam")
lines(balance.foam.mra.xy$scales,balance.foam.mra.xy$betas, col='blue')
legend("topright", legend = c("COPy>COPx", "COPx>COPy"), col=c('black','blue'),lty=1)

plot(balance.foam.mra.xy$scales,balance.foam.mra.xy$betas,type='l')

