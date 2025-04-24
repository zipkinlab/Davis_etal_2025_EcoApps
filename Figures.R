# Shorebird single-spp IPM figures

#7F7F7F gray - pop size
#BFBFBF lt gray

#FFBF04 yellow - immigration
#FFEEBD lt yellow

#3B9CCE blue - adult survival
#B2D9EC lt blue

#FE7F9C teal - fecundity
#B8FEFB lt teal
library(openxlsx)
library(MCMCvis)
library(ggplot2)
library(plotrix)

setwd("")

# load model results
load(file = "amgp_18.Rdata")
load(file = "dunl_18.Rdata")
load(file = "sesa_18.Rdata")

MCMCsummary(amgp,
            params = c("beta.phia1","beta.phia2","beta.phia3",
                       "beta.fec1", "beta.fec2","beta.fec3", 
                       "beta.gam1", "beta.gam2", "beta.gam3"),
            probs = c(0.025, 0.05, 0.075, 0.1,0.25, 0.5, 0.75, 0.9, 0.925, 0.95, 0.975),
            round = 2)
MCMCsummary(dunl,
            params = c("beta.phia1","beta.phia2","beta.phia3",
                       "beta.fec1", "beta.fec2","beta.fec3", 
                       "beta.gam1", "beta.gam2", "beta.gam3"),
            probs = c(0.025, 0.05, 0.075, 0.1,0.25, 0.5, 0.75, 0.9, 0.925, 0.95, 0.975),
            round = 2)
MCMCsummary(sesa,
            params = c("beta.phia1","beta.phia2","beta.phia3",
                       "beta.fec1", "beta.fec2","beta.fec3", 
                       "beta.gam1", "beta.gam2", "beta.gam3"),
            probs = c(0.025, 0.05, 0.075, 0.1,0.25, 0.5, 0.75, 0.9, 0.925, 0.95, 0.975),
            round = 2)

#load pair data
sesa.dat <- read.csv("SESA_nests_2005_2022_18.csv")
dunl.dat <- read.csv("dunl_nests_2005_2022_18.csv")
amgp.dat <- read.csv("amgp_nests_2005_2022_18.csv")

# breeding pairs and productivity
amgp.pr <- amgp.dat$minPairs 
dunl.pr <- dunl.dat$minPairs 
sesa.pr <- sesa.dat$minPairs 

# read in covariate data 
yeareffects = read.xlsx("YearCovs.xlsx")

#name covariates for fecundity and adult survival
snow = yeareffects$Melt[3:20]
fox_high = yeareffects$Fox_mid_high[3:20]
pdo = yeareffects$PDO[3:20]

nyears <- 18


#------------------------------------------------------------------------------------------
# IPM Results plots - Fig 2
#------------------------------------------------------------------------------------------

#save plot
units1 <- "ssIPM_"
fname <- paste0(units1[1],"results18.pdf")
pdf(fname, height=8, width=12)

#IPM results plot
par(mfrow = c(4, 3), cex.axis = 1.5, cex.lab = 1.5, las = 1, mar = c(2, 3, 1, 1), mgp=c(3, 1, 0), omi=c(0.2,0.5,0,0))

#amgp pop size
lower <- upper <- numeric()
year <- 2005:2022
for (i in 1:nyears){
  lower[i] <- quantile(amgp$sims.list$Nad[,i], 0.025)
  upper[i] <- quantile(amgp$sims.list$Nad[,i], 0.975)}
m1 <- min(c(amgp$mean$Nad, amgp.pr, lower), na.rm = T)
m2 <- max(c(amgp$mean$Nad, amgp.pr, upper), na.rm = T)
plot(0, 0, ylim = c(0, m2), xlim = c(1, nyears), ylab = "Population size (pairs)", xlab = " ", col = "black", type = "l",  axes = F, frame = F)
axis(2)
axis(1, at = 1:nyears, labels = year)
polygon(x = c(1:nyears, nyears:1), y = c(lower, upper[nyears:1]), col = "#BFBFBF", border = "#BFBFBF")
points(amgp$mean$Nad, type = "l", col = "#7F7F7F", lwd = 1.4)
points(amgp.pr, type = "l", col = "black", lwd = 1.4)
legend(x = 11, y = 25, legend = c("Counts", "Estimates"), lty = c(1, 1),lwd = c(2, 2), col = c("black", "#7F7F7F"), bty = "n", cex = 1.5)
corner.label("(a)", font=2, cex=2, xoff=0.2)

#dunl pop size
lower <- upper <- numeric()
year <- 2005:2022
for (i in 1:nyears){
  lower[i] <- quantile(dunl$sims.list$Nad[,i], 0.025)
  upper[i] <- quantile(dunl$sims.list$Nad[,i], 0.975)}
m1 <- min(c(dunl$mean$Nad, dunl.pr, lower), na.rm = T)
m2 <- max(c(dunl$mean$Nad, dunl.pr, upper), na.rm = T)
plot(0, 0, ylim = c(0, m2), xlim = c(1, nyears), xlab = " ", col = "black", type = "l",  axes = F, frame = F)
axis(2)
axis(1, at = 1:nyears, labels = year)
polygon(x = c(1:nyears, nyears:1), y = c(lower, upper[nyears:1]), col = "#BFBFBF", border = "#BFBFBF")
points(dunl$mean$Nad, type = "l", col = "#7F7F7F", lwd = 1.4)
points(dunl.pr, type = "l", col = "black", lwd = 1.4)
legend(x = 0.5, y = 14, legend = c("Counts", "Estimates"), lty = c(1, 1),lwd = c(2, 2), col = c("black", "#7F7F7F"), bty = "n", cex = 1.5)
corner.label("(b)", font=2, cex=2, xoff=0.2)

#sesa pop size
lower <- upper <- numeric()
year <- 2005:2022
for (i in 1:nyears){
  lower[i] <- quantile(sesa$sims.list$Nad[,i], 0.025)
  upper[i] <- quantile(sesa$sims.list$Nad[,i], 0.975)}
m1 <- min(c(sesa$mean$Nad, sesa.pr, lower), na.rm = T)
m2 <- max(c(sesa$mean$Nad, sesa.pr, upper), na.rm = T)
plot(0, 0, ylim = c(0, m2), xlim = c(1, nyears), xlab = " ", col = "black", type = "l",  axes = F, frame = F)
axis(2)
axis(1, at = 1:nyears, labels = year)
polygon(x = c(1:nyears, nyears:1), y = c(lower, upper[nyears:1]), col = "#BFBFBF", border = "#BFBFBF")
points(sesa$mean$Nad, type = "l", col = "#7F7F7F", lwd = 1.4)
points(sesa.pr, type = "l", col = "black", lwd = 1.4)
legend(x = 11, y = 55, legend = c("Counts", "Estimates"), lty = c(1, 1),lwd = c(2, 2), col = c("black", "#7F7F7F"), bty = "n", cex = 1.5)
corner.label("(c)", font=2, cex=2, xoff=0.2)

#amgp imm/rec
lower <- upper <- numeric()
T <- nyears
for (t in 2:T){
  lower[t] <- quantile(amgp$sims.list$I[,t], 0.025)
  upper[t] <- quantile(amgp$sims.list$I[,t], 0.975)}

plot(0,0, ylim = c(0, 20), xlim = c(1,nyears), xaxt = "n", ylab = "Adults gained", xlab = "", axes = F, cex = 2, frame = F, lwd = 1.3)
axis(2)
axis(1, at = 1:nyears, labels = year)

segments((1:nyears), lower, (1:nyears), upper, col = "#FFBF04")
points(y = amgp$mean$I, x = (1:nyears), type = "b", pch = 16, col = "#FFBF04", cex = 1.5)
legend(x = 1, y = 22, legend = "Mean = 4 (1, 7)", bty = "n", cex = 1.5)
corner.label("(d)", font=2, cex=2, xoff=0.2)

#dunl imm/rec
lower <- upper <- numeric()
T <- nyears
for (t in 2:T){
  lower[t] <- quantile(dunl$sims.list$I[,t], 0.025)
  upper[t] <- quantile(dunl$sims.list$I[,t], 0.975)}

plot(0,0, ylim = c(0, 40), xlim = c(1,nyears), xaxt = "n", xlab = "", axes = F, cex = 2, frame = F, lwd = 1.3)
axis(2)
axis(1, at = 1:(T), labels = year)

segments((1:T), lower, (1:T), upper, col = "#FFBF04")
points(y = dunl$mean$I, x = (1:T), type = "b", pch = 16, col = "#FFBF04", cex = 1.5)
legend(x = 1, y = 45, legend = "Mean = 14 (10,17) ", bty = "n", cex = 1.5)
corner.label("(e)", font=2, cex=2, xoff=0.2)

#sesa imm/rec
lower <- upper <- numeric()
T <- nyears
for (t in 2:T){
  lower[t] <- quantile(sesa$sims.list$I[,t], 0.025)
  upper[t] <- quantile(sesa$sims.list$I[,t], 0.975)}

plot(0,0, ylim = c(0, 50), xlim = c(1,nyears), xaxt = "n", xlab = "", axes = F, cex = 2, frame = F, lwd = 1.3)
axis(2)
axis(1, at = 1:(T), labels = year)

segments((1:T), lower, (1:T), upper, col = "#FFBF04")
points(y = sesa$mean$I, x = (1:T), type = "b", pch = 16, col = "#FFBF04", cex = 1.5)
legend(x = 1, y = 55, legend = "Mean = 6 (2, 10) ", bty = "n", cex = 1.5)
corner.label("(f)", font=2, cex=2, xoff=0.2)

#amgp ad surv
lower <- upper <- numeric()
T <- nyears-1
for (t in 1:T){
  lower[t] <- quantile(amgp$sims.list$phia[,t], 0.025)
  upper[t] <- quantile(amgp$sims.list$phia[,t], 0.975)}
plot(0,0, cex = 1.1, lwd = 1.3, ylim = c(0, 1.0), xlim = c(1,nyears), ylab = "Apparent adult survival", xaxt = "n", xlab = "", frame = F)
axis(2)
axis(1, at = 1:(nyears), labels = year)

segments((1:T)+0.5, lower, (1:T)+0.5, upper, col = "#3B9CCE")
points(y = amgp$mean$phia, x = (1:T)+0.5, type = "b", pch = 16, col = "#3B9CCE", cex = 1.5 )
legend(x = 0, y = 0.2, legend = "Mean = 0.57 (0.42, 0.77)", bty = "n", cex = 1.5)
corner.label("(g)", font=2, cex=2, xoff=0.2)

#dunl ad surv
lower <- upper <- numeric()
T <- nyears-1
for (t in 1:T){
  lower[t] <- quantile(dunl$sims.list$phia[,t], 0.025)
  upper[t] <- quantile(dunl$sims.list$phia[,t], 0.975)}
plot(0,0, cex = 1.1, lwd = 1.3, ylim = c(0, 1.0), xlim = c(1,nyears), xaxt = "n", xlab = "", frame = F)
axis(2)
axis(1, at = 1:(nyears), labels = year)

segments((1:T)+0.5, lower, (1:T)+0.5, upper, col = "#3B9CCE")
points(y = dunl$mean$phia, x = (1:T)+0.5, type = "b", pch = 16, col = "#3B9CCE", cex = 1.5)
legend(x = 0, y = 0.2, legend = "Mean = 0.64 (0.60, 0.69)", bty = "n", cex = 1.5)
corner.label("(h)", font=2, cex=2, xoff=0.2)

#sesa ad surv
lower <- upper <- numeric()
T <- nyears-1
for (t in 1:T){
  lower[t] <- quantile(sesa$sims.list$phia[,t], 0.025)
  upper[t] <- quantile(sesa$sims.list$phia[,t], 0.975)}
plot(0,0, cex = 1.1, lwd = 1.3, ylim = c(0, 1.0), xlim = c(1,nyears), xaxt = "n", xlab = "", frame = F)
axis(2)
axis(1, at = 1:(nyears), labels = year)

segments((1:T)+0.5, lower, (1:T)+0.5, upper, col = "#3B9CCE")
points(y = sesa$mean$phia, x = (1:T)+0.5, type = "b", pch = 16, col = "#3B9CCE", cex = 1.5)
legend(x = 0, y = 0.2, legend = "Mean = 0.59 (0.53, 0.65)", bty = "n", cex = 1.5)
corner.label("(i)", font=2, cex=2, xoff=0.2)

#amgp fec
lower <- upper <- numeric()
T <- nyears
for (t in 1:T){
  lower[t] <- quantile(amgp$sims.list$f[,t], 0.025)
  upper[t] <- quantile(amgp$sims.list$f[,t], 0.975)}

plot(0,0, ylim = c(0, 4), xlim=c(1,nyears), ylab = "Fecundity (fledgling / female)", xlab = "", axes = F, cex = 1.1, frame = F, lwd = 1.3, col = '#b4de2c')
axis(2, ylim = c(0, 4))
axis(1, at = 1:(T), labels = year)
par(new = T)

points(amgp$mean$f[c(1:4, 6:12)], x = c(1:4, 6:12), col = "#014d4e", pch = 16, cex = 1.5)
points(amgp$mean$f[c(5, 13:18)], x = c(5, 13:18), col = "#249c72", pch = 16, cex = 1.5)

segments(c(1:4, 6:12), lower[c(1:4, 6:12)], c(1:4, 6:12), upper[c(1:4, 6:12)], col = "#014d4e")
segments(c(5, 13:18), lower[c(5, 13:18)], c(5, 13:18), upper[c(5, 13:18)], col = "#249c72")

legend(x = 1, y = 4, legend = "Mean no fox removal=0.29 (0.16, 0.46)", pch = 16, col = "#249c72", bty = "n", cex = 1.5)
legend(x = 1, y = 4.5, legend = "Mean fox removal=2.40 (2.02, 2.82)", pch = 16, col = "#014d4e", bty = "n", cex = 1.5)

corner.label("(j)", font=2, cex=2, xoff=0.2)

#dunl fec
lower <- upper <- numeric()
T <- nyears
for (t in 1:T){
  lower[t] <- quantile(dunl$sims.list$f[,t], 0.025)
  upper[t] <- quantile(dunl$sims.list$f[,t], 0.975)}

plot(0,0, ylim = c(0, 4), xlim=c(1,nyears), xlab = "", axes = F, cex = 1.1, frame = F, lwd = 1.3, col = '#b4de2c')
axis(2, ylim = c(0, 4))
axis(1, at = 1:(T), labels = year)
par(new = T)

points(dunl$mean$f[c(1:4, 6:12)], x = c(1:4, 6:12), col = "#014d4e", pch = 16, cex = 1.5)
points(dunl$mean$f[c(5, 13:18)], x = c(5, 13:18), col = "#249c72", pch = 16, cex = 1.5)

segments(c(1:4, 6:12), lower[c(1:4, 6:12)], c(1:4, 6:12), upper[c(1:4, 6:12)], col = "#014d4e")
segments(c(5, 13:18), lower[c(5, 13:18)], c(5, 13:18), upper[c(5, 13:18)], col = "#249c72")

legend(x = 0, y = 1.2, legend = "Mean = 2.91 (2.69, 3.15)", pch = 16, col = "#014d4e", bty = "n", cex = 1.5)
legend(x = 0, y = 0.7, legend = "Mean = 1.08 (0.90, 1.26)", pch = 16,  col = "#249c72", bty = "n", cex = 1.5)

corner.label("(k)", font=2, cex=2, xoff=0.2)

#sesa fec
lower <- upper <- numeric()
T <- nyears
for (t in 1:T){
  lower[t] <- quantile(sesa$sims.list$f[,t], 0.025)
  upper[t] <- quantile(sesa$sims.list$f[,t], 0.975)}

plot(0,0, ylim = c(0, 4), xlim=c(1,nyears), xlab = "", axes = F, cex = 1.1, frame = F, lwd = 1.3, col = '#b4de2c')
axis(2, ylim = c(0, 4))
axis(1, at = 1:(T), labels = year)
par(new = T)

points(sesa$mean$f[c(1:4, 6:12)], x = c(1:4, 6:12), col = "#014d4e", pch = 16, cex = 1.5)
points(sesa$mean$f[c(5, 13:18)], x = c(5, 13:18), col = "#249c72", pch = 16, cex = 1.5)

segments(c(1:4, 6:12), lower[c(1:4, 6:12)], c(1:4, 6:12), upper[c(1:4, 6:12)], col = "#014d4e")
segments(c(5, 13:18), lower[c(5, 13:18)], c(5, 13:18), upper[c(5, 13:18)], col = "#249c72")

legend(x = 0, y = 0.7, legend = "Mean = 1.25 (1.01, 1.53)", pch = 16,col = "#249c72", bty = "n", cex = 1.5)
legend(x = 0, y = 1.2, legend = "Mean = 2.81 (2.54, 3.08)", pch = 16, col = "#014d4e", bty = "n", cex = 1.5)

corner.label("(l)", font=2, cex=2, xoff=0.2)
dev.off()




#------------------------------------------------------------------------------------------
# Covariate effects - Fig 3
#------------------------------------------------------------------------------------------


zsnow <- scale(snow)
zpdo <- scale(pdo)
z = (seq(min(snow), max(snow), length.out = 100)-mean(snow))/sd(snow)
zp = (seq(min(zpdo), max(zpdo), length.out = 100))
a <- (z*sd(snow))+mean(snow)
ap <- (zp*sd(pdo))+mean(pdo)

gam.dat <- cbind(z, a, dunl$mean$gam.pred, dunl$q2.5$gam.pred, dunl$q97.5$gam.pred,
                 amgp$mean$gam.pred, amgp$q2.5$gam.pred, amgp$q97.5$gam.pred,
                 sesa$mean$gam.pred, sesa$q2.5$gam.pred, sesa$q97.5$gam.pred)
colnames(gam.dat) <- c("z", "a", "dunl.m", "dunl.l", "dunl.u",
                       "amgp.m", "amgp.l", "amgp.u",
                       "sesa.m", "sesa.l", "sesa.u")
gam.dat <- as.data.frame(gam.dat)
head(gam.dat)
max(gam.dat$dunl.m) #157.5
max(gam.dat$amgp.m) #156.2
max(gam.dat$sesa.m) #154


pdf("Imm_snow18.pdf", height=5.2, width=6.5)
ggplot(data = gam.dat) + 
  geom_segment(aes(x = 157.5, y = -2, xend = 157.5, yend = 16.2), linetype = "dashed") + 
  geom_segment(aes(x = 156, y = -2, xend = 156, yend = 9), linetype = "dashed") + 
  geom_line(aes(y=dunl.m, x = a), colour="#6e521c", size = 2) + 
  geom_ribbon(aes(ymin=dunl.l, ymax=dunl.u, x = a), fill="#6e521c", alpha=0.2) +
  geom_line(aes(y=amgp.m, x = a), colour="#7d2f5a", size = 2) + 
  geom_ribbon(aes(ymin=amgp.l, ymax=amgp.u, x = a), fill="#7d2f5a", alpha=0.2) +
  geom_segment(aes(x = 169, y = 30, xend = 171, yend = 30), colour = "#7d2f5a", size  = 2) +
  geom_segment(aes(x = 169, y = 28, xend = 171, yend = 28), colour = "#6e521c", size  = 2) +
  geom_text(x=173.5, y=30, label="AMGP", size = 7) +
  geom_text(x=173.5, y=28, label="DUNL", size = 7) +
  theme_classic()+
  ylab(bquote(Adults~gained[t]))+
  xlab(bquote(Snowmelt[t]))+
  theme(text = element_text(size = 24)) +
  coord_cartesian(ylim=c(0,30))
dev.off()
  

fec.dat <- cbind(z, a, dunl$mean$fec.pred, dunl$q2.5$fec.pred, dunl$q97.5$fec.pred,
                 amgp$mean$fec.pred, amgp$q2.5$fec.pred, amgp$q97.5$fec.pred,
                 sesa$mean$fec.pred, sesa$q2.5$fec.pred, sesa$q97.5$fec.pred)
colnames(fec.dat) <- c("z", "a", "dunl.m", "dunl.l", "dunl.u",
                       "amgp.m", "amgp.l", "amgp.u",
                       "sesa.m", "sesa.l", "sesa.u")
fec.dat <- as.data.frame(fec.dat)
head(fec.dat)
max(fec.dat$dunl.m) #148
max(fec.dat$amgp.m) #163
max(fec.dat$sesa.m) #157

pdf("Fec_snow18.pdf", height=5.2, width=6.5)
ggplot(data = fec.dat) + 
  geom_segment(aes(x = 157, y = -2, xend = 157, yend = 1.34), linetype = "dashed") + 
  geom_line(aes(y=dunl.m, x = a), colour="#6e521c", size = 2) + 
  geom_ribbon(aes(ymin=dunl.l, ymax=dunl.u, x = a), fill="#6e521c", alpha=0.2) +
  geom_line(aes(y=sesa.m, x = a), colour="#232757", size = 2) + 
  geom_ribbon(aes(ymin=sesa.l, ymax=sesa.u, x = a), fill="#232757", alpha=0.2) +
  geom_segment(aes(x = 169, y = 2, xend = 171, yend = 2), colour = "#6e521c", size  = 2) +
  geom_segment(aes(x = 169, y = 1.85, xend = 171, yend = 1.85), colour = "#232757", size  = 2) +
  geom_text(x=173.5, y=2, label="DUNL", size  = 7) +
  geom_text(x=173.5, y=1.85, label="SESA", size  = 7) +
  theme_classic()+
  ylab(bquote(Fecundity[t]))+
  xlab(bquote(Snowmelt[t]))+
  theme(text = element_text(size = 24))+
  coord_cartesian(ylim=c(0,2))
dev.off()


# fecundity fox box plot
# create a data frame
Species=rep(c("AMGP", "DUNL", "SESA"), each=60000)
Fox=rep(c("High", "Low"), each = 30000, 3)

amgp.high <- rep(NA, 30000)
for (i in 1:30000){
  amgp.high[i] <- amgp$sims.list$beta.fec3[i] + amgp$sims.list$mfec[i]
}
amgp.low <- amgp$sims.list$mfec 

dunl.high <- rep(NA, 30000)
for (i in 1:30000){
  dunl.high[i] <- dunl$sims.list$beta.fec3[i] + dunl$sims.list$mfec[i]
}
dunl.low <- dunl$sims.list$mfec 

sesa.high <- rep(NA, 30000)
for (i in 1:30000){
  sesa.high[i] <- sesa$sims.list$beta.fec3[i] + sesa$sims.list$mfec[i]
}
sesa.low <- sesa$sims.list$mfec 

data=data.frame(Species, Fox) 
data$fox.dat <- c(amgp.high, amgp.low, dunl.high, dunl.low, sesa.high, sesa.low)


# grouped boxplot
pdf("fox_box.pdf", height=5.2, width=6.5)
ggplot(data, aes(x=Species, y=fox.dat, fill=Fox)) + 
  geom_boxplot()+
  scale_fill_manual(values = c(alpha("black", 0.6), "white"), name = bquote("Fox Control"[t]))+
  ylab("")+
  ylab(bquote(Fecundity[t]))+
  theme_classic()+
  theme(text = element_text(size = 24))
dev.off()


### Adult survival and previous year snow effect
phi.dat <- cbind(z, a, dunl$mean$phia.pred, dunl$q2.5$phia.pred, dunl$q97.5$phia.pred,
                 sesa$mean$phia.pred, sesa$q2.5$phia.pred, sesa$q97.5$phia.pred)
colnames(phi.dat) <- c("z", "a", "dunl.m", "dunl.l", "dunl.u",
                       "sesa.m", "sesa.l", "sesa.u")
phi.dat <- as.data.frame(phi.dat)
head(phi.dat)

lower <- upper <- numeric()
T <- 17
for (t in 1:T){
  lower[t] <- quantile(sesa$sims.list$phia[,t], 0.025)
  upper[t] <- quantile(sesa$sims.list$phia[,t], 0.975)
}
phia <- sesa$mean$phia
sphia <- data.frame(phia, lower, upper, snow[3:19])

pdf("sesa_phia_snow.pdf", height=5.2, width=6.5)
ggplot(data = phi.dat) + 
  geom_point(data = sphia,aes(x=snow.3.19., y=phia), size = 5, color = "#232757" ) + 
  geom_linerange(data = sphia, aes(x = snow.3.19., ymin=lower, ymax=upper), color  = "#232757") + 
  geom_line(aes(y=sesa.m, x = a), colour="#232757", size = 2) + 
  geom_ribbon(aes(ymin=sesa.l, ymax=sesa.u, x = a), fill="#232757", alpha=0.2) +
  geom_segment(aes(x = 169, y = 0.95, xend = 171, yend = 0.95), colour = "#232757", size  = 2) +
  geom_text(x=173.5, y=0.95, label="SESA", size  = 7) +
  theme_classic()+
  ylab(bquote(Adult~survival[t]))+
  xlab(bquote(Snowmelt[t-1]))+
  theme(text = element_text(size = 24))+
  coord_cartesian(ylim=c(0,1))
dev.off()


#AMGP and SESA PDO
pdo.dat <- cbind(zp, ap, amgp$mean$pdo.pred, amgp$q2.5$pdo.pred, amgp$q97.5$pdo.pred,
                 sesa$mean$pdo.pred, sesa$q2.5$pdo.pred, sesa$q97.5$pdo.pred)
colnames(pdo.dat) <- c("z", "a", "amgp.m", "amgp.l", "amgp.u",
                       "sesa.m", "sesa.l", "sesa.u")
pdo.dat <- as.data.frame(pdo.dat)
head(pdo.dat)

pdf("Phia_pdo18.pdf", height=5.2, width=6.5)
ggplot(data = pdo.dat) + 
  geom_line(aes(y=amgp.m, x = a), colour="#7d2f5a", size = 2) + 
  geom_ribbon(aes(ymin=amgp.l, ymax=amgp.u, x = a), fill="#7d2f5a", alpha=0.2) +
  geom_line(aes(y=sesa.m, x = a), colour="#232757", size = 2) +
  geom_ribbon(aes(ymin=sesa.l, ymax=sesa.u, x = a), fill="#232757", alpha=0.2) +
  geom_segment(aes(x = 0.25, y = 1.0, xend = 0.5, yend = 1), colour = "#7d2f5a", size  = 2) +
  geom_segment(aes(x = 0.25, y = 0.92, xend = 0.5, yend = 0.92), colour = "#232757", size  = 2) +
  geom_text(x=0.8, y=1, label="AMGP", size  = 7) +
  geom_text(x=0.8, y=0.92, label="SESA", size  = 7) +
  theme_classic()+
  theme(legend.position="none")+
  ylab(bquote(Adult~survival[t]))+
  xlab(bquote(PDO[t]))+
  theme(text = element_text(size = 24))+
  coord_cartesian(ylim=c(0,1))
dev.off()





#------------------------------------------------------------------------------------------
# Population growth rates - Fig 4
#------------------------------------------------------------------------------------------

# examine correlations and plot
lambda.h <- lam.lower.h <- lam.upper.h <- numeric()
Fitted.h <- lower.h <- upper.h <- matrix(NA, nrow = nyears-1, ncol = 2)
for (i in 1:(nyears-1)){
  lambda.h[i] <- quantile(amgp$sims.list$lambda[,i], 0.50)
  lam.lower.h[i] <- quantile(amgp$sims.list$lambda[,i], 0.025)
  lam.upper.h[i] <- quantile(amgp$sims.list$lambda[,i], 0.975)
}
for (i in 1:(nyears-1)){
  Fitted.h[i,1] <- mean(amgp$sims.list$phia[,i])
  lower.h[i,1] <- quantile(amgp$sims.list$phia[,i], 0.025)
  upper.h[i,1] <- quantile(amgp$sims.list$phia[,i], 0.975)
}

for (i in 1:(nyears-1)){
  Fitted.h[i,2] <- mean(amgp$sims.list$f[,i])
  lower.h[i,2] <- quantile(amgp$sims.list$f[,i], 0.025)
  upper.h[i,2] <- quantile(amgp$sims.list$f[,i], 0.975)
}

Fitted.i <- lower.i <- upper.i <- numeric()
for (i in 2:(nyears)){
  Fitted.i[i] <- mean(amgp$sims.list$I[,i])
  lower.i[i] <- quantile(amgp$sims.list$I[,i], 0.025)
  upper.i[i] <- quantile(amgp$sims.list$I[,i], 0.975)
}

# calculate some correlation coefficients
correl.h <- matrix(NA, ncol = 3, nrow = 60001)
for (i in 1:30000){
  correl.h[i,1] <- cor(amgp$sims.list$lambda[i,], amgp$sims.list$phia[i,])
  correl.h[i,2] <- cor(amgp$sims.list$lambda[i,], amgp$sims.list$I[i,2:18])
  correl.h[i,3] <- cor(amgp$sims.list$lambda[i,], amgp$sims.list$f[i,1:17])
}

# credible intervals of correlation coefficients
quantile(correl.h[,1], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,2], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,3], c(0.05, 0.5, 0.95), na.rm = TRUE)

# compute the posterior modes of correlation coefficients
m <- density(correl.h[,1], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,2], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,3], na.rm = TRUE)
m$x[which(m$y==max(m$y))]


# probability that correlation coefficients (r) > 0
sum(correl.h[!is.na(correl.h[,1]),1]>0)/30000
sum(correl.h[!is.na(correl.h[,2]),2]>0)/30000
sum(correl.h[!is.na(correl.h[,3]),3]>0)/30000



# Plot retrospective fig
#save plot
pdf("amgp.pgr.pdf", height=4, width=11)

par(mfrow = c(1,3), mar = c(4, 5, 1, 1), mgp=c(3, 1, 0), las = 1, cex = 1.2)
linecol <- alpha("#7d2f5a", 0.3)
plot(y = lambda.h, Fitted.h[,1],  cex = 1.2, cex.lab = 1.2, lwd = 1.3, type = "n", xlim = c(0, 1), ylim = c(0, 6), ylab = "Population growth rate", xlab = "Adult survival", frame = FALSE, pch = 19)
segments(Fitted.h[,1], lam.lower.h, Fitted.h[,1], lam.upper.h, col = linecol, lwd = 1.3)
segments(lower.h[,1], lambda.h, upper.h[,1], lambda.h, col = linecol, lwd = 1.3)
points(y = lambda.h, Fitted.h[,1], pch = 19, col = "#7d2f5a", cex = 1.1)
text(x = -0.05, y = 5.2, "r = 0.26 (-0.24, 0.47)", pos = 4, font = 3, cex = 1.1)
text(x = -0.05, y = 4.7, "P(r>0) = 0.77", pos = 4, font = 3, cex = 1.1)
corner.label("(a)", font=2, cex=1.3, xoff=0.05)

par(mar = c(4, 3, 1, 1))
plot(y = lambda.h, Fitted.i[2:18],  cex = 1.2, cex.lab = 1.2, lwd = 1.3, type = "n", xlim = c(0, 18), ylim = c(0, 6),  ylab = "", xlab = "Adults gained", frame.plot = FALSE, pch = 19)
segments(Fitted.i[2:18], lam.lower.h, Fitted.i[2:18], lam.upper.h, col = linecol, lwd = 1.3)
segments(lower.i[2:18], lambda.h, upper.i[2:18], lambda.h, col = linecol, lwd = 1.3)
points(y = lambda.h, Fitted.i[2:18], pch = 19, col = "#7d2f5a", cex = 1.1)
text(x = 0, y = 5.2, "r = 0.34 (-0.06, 0.69)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 4.7, "P(r>0) = 0.90", pos = 4, font = 3, cex = 1.1)
corner.label("(b)", font=2, cex=1.3, xoff=0.75)


par(mar = c(4, 3, 1, 1))
plot(y = lambda.h, Fitted.h[,2][1:17],  cex = 1.2, cex.lab = 1.2, lwd = 1.3, type = "n", xlim = c(0, 3), ylim = c(0, 6),  ylab = "", xlab = "Fecundity", frame.plot = FALSE, pch = 19)
segments(Fitted.h[,2][1:17], lam.lower.h, Fitted.h[,2][1:17], lam.upper.h, col = linecol, lwd = 1.3)
segments(lower.h[,2][1:17], lambda.h, upper.h[,2][1:17], lambda.h, col = linecol, lwd = 1.3)
points(y = lambda.h, Fitted.h[,2][1:17], pch = 19, col = "#7d2f5a", cex = 1.1)
text(x = 0, y = 5.2, "r = 0.25 (0.11, 0.45)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 4.7, "P(r>0) = 0.99", pos = 4, font = 3, cex = 1.1)
corner.label("(c)", font=2, cex=1.3, xoff=0.1)

dev.off()

# examine correlations and plot
lambda.h <- lam.lower.h <- lam.upper.h <- numeric()
Fitted.h <- lower.h <- upper.h <- matrix(NA, nrow = nyears-1, ncol = 3)
for (i in 1:(nyears-1)){
  lambda.h[i] <- mean(dunl$sims.list$lambda[,i])
  lam.lower.h[i] <- quantile(dunl$sims.list$lambda[,i], 0.025)
  lam.upper.h[i] <- quantile(dunl$sims.list$lambda[,i], 0.975)
}
for (i in 1:(nyears-1)){
  Fitted.h[i,1] <- mean(dunl$sims.list$phia[,i])
  lower.h[i,1] <- quantile(dunl$sims.list$phia[,i], 0.025)
  upper.h[i,1] <- quantile(dunl$sims.list$phia[,i], 0.975)
}
for (i in 1:(nyears-1)){
  Fitted.h[i,2] <- mean(dunl$sims.list$f[,i])
  lower.h[i,2] <- quantile(dunl$sims.list$f[,i], 0.025)
  upper.h[i,2] <- quantile(dunl$sims.list$f[,i], 0.975)
}

Fitted.i <- lower.i <- upper.i <- numeric()
for (i in 2:(nyears)){
  Fitted.i[i] <- mean(dunl$sims.list$I[,i])
  lower.i[i] <- quantile(dunl$sims.list$I[,i], 0.025)
  upper.i[i] <- quantile(dunl$sims.list$I[,i], 0.975)
}

# calculate some correlation coefficients
correl.h <- matrix(NA, ncol = 3, nrow = 60001)
for (i in 1:30000){
  correl.h[i,1] <- cor(dunl$sims.list$lambda[i,], dunl$sims.list$phia[i,])
  correl.h[i,2] <- cor(dunl$sims.list$lambda[i,], dunl$sims.list$I[i,2:18])
  correl.h[i,3] <- cor(dunl$sims.list$lambda[i,], dunl$sims.list$f[i,1:17])
}

# credible intervals of correlation coefficients
quantile(correl.h[,1], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,2], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,3], c(0.05, 0.5, 0.95), na.rm = TRUE)


# compute the posterior modes of correlation coefficients
m <- density(correl.h[,1], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,2], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,3], na.rm = TRUE)
m$x[which(m$y==max(m$y))]


# probability that correlation coefficients (r) > 0
sum(correl.h[!is.na(correl.h[,1]),1]>0)/30000
sum(correl.h[!is.na(correl.h[,2]),2]>0)/30000
sum(correl.h[!is.na(correl.h[,3]),3]>0)/30000


# Plot retrospective fig
#save plot
pdf("dunl.pgr.pdf", height=4, width=11)

par(mfrow = c(1,3), mar = c(4, 5, 1, 1), mgp=c(3, 1, 0), las = 1, cex = 1.2)
linecol <- alpha("#6e521c", 0.3)
plot(y = lambda.h, Fitted.h[,1],  cex = 1.2, cex.lab = 1.2, lwd = 1.3, type = "n", xlim = c(0, 1), ylim = c(0, 3), ylab = "Population growth rate", xlab = "Adult survival", frame = FALSE, pch = 19)
segments(Fitted.h[,1], lam.lower.h, Fitted.h[,1], lam.upper.h, col = linecol, lwd = 1.3)
segments(lower.h[,1], lambda.h, upper.h[,1], lambda.h, col = linecol, lwd = 1.3)
points(y = lambda.h, Fitted.h[,1], pch = 19, col = "#6e521c", cex = 1.1)
text(x = -0.05, y = 2.65, "r = 0.27 (-0.29, 0.58)", pos = 4, font = 3, cex = 1.1)
text(x = -0.05, y = 2.4, "P(r>0) = 0.77", pos = 4, font = 3, cex = 1.1)
corner.label("(d)", font=2, cex=1.3, xoff=0.05)


par(mar = c(4, 3, 1, 1))
plot(y = lambda.h, Fitted.i[2:18],  cex = 1.2, cex.lab = 1.2, lwd = 1.3,type = "n", xlim = c(0, 25), ylim = c(0, 3),  ylab = "", xlab = "Adults gained", frame.plot = FALSE, pch = 19)
segments(Fitted.i[2:18], lam.lower.h, Fitted.i[2:18], lam.upper.h, col = linecol, lwd = 1.3)
segments(lower.i[2:18], lambda.h, upper.i[2:18], lambda.h, col = linecol, lwd = 1.3)
points(y = lambda.h, Fitted.i[2:18], pch = 19, col = "#6e521c", cex = 1.1)
text(x = 0, y = 2.65, "r = 0.79 (0.59, 0.88)", pos = 4, font = 3, cex = 01.1)
text(x = 0, y = 2.4, "P(r>0) = 1.00", pos = 4, font = 3, cex = 1.1)
corner.label("(e)", font=2, cex=1.3, xoff=1)

par(mar = c(4, 3, 1, 1))
plot(y = lambda.h, Fitted.h[,2][1:17],  cex = 1.2, cex.lab = 1.2, lwd = 1.3,type = "n", xlim = c(0, 4), ylim = c(0, 3),  ylab = "", xlab = "Fecundity", frame.plot = FALSE, pch = 19)
segments(Fitted.h[,2][1:17], lam.lower.h, Fitted.h[,2][1:17], lam.upper.h, col = linecol, lwd = 1.3)
segments(lower.h[,2][1:17], lambda.h, upper.h[,2][1:17], lambda.h, col = linecol, lwd = 1.3)
points(y = lambda.h, Fitted.h[,2][1:17], pch = 19, col = "#6e521c", cex = 1.1)
text(x = 0, y = 2.65, "r = 0.01 (-0.17, 0.14)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 2.4, "P(r>0) = 0.46", pos = 4, font = 3, cex = 1.1)
corner.label("(f)", font=2, cex=1.3, xoff=0.2)

dev.off()

# examine correlations and plot
lambda.h <- lam.lower.h <- lam.upper.h <- numeric()
Fitted.h <- lower.h <- upper.h <- matrix(NA, nrow = nyears-1, ncol = 2)
for (i in 1:(nyears-1)){
  lambda.h[i] <- quantile(sesa$sims.list$lambda[,i], 0.50)
  lam.lower.h[i] <- quantile(sesa$sims.list$lambda[,i], 0.025)
  lam.upper.h[i] <- quantile(sesa$sims.list$lambda[,i], 0.975)
}
for (i in 1:(nyears-1)){
  Fitted.h[i,1] <- mean(sesa$sims.list$phia[,i])
  lower.h[i,1] <- quantile(sesa$sims.list$phia[,i], 0.025)
  upper.h[i,1] <- quantile(sesa$sims.list$phia[,i], 0.975)
}

for (i in 1:(nyears-1)){
  Fitted.h[i,2] <- mean(sesa$sims.list$f[,i])
  lower.h[i,2] <- quantile(sesa$sims.list$f[,i], 0.025)
  upper.h[i,2] <- quantile(sesa$sims.list$f[,i], 0.975)
}

Fitted.i <- lower.i <- upper.i <- numeric()
for (i in 2:(nyears)){
  Fitted.i[i] <- mean(sesa$sims.list$I[,i])
  lower.i[i] <- quantile(sesa$sims.list$I[,i], 0.025)
  upper.i[i] <- quantile(sesa$sims.list$I[,i], 0.975)
}

# calculate some correlation coefficients
correl.h <- matrix(NA, ncol = 3, nrow = 60001)
for (i in 1:30000){
  correl.h[i,1] <- cor(sesa$sims.list$lambda[i,], sesa$sims.list$phia[i,])
  correl.h[i,2] <- cor(sesa$sims.list$lambda[i,], sesa$sims.list$I[i,2:18])
  correl.h[i,3] <- cor(sesa$sims.list$lambda[i,], sesa$sims.list$f[i,1:17])
  
}

# credible intervals of correlation coefficients
quantile(correl.h[,1], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,2], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,3], c(0.05, 0.5, 0.95), na.rm = TRUE)



# compute the posterior modes of correlation coefficients
m <- density(correl.h[,1], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,2], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,3], na.rm = TRUE)
m$x[which(m$y==max(m$y))]


# probability that correlation coefficients (r) > 0
sum(correl.h[!is.na(correl.h[,1]),1]>0)/30000
sum(correl.h[!is.na(correl.h[,2]),2]>0)/30000
sum(correl.h[!is.na(correl.h[,3]),3]>0)/30000



# Plot retrospective fig
#save plot
pdf("sesa.pgr.pdf", height=4, width=11)

par(mfrow = c(1,3), mar = c(4, 5, 1, 1), mgp=c(3, 1, 0), las = 1, cex = 1.2)
linecol <- alpha("#232757", 0.3)
plot(y = lambda.h, Fitted.h[,1], cex = 1.2, cex.lab = 1.2, lwd = 1.3,type = "n", xlim = c(0, 1), ylim = c(0, 4), ylab = "Population growth rate", xlab = "Adult survival", frame = FALSE, pch = 19)
segments(Fitted.h[,1], lam.lower.h, Fitted.h[,1], lam.upper.h, col = linecol, lwd = 1.3)
segments(lower.h[,1], lambda.h, upper.h[,1], lambda.h, col = linecol, lwd = 1.3)
points(y = lambda.h, Fitted.h[,1], pch = 19, col = "#232757", cex = 1.1)
text(x = -0.02, y = 3.5, "r = 0.30 (-0.08, 0.59)", pos = 4, font = 3, cex = 1.1)
text(x = -0.02, y = 3.2, "P(r>0) = 0.90", pos = 4, font = 3, cex = 1.1)
corner.label("(g)", font=2, cex=1.3, xoff=0.05)


par(mar = c(4, 3, 1, 1))
plot(y = lambda.h, Fitted.i[2:18],  cex = 1.2, cex.lab = 1.2, lwd = 1.3,type = "n", xlim = c(0, 40), ylim = c(0, 4),  ylab = "", xlab = "Adults gained", frame.plot = FALSE, pch = 19)
segments(Fitted.i[2:18], lam.lower.h, Fitted.i[2:18], lam.upper.h, col = linecol, lwd = 1.3)
segments(lower.i[2:18], lambda.h, upper.i[2:18], lambda.h, col = linecol, lwd = 1.3)
points(y = lambda.h, Fitted.i[2:18], pch = 19, col = "#232757", cex = 1.1)
text(x = 0, y = 3.5, "r = 0.71 (0.23, 0.83)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 3.2, "P(r>0) = 0.99", pos = 4, font = 3, cex = 1.1)
corner.label("(h)", font=2, cex=1.3, xoff=0.75)


par(mar = c(4, 3, 1, 1))
plot(y = lambda.h, Fitted.h[,2][1:17],  cex = 1.2, cex.lab = 1.2, lwd = 1.3,type = "n", xlim = c(0, 4), ylim = c(0, 4),  ylab = "", xlab = "Fecundity", frame.plot = FALSE, pch = 19)
segments(Fitted.h[,2][1:17], lam.lower.h, Fitted.h[,2][1:17], lam.upper.h, col = linecol, lwd = 1.3)
segments(lower.h[,2][1:17], lambda.h, upper.h[,2][1:17], lambda.h, col = linecol, lwd = 1.3)
points(y = lambda.h, Fitted.h[,2][1:17], pch = 19, col = "#232757", cex = 1.1)
text(x = 0, y = 3.5, "r = 0.46 (0.18, 0.63)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 3.2, "P(r>0) = 1.00", pos = 4, font = 3, cex = 1.1)
corner.label("(i)", font=2, cex=1.3, xoff=0.15)

dev.off()



#------------------------------------------------------------------------------------------
# Demographic synchrony - Fig 5
#------------------------------------------------------------------------------------------
# Synchrony among species demographic rates
# compare fec, imm, ad surv
# amgp - dunl
# amgp - sesa
# dunl - sesa


# examine correlations and plot
Fitted.h <- lower.h <- upper.h <- matrix(NA, nrow = nyears-1, ncol = 3)

for (i in 1:(nyears-1)){
  Fitted.h[i,1] <- mean(amgp$sims.list$phia[,i])
  lower.h[i,1] <- quantile(amgp$sims.list$phia[,i], 0.025)
  upper.h[i,1] <- quantile(amgp$sims.list$phia[,i], 0.975)
}
for (i in 1:(nyears-1)){
  Fitted.h[i,2] <- mean(dunl$sims.list$phia[,i])
  lower.h[i,2] <- quantile(dunl$sims.list$phia[,i], 0.025)
  upper.h[i,2] <- quantile(dunl$sims.list$phia[,i], 0.975)
}
for (i in 1:(nyears-1)){
  Fitted.h[i,3] <- mean(sesa$sims.list$phia[,i])
  lower.h[i,3] <- quantile(sesa$sims.list$phia[,i], 0.025)
  upper.h[i,3] <- quantile(sesa$sims.list$phia[,i], 0.975)
}

Fitted.af <- lower.af <- upper.af <- matrix(NA, nrow = 18, ncol = 3)
for (i in 1:18){
    Fitted.af[i,1]<- mean(amgp$sims.list$f[,i])
    lower.af[i,1] <- quantile(amgp$sims.list$f[,i], 0.025)
    upper.af[i,1] <- quantile(amgp$sims.list$f[,i], 0.975)
  }
for (i in 1:18){
    Fitted.af[i,2] <- mean(dunl$sims.list$f[,i])
    lower.af[i,2] <- quantile(dunl$sims.list$f[,i], 0.025)
    upper.af[i,2] <- quantile(dunl$sims.list$f[,i], 0.975)
  }
  
for (i in 1:18){
      Fitted.af[i,3] <- mean(sesa$sims.list$f[,i])
      lower.af[i,3] <- quantile(sesa$sims.list$f[,i], 0.025)
      upper.af[i,3] <- quantile(sesa$sims.list$f[,i], 0.975)
  }

Fitted.f <- lower.f <- upper.f <- matrix(NA, nrow = 18, ncol = 3)
for (i in 1:18){
  if (fox_high[i]>0){
  Fitted.f[i,1]<- mean(amgp$sims.list$f[,i])
  lower.f[i,1] <- quantile(amgp$sims.list$f[,i], 0.025)
  upper.f[i,1] <- quantile(amgp$sims.list$f[,i], 0.975)
  }
}
for (i in 1:18){
  if (fox_high[i]>0){
  Fitted.f[i,2] <- mean(dunl$sims.list$f[,i])
  lower.f[i,2] <- quantile(dunl$sims.list$f[,i], 0.025)
  upper.f[i,2] <- quantile(dunl$sims.list$f[,i], 0.975)
  }
}
  for (i in 1:18){
    if (fox_high[i]>0){
  Fitted.f[i,3] <- mean(sesa$sims.list$f[,i])
  lower.f[i,3] <- quantile(sesa$sims.list$f[,i], 0.025)
  upper.f[i,3] <- quantile(sesa$sims.list$f[,i], 0.975)
    }
  }

Fitted.f <- na.omit(Fitted.f)
lower.f <- na.omit(lower.f)
upper.f <- na.omit(upper.f)

Fitted.nf <- lower.nf <- upper.nf <- matrix(NA, nrow = 18, ncol = 3)
for (i in 1:18){
  if (fox_high[i]==0){
  Fitted.nf[i,1] <- mean(amgp$sims.list$f[,i])
  lower.nf[i,1] <- quantile(amgp$sims.list$f[,i], 0.025)
  upper.nf[i,1] <- quantile(amgp$sims.list$f[,i], 0.975)
  }
}
for (i in 1:18){
  if (fox_high[i]==0){
  Fitted.nf[i,2] <- mean(dunl$sims.list$f[,i])
  lower.nf[i,2] <- quantile(dunl$sims.list$f[,i], 0.025)
  upper.nf[i,2] <- quantile(dunl$sims.list$f[,i], 0.975)
  }
}
for (i in 1:18){
  if (fox_high[i]==0){
  Fitted.nf[i,3] <- mean(sesa$sims.list$f[,i])
  lower.nf[i,3] <- quantile(sesa$sims.list$f[,i], 0.025)
  upper.nf[i,3] <- quantile(sesa$sims.list$f[,i], 0.975)
  }
}
Fitted.nf <- na.omit(Fitted.nf)
lower.nf <- na.omit(lower.nf)
upper.nf <- na.omit(upper.nf)


Fitted.i <- lower.i <- upper.i <- matrix(NA, nrow = nyears, ncol = 3)
for (i in 2:(nyears)){
  Fitted.i[i,1] <- mean(amgp$sims.list$I[,i])
  lower.i[i,1] <- quantile(amgp$sims.list$I[,i], 0.025)
  upper.i[i,1] <- quantile(amgp$sims.list$I[,i], 0.975)
}
for (i in 2:(nyears)){
  Fitted.i[i,2] <- mean(dunl$sims.list$I[,i])
  lower.i[i,2] <- quantile(dunl$sims.list$I[,i], 0.025)
  upper.i[i,2] <- quantile(dunl$sims.list$I[,i], 0.975)
}
for (i in 2:(nyears)){
  Fitted.i[i,3] <- mean(sesa$sims.list$I[,i])
  lower.i[i,3] <- quantile(sesa$sims.list$I[,i], 0.025)
  upper.i[i,3] <- quantile(sesa$sims.list$I[,i], 0.975)
}

# calculate some correlation coefficients
correl.h <- matrix(NA, ncol = 12, nrow = 60001)
for (i in 1:30000){
  correl.h[i,1] <- cor(amgp$sims.list$phia[i,], dunl$sims.list$phia[i,])
  correl.h[i,2] <- cor(amgp$sims.list$phia[i,], sesa$sims.list$phia[i,])
  correl.h[i,3] <- cor(dunl$sims.list$phia[i,], sesa$sims.list$phia[i,])
  
  correl.h[i,4] <- cor(amgp$sims.list$f[i,c(1:4,6:12)], dunl$sims.list$f[i,c(1:4,6:12)])
  correl.h[i,5] <- cor(amgp$sims.list$f[i,c(1:4,6:12)], sesa$sims.list$f[i,c(1:4,6:12)])
  correl.h[i,6] <- cor(dunl$sims.list$f[i,c(1:4,6:12)], sesa$sims.list$f[i,c(1:4,6:12)])
  
  correl.h[i,7] <- cor(amgp$sims.list$f[i,c(5,13:18)], dunl$sims.list$f[i,c(5,13:18)])
  correl.h[i,8] <- cor(amgp$sims.list$f[i,c(5,13:18)], sesa$sims.list$f[i,c(5,13:18)])
  correl.h[i,9] <- cor(dunl$sims.list$f[i,c(5,13:18)], sesa$sims.list$f[i,c(5,13:18)])
  
  correl.h[i,10] <- cor(amgp$sims.list$f[i,], dunl$sims.list$f[i,])
  correl.h[i,11] <- cor(amgp$sims.list$f[i,], sesa$sims.list$f[i,])
  correl.h[i,12] <- cor(dunl$sims.list$f[i,], sesa$sims.list$f[i,])
  
}

correl.i <- matrix(NA, ncol = 3, nrow = 60001)
for (i in 1:30000){
  correl.i[i,1] <- cor(amgp$sims.list$I[i,2:18], dunl$sims.list$I[i,2:18])
  correl.i[i,2] <- cor(amgp$sims.list$I[i,2:18], sesa$sims.list$I[i,2:18])
  correl.i[i,3] <- cor(dunl$sims.list$I[i,2:18], sesa$sims.list$I[i,2:18])
}

# credible intervals of correlation coefficients
quantile(correl.h[,1], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,2], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,3], c(0.05, 0.5, 0.95), na.rm = TRUE)

quantile(correl.h[,4], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,5], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,6], c(0.05, 0.5, 0.95), na.rm = TRUE)

quantile(correl.h[,7], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,8], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,9], c(0.05, 0.5, 0.95), na.rm = TRUE)

quantile(correl.h[,10], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,11], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.h[,12], c(0.05, 0.5, 0.95), na.rm = TRUE)

quantile(correl.i[,1], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.i[,2], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.i[,3], c(0.05, 0.5, 0.95), na.rm = TRUE)

# compute the posterior modes of correlation coefficients
m <- density(correl.h[,1], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,2], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,3], na.rm = TRUE)
m$x[which(m$y==max(m$y))]

m <- density(correl.h[,4], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,5], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,6], na.rm = TRUE)
m$x[which(m$y==max(m$y))]

m <- density(correl.h[,7], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,8], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,9], na.rm = TRUE)
m$x[which(m$y==max(m$y))]

m <- density(correl.h[,10], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,11], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.h[,12], na.rm = TRUE)
m$x[which(m$y==max(m$y))]

m <- density(correl.i[,1], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.i[,2], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.i[,3], na.rm = TRUE)
m$x[which(m$y==max(m$y))]

# probability that correlation coefficients (r) > 0
sum(correl.h[!is.na(correl.h[,1]),1]>0)/30000
sum(correl.h[!is.na(correl.h[,2]),2]>0)/30000
sum(correl.h[!is.na(correl.h[,3]),3]>0)/30000

sum(correl.h[!is.na(correl.h[,4]),4]>0)/30000
sum(correl.h[!is.na(correl.h[,5]),5]>0)/30000
sum(correl.h[!is.na(correl.h[,6]),6]>0)/30000

sum(correl.h[!is.na(correl.h[,7]),7]>0)/30000
sum(correl.h[!is.na(correl.h[,8]),8]>0)/30000
sum(correl.h[!is.na(correl.h[,9]),9]>0)/30000

sum(correl.h[!is.na(correl.h[,10]),10]>0)/30000
sum(correl.h[!is.na(correl.h[,11]),11]>0)/30000
sum(correl.h[!is.na(correl.h[,12]),12]>0)/30000

sum(correl.i[!is.na(correl.i[,1]),1]>0)/30000
sum(correl.i[!is.na(correl.i[,2]),2]>0)/30000
sum(correl.i[!is.na(correl.i[,3]),3]>0)/30000

# Plot retrospective fig
#save plot
pdf("sync_phia.pdf", height=4, width=11)

#phia
# amgp - dunl
par(mfrow = c(1,3), mar = c(5, 4, 1, 1.2), mgp=c(3, 1, 0), las = 1, cex = 1.2, cex.lab = 1.2)
linecol <- alpha("#3B9CCE", 0.3)
plot(y = Fitted.h[,1], Fitted.h[,2],  cex = 1.1, lwd = 1.3,type = "n", xlim = c(0, 1), ylim = c(0, 1), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0 = Fitted.h[,1], lower.h[,2], y1 = Fitted.h[,1], upper.h[,2], col = linecol, lwd = 1.3)
segments(y0 = lower.h[,1], Fitted.h[,2], y1 = upper.h[,1], Fitted.h[,2], col = linecol, lwd = 1.3)
points(y = Fitted.h[,1], Fitted.h[,2], pch = 19, col = "#3B9CCE", cex = 1.1)
text(x = 0, y = .12, "r = 0.05 (-0.46, 0.53)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 0.02, "P(r>0) = 0.56", pos = 4, font = 3, cex = 1.1)
title(ylab = "American golden-plover", xlab = "Dunlin", line  = 2.2)
corner.label(label = "(d)", font = 2, cex = 1.3, xoff = 0.1)

#amgp - sesa
plot(y = Fitted.h[,1], Fitted.h[,3],  cex = 1.2, lwd = 1.3,type = "n", xlim = c(0, 1), ylim = c(0, 1), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0 = Fitted.h[,1], lower.h[,3], y1 = Fitted.h[,1], upper.h[,3], col = linecol, lwd = 1.3)
segments(y0 = lower.h[,1], Fitted.h[,3], y1 = upper.h[,1], Fitted.h[,3], col = linecol, lwd = 1.3)
points(Fitted.h[,3], Fitted.h[,1], pch = 19, col = "#3B9CCE", cex = 1.1)
text(x = 0, y = .12, "r = -0.51 (-0.82, 0.05)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 0.02, "P(r>0) = 0.07", pos = 4, font = 3, cex = 1.1)
title(ylab = "American golden-plover", xlab = "Semipalmated sandpiper", line  = 2.2)
corner.label(label = "(e)", font = 2, cex = 1.3, xoff = 0.1)

#dunl - sesa
plot(y = Fitted.h[,2], Fitted.h[,3],  cex = 1.2, lwd = 1.3,type = "n", xlim = c(0, 1), ylim = c(0, 1), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0 = Fitted.h[,2], lower.h[,3], y1 = Fitted.h[,2], upper.h[,3], col = linecol, lwd = 1.3)
segments(y0 = lower.h[,2], Fitted.h[,3], y1 = upper.h[,2], Fitted.h[,3], col = linecol, lwd = 1.3)
points(y = Fitted.h[,2], Fitted.h[,3], pch = 19, col = "#3B9CCE", cex = 1.1)
text(x = 0, y = .12, "r = 0.06 (-0.59, 0.58)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 0.02, "P(r>0) = 0.53", pos = 4, font = 3, cex = 1.1)
title(ylab = "Dunlin", xlab = "Semipalmated sandpiper", line  = 2.2)
corner.label(label = "(f)", font = 2, cex = 1.3, xoff = 0.1)

dev.off()

#imm
#save plot
pdf("sync_imm.pdf", height=4, width=11)
# amgp - dunl
par(mfrow = c(1,3), mar = c(5, 4, 1, 1), mgp=c(3, 1, 0), las = 1, cex = 1.2, cex.lab = 1.2, lwd = 1.3)
linecol <- alpha("#FFBF04", 0.3)
plot(y = Fitted.i[,1], Fitted.i[,2],  cex = 1.1, lwd = 1.3,type = "n", xlim = c(0, 30), ylim = c(0, 20), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0 = Fitted.i[,1], lower.i[,2], y1 = Fitted.i[,1], upper.i[,2], col = linecol, lwd = 1.3)
segments(y0 = lower.i[,1], Fitted.i[,2], y1 = upper.i[,1], Fitted.i[,2], col = linecol, lwd = 1.3)
points(Fitted.i[,2], Fitted.i[,1], pch = 19, col = "#FFBF04", cex = 1.1)
text(x = 0, y = 17.5, "r = 0.58 (0.17, 0.77)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 15.5, "P(r>0) = 0.99", pos = 4, font = 3, cex = 1.1)
title(ylab = "American golden-plover", xlab = "Dunlin", line  = 2)
corner.label(label = "(a)", font = 2, cex = 1.3, xoff = 1)

#amgp - sesa
plot(y = Fitted.i[,1], Fitted.i[,3],  cex = 1.2, lwd = 1.3,type = "n", xlim = c(0, 40), ylim = c(0, 20), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0=Fitted.i[,1], lower.i[,3], y1=Fitted.i[,1], upper.i[,3], col = linecol, lwd = 1.3)
segments(y0=lower.i[,1], Fitted.i[,3], y1=upper.i[,1], Fitted.i[,3], col = linecol, lwd = 1.3)
points(Fitted.i[,3], Fitted.i[,1], pch = 19, col = "#FFBF04", cex = 1.1)
text(x = 0, y = 17.5, "r = 0.77 (0.25, 0.88)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 15.5, "P(r>0) = 0.99", pos = 4, font = 3, cex = 1.1)
title(ylab = "American golden-plover", xlab = "Semipalmated sandpiper", line  = 2)

corner.label(label = "(b)", font = 2, cex = 1.3, xoff = 1)

#dunl - sesa
plot(y = Fitted.i[,2], Fitted.i[,3],  cex = 1.2, lwd = 1.3,type = "n", xlim = c(0, 40), ylim = c(0, 30), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0=Fitted.i[,2], lower.i[,3], y1=Fitted.i[,2], upper.i[,3], col = linecol, lwd = 1.3)
segments(y0=lower.i[,2], Fitted.i[,3], y1=upper.i[,2], Fitted.i[,3], col = linecol, lwd = 1.3)
points(Fitted.i[,3], Fitted.i[,2], pch = 19, col = "#FFBF04", cex = 1.1)
text(x = 0, y = 26, "r = 0.51 (-0.06, 0.71)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 23, "P(r>0) = 0.93", pos = 4, font = 3, cex = 1.1)
title(ylab = "Dunlin", xlab = "Semipalmated sandpiper", line  = 2)
corner.label(label = "(c)", font = 2, cex = 1.3, xoff = 1)

dev.off()

#fec

# Full fec time 
pdf("sync_fec.pdf", height=4, width=11)
# amgp - dunl
par(mfrow = c(1,3), mar = c(5, 4, 1, 1), mgp=c(3, 1, 0), las = 1, cex = 1.2, cex.lab = 1.2, lwd = 1.3)
linecol <- alpha("#007d4d", 0.3)
plot(y = Fitted.af[,1], Fitted.af[,2],  cex = 1.2, lwd = 1.3,type = "n", xlim = c(0, 4), ylim = c(0, 4), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0=Fitted.af[,1], lower.af[,2], y1=Fitted.af[,1], upper.af[,2], col = linecol, lwd = 1.3)
segments(y0=lower.af[,1], Fitted.af[,2], y1=upper.af[,1], Fitted.af[,2], col = linecol, lwd = 1.3)
points(Fitted.af[,2], Fitted.af[,1], pch = 19, col = "#007d4d", cex = 1.1)
text(x = 0, y = 3.5, "r = 0.95 (0.84, 0.99)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 3.1, "P(r>0) = 1.00", pos = 4, font = 3, cex = 1.1)
title(ylab = "American golden-plover", xlab = "Dunlin", line = 2)
corner.label(label = "(g)", font = 2, cex = 1.3, xoff = 0.1)


#amgp - sesa
plot(y = Fitted.af[,1], Fitted.af[,3],  cex = 1.2, lwd = 1.3,type = "n", xlim = c(0, 4), ylim = c(0, 4), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0=Fitted.af[,1], lower.af[,3], y1=Fitted.af[,1], upper.af[,3], col = linecol, lwd = 1.3)
segments(y0=lower.af[,1], Fitted.af[,3], y1=upper.af[,1], Fitted.af[,3], col = linecol, lwd = 1.3)
points(Fitted.af[,3], Fitted.af[,1], pch = 19, col = "#007d4d", cex = 1.1)
text(x = 0, y = 3.5, "r = 0.95 (0.83, 0.98)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 3.1, "P(r>0) = 1.00", pos = 4, font = 3, cex = 1.1)
title(ylab = "American golden-plover", xlab = "Semipalmated Sandpiper", line = 2)
corner.label(label = "(h)", font = 2, cex = 1.3, xoff = 0.1)


#dunl - sesa
plot(y = Fitted.af[,2], Fitted.af[,3],  cex = 1.2, lwd = 1.3, type = "n", xlim = c(0, 4), xlab = '', ylab = '', ylim = c(0, 4), frame = FALSE, pch = 19)
segments(y0=Fitted.af[,2], lower.af[,3], y1=Fitted.af[,2], upper.af[,3], col = linecol, lwd = 1.3)
segments(y0=lower.af[,2], Fitted.af[,3], y1=upper.af[,2], Fitted.af[,3], col = linecol, lwd = 1.3)
points(Fitted.af[,3], Fitted.af[,2], pch = 19, col = "#007d4d", cex = 1.1)
text(x = 0, y = 3.5, "r = 0.97 (0.89, 0.99)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 3.1, "P(r>0) = 0.81", pos = 4, font = 3, cex = 1.1)
title(ylab = "Dunlin", xlab = "Semipalmated sandpiper", line  = 2)
corner.label(label = "(i)", font = 2, cex = 1.3, xoff = 0.1)
dev.off()

#save plot
pdf("sync_fec_nofox.pdf", height=4, width=11)
# No fox control
# amgp - dunl
par(mfrow = c(1,3), mar = c(5, 4, 1, 1), mgp=c(3, 1, 0), las = 1, cex = 1.2, cex.lab = 1.2, lwd = 1.3)
linecol <- alpha("#249c72", 0.3)
plot(y = Fitted.nf[,1], Fitted.nf[,2],  cex = 1.1, lwd = 1.3,type = "n", xlim = c(0, 1.5), ylim = c(0, 1.5), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0=Fitted.nf[,1], lower.nf[,2], y1=Fitted.nf[,1], upper.nf[,2], col = linecol, lwd = 1.3)
segments(y0=lower.nf[,1], Fitted.nf[,2], y1=upper.nf[,1], Fitted.nf[,2], col = linecol, lwd = 1.3)
points(Fitted.nf[,2], Fitted.nf[,1], pch = 19, col = "#249c72", cex = 1.1)
text(x = 0, y = 1.3, "r = -0.96 (-1.00, 0.98)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 1.15, "P(r>0) = 0.50", pos = 4, font = 3, cex = 1.1)
title(ylab = "American golden-plover", xlab = "Dunlin", line  = 2)
corner.label(label = "(m)", font = 2, cex = 1.3, xoff = 0.1)


#amgp - sesa
plot(y = Fitted.nf[,1], Fitted.nf[,3],  cex = 1.1, lwd = 1.3,type = "n", xlim = c(0, 1.5), ylim = c(0, 1.5), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0=Fitted.nf[,1], lower.nf[,3], y1=Fitted.nf[,1], upper.nf[,3], col = linecol, lwd = 1.3)
segments(y0=lower.nf[,1], Fitted.nf[,3], y1=upper.nf[,1], Fitted.nf[,3], col = linecol, lwd = 1.3)
points(Fitted.nf[,3], Fitted.nf[,1], pch = 19, col = "#249c72", cex = 1.1)
text(x = 0, y = 1.3, "r = 0.94 (-0.99, 1.00)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 1.15, "P(r>0) = 0.55", pos = 4, font = 3, cex = 1.1)
title(ylab = "American golden-plover", xlab = "Semipalmated sandpiper", line  = 2)
corner.label(label = "(n)", font = 2, cex = 1.3, xoff = 0.1)


#dunl - sesa
plot(y = Fitted.nf[,2], Fitted.nf[,3],  cex = 1.1, lwd = 1.3, type = "n", xlim = c(0, 1.5), ylim = c(0, 1.5), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0=Fitted.nf[,2], lower.nf[,3], y1=Fitted.nf[,2], upper.nf[,3], col = linecol, lwd = 1.3)
segments(y0=lower.nf[,2], Fitted.nf[,3], y1=upper.nf[,2], Fitted.nf[,3], col = linecol, lwd = 1.3)
points(Fitted.nf[,3], Fitted.nf[,2], pch = 19, col = "#249c72", cex = 1.1)
text(x = 0, y = 1.3, "r = 0.99 (-0.58, 1.00)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 1.15, "P(r>0) = 0.92", pos = 4, font = 3, cex = 1.1)
title(ylab = "Dunlin", xlab = "Semipalmated sandpiper", line  = 2)
corner.label(label = "(o)", font = 2, cex = 1.3, xoff = 0.1)
dev.off()

# With fox control
pdf("sync_fec_fox.pdf", height=4, width=11)
# amgp - dunl
par(mfrow = c(1,3), mar = c(5, 4, 1, 1), mgp=c(3, 1, 0), las = 1, cex = 1.2, cex.lab = 1.2, lwd = 1.3)
linecol <- alpha("#014d4e", 0.3)
plot(y = Fitted.f[,1], Fitted.f[,2],  cex = 1.2, lwd = 1.3,type = "n", xlim = c(0, 4), ylim = c(0, 4), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0=Fitted.f[,1], lower.f[,2], y1=Fitted.f[,1], upper.f[,2], col = linecol, lwd = 1.3)
segments(y0=lower.f[,1], Fitted.f[,2], y1=upper.f[,1], Fitted.f[,2], col = linecol, lwd = 1.3)
points(Fitted.f[,2], Fitted.f[,1], pch = 19, col = "#014d4e", cex = 1.1)
text(x = 0, y = 0.5, "r = -0.96 (-1.00, 0.67)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 0.1, "P(r>0) = 0.14", pos = 4, font = 3, cex = 1.1)
title(ylab = "American golden-plover", xlab = "Dunlin", line  = 2)
corner.label(label = "(j)", font = 2, cex = 1.3, xoff = 0.1)


#amgp - sesa
plot(y = Fitted.f[,1], Fitted.f[,3],  cex = 1.2, lwd = 1.3,type = "n", xlim = c(0, 4), ylim = c(0, 4), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0=Fitted.f[,1], lower.f[,3], y1=Fitted.f[,1], upper.f[,3], col = linecol, lwd = 1.3)
segments(y0=lower.f[,1], Fitted.f[,3], y1=upper.f[,1], Fitted.f[,3], col = linecol, lwd = 1.3)
points(Fitted.f[,3], Fitted.f[,1], pch = 19, col = "#014d4e", cex = 1.1)
text(x = 0, y = 0.5, "r = 0.05 (-0.89, 0.94)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 0.1, "P(r>0) = 0.53", pos = 4, font = 3, cex = 1.1)
title(ylab = "American golden-plover", xlab = "Semipalmated sandpiper", line  = 2)
corner.label(label = "(k)", font = 2, cex = 1.3, xoff = 0.1)


#dunl - sesa
plot(y = Fitted.f[,2], Fitted.f[,3],  cex = 1.2, lwd = 1.3, type = "n", xlim = c(0, 4), ylim = c(0, 4), ylab = "", xlab = "", frame = FALSE, pch = 19)
segments(y0=Fitted.f[,2], lower.f[,3], y1=Fitted.f[,2], upper.f[,3], col = linecol, lwd = 1.3)
segments(y0=lower.f[,2], Fitted.f[,3], y1=upper.f[,2], Fitted.f[,3], col = linecol, lwd = 1.3)
points(Fitted.f[,3], Fitted.f[,2], pch = 19, col = "#014d4e", cex = 1.1)
text(x = 0, y = 0.5, "r = 0.94 (-0.40, 0.98)", pos = 4, font = 3, cex = 1.1)
text(x = 0, y = 0.1, "P(r>0) = 0.81", pos = 4, font = 3, cex = 1.1)
title(ylab = "Dunlin", xlab = "Semipalmated sandpiper", line  = 2)
corner.label(label = "(l)", font = 2, cex = 1.3, xoff = 0.1)
dev.off()


# ----------------------------------------------------------------------
# Summary stats for text
# ----------------------------------------------------------------------

# Average percent of population comprised of immigrants
mean(amgp$mean$I[2:18]/amgp$mean$Nad[2:18])
mean(dunl$mean$I[2:18]/dunl$mean$Nad[2:18])
mean(sesa$mean$I[2:18]/sesa$mean$Nad[2:18])
sesa$mean$I/sesa$mean$Nad

# Probability of betas < or > 0 
sum(dunl$sims.list$beta.gam2<0)/30000
sum(amgp$sims.list$beta.gam2<0)/30000

sum(amgp$sims.list$beta.phia1>0)/30000
sum(sesa$sims.list$beta.phia1<0)/30000

sum(dunl$sims.list$beta.phia2<0)/30000
sum(sesa$sims.list$beta.phia2<0)/30000

sum(dunl$sims.list$beta.fec1<0)/30000
sum(sesa$sims.list$beta.fec1<0)/30000
sum(dunl$sims.list$beta.fec2<0)/30000
sum(sesa$sims.list$beta.fec2<0)/30000



# calculate some correlation coefficients for fecundity and adult survival 
correl.phf <- matrix(NA, ncol = 3, nrow = 60001)
for (i in 1:30000){
  correl.phf[i,1] <- cor(amgp$sims.list$phia[i,], amgp$sims.list$f[i,1:19])
  correl.phf[i,2] <- cor(dunl$sims.list$phia[i,], dunl$sims.list$f[i,1:19])
  correl.phf[i,3] <- cor(sesa$sims.list$phia[i,], sesa$sims.list$f[i,1:19])
}


# credible intervals of correlation coefficients
quantile(correl.phf[,1], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.phf[,2], c(0.05, 0.5, 0.95), na.rm = TRUE)
quantile(correl.phf[,3], c(0.025, 0.5, 0.975), na.rm = TRUE)


# compute the posterior modes of correlation coefficients
m <- density(correl.phf[,1], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.phf[,2], na.rm = TRUE)
m$x[which(m$y==max(m$y))]
m <- density(correl.phf[,3], na.rm = TRUE)
m$x[which(m$y==max(m$y))]


# probability that correlation coefficients (r) > 0
sum(correl.h[!is.na(correl.h[,1]),1]>0)/30000
sum(correl.h[!is.na(correl.h[,2]),2]>0)/30000
sum(correl.h[!is.na(correl.h[,3]),3]>0)/30000


