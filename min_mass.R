#### T_c-age and T_c-rho_c diagrams 
#### Author: Earl Patrick Bellinger ( bellinger@phys.au.dk ) 
#### Stellar Astrophysics Centre Aarhus 

library(magicaxis)

dirs <- c('0.05', '0.06', '0.065', '0.07',
    '0.08', '0.09', '0.10', '0.11', '0.12')

DFs <- Map(function(dir) {
    DF <- read.table(file.path(dir, 'history.data'), header=1, skip=5)
    DF <- DF[DF$star_age <= 13.799*10**9,]
    DF
}, dirs)

blue <- "#0571b0"
red  <- "#CA0020"

### T_c vs. stellar age 
ylim <- c(5.6, 6.9)
xlim <- c(10**5, 8*10**10)

pdf('T_c.pdf', height=4, width=4*1.45)

par(mar=c(4.5, 5, 1, 1))

plot(NA, axes=F,
    xaxs='i', yaxs='i',
    xlim=log10(xlim), ylim=ylim,
    xlab=expression("Age"~tau/yr),
    ylab=expression("Core temperature"~T[c]/K))

for (DF in DFs) {
    lines(log10(DF$star_age), DF$log_cntr_T, lwd=1.5)
    burn <- (10**DF$log_Lnuc / 10**DF$log_L) > 0.9
    lines(log10(DF[burn,]$star_age), DF[burn,]$log_cntr_T, lwd=1.5, col=red)
    last <- DF[nrow(DF),]
    if (last$star_mass < 0.09 || last$star_mass == 0.1) 
        text(log10(last$star_age)-.05, last$log_cntr_T, 
            label=paste0(ifelse(last$star_mass == 0.1, 'M = ', ''), 
                         last$star_mass),
            pos=4)
}

legend('topleft', col=red, lwd=1.5, expression(L['nuc'] / L > 0.9), 
    bty='n', inset=c(0.01, 0.04))

abline(v=log10(13.799*10**9), lty=3, lwd=1.5, col=blue)

magaxis(1:4, labels=c(T,T,F,F), unlog='xy', las=1, lwd.ticks=1.5)
box(lwd=1.5)
dev.off()


### T_c vs. rho_c
ylim <- c(5.6, 6.9)
xlim <- c(-1.5, 3.8)

pdf('rho_c.pdf', height=4, width=4*1.45)

par(mar=c(4.5, 5, 1, 1))

plot(NA, axes=F,
    xaxs='i', yaxs='i', 
    xlim=xlim, ylim=ylim,
    xlab=expression("Core density"~rho/(g~cm^3)),
    ylab=expression("Core temperature"~T[c]/K))

burn_rho <- c()
burn_T <- c()
for (DF in DFs) {
    lines(DF$log_cntr_Rho, 
          DF$log_cntr_T, lwd=1.5)
    burn <- (10**DF$log_Lnuc / 10**DF$log_L) > 0.9
    if (any(burn)) {
        burn_rho <- c(burn_rho, DF[burn,]$log_cntr_Rho[1])
        burn_T   <- c(burn_T,   DF[burn,]$log_cntr_T[1])
        lines(DF[burn,]$log_cntr_Rho, 
              DF[burn,]$log_cntr_T, 
              lwd=1.5, col='red')
    }
}

rhos <- seq(-1.5, 3.8, 0.01)
lines(rhos, splinefun(burn_rho, burn_T, method='monoH.FC')(rhos), 
    lty=3, lwd=1.5, col=blue)

for (DF in DFs) {
    last <- DF[nrow(DF),]
    if (last$star_mass < 0.09 || last$star_mass == 0.1) 
        text(last$log_cntr_Rho-.05, last$log_cntr_T, 
            label=paste0(ifelse(last$star_mass == 0.1, 'M = ', ''), 
                         last$star_mass),
            pos=4)
}

legend('topleft', col=red, lwd=1.5, expression(L['nuc'] / L > 0.9), 
    bty='n', inset=c(0.01, 0.04))

magaxis(1:4, labels=c(T,T,F,F), unlog='xy', las=1, lwd.ticks=1.5)
box(lwd=1.5)
dev.off()
