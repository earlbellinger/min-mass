dirs <- c('0.05', '0.06', '0.07', '0.08', '0.09', '0.10', '0.11', '0.12')
DFs <- Map(function(dir) {
    read.table(file.path(dir, 'history.data'), header=1, skip=5)
}, dirs)

ylim <- c(5.6, 6.9)
xlim <- c(10**5, 7*10**10)

pdf('T_c.pdf', height=4, width=4*1.45)

par(mar=c(4.5, 5, 1, 1))

plot(NA, axes=F,
    xaxs='i', yaxs='i',
    xlim=log10(xlim), ylim=ylim,
    xlab=expression("Age"~tau/yr),
    ylab=expression("Core temperature"~T[c]/K))

for (DF in DFs) {
    print(names(DF))
    lines(log10(DF$star_age), DF$log_cntr_T, lwd=1.5)
    burn <- (10**DF$log_Lnuc / 10**DF$log_L) > 0.9
    lines(log10(DF[burn,]$star_age), DF[burn,]$log_cntr_T, lwd=1.5, col=red)
    last <- DF[nrow(DF),]
    if (last$star_mass < 0.09 || last$star_mass == 0.1) 
        text(log10(last$star_age)-.05, last$log_cntr_T, label=last$star_mass,
            pos=4)
}

abline(v=log10(13.799*10**9), lty=2, lwd=1.5)

magaxis(1:4, labels=c(T,T,F,F), unlog='xy', las=1, lwd.ticks=1.5)
box(lwd=1.5)
dev.off()
