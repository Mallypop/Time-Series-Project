noseas.ts = ts(ecgtest$V2, 0, 162, deltat= 162/5400)

noseas.ts
end(noseas.ts)
start(noseas.ts)

par(mfrow=c(1,1))
plot.ts(noseas.ts)
plot(decompose(noseas.ts))


