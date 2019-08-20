
### Make the calibration plot - images

# Validate X-X
pdf(paste(filepath,"/calibrationXX.pdf", sep=""),width = 4, height = 4)
par(mar=c(5,5,4,2))
SpagGraph(cal.XX,500)
dev.off()

# Validate X-W
pdf(paste(filepath,"/calibrationXW.pdf", sep=""),width = 4, height = 4)
par(mar=c(5,5,4,2))
SpagGraph(cal.XW,500)
dev.off()

# Validate W-X
pdf(paste(filepath,"/calibrationWX.pdf", sep=""),width = 4, height = 4)
par(mar=c(5,5,4,2))
SpagGraph(cal.WX,500)
dev.off()

# Validata W-W
pdf(paste(filepath,"/calibrationWW.pdf", sep=""),width = 4, height = 4)
par(mar=c(5,5,4,2))
SpagGraph(cal.WW,500)
dev.off()

# Validate shrunken X-X
pdf(paste(filepath,"/calibrationXXshrink.pdf",sep=""),width = 4, height = 4)
par(mar=c(5,5,4,2))
SpagGraph(cal.XX.shrink,500)
dev.off()

# Validate shrunken X-W
pdf(paste(filepath,"/calibrationXWshrink.pdf",sep=""),width = 4, height = 4)
par(mar=c(5,5,4,2))
SpagGraph(cal.XW.shrink,500)
dev.off()

# Validate shrunken W-X
pdf(paste(filepath,"/calibrationWXshrink.pdf",sep=""),width = 4, height = 4)
par(mar=c(5,5,4,2))
SpagGraph(cal.WX.shrink,500)
dev.off()

# Validata shrunken W-W
pdf(paste(filepath,"/calibrationWWshrink.pdf",sep=""),width = 4, height = 4)
par(mar=c(5,5,4,2))
SpagGraph(cal.WW.shrink,500)
dev.off()