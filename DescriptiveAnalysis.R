

setwd('~/Github/MethodsDiversity')


################ APSA 2017 Participation
### Read in apsa 2017 data
apsa17<-read.csv("APSA2017.csv", header = TRUE, stringsAsFactors = F)[1:219,]

## Figure
pdf(file="apsaGender.pdf", height=4, width=6)
par(mar=c(2,2,2,2))
gendTab<-table(apsa17$Gender)
xx<-barplot(gendTab, names.arg=c("Men", "Women")
        , ylab="Number", col=c("darkred","darkblue"), ylim=c(0, 195))
gendTab/sum(gendTab)
text(xx, y=gendTab+10, c("N=177 (81%)" , "N=42 (19%)"))
## Note ugly hard coding of numbers in line above
dev.off()

################ APSA Section Data

# Read in data
sections<-read.csv("APSASections.csv", header = TRUE, stringsAsFactors = F)
## Re-order by rank
sections<-sections[order(sections$X..Female),]
sections<-sections[-44,] ### Removing the women& politics section as outlier


pdf(file="sections.pdf", height=4, width=7)
par(tcl=0)
par(mar=c(2,2,2,2))
plot(x, y=sections$X..Female, xlim=c(-8, 43), ylim=c(18,70), yaxt="n", xaxt="n")
abline(h=36.7, lty=2)
text(x-.6, y=sections$X..Female+.8, sections$Section, cex=.7, adj = 1, srt=320)
text(35, 35, "APSA Mean", cex=.8)
points(x[1], sections$X..Female[1], col="darkred", pch=19, cex=2)
mtext(seq(20, 70, by=10), at=seq(20, 70, by=10), side=2, line=0)
axis(side=2, labels = "Percent Women", at=40)
dev.off()


############# Polmeth
polmeth<-read.csv("Polmeth2017.csv", header = TRUE, stringsAsFactors = F)

gendTab<-table(polmeth$Gender)
womenAll<-(gendTab/sum(gendTab))[2]

gendTabFac<-table(polmeth$Gender[polmeth$X.2=="Faculty"])
womenFac<-(gendTabFac/sum(gendTabFac))[2]

gendTabGrad<-table(polmeth$Gender[polmeth$X.2=="Grad"])
womenGrad<-(gendTabGrad/sum(gendTabGrad))[2]

gendTabSpeak<-table(polmeth$Gender[polmeth$Speaker==1])
womenSpeak<-(gendTabSpeak/sum(gendTabSpeak))[2]

gendPosterFac<-table(polmeth$Gender[polmeth$Poster==1 & polmeth$X.2=="Faculty"])
womenFacPoster<-(gendPosterFac/sum(gendPosterFac))[2]

gendPosterGrad<-table(polmeth$Gender[polmeth$Poster==1 & polmeth$X.2=="Grad"])
womenGradPoster<-(gendPosterGrad/sum(gendPosterGrad))[2]


data<-c(womenAll, womenFac, womenGrad, womenSpeak, womenFacPoster, womenGradPoster)
names(data)<-c("Overall", "Faculty", "Grad.", "Main Program", "Facult/Poster", "Grad./Poster")
data<-rev(data)


par(mar=c(2,2,2,2), mgp=c(1,0,0))
dotchart(data, pch=19, xlab="Percent women", )




