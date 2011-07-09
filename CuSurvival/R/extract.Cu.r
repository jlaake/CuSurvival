extract.Cu <-
function(file="cutagnew.mdb",dir="",begin=615,end=831)
{
# Attach database and fetch the tag and resight tables
	if(dir=="")dir=system.file(package="CIPinnipedAnalysis")
	xx=require(RODBC,quietly=TRUE)
 	fdir=file.path(dir,file)
	connection=odbcConnectAccess(fdir)
	Alive=sqlFetch(connection,"Cutag Live Resights")
	Alive$pupyear=as.numeric(as.POSIXlt(Alive$sitedate)$year)+1900		
	Tagged=sqlFetch(connection,"Cutags")
	Tagged$ID=as.character(Tagged$ID)
	Alive$ID=as.character(Alive$ID)
	mon=as.POSIXlt(Tagged$sitedate)$mon+1
# remove duplicates across years -- there shouldn't be any	
	Dups=sqlFetch(connection,"DuplicateTags0708")
	Tagged=merge(Tagged,subset(Dups,select=c("ID","cohort")),by="ID",all.x=TRUE)
	Tagged=subset(Tagged,subset=is.na(cohort.y))
	Tagged$cohort.y=NULL
	names(Tagged)[names(Tagged)=="cohort.x"]="cohort"
# Only use seals with known sex and weight and tagged in Sept or later and exclude those tagged on Castle Rock
	Tagged=subset(Tagged,subset=sex!="U" & weight>0 & mon>8 & area!="CAS")
# Tag number and type
	Tagged$numtags=2
	Tagged$rtagtype=as.character(Tagged$rtagtype)
	Tagged$ltagtype=as.character(Tagged$ltagtype)
	Tagged$rtagtype[is.na(Tagged$rtagtype)]=" "
	Tagged$ltagtype[is.na(Tagged$ltagtype)]=" "
	Tagged$numtags[Tagged$ltagtype==" " |Tagged$rtagtype==" "]=1
	Tagged$tagtype="PRT"
	Tagged$tagtype[(Tagged$rtagtype=="PRT" & Tagged$ltagtype=="SML") | (Tagged$ltagtype=="PRT" & Tagged$rtagtype=="SML") ]="PRTSML"
	Tagged$tagtype[(Tagged$rtagtype=="GRE")]="GRE"
	Tagged$tagtype[(Tagged$rtagtype=="SML" & (Tagged$ltagtype=="SML" | Tagged$ltagtype==" ")) | (Tagged$ltagtype=="SML" & (Tagged$rtagtype=="SML" | Tagged$rtagtype==" ")) ]="SML"
	Tagged$Tagtype=paste(Tagged$ltagtype,Tagged$rtagtype,sep="/")
# Restrict resights to specific time frame
	xx=Alive
	mday=as.numeric(as.POSIXlt(xx$sitedate)$mon+1)*100+as.POSIXlt(xx$sitedate)$mday
	LimitedResights=subset(xx,subset=mday<=end&mday>=begin)
	LimitedResights$repro=ifelse(LimitedResights$repstatus%in%c("Y","T","P","TWF","TWOF"),1,0)
# TagResightJoin
	TagResightJoin=merge(Tagged,LimitedResights,by="ID",all.x=TRUE)
# CaptureHistory
	resight.count.table=with(TagResightJoin,table(ID,pupyear))
	resight.count.table=cbind(matrix(0,ncol=3,nrow=nrow(resight.count.table)),resight.count.table)
	cohort.count.table=with(TagResightJoin,table(ID,cohort.x))
	resight.count.table=ifelse(resight.count.table<1,0,1)
	cohort.count.table=ifelse(cohort.count.table<=0,0,1)
	capture.history=cohort.count.table+resight.count.table
	capture.history[capture.history>1]=1
	xx=subset(Tagged,select=c("ID","sex","cohort","weight","tagtype","numtags"))
	CaptureHistory=as.data.frame(capture.history)
	CaptureHistory$ID=row.names(capture.history)
	CaptureHistory=merge(xx,CaptureHistory,by="ID",all.x=TRUE)
	capture.history=CaptureHistory[,-(1:6)]
	CaptureHistory$TotalTimesResighted=rowSums(capture.history)-1
	CaptureHistory$recap=ifelse(CaptureHistory$TotalTimesResighted>0,1,0)
# ReproCovariates
	xx=with(LimitedResights[LimitedResights$repro==1,],tapply(pupyear,ID,min))
	repro.table=data.frame(ID=names(xx),repro.year=as.numeric(xx))
	repro.table=with(repro.table,table(ID,repro.year))
	xx=as.matrix(repro.table)
	class(xx)="matrix"
#   Add dummy 0 values for years 1976-1990 when these data were not collected
	xx=cbind(data.frame(ID=row.names(repro.table)),xx)
	xx=merge(subset(Tagged,select="ID"),xx,all.x=TRUE,by="ID")
	xx[is.na(xx)]=0
	xx[,-1]=t(apply(xx[,-1],1,cumsum))
	for(names in as.character(1976:1990))
		xx[,names]=0
	ReproCovariates=xx
	names(ReproCovariates)[-1]=paste("repro",names(ReproCovariates)[-1],sep="")
# MarkData
	CaptureHistory$ch=apply(capture.history,1,paste,collapse="")
	td=as.data.frame(capture.history[-ncol(capture.history)])
	td=td-table(CaptureHistory$ID,CaptureHistory$cohort)
	names(td)=paste("td",as.numeric(names(td))+1,sep="")
	MarkData=subset(CaptureHistory,select=c("ch","cohort","sex","weight","ID","tagtype","numtags"))
	MarkData=cbind(MarkData,ReproCovariates[,-1],td,subset(CaptureHistory,select=c("recap","TotalTimesResighted")))
    MarkData$cohort=factor(MarkData$cohort)
	MarkData$numtags=factor(MarkData$numtags)
	MarkData$tagtype[MarkData$tagtype=="PRT"]="PRT2"
	MarkData$tagtype[MarkData$tagtype=="PRT2" & MarkData$numtags==1]="PRT1"
	MarkData$tagtype[MarkData$tagtype=="SML"]="SML2"
	MarkData$tagtype[MarkData$tagtype=="SML2" & MarkData$numtags==1]="SML1"
	MarkData$tagtype=factor(MarkData$tagtype)
	MarkData$tagtype=relevel(MarkData$tagtype,ref="PRT2")
	return(MarkData)
}

