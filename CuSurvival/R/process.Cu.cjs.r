process.Cu.cjs <-
function(model="CJS")
{
	cudata.proc=process.data(cudata,model=model,groups=c("sex","tagtype"),begin.time=1975)
	cu.ddl=make.design.data(cudata.proc,parameters=list(Phi=list(time.varying=c("td","repro")),p=list(time.varying=c("td","repro"))))
	cu.ddl$p$time=relevel(cu.ddl$p$time,ref="2010")
	cu.ddl$Phi$time=relevel(cu.ddl$Phi$time,ref="1994")
#
# Add pup/yearling/nonpup/twoplus fields to Phi design data; yearling effect excluded from 1989 which
# has the separate multi-year survival rates
#
	cu.ddl$Phi$pup = 0
	cu.ddl$Phi$yearling = 0
	cu.ddl$Phi$twoplus = 0
	cu.ddl$Phi$threeplus = 0
	cu.ddl$Phi$two = 0
	cu.ddl$Phi$pup[cu.ddl$Phi$age==0] = 1
	cu.ddl$Phi$nonpup = 1 - cu.ddl$Phi$pup
	cu.ddl$Phi$yearling[cu.ddl$Phi$age==1&cu.ddl$Phi$Time>=3]= 1
	cu.ddl$Phi$two[cu.ddl$Phi$age==2&cu.ddl$Phi$Time>=3]= 1
	cu.ddl$Phi$twoplus[cu.ddl$Phi$Age>=2]= 1
	cu.ddl$Phi$threeplus[cu.ddl$Phi$Age>=3]= 1
	cu.ddl$Phi$EN92=0
	cu.ddl$Phi$male=0
	cu.ddl$Phi$male[cu.ddl$Phi$sex=="M"]=1
	cu.ddl$Phi$female= 1-cu.ddl$Phi$male
	cu.ddl$Phi$adfemale= cu.ddl$Phi$female
	cu.ddl$Phi$adfemale[cu.ddl$Phi$Age<5]=0
	cu.ddl$Phi$EN97=0
	cu.ddl$Phi$EN97[cu.ddl$Phi$time==1997]=1
	cu.ddl$Phi$EN92[cu.ddl$Phi$time==1992]=1
	cu.ddl$Phi$EN03=0
	cu.ddl$Phi$EN03[cu.ddl$Phi$time==2003]=1
	cu.ddl$Phi$EN= cu.ddl$Phi$EN92+cu.ddl$Phi$EN97
#
# Add age bins for Phi and p
#
	cu.ddl$Phi$ageS=cut(cu.ddl$Phi$Age,c(0,5,8,11,15,36),right=FALSE)
	cu.ddl$p$agep=cut(cu.ddl$p$Age,c(1,3,6,9,37),right=FALSE)
	levels(cu.ddl$Phi$ageS)=c("0to4","5to7","8to10","11to14","15plus")
	cu.ddl$Phi$ageS=relevel(cu.ddl$Phi$ageS,ref="8to10")
	levels(cu.ddl$p$agep)=c("1to2","3to5","6to8","9plus")
	cu.ddl$p$agep=relevel(cu.ddl$p$agep,ref="6to8")
	cu.ddl$Phi$young=0
	cu.ddl$Phi$young[cu.ddl$Phi$Age<3]=1
	cu.ddl$p$twoplus = 0
	cu.ddl$p$twoplus[cu.ddl$p$Age>=2]= 1
#
#  Add a male covariate to p to adjust area1/area2 affects relative to area3
#
	cu.ddl$p$male=0
	cu.ddl$p$male[cu.ddl$p$sex=="M"]=1
	return(list(data.proc=cudata.proc,ddl=cu.ddl))
}

