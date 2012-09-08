#'Processes Cu SMI data for use with marked package

#' Creates list with processed data structure for marked package and the 
#' accompanying design data for the model. 
#'  
#' For process data, it uses default CJS model with a begin time of 1975 and uses sex and tagtype for group variables.
#' Creates default design data to create groupsAttaches directly to ACCESS database and extracts initial brand and all
#' resighting information.  Excludes any fur seals with missing sex or initial
#' weight. Also, excludes any not tagged in the fall or any from Castle Rock.  Restricts resightings to
#' those seen between \code{begin} and \code{end} inclusive of those dates.
#'
#' @export
#' @param cudata cu survival dataframe
#' @param model type of capture-recapture model
#' @return list with elements \item{cudata.proc}{processed data} \item{cu.ddl}{design data}
#' 
#' @author Jeff Laake
#' @examples 
#' zcdata=extract.Zc()
process.Cu.cjs <-function(cudata,model="cjs")
{
#   Process data using sex and tagtype to create groups and specify beginning time as 1975 for labelling
	cudata.proc=process.data(cudata,model=model,groups=c("sex","tagtype"),begin.time=1975)
#   Create design data and specify td (trap dependence) and repro (observed repro status)
	cu.ddl=make.design.data(cudata.proc,parameters=list(Phi=list(time.varying=c("td","repro")),p=list(time.varying=c("td","repro"))))
#   Specify 2010 and 1994 as intercepts levels for p and Phi respectively
	cu.ddl$p$time=relevel(cu.ddl$p$time,ref="2010")
	cu.ddl$Phi$time=relevel(cu.ddl$Phi$time,ref="1994")
#   Create male and female covariates
	cu.ddl$Phi$male=0
	cu.ddl$Phi$male[cu.ddl$Phi$sex=="M"]=1
	cu.ddl$Phi$female= 1-cu.ddl$Phi$male
	cu.ddl$p$male=0
	cu.ddl$p$male[cu.ddl$p$sex=="M"]=1
#   Add age bins for Phi and p and set intercept levels
	cu.ddl$Phi$ageS=cut(cu.ddl$Phi$Age,c(0,3,6,9,12,15,36),right=FALSE)
	levels(cu.ddl$Phi$ageS)=c("0-2","3-5","6-8","9-11","12-14","15+")
	cu.ddl$Phi$ageS=relevel(cu.ddl$Phi$ageS,ref="6-8")
	cu.ddl$p$agep=cut(cu.ddl$p$Age,c(1,3,6,9,37),right=FALSE)
	levels(cu.ddl$p$agep)=c("1-2","3-5","6-8","9plus")
	cu.ddl$p$agep=relevel(cu.ddl$p$agep,ref="6-8")
#   Create age covariates for Phi and p
	cu.ddl$Phi$young=0
	cu.ddl$Phi$young[cu.ddl$Phi$Age<3&cu.ddl$Phi$time!=1994]=1
	cu.ddl$p$twoplus = 0
	cu.ddl$p$twoplus[cu.ddl$p$Age>=2]= 1
#   Return list containing processed data list and design data list
	return(list(data.proc=cudata.proc,ddl=cu.ddl))
}
#' Define model set used in Cu survival analysis
#' 
#' Creates and returns a function that defines p and Phi model specifications for 
#' survival analysis.
#' 
#' @param cudata.proc processed dataframe for Cu analysis
#' @return function that can be used to select a subset of models to be
#' used in the analysis
#' 
#' @export define.models
#' 
#' @author Jeff Laake
define.models=function(cudata.proc)
{
	########################## Fixed parameters ##############################################
	#####################  Model specifications ###################################################
# p
	p.0=list(formula=~time+agep)
	p.1=list(formula=~time+agep+sex)
	p.2=list(formula=~time+agep+sex+tagtype)
	p.3=list(formula=~time+agep+sex+repro)
	p.4=list(formula=~time+agep*sex+tagtype)
	p.5=list(formula=~time+agep*sex+repro)
	p.6=list(formula=~time+agep*sex)
	p.7=list(formula=~time+agep*sex+repro+tagtype)
# Phi
	Phi.00=list(formula=~1)
	Phi.01=list(formula=~ageS)
	Phi.02=list(formula=~sex*ageS)
	Phi.03=list(formula=~young:time+sex*ageS)
	Phi.04=list(formula=~young:time+sex*ageS + young:weight)
	model.list=create.model.list(c("Phi","p"))
	models=function(x)
	{
		model.parameters=list()
		for(j in 1:length(x))
		{
			if(!is.list(eval(parse(text=x[j]),envir=environment(models))[[1]]))
				model.parameters[[names(x)[j]]]=eval(parse(text=(as.character(x[j]))),envir=environment(models))
		}
		for(j in 1:length(x))
		{
			if(is.list(eval(parse(text=x[j]),envir=environment(models))[[1]]))
				model.parameters=c(model.parameters,eval(parse(text=(as.character(x[j]))),envir=environment(models)))
		}
		model.name=paste(x,collapse=".")
		return(list(model.name=model.name,model.parameters=model.parameters))
		
	}
	return(models)
}
library(marked)
# extract data using code in package
cudata=extract.Cu()
# create processed data and design data list
cu.list=process.Cu.cjs(cudata)
# create function with set of models that you want to consider
crm_models=define.models(cu.list$data.proc)
# create the list of models that you want to run from the set in crm_models
model.list=as.matrix(expand.grid(p=paste("p",7,sep="."),Phi=paste("Phi",formatC(4,digits=1,flag="00"),sep=".")))
# run the set of models that you have selected; they are stored on disk
crm.wrapper(model.list,cu.list$data.proc,cu.list$ddl,crm_models,method=c("nlminb"))

