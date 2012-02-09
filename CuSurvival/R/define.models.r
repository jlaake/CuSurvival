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
p.0=list(formula=~1)
p.1=list(formula=~time)
p.2=list(formula=~time+agep)
p.3=list(formula=~time+agep+sex)
p.4=list(formula=~time+agep+sex+tagtype)
p.5=list(formula=~time+agep+sex+repro)
p.6=list(formula=~time+agep*sex+tagtype)
p.7=list(formula=~time+agep*sex+repro)
p.8=list(formula=~time+agep*sex)
# Phi
Phi.00=list(formula=~1)
Phi.01=list(formula=~ageS)
Phi.02=list(formula=~sex*ageS)
Phi.03=list(formula=~young:time+sex*ageS))
Phi.04=list(formula=~young:time+sex*ageS + young:weight))
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





