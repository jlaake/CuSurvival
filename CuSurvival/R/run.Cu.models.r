run.Cu.models <-
function(test=FALSE,p.models,Phi.models,formulas,cudata.proc,cu.ddl,use.initial=FALSE,initial=NULL)
{
	for(i in 1:p.models)assign(paste("p",i,sep="."),formulas$p[[i]])
	for(i in 1:Phi.models)assign(paste("Phi",i,sep="."),formulas$Phi[[i]])
	cml=create.model.list("CJS")
	if(test)
		return(mark.wrapper(cml,data=cudata.proc,ddl=cu.ddl,run=FALSE))
	else
		return(mark.wrapper(cml,data=cudata.proc,ddl=cu.ddl,output=FALSE,invisible=FALSE,use.initial=use.initial,initial=initial,filename="cu_mark"))
}

