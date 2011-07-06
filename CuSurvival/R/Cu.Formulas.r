Cu.Formulas <-
function()
{
# capture probability formulas
	p=vector("list",6)
	p[[1]]=list(formula=~time+agep+sex+repro)
	p[[2]]=list(formula=~time+agep+sex+tagtype)
	p[[3]]=list(formula=~time:agep+sex+tagtype) 
	p[[4]]=list(formula=~time*sex+agep+tagtype)  
	p[[5]]=list(formula=~time+agep*sex) 
	p[[6]]=list(formula=~time+agep*sex+tagtype) 
# survival probability formulas
    Phi=vector("list",2)
	Phi[[1]]=list(formula=~young:time+ageS)
#	Phi[[2]]=list(formula=~young:time+ageS+young:weight)
	Phi[[2]]=list(formula=~young:time+sex*ageS)
#	Phi[[4]]=list(formula=~young:time+sex*ageS+young:weight)
    return(list(p=p,Phi=Phi))
}

