
loadrlmir = function (path, file) {

resulting.rlmir=readRDS(file=paste(path,file,".RDS",sep=""))

resulting.rlmir$input.raster=raster(paste(path,file,"_ir",sep=""))

resulting.rlmir$stack.of.randomizations=brick(paste(path,file,"_sor",sep=""))
	resulting.rlmir$stack.of.randomizations=setValues(resulting.rlmir$stack.of.randomizations,getValues(resulting.rlmir$stack.of.randomizations))
resulting.rlmir$stack.of.correlation.coefficient=brick(paste(path,file,"_socc",sep=""))		
	resulting.rlmir$stack.of.correlation.coefficient=setValues(resulting.rlmir$stack.of.correlation.coefficient,getValues(resulting.rlmir$stack.of.correlation.coefficient))
resulting.rlmir$stack.of.lm.coefficient=brick(paste(path,file,"_solc",sep=""))			
	resulting.rlmir$stack.of.lm.coefficient=setValues(resulting.rlmir$stack.of.lm.coefficient,getValues(resulting.rlmir$stack.of.lm.coefficient))
resulting.rlmir$stack.of.lm.Intercept=brick(paste(path,file,"_soli",sep=""))				
	resulting.rlmir$stack.of.lm.Intercept=setValues(resulting.rlmir$stack.of.lm.Intercept,getValues(resulting.rlmir$stack.of.lm.Intercept))
resulting.rlmir$stack.of.r.squared=brick(paste(path,file,"_sors",sep=""))				
	resulting.rlmir$stack.of.r.squared=setValues(resulting.rlmir$stack.of.r.squared,getValues(resulting.rlmir$stack.of.r.squared))
resulting.rlmir$stack.of.adjusted.r.squared=brick(paste(path,file,"_soars",sep=""))			
	resulting.rlmir$stack.of.adjusted.r.squared=setValues(resulting.rlmir$stack.of.adjusted.r.squared,getValues(resulting.rlmir$stack.of.adjusted.r.squared))
resulting.rlmir$stack.of.median.of.residuals=brick(paste(path,file,"_somor",sep=""))			
	resulting.rlmir$stack.of.median.of.residuals=setValues(resulting.rlmir$stack.of.median.of.residuals,getValues(resulting.rlmir$stack.of.median.of.residuals))
resulting.rlmir$stack.of.mean.of.residuals=brick(paste(path,file,"_someor",sep=""))			
	resulting.rlmir$stack.of.mean.of.residuals=setValues(resulting.rlmir$stack.of.mean.of.residuals,getValues(resulting.rlmir$stack.of.mean.of.residuals))
resulting.rlmir$stack.of.sum.of.residuals=brick(paste(path,file,"_sosor",sep=""))			
	resulting.rlmir$stack.of.sum.of.residuals=setValues(resulting.rlmir$stack.of.sum.of.residuals,getValues(resulting.rlmir$stack.of.sum.of.residuals))
resulting.rlmir$stack.of.median.of.abs.of.residuals=brick(paste(path,file,"_somoaor",sep=""))	
	resulting.rlmir$stack.of.median.of.abs.of.residuals=setValues(resulting.rlmir$stack.of.median.of.abs.of.residuals,getValues(resulting.rlmir$stack.of.median.of.abs.of.residuals))
resulting.rlmir$stack.of.mean.of.abs.of.residuals=brick(paste(path,file,"_someoaor",sep=""))		
	resulting.rlmir$stack.of.mean.of.abs.of.residuals=setValues(resulting.rlmir$stack.of.mean.of.abs.of.residuals,getValues(resulting.rlmir$stack.of.mean.of.abs.of.residuals))
resulting.rlmir$stack.of.sum.of.abs.of.residuals=brick(paste(path,file,"_sosoaor",sep=""))		
	resulting.rlmir$stack.of.sum.of.abs.of.residuals=setValues(resulting.rlmir$stack.of.sum.of.abs.of.residuals,getValues(resulting.rlmir$stack.of.sum.of.abs.of.residuals))
resulting.rlmir$stack.of.p.value=brick(paste(path,file,"_sopv",sep=""))					
	resulting.rlmir$stack.of.p.value=setValues(resulting.rlmir$stack.of.p.value,getValues(resulting.rlmir$stack.of.p.value))
resulting.rlmir$stack.of.standard.error=brick(paste(path,file,"_sose",sep=""))
	resulting.rlmir$stack.of.standard.error=setValues(resulting.rlmir$stack.of.standard.error,getValues(resulting.rlmir$stack.of.standard.error))

resulting.rlmir

}


