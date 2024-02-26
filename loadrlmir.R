
loadrlmir = function (path, file) {

resulting.rlmir=readRDS(file=paste(path,file,".RDS",sep=""))

resulting.rlmir$input.raster=raster(paste(path,file,"_ir",sep=""))

resulting.rlmir$brick.of.randomizations=brick(paste(path,file,"_bor",sep=""))
	resulting.rlmir$brick.of.randomizations=setValues(resulting.rlmir$brick.of.randomizations,getValues(resulting.rlmir$brick.of.randomizations))
resulting.rlmir$brick.of.correlation.coefficient=brick(paste(path,file,"_bocc",sep=""))		
	resulting.rlmir$brick.of.correlation.coefficient=setValues(resulting.rlmir$brick.of.correlation.coefficient,getValues(resulting.rlmir$brick.of.correlation.coefficient))
resulting.rlmir$brick.of.lm.coefficient=brick(paste(path,file,"_bolc",sep=""))			
	resulting.rlmir$brick.of.lm.coefficient=setValues(resulting.rlmir$brick.of.lm.coefficient,getValues(resulting.rlmir$brick.of.lm.coefficient))
resulting.rlmir$brick.of.lm.Intercept=brick(paste(path,file,"_boli",sep=""))				
	resulting.rlmir$brick.of.lm.Intercept=setValues(resulting.rlmir$brick.of.lm.Intercept,getValues(resulting.rlmir$brick.of.lm.Intercept))
resulting.rlmir$brick.of.r.squared=brick(paste(path,file,"_bors",sep=""))				
	resulting.rlmir$brick.of.r.squared=setValues(resulting.rlmir$brick.of.r.squared,getValues(resulting.rlmir$brick.of.r.squared))
resulting.rlmir$brick.of.adjusted.r.squared=brick(paste(path,file,"_boars",sep=""))			
	resulting.rlmir$brick.of.adjusted.r.squared=setValues(resulting.rlmir$brick.of.adjusted.r.squared,getValues(resulting.rlmir$brick.of.adjusted.r.squared))
resulting.rlmir$brick.of.median.of.residuals=brick(paste(path,file,"_bomor",sep=""))			
	resulting.rlmir$brick.of.median.of.residuals=setValues(resulting.rlmir$brick.of.median.of.residuals,getValues(resulting.rlmir$brick.of.median.of.residuals))
resulting.rlmir$brick.of.mean.of.residuals=brick(paste(path,file,"_bomeor",sep=""))			
	resulting.rlmir$brick.of.mean.of.residuals=setValues(resulting.rlmir$brick.of.mean.of.residuals,getValues(resulting.rlmir$brick.of.mean.of.residuals))
resulting.rlmir$brick.of.sum.of.residuals=brick(paste(path,file,"_bosor",sep=""))			
	resulting.rlmir$brick.of.sum.of.residuals=setValues(resulting.rlmir$brick.of.sum.of.residuals,getValues(resulting.rlmir$brick.of.sum.of.residuals))
resulting.rlmir$brick.of.median.of.abs.of.residuals=brick(paste(path,file,"_bomoaor",sep=""))	
	resulting.rlmir$brick.of.median.of.abs.of.residuals=setValues(resulting.rlmir$brick.of.median.of.abs.of.residuals,getValues(resulting.rlmir$brick.of.median.of.abs.of.residuals))
resulting.rlmir$brick.of.mean.of.abs.of.residuals=brick(paste(path,file,"_bomeoaor",sep=""))		
	resulting.rlmir$brick.of.mean.of.abs.of.residuals=setValues(resulting.rlmir$brick.of.mean.of.abs.of.residuals,getValues(resulting.rlmir$brick.of.mean.of.abs.of.residuals))
resulting.rlmir$brick.of.sum.of.abs.of.residuals=brick(paste(path,file,"_bosoaor",sep=""))		
	resulting.rlmir$brick.of.sum.of.abs.of.residuals=setValues(resulting.rlmir$brick.of.sum.of.abs.of.residuals,getValues(resulting.rlmir$brick.of.sum.of.abs.of.residuals))
resulting.rlmir$brick.of.p.value=brick(paste(path,file,"_bopv",sep=""))					
	resulting.rlmir$brick.of.p.value=setValues(resulting.rlmir$brick.of.p.value,getValues(resulting.rlmir$brick.of.p.value))
resulting.rlmir$brick.of.standard.error=brick(paste(path,file,"_bose",sep=""))
	resulting.rlmir$brick.of.standard.error=setValues(resulting.rlmir$brick.of.standard.error,getValues(resulting.rlmir$brick.of.standard.error))

resulting.rlmir

}


