
saverlmir = function (rlmir, path, file) {

saveRDS(rlmir, file=paste(path,file,".RDS",sep=""))

writeRaster(rlmir$input.raster,filename=paste(path,file,"_ir",sep=""))

writeRaster(rlmir$brick.of.randomizations, filename=paste(path,file,"_bor",sep=""))			
writeRaster(rlmir$brick.of.correlation.coefficient, filename=paste(path,file,"_bocc",sep=""))		
writeRaster(rlmir$brick.of.lm.coefficient, filename=paste(path,file,"_bolc",sep=""))			
writeRaster(rlmir$brick.of.lm.Intercept, filename=paste(path,file,"_boli",sep=""))				
writeRaster(rlmir$brick.of.r.squared, filename=paste(path,file,"_bors",sep=""))				
writeRaster(rlmir$brick.of.adjusted.r.squared, filename=paste(path,file,"_boars",sep=""))			
writeRaster(rlmir$brick.of.median.of.residuals, filename=paste(path,file,"_bomor",sep=""))			
writeRaster(rlmir$brick.of.mean.of.residuals, filename=paste(path,file,"_bomeor",sep=""))			
writeRaster(rlmir$brick.of.sum.of.residuals, filename=paste(path,file,"_bosor",sep=""))			
writeRaster(rlmir$brick.of.median.of.abs.of.residuals, filename=paste(path,file,"_bomoaor",sep=""))	
writeRaster(rlmir$brick.of.mean.of.abs.of.residuals, filename=paste(path,file,"_bomeoaor",sep=""))		
writeRaster(rlmir$brick.of.sum.of.abs.of.residuals, filename=paste(path,file,"_bosoaor",sep=""))		
writeRaster(rlmir$brick.of.p.value, filename=paste(path,file,"_bopv",sep=""))					
writeRaster(rlmir$brick.of.standard.error, filename=paste(path,file,"_bose",sep=""))	



}


