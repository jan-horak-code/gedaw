
saverlmir = function (rlmir, path, file) {

saveRDS(rlmir, file=paste(path,file,".RDS",sep=""))

writeRaster(rlmir$input.raster,filename=paste(path,file,"_ir",sep=""))

writeRaster(rlmir$stack.of.randomizations, filename=paste(path,file,"_sor",sep=""))			
writeRaster(rlmir$stack.of.correlation.coefficient, filename=paste(path,file,"_socc",sep=""))		
writeRaster(rlmir$stack.of.lm.coefficient, filename=paste(path,file,"_solc",sep=""))			
writeRaster(rlmir$stack.of.lm.Intercept, filename=paste(path,file,"_soli",sep=""))				
writeRaster(rlmir$stack.of.r.squared, filename=paste(path,file,"_sors",sep=""))				
writeRaster(rlmir$stack.of.adjusted.r.squared, filename=paste(path,file,"_soars",sep=""))			
writeRaster(rlmir$stack.of.median.of.residuals, filename=paste(path,file,"_somor",sep=""))			
writeRaster(rlmir$stack.of.mean.of.residuals, filename=paste(path,file,"_someor",sep=""))			
writeRaster(rlmir$stack.of.sum.of.residuals, filename=paste(path,file,"_sosor",sep=""))			
writeRaster(rlmir$stack.of.median.of.abs.of.residuals, filename=paste(path,file,"_somoaor",sep=""))	
writeRaster(rlmir$stack.of.mean.of.abs.of.residuals, filename=paste(path,file,"_someoaor",sep=""))		
writeRaster(rlmir$stack.of.sum.of.abs.of.residuals, filename=paste(path,file,"_sosoaor",sep=""))		
writeRaster(rlmir$stack.of.p.value, filename=paste(path,file,"_sopv",sep=""))					
writeRaster(rlmir$stack.of.standard.error, filename=paste(path,file,"_sose",sep=""))	



}


