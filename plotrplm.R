
plotrplm = function (rplm,id=1) {


	plot(rplm$linear.models[[id]]$data.extracted[,rplm$overview$ycols[id]]~rplm$linear.models[[id]]$data.extracted[,rplm$overview$xcols[id]],xlab=rplm$overview$xnames[id],ylab=rplm$overview$ynames[id])
		mtext(paste("adj. r sq.: ", round(rplm$linear.models[[id]]$linm.extracted$arsq,1), ";  m. r sq.: ", round(rplm$linear.models[[id]]$linm.extracted$rsq,1), ";  p: ", round(rplm$linear.models[[id]]$linm.extracted$pvalue,2), ";  cc spearman: ", round(rplm$linear.models[[id]]$linm.extracted$ccs,1), sep=""), 3, line=2.5)
		mtext(paste("F statistic: ", round(rplm$linear.models[[id]]$linm.extracted$fstat,2), " on ", round(rplm$linear.models[[id]]$linm.extracted$numdf,2), " and ", round(rplm$linear.models[[id]]$linm.extracted$dendf,2), " DF", sep=""), 3, line=1.3)
		if (rplm$linear.models[[id]]$linm.extracted$betaone<0)
			{
				mtext(paste("y = ", round(rplm$linear.models[[id]]$linm.extracted$betazero,2), "  ", round(rplm$linear.models[[id]]$linm.extracted$betaone,2), "x + e,  where e ~ N (0, ", round(rplm$linear.models[[id]]$linm.extracted$rserror,2), "^2)", sep=""), 3, line=0.2)
			} 
			else 
			{
				mtext(paste("y = ", round(rplm$linear.models[[id]]$linm.extracted$betazero,2), " + ", round(rplm$linear.models[[id]]$linm.extracted$betaone,2), "x + e,  where e ~ N (0, ", round(rplm$linear.models[[id]]$linm.extracted$rserror,2), "^2)", sep=""), 3, line=0.2)
			}
		abline(rplm$linear.models[[id]]$linm)
	
}

