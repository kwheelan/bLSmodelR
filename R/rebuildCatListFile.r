rebuildCatListFile <- function(C.Path,File=character(0),fromScratch=FALSE){
	
	Existing <- dir(C.Path,pattern="Cat_Zm.*_[0-9]{14}$")
	ExistingFull <- paste(C.Path,Existing,sep="/")
	CatfileOrig <- paste0(C.Path,"/.CatList")
	Catfile <- tempfile(paste0('CatList', sample(1e5, 1)))
	if(fromScratch || !file.exists(CatfileOrig)){
		if (file.exists(Catfile)) file.remove(Catfile)
	} else {
        file.copy(CatfileOrig, Catfile, overwrite = TRUE)
    }

	if(length(Existing) > 0){
		if(!file.exists(Catfile)){
			CatList <- as.data.frame(c(list(a=character(0)),rep(list(a=numeric(0)),13)),stringsAsFactors=FALSE)
			colnames(CatList) <- c("Name","N0","ZSens","Ustar","L","Zo","Su_Ustar","Sv_Ustar","bw","C0","kv","A","alpha","MaxFetch")
		} else {
			CatList <- read.table(Catfile,header=TRUE,as.is=TRUE,colClasses=c("character",rep("numeric",13)))
			# check erroneous
			CatList <- CatList[grepl("Cat_Zm.*_[0-9]{14}$",CatList[,1]),]
			# remove duplicates
			CatList <- CatList[!(CatList[,1] %in% unique(CatList[duplicated(CatList[,1]),1])),]
		}		
		CatNames <- CatList[,1] %w/o% File
		if(!all(CatNames%in%Existing)){
			CatList <- CatList[CatNames%in%Existing,]
			write.table(CatList,file=Catfile,row.names=FALSE,col.names=TRUE)
		}
		if(!all(exCat <- Existing%in%CatNames)){
			CatAdd <- data.frame(matrix(NA,nrow=sum(!exCat),ncol=ncol(CatList)),stringsAsFactors=FALSE)
			colnames(CatAdd) <- colnames(CatList)
			for(i in seq_along(ExCat <- ExistingFull[!exCat])){
				Cat <- try(readCatalog(ExCat[i]))
                if (inherits(Cat, 'try-error')) {
                    file.remove(ExCat[i])
                } else {
                    Head <- unlist(strsplit(attr(Cat,"header"),"\n"))[-1]
                    Whead <- matrix(as.numeric(gsub(".*[=] ","",Head)),nrow=1)
                    CatAdd[i,-1] <- Whead
                    CatAdd[i,1] <- basename(ExCat[i])
                }
			}
			CatList <- na.omit(rbind(CatList,CatAdd))
			write.table(CatList,file=Catfile,row.names=FALSE,col.names=TRUE)
		}
        file.copy(Catfile, CatfileOrig, overwrite = TRUE)
        file.remove(Catfile)
	} else {
		CatList <- as.data.frame(c(list(a=character(0)),rep(list(a=numeric(0)),13)),stringsAsFactors=FALSE)
		colnames(CatList) <- c("Name","N0","ZSens","Ustar","L","Zo","Su_Ustar","Sv_Ustar","bw","C0","kv","A","alpha","MaxFetch")
	}
	invisible(CatList)
}

