rebuildCatListFile <- function(cat_path, exclude_files = character(0), from_scratch = FALSE) {

    # list catalog files in dir
	existing_catalogs <- list.files(cat_path, pattern = "Cat_Zm.*_[0-9]{14}$")
    # .CatList file path
	catlist_file <- file.path(cat_path, '.CatList')
    # temporary .CatList file path
	temp_file <- tempfile(paste0('CatList', sample(1e5, 1)))

    # rebuild .CatList if any catalogs exist
	if(length(existing_catalogs) > 0){

        # copy existing file to temporary file OR remove existing temp_file
        if(from_scratch || !file.exists(catlist_file)){
            if (file.exists(temp_file)) file.remove(temp_file)
        } else {
            file.copy(catlist_file, temp_file, overwrite = TRUE)
        }

		if(!file.exists(temp_file)){
			CatList <- as.data.frame(c(list(a=character(0)),rep(list(a=numeric(0)),13)),stringsAsFactors=FALSE)
			colnames(CatList) <- c("Name","N0","ZSens","Ustar","L","Zo","Su_Ustar","Sv_Ustar","bw","C0","kv","A","alpha","MaxFetch")
		} else {
			CatList <- read.table(temp_file,header=TRUE,as.is=TRUE,colClasses=c("character",rep("numeric",13)))
			# check erroneous
			CatList <- CatList[grepl("Cat_Zm.*_[0-9]{14}$",CatList[,1]),]
			# remove duplicates
			CatList <- CatList[!(CatList[,1] %in% unique(CatList[duplicated(CatList[,1]),1])),]
		}		
		CatNames <- CatList[,1] %w/o% exclude_files
		if(!all(CatNames%in%existing_catalogs)){
			CatList <- CatList[CatNames%in%existing_catalogs,]
			write.table(CatList,file=temp_file,row.names=FALSE,col.names=TRUE)
		}
		if(!all(exCat <- existing_catalogs%in%CatNames)){
			CatAdd <- data.frame(matrix(NA,nrow=sum(!exCat),ncol=ncol(CatList)),stringsAsFactors=FALSE)
			colnames(CatAdd) <- colnames(CatList)
			for(i in seq_along(ExCat <- file.path(cat_path, existing_catalogs)[!exCat])){
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
			write.table(CatList,file=temp_file,row.names=FALSE,col.names=TRUE)
		}
        file.copy(temp_file, catlist_file, overwrite = TRUE)
        file.remove(temp_file)
	} else {
        # build empty CatList
		CatList <- as.data.frame(c(list(a=character(0)),rep(list(a=numeric(0)),13)),stringsAsFactors=FALSE)
		colnames(CatList) <- c("Name","N0","ZSens","Ustar","L","Zo","Su_Ustar","Sv_Ustar","bw","C0","kv","A","alpha","MaxFetch")
        # remove existing .CatList file
        if (file.exists(catlist_file)) file.remove(catlist_file)
	}
	invisible(CatList)
}

