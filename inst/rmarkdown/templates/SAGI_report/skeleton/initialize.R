library(knitr)
options(formatR.arrow=FALSE,xtable.comment=FALSE)
opts_chunk$set(fig.path='figure/Rplots-',fig.align='center',fig.show='hold',comment=NA,background='white',highlight=FALSE,tidy=TRUE,size="small",prompt=TRUE,continue=" ")
knit_hooks$set(source=function(x,options){
    prp <- c("R> ")
    if(!options$prompt) prp <- ""
    wd <- getOption("width")
    if(!is.null(width <- options$tidy.opts$width))
        options(width = width)
    x <- strwrap(x, width = getOption("width"))
    lenx <- length(x)
    pl <- unlist(sapply(gregexpr("\\(", x), function(el){
        if((length(el) == 1))
            if(unique(el) == -1) 0 else 1
        else length(el)}))
    pr <- unlist(sapply(gregexpr("\\)", x), function(el){
        if((length(el) == 1))
            if(unique(el) == -1) 0 else 1
        else length(el)}))
    wp <- rep(prp, length(x))
    if(length(x) > 1){
        xns <- gsub(" ","",x)
        op <- gregexpr("\\+|-|\\*|\\|=",x)
        ct <- sapply(1:(length(x) - 1), function(i, xns, op)
                     (nchar(x[i]) %in% op[[i]]) | (1 %in% op[[i + 1]]), xns,op)
        for(i in 2:length(x)){
            if((sum(pl[1:(i-1)]) != sum(pr[1:(i-1)])) | ct[i - 1])
                wp[i] <- paste(options$continue, "   ", sep = "")
        }
    }
    options(width = wd)
    paste(c("\\begin{Rinput}",paste(wp, x, sep= ""), "\\end{Rinput}",""), collapse = "\n")
},  output=function(x,options){
     if(all(gregexpr("begin\\{tabu|begin\\{longtab",x)[[1]] > 0)) x
     else paste(c("\\begin{Routput}\n",x, "\\end{Routput}\n"), sep = "")
 })
