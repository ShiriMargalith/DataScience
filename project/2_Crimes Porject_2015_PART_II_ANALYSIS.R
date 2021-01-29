library(readr)
library(DBI)
library(dplyr)
library(odbc)
library(Hmisc)
library(devtools)
library(ggplot2)
library(corrgram)
library(tree)
library(rpart)
library(randomForest)
library(ranger)
library(xgboost)
library(class)
library(liquidSVM)
library(caret)
library(mlbench)
library(cocor)
library(dbscan)
library(naniar)
library(gridExtra)
library(class)
library(DMwR)
library(writexl)
library(VIM)

####################### MECHKAR ####################### 
### Mechkar part###
exploreData <- function(data=data, y=NULL, rn=NULL, factorSize=10, dir=tempdir(), debug=FALSE, ...) {
  
  whatVarType <- function(var) {
    suppressWarnings(if (var=="integer" | var=="numeric") {
      return(1)
    } else if (var=="factor" | var=="character") {
      return(2)
    } else if (var=="Date" | "POSIXct" %in% var[[1]]) {
      return(3)
    } else {
      return(0)
    })
  }
  
  drawHistogram <- function(imgname=imgname, x=x) {
    d=stats::density(x, kernel = "gaussian",na.rm=TRUE)
    breakstar=(max(x,na.rm=TRUE) -min(x,na.rm=TRUE))/d$bw
    h=graphics::hist(x, breaks=breakstar)
    graphics::plot(h,main="",xlab=imgname)
    yfit<-seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length=40)
    ffit<-stats::dnorm(yfit,mean=mean(x,na.rm=TRUE),sd=stats::sd(x,na.rm=TRUE))
    ffit <- ffit*diff(h$mids[1:2])*length(x)
    lines(yfit, ffit, col="blue", lwd=2)
  }
  
  drawFakeGraph <- function(imgname=imgname) {
    graphics::plot.window(xlim = c(0,0),ylim = c(0,0))
  }
  
  drawBars <- function(imgname=imgname, x=x) {
    graphics::plot(x)
  }
  
  drawGraphOne <- function(imgname=imgname, numVar=x, vartype=1) {
    if(vartype==1) {
      drawHistogram(imgname,numVar)
    } else if(vartype==2) {
      drawBars(imgname,numVar)
    } else {
      drawFakeGraph(imgname)
    }
  }
  
  getContinuousStats <- function(x) {
    N <- length(x)
    n <- length(x[which(is.na(x)==FALSE)])
    pct <- formatC(n/N * 100)
    nmiss <- length(x[which(is.na(x)==TRUE)])
    npct <- formatC(nmiss/N *100)
    ma <- mean(x, na.rm=TRUE)
    s <- stats::sd(x, na.rm=TRUE)
    me <- formatC(stats::median(x, na.rm=TRUE))
    q1 <- formatC(stats::quantile(x,1/4, na.rm=TRUE))
    q3 <- formatC(stats::quantile(x,3/4, na.rm=TRUE))
    mn <- formatC(min(x, na.rm=TRUE))
    mx <- formatC(max(x, na.rm=TRUE))
    html <- paste("<div class='Cell' style='align: top;'> <u>Data type</u>: Continuous <p> <u>Data length</u>: ",n ,"/", N, " (", pct, "%) <br> <u>Missing</u>: ",
                  nmiss, " (", npct, "%)<p> <u>Mean</u>: ", formatC(ma), "\t <u>StdDev</u>: ", formatC(s), "<br><u>Median</u>: ",me,
                  "\t <u>IQR</u>: ", q1, "-", q3, "<br><u>Min</u>: ", mn, "\t <u>Max</u>: ", mx, "</div>")
    return(html)
  }
  
  getCategortyStats <- function(x) {
    N <- length(x)
    n <- length(x[which(is.na(x)==FALSE)])
    pct <- formatC(n/N * 100)
    nmiss <- length(x[which(is.na(x)==TRUE)])
    npct <- formatC(nmiss/N *100)
    l <- levels(x)
    s <- summary(x)
    htm <- "<ul>"
    if (length(l) < 5) {
      for (lv in l) {
        htm <- paste(htm, "<li><u>", lv, "</u>: ", s[[lv]], "</li>")
      }
      htm <- paste(htm,"</ul>")
    }
    html <- paste("<div class='Cell'> <u>Data type</u>: Categorical Data <p> <u>Data length</u>: ",n, "/", N, " (", pct, "%) <br> <u>Missing</u>: ",
                  nmiss, " (", npct, "%) <p> <u>Number of levels</u>: ", length(l), "<br>", htm, "</div>")
    return(html)
  }
  
  getDatesStats <- function(x) {
    N <- length(x)
    n <- length(x[which(is.na(x)==FALSE)])
    pct <- formatC(n/N * 100)
    nmiss <- length(x[which(is.na(x)==TRUE)])
    npct <- formatC(nmiss/N *100)
    s <- summary(x)
    html <- paste("<div class='Cell'> <u>Data type</u>: Date <p> <u>Data length</u>: ",n, "/", N, " (", pct, "%) <br> <u>Missing</u>: ",
                  nmiss, " (", npct, "%) <p> <u>Min date</u>: ", min(x, na.rm=TRUE), "<br><u>Max date</u>:",max(x, na.rm=TRUE) , "</div>")
    return(html)
  }
  
  getStats <- function(numVar=x, vartype=1) {
    if(vartype==1) {
      html <- getContinuousStats(numVar)
    } else if(vartype==2) {
      html <- getCategortyStats(numVar)
    } else if (vartype==3) {
      html <- getDatesStats(numVar)
    } else {
      html <- "<div class='Cell'></div>"
    }
    return(html)
  }
  
  getOutliers <- function(x) {
    bp <- graphics::boxplot(x,plot=FALSE)
    return(bp$out)
  }
  
  getOutlierGraph <- function(x) {
    #  mod <- tryCatch({
    outl <- getOutliers(x)
    df <- data.frame(x=x, cl=1)
    if(length(outl)>0) {
      df$cl[which(df$x %in% outl)] <- 2
    }
    #pl <- stats::scatter.smooth(df$x,col=df$cl)
    pl <- tryCatch({
      stats::scatter.smooth(df$x,col=df$cl,xlab="index")
    }, warning = function(w) {
      suppressWarnings(w)
      #n <- "warning!"
    }, error = function(e) {
      n <- "error!"
    }, finally = {
      graphics::plot(df$x ~ row.names(df),col=df$cl,xlab="index")
    })
    ma <- mean(x, na.rm=TRUE)
    s <- stats::sd(x, na.rm=TRUE)
    graphics::abline(h=ma-(2*s), col="red", lty=2)
    graphics::abline(h=ma+(2*s), col="red", lty=2)
    #  }, error = function(e) {
    #    pl <- drawFakeGraph("none")
    #  })
    return(pl)
  }
  
  getScatterGraph <- function(df=data,x,y,dtype=1) {
    #  mod <- tryCatch({
    if(dtype==1) {
      pl <- ggplot2::ggplot(df) + ggplot2::geom_smooth(ggplot2::aes(x=data[[x]], y=data[[y]]), method="loess") + ggplot2::xlab(x) + ggplot2::ylab(y)
    } else {
      pl <- ggplot2::ggplot(df) + ggplot2::geom_boxplot(ggplot2::aes(y=data[[x]], color=data[[y]])) + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::labs(color=y)
    }
    return(pl)
  }
  
  getOutliersHtml <- function(imgname=imgname, x=x, srcdir=srcdir) {
    bp <- getOutliers(x)
    if (length(unique(bp)) > 10) {
      xtrm <- paste("There are ", length(unique(bp)), " outlier values")
    } else if (length(unique(bp)) == 0) {
      xtrm <- "No outlier values found"
    } else {
      xtrm <- paste(formatC(unique(bp)), collapse=', ' )
    }
    #imgsrc = paste(paste0(srcdir,"/fig/"),imgname, "_2.png",sep="")
    imgsrc = paste(paste0("fig/"),imgname, "_2.png",sep="")
    html <- paste0("<div class='Cell'><img class='origimg' src='",imgsrc,"' height='150' width='250'><br> <u>Outlier values</u>: <br> ", xtrm, "</div>")
    return(html)
  }
  ################## Prepare for the report ###################
  #report <- paste(mydir,"/report",sep="")
  
  ################## Check for values for rn ##################
  if(!is.null(rn)) {
    if(length(rn)!=ncol(data)) {
      message("the value of the 'rn' argument was avoided because it does not have the same number of columns of the dataframe")
      rn <- NULL
    }
    xname <- rn
    names(xname) <- names(data)
  } else {
    xname <- NULL
  }
  
  report <- dir
  if (!file.exists(report)) {
    dir.create(report)
  }
  fig <- paste(report,"/fig",sep="")
  if (!file.exists(fig)) {
    dir.create(fig)
  }
  srcdir <- report
  
  # determine which columns are integer
  int_col <- which(sapply(data, is.integer))
  int_col <- c(int_col,(which(sapply(data, is.numeric))))
  mi <- vector()
  # find only those integers with less than 10 unique values and convert to factor
  for (li in int_col) {
    if (length(unique(data[,li])) < factorSize) {
      mi <- c(mi,li)
      if (is.factor(data[,li]) == FALSE) {
        data[,li] <- factor(data[,li])
      }
    }
  }
  
  str_col <- which(sapply(data, is.character))
  mi <- vector()
  # find only those integers with less than 10 unique values and convert to factor
  for (li in str_col) {
    mi <- c(mi,li)
    data[,li] <- factor(data[,li])
  }
  
  # create the html report page
  myhtml <- paste(report,"/report.html",sep="")
  cat("<!DOCTYPE html>
      <html>
      <head>
      <title>Data Visualization</title>
      <meta http-equiv='Content-Type' content='text/html; charset=UTF-8' />
      <link rel='stylesheet' href='http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.css'>

      <script src='http://code.jquery.com/jquery-1.10.2.js'></script>
      <script>
      $(document).ready(function(){
      $('.onetoone').hide();
      });

      $(function() {
      $('.origimg').click(function(e) {
      $('#popup_img').attr('src',$(this).attr('src'));
      $('#myContainer').hide();
      var pos = $(document).scrollTop();
      $('#myContainer').css({'top':pos+20,'left':250, 'position':'absolute', 'border':'1px solid black', 'padding':'0px'});
      $('#myContainer').show();
      });
      $('#myContainer').click(function(e) {
      $('#myContainer').hide();
      });

      $('#myform2').submit(function(e) {
      e.preventDefault();
      });

      $('#onetoone').on('click',function() {
      console.log('onetone button - 1');
      $('#onetoone').hide();
      $('#aslist').show();
      // To show only individual rows:
      $('.Row').hide();
      $('.onetoone').show();
      // then we iterate
      var i = $('.Row').length;
      // Then we iterate
      var nxt = $('#idx').val();
      if (nxt < i & nxt >0) {
      $('.Row').hide();
      $('.Row').eq(0).show();
      $('.Row').eq(nxt).show();
      } else {
      $('#idx').val(1)
      }
      console.log('onetone button - 2');
      });

      $('#aslist').on('click',function() {
      console.log('aslist button - 1');
      $('#onetoone').show();
      $('#aslist').hide();
      $('.onetoone').hide();
      $('.Row').show();
      console.log('aslist button - 2');
      });

      $('#less').on('click',function(){
      console.log('less button - 1');
      var i = $('.Row').length;
      var nxt = parseInt($('#idx').val(),10) - 1;
      if (nxt < i & nxt >0) {
      $('#idx').val(nxt)
      $('.Row').hide();
      $('.Row').eq(0).show();
      $('.Row').eq(nxt).show();
      } else {
      $('#idx').val(1)
      }
      console.log('less button - 2');
      });

      $('#more').on('click',function(){
      console.log('more button - 1');
      var i = $('.Row').length;
      var nxt = parseInt($('#idx').val(),10) + 1;
      if (nxt < i & nxt >0) {
      $('#idx').val(nxt)
      $('.Row').hide();
      $('.Row').eq(0).show();
      $('.Row').eq(nxt).show();
      } else {
      $('#idx').val(i)
      }
      console.log('more button - 2');
      });

      $('#idx').on('change', function(){
      console.log('idx changed - 1');
      var i = $('.Row').length;
      var nxt = $('#idx').val();
      if (nxt < i & nxt >0) {
      $('#idx').val(nxt)
      $('.Row').hide();
      $('.Row').eq(0).show();
      $('.Row').eq(nxt).show();
      } else {
      $('#idx').val(i)
      }
      console.log('idx changed - 2');
      });
      });

      </script>

      <style type='text/css'>
      .Table
      {
      display: table;
      }
      .Title
      {
      display: table-caption;
      text-align: center;
      font-weight: bold;
      font-size: larger;
      background-color:#4C6F50;
      color: #fff;
      }
      .Row
      {
      display: table-row;
      }
      .Row:nth-child(even) {
        background-color: #56882433;
      }
      .Cell
      {
      display: table-cell;
      border: solid;
      border-width: thin;
      padding-left: 5px;
      padding-right: 5px;
      vertical-align: top;
      font-family: Arial, Helvetica, sans-serif;
      font-size: 14px;
      }
      </style>

      </head>

      <body>
      <div id='pageone' data-role='main' class='ui-content'>
      ", file = myhtml, sep='\n',append=FALSE)
  
  html <- paste("<p><p><h1> Data Visualization & Exploration </h1>
                <form>
                <input type='button' id='onetoone' value='Show as Cards'>
                <input type='button' id='aslist' class='onetoone' value='Show as List'>
                </form>
                <p>
                ")
  cat(html, file = myhtml, sep='\n', append=TRUE)
  # begin table
  alt1 <- ifelse(is.null(y)== TRUE, "", "<div class='Cell Title'> Dependent <br> Variable <br> Distribution </div>")
  html <- paste("<p><p>
                <div class='Table'>
                <div class='Row'>
                <div class='Cell Title'> Variable </div>
                <div class='Cell Title'> Distribution </div>
                <div class='Cell Title'> Descriptive <br> Statistics</div>
                <div class='Cell Title'> Outliers </div>"
                , alt1,
                "</div>")
  cat(html, file = myhtml, sep='\n', append=TRUE)
  
  #### determinate the type of each variable...
  data_types <- sapply(sapply(data, class), whatVarType)
  ln <- length(data)
  ii <- 0
  pb <- utils::txtProgressBar(min=0,max=ln,style=3)
  for(x in names(data)) {
    
    ## check if the value has at least more than one unique value...
    if(length(unique(data[[x]])) < 2) {
      message(paste("The variable",x,"has less than two unique values, so will not be included"))
    } else {
      
      if(debug==TRUE) {
        message(x)
      } else {
        pb <- utils::txtProgressBar(min=0,max=ln,style=3)
      }
      
      html <- paste("<div class='Row'><div class='Cell'><b>",x,"</b><p>",xname[x],"</p></div>")
      
      cat(html, file = myhtml, sep='\n', append=TRUE)
      #### initialize the first graph
      imgname = paste(fig,"/",x, "_1.png",sep="")
      #imgsrc = paste(paste0(srcdir,"/fig/"),x, "_1.png",sep="")
      imgsrc = paste("fig/",x, "_1.png",sep="")
      ### send the data with the type to generate the correct graph..
      grDevices::png(imgname)
      drawGraphOne(x, data[[x]], data_types[x])
      grDevices::dev.off()
      html <- paste0("<div class='Cell'><img class='origimg'  src='",imgsrc,"' height='150' width='150'><br></div>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
      
      # second, show the statistics
      html <- getStats(data[[x]],data_types[x])
      cat(html, file = myhtml, sep='\n', append=TRUE)
      
      # third, determine the outliers
      imgname = paste(fig,"/",x, "_2.png",sep="")
      if(data_types[x]==1) {
        grDevices::png(imgname)
        getOutlierGraph(data[[x]])
        grDevices::dev.off()
        html <- getOutliersHtml(x,data[[x]],srcdir)
      } else {
        html <- "<div class='Cell'></div>"
      }
      cat(html, file = myhtml, sep='\n', append=TRUE)
      
      # fourth, if y is assigned, make a corresponding plot
      if(is.null(y)==FALSE) {
        imgname = paste(fig,"/",x, "_3.png",sep="")
        #imgsrc = paste(paste0(srcdir,"/fig/"),x, "_3.png",sep="")
        imgsrc = paste("fig/",x, "_3.png",sep="")
        grDevices::png(imgname)
        ### scatter.smooth(data[[x]] ~ data[[y]])
        #suppressWarnings(getScatterGraph(data,x,y,data_types[y]))
        plot(getScatterGraph(data,x,y,data_types[y]))
        grDevices::dev.off()
        html <- paste0("<div class='Cell'><img class='origimg' src='",imgsrc,"' height='150' width='150'><br></div>")
        cat(html, file = myhtml, sep='\n', append=TRUE)
      }
      html <- paste("</div>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
      
      if(debug==FALSE) {
        utils::setTxtProgressBar(pb,ii)
        ii <- ii + 1
      }
    }
  }
  html <- paste("</div>")
  cat(html, file = myhtml, sep='\n', append=TRUE)
  # end table
  html <- paste("</div>
                <div data-role='popup' id='myContainer' style='display: none;'>
                <img id='popup_img' src='' />
                </div>
                </div>
                </div>
                </div>
                <p>
                <div class='onetoone'>
                <form id='myform2'>
                <span> <input type='button' id='less' value=' << '> </span>
                <span> <input id='idx' name='idx' value='1'></input></span>
                <span> <input type='button' id='more' value=' >> '> </span>
                </form>
                </div>
                <p>
                </body></html>
                ")
  cat(html, file = myhtml, sep='\n', append=TRUE)
  ## call the default browser or the one which is open (if any)
  browseURL(myhtml)
}

###################### END exploreData ###############



############################################################################
#####   TABLE 1                                                         ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-03-09                                       ####
#####   Last Modified: 2018-12-19                                       ####
############################################################################

####################  FUNCTIONS  ###########################################
#### Usage:
####   x: character vector with the name of the variables
####   y: the name of the strata variable (optional)
####   rn: character vector with the text we want to replace the variable names
####   data: the dataset to be used
####   miss: include missing statistics: [0=none, 1=only for categorical variables, 2=for all variables]
####   excel: export the table to excel [0=no, 1=yes]
####   excel_file: the name of the excel file we want to save the table (optional)
####
###################

Table1 <- function(x=NULL, y=NULL, rn=NULL, data=NULL, miss=3, catmiss=TRUE, formatted=TRUE, categorize=FALSE,
                   factorVars=NULL, maxcat=10, delzero=TRUE, decimals=1, messages=TRUE, excel=0, excel_file=NULL,
                   debug=FALSE) {
  ### define sub-functions
  Del <- NULL
  Pop <- NULL
  n <- NULL
  g1 <- function(var)c(Mean=mean(var,na.rm=TRUE), SD=stats::sd(var,na.rm=TRUE))
  g2 <- function(var)c(Median=stats::median(var,na.rm=TRUE), IQR=stats::quantile(var,c(0.25,0.75),na.rm=TRUE))
  msg <- NULL
  
  ### function for transforming variables to factors
  setFactors <- function(data=data, factorVars=factorVars, catmiss=catmiss, maxcat=maxcat) {
    if(is.null(factorVars)==TRUE) {
      aa <- sapply(sapply(data, unique), length)
      factorVars <- names(which(aa <= maxcat))
    }
    for (v in factorVars) {
      ct <- ifelse( ((is.null(factorVars)==FALSE & (v %in% factorVars)) | (is.null(factorVars)==TRUE & length(unique(data[[v]])) <= maxcat)),1,0)
      if (ct == 1) {
        data[[v]] <- factor(data[[v]])
        if(catmiss == TRUE & sum(is.na(data[[v]])==TRUE) > 0) {
          data[[v]] <- factor(data[[v]],levels=c(levels(data[[v]]),"Missing"))
          data[[v]][which(is.na(data[[v]])==TRUE)] <- "Missing"
        }
      }
    }
    return(data)
  }
  ### proceed to convert varibles to factors
  if (categorize == TRUE | is.null(factorVars)==FALSE ) {
    data <- setFactors(data, factorVars, catmiss, maxcat)
  }
  
  getSimpleTable  <- function(x=x, rn=rn, data=data, miss=miss, catmiss=catmiss,formatted=formatted,
                              categorize=categorize,maxcat=maxcat, delzero=delzero) {
    if (is.null(x)==TRUE) { x <- names(data)}
    if (is.null(rn)==TRUE) { rn <- x}
    ln <- length(x)
    pb <- utils::txtProgressBar(min=0,max=ln,style=3)
    msg <- NULL
    ### define the column names
    tableaaaa <- cbind(Del="Del",V1="Variables",V2="Categories",n="n","Population")
    tablebbbb <- cbind(Del="Del",V1="Variables",V2="Categories",n="n",val1="val1",val2="val2",val3="val3")
    tbl1 <- cbind(0,"Individuals","n",n=1, nrow(data))
    tbl2 <- cbind(0,"Individuals","n",n=1, nrow(data),NA,NA)
    tableaaaa <- rbind(tableaaaa,tbl1)
    tablebbbb <- rbind(tablebbbb,tbl2)
    q <- 1
    n <- 1
    ii <- 1
    for (v in x)
    {
      if (v %in% names(data)) {
        ### define if the actual variable has to be treated as numeric or factor
        ct <- ifelse(is.numeric(data[[v]])==TRUE & categorize==TRUE &
                       ((is.null(factorVars)==FALSE & (v %in% factorVars)) |
                          (is.null(factorVars)==TRUE & length(unique(data[[v]])) <= maxcat)),1,0)
        ### treat as numeric
        if (length(unique(data[v]))==0) {
          if (messages==TRUE) {
            msg <- c(msg, paste("The variable",v,"has no data... avoided"))
          }
        } else if (inherits(data[[v]], "Date")==TRUE) {
          if (messages==TRUE) {
            msg <- c(msg, paste("The variable",v,"is a date. Dates are not allowed in Table1... avoided"))
          }
        } else if (is.numeric(data[[v]])==TRUE & ct==0) {
          ## report mean and standard deviation
          t_n <- g1(data[[v]])
          tp <- paste(format(round(t_n[1],decimals),nsmall=1,big.mark=",")," (", format(round(t_n[2],decimals),nsmall=1,big.mark=","),")",sep="")
          tbl1 <- cbind(0,rn[q],"Mean (SD)",n=1, tp)
          tbl2 <- cbind(0,rn[q],"Mean (SD)",n=1,t_n[1],t_n[2],NA)
          tableaaaa <- rbind(tableaaaa,tbl1)
          tablebbbb <- rbind(tablebbbb,tbl2)
          ## report median and Interquartile ranges (25%,75%)
          t_n <- g2(data[[v]])
          tp <- paste(format(round(t_n[1],decimals),nsmall=1,big.mark=",")," (", format(round(t_n[2],decimals),nsmall=1,big.mark=","),"-", format(round(t_n[3],decimals),nsmall=1,big.mark=","), ")",sep="")
          tbl1 <- cbind(0,rn[q],"Median (IQR)",n=2, format(tp,big.mark=","))
          tbl2 <- cbind(0,rn[q],"Median (IQR)",n=2,t_n[1],t_n[2],t_n[3])
          tableaaaa <- rbind(tableaaaa,tbl1)
          tablebbbb <- rbind(tablebbbb,tbl2)
          ## report number and percent of missing
          if (miss >= 1) {
            datams <- subset(data,is.na(data[[v]])==TRUE)
            if (nrow(datams)>0) {
              data$cnt <- 1
              datams$cnt <- 1
              t_n <- table(data$cnt)
              t_m <- sum(datams$cnt)
              tp <- paste(format(t_m,big.mark=",")," (",format(round((t_m/t_n)*100,decimals),nsmall=1,big.mark=","),"%)",sep="")
              tbl1 <- cbind(0,rn[q],"Missing (%)",n=3, tp)
              tbl2 <- cbind(0,rn[q],"Missing (%)",n=3, t_m, (t_m/t_n)*100, NA)
            } else {
              tbl1 <- cbind(1,rn[q],"Missing (%)",n=3, " -- ")
              tbl2 <- cbind(1,rn[q],"Missing (%)",n=3, NA, NA, NA)
            }
            tableaaaa <- rbind(tableaaaa,tbl1)
            tablebbbb <- rbind(tablebbbb,tbl2)
          }
        } else {
          t_n <- table(data[[v]])
          ttotal <- sum(t_n)
          nm <- row.names(t_n)
          for (f in 1:length(nm)) {
            del1 <- ifelse(length(nm)==2 & (nm[f]=="No" | nm[f]=="no" | nm[f]==0 | nm[f]=="0" | nm[f]=="None" | nm[f]=="none"),1,0)
            tp <- t_n[f] / ttotal * 100
            pct <- paste(format(round(t_n[f],decimals),nsmall=0,big.mark=",")," (", format(round(tp,decimals),nsmall=1,big.mark=","), "%)",sep="")
            tbl1 <- cbind(del1,rn[q],nm[f],n=f, pct)             ########### delete rows 0/1 !!!!!!!!!
            tbl2 <- cbind(del1,rn[q],nm[f],n=f, t_n[f], tp, NA)  ########### delete rows 0/1 !!!!!!!!!
            tableaaaa <- rbind(tableaaaa,tbl1)
            tablebbbb <- rbind(tablebbbb,tbl2)
          }
          if (miss >= 2 & catmiss==FALSE ) {
            datams <- subset(data,is.na(data[[v]])==TRUE)
            if (nrow(datams)>0) {
              data$cnt <- 1
              datams$cnt <- 1
              t_n <- table(data$cnt)
              t_m <- sum(datams$cnt)
              tp <- paste(format(t_m,big.mark=",")," (",format(round((t_m/t_n)*100,decimals),nsmall=1,big.mark=","),"%)",sep="")
              tbl1 <- cbind(0,rn[q],"Missing (%)",n=f, tp)
              tbl2 <- cbind(0,rn[q],"Missing (%)",n=f, t_m, (t_m/t_n)*100, NA)
            } else {
              tbl1 <- cbind(1,rn[q],"Missing (%)",n=f, " -- ")
              tbl2 <- cbind(1,rn[q],"Missing (%)",n=f, NA, NA, NA)
            }
            tableaaaa <- rbind(tableaaaa,tbl1)
            tablebbbb <- rbind(tablebbbb,tbl2)
          }
        }
      } else {
        if (messages==TRUE) {
          msg <- c(msg, paste("The variable",v,"doesn't exists in the dataset... avoiding"))
        }
      }
      q <- q + 1
      if(debug==FALSE) {
        utils::setTxtProgressBar(pb,ii)
        ii <- ii + 1
      } else {
        message(v)
      }
    }
    if(formatted==TRUE) {
      return(tableaaaa)
    } else {
      return(tablebbbb)
    }
    close(pb)
  }
  
  pvals <- function(x=x,y=y,rn=rn,data=data,categorize=categorize,maxcat=maxcat) {
    ptab <- NULL
    if (is.null(y)==FALSE) {
      if (y %in% names(data)) {
        if (is.null(x)==TRUE) { x <- names(data)}
        if (is.null(rn)==TRUE | length(rn)<2) {rn <- x}
        q <- 1
        ptab <- cbind(V="Variables",pval="pval", n="n")
        
        ln <- length(x)
        ii <- 0
        pb <- utils::txtProgressBar(min=0,max=ln,style=3)
        
        for (v in x) {
          if (v %in% names(data)) {
            ct <- ifelse(is.numeric(data[[v]])==TRUE & categorize==TRUE & length(unique(data[[v]])) <= maxcat,1,0)
            if (is.numeric(data[[y]])==TRUE & categorize==TRUE & length(unique(data[[y]])) <= maxcat) {
              data[[y]] <- as.factor(data[[y]])
            } else if (is.numeric(data[[y]])==TRUE) {
              if (messages==TRUE) {
                msg <- c(msg, paste("The variable",y,"is not a factor. Please convert to factor or change the 'categorize' flag to TRUE."))
              }
              pval <- "Please rerun!!!"
            }
            if (is.numeric(data[[v]])==TRUE & length(unique(data[[v]])) > 1 & ct == 0) {
              ### first check for homoscedasticity
              tryCatch({
                if (stats::bartlett.test(data[[v]], data[[y]])[3] >= 0.05) {
                  pval <- suppressMessages(round(as.numeric(suppressMessages(car::Anova(stats::lm(data[[v]] ~ data[[y]])))[1, 4]), 3))
                } else {
                  pval <- suppressMessages(round(as.numeric(suppressMessages(car::Anova(stats::lm(data[[v]] ~ data[[y]]), white.adjust = TRUE))[1, 3]), 3))
                }
              }, warning = function(w) {
                suppressWarnings(w)
                #ww <- "suppress warnings..."
              }, error = function(e) {
                pval <- "---"
              })
            } else if (length(unique(data[[v]]))==1) {
              pval <- NA
            } else {
              if(length(unique(data[[v]])) < 15) {
                if (min(table(data[[v]],data[[y]])) > 5) {
                  pval <- round(as.numeric(stats::chisq.test(data[[v]],data[[y]])$p.val),3)
                } else {
                  if(min(table(data[[v]],data[[y]]))==0) {
                    #in cases where there are cells with zero, we use Fisher's exact test
                    tryCatch(
                      pval <- round(as.numeric(stats::fisher.test(data[[v]],data[[y]], workspace=1e9)$p.val),3),
                      error = function(e) {msg <- c(msg,paste0("Unable to calcualte the Fisher test for variables ",v," and ",y))})
                  } else {
                    pval <- round(as.numeric(stats::kruskal.test(data[[v]],data[[y]], workspace=1e9)$p.val),3)
                  }
                }
              } else {
                pval <- NA
              }
            }
            ptab <- rbind(ptab,cbind(V=rn[q],pval=pval,n=2))
          }
          if(debug==FALSE) {
            utils::setTxtProgressBar(pb,ii)
            ii <- ii + 1
          }
          q <- q + 1
        }
      }
    }
    return(ptab)
  }
  ####################### Begin analysis
  ##### check for x's witch have one unique values...get them out...
  vv <- NULL
  j <- 0
  jj <- NULL
  for(v in x) {
    if(length(unique(data[[v]])) < 2) {
      vv <- c(vv,v)
      j <- j + 1
      jj <- c(jj,j)
    }
  }
  warning(paste("The following variables have unique values and will not be included in the analysis:",vv))
  x <- setdiff(x, vv)
  if(is.null(rn)==FALSE & length(jj)>0) {
    rn <- rn[-jj]
  }
  
  ##### if y is null then make a simple table
  tabaaa1 <- getSimpleTable(x=x, rn=rn, data=data, miss=miss, catmiss=catmiss,formatted=formatted,categorize=categorize,maxcat=maxcat, delzero=delzero)
  tabaaa1 <- tibble::as_tibble(tabaaa1)
  ############################  CHANGE TO 5 !!!!!!!!!!!!!!
  if(length(tabaaa1) > 5) {
    names(tabaaa1) <- c("Del","V1","V2","n","Pop","pop2","pop3")
  } else {
    names(tabaaa1) <- c("Del","V1","V2","n","Pop")
  }
  ##### if y has two levels, then make a compound comparison
  if (is.null(y)==FALSE){
    if (y %in% names(data)) {
      if (is.factor(data[[y]])==FALSE) {
        if (length(levels(factor(data[[y]]))) > 8) {
          if (messages==TRUE) {
            message("The dependent variable has more than 8 levels, table too large!")
          }
        } else if(min(table(data[[y]]))==0) {
          message("The dependent variable has one or more levels with no individuals assigned!")
        } else {
          data[[y]] <- factor(data[[y]])
        }
      }
      if (length(levels(data[[y]])) >= 2) {
        for (lv in levels(data[[y]])) {
          dtsub <- subset(data, data[[y]]==lv)
          tab <- getSimpleTable(x=x, rn=rn, data=dtsub, miss=miss, catmiss=catmiss, formatted=formatted,categorize=categorize,maxcat=maxcat, delzero=delzero)
          tab <- data.frame(tab)
          ############################  CHANGE TO 5 !!!!!!!!!!!!!!
          if(length(tab) > 5) {
            names(tab) <- c("Del","V1","V2","n",paste0(lv,"_1"),paste0(lv,"_2"),paste0(lv,"_3"))
          } else {
            names(tab) <- c("Del","V1","V2","n",lv)
          }
          ############################  CHANGE TO 5 !!!!!!!!!!!!!!
          tab[1,5] <- lv
          tabaaa1 <- suppressMessages(dplyr::left_join(tabaaa1, tab))
        }
        # what to do with dichotomous variables? We remove the "Zero" label...
        # clean unnecesary rows
        if (delzero == TRUE) {
          tabaaa1 <- dplyr::filter(tabaaa1,Del==0)
        }
        ### calculate the p-value
        ptab <- data.frame(pvals(x=x,y=y,rn=rn,data=data,categorize=categorize,maxcat=maxcat))
        names(ptab) <- c("V1","pval","n")
        tabaaa1 <- suppressMessages(dplyr::left_join(tabaaa1, ptab))
        tabaaa1 <- dplyr::filter(tabaaa1,Pop != " -- ") #%>%
      }
    }
  }
  
  tabaaa1 <- dplyr::select(tabaaa1,-n)
  tabaaa1 <- dplyr::select(tabaaa1,-Del)
  
  ##### Join the tables...
  #Sys.setenv(JAVA_HOME="")
  if (excel==1) {
    #wb <- xlsx::createWorkbook()
    #sheet1 <- xlsx::createSheet(wb, sheetName="Table 1")
    #xlsx::addDataFrame(tabaaa1,sheet1)
    #### save and close the workbook
    #xlsx::saveWorkbook(wb, excel_file)
    writexl::write_xlsx(tabaaa1,excel_file)
    return(tabaaa1)
  } else {
    return(tabaaa1)
  }
}

########################## END Table1 ###############

############################################################################
#####   TEST & TRAIN DATASET GENERATION                                 ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-08-17                                       ####
############################################################################

train_test <- function(data=NULL,train_name=NULL,test_name=NULL,prop=NULL,seed=123,tableone=FALSE)
{
  pval <- NULL
  checkTrainTest <- function(train=NULL,test=NULL) {
    train[["traintest_ind_"]] <- 1
    test[["traintest_ind_"]] <- 2
    df <- rbind(train, test)
    tab <- Table1(data=df, y="traintest_ind_",messages = FALSE)
    vars <- subset(tab, pval < 0.05)$V1
    vars <- setdiff(vars,"traintest_ind_")
    if (length(vars)==0) {
      message(" ")
      message("You got a perfectly balanced training and test datasets")
      message(" ")
    } else {
      message("WARNING: The following variables are not balanced between the training and test datasets:")
      for (v in vars) { message(paste("*",v)) }
      message("You can try to change the seed value until you get a balanced partition.")
      message("Alternatively, you can ommit this warning and exclude those variables from your model")
      message(" ")
    }
    return(tab)
  }
  nm <- 1
  ttenv = as.environment(nm)
  ## set the seed to make your partition reproductible
  set.seed(seed)
  smp_size <- floor(prop * nrow(data))
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  assign(train_name, data[train_ind, ], envir=ttenv)
  assign(test_name, data[-train_ind, ], envir=ttenv)
  message(paste("Dataset partitioned into:"))
  message(paste(" + Train dataset:", train_name))
  message(paste(" + Test dataset:", test_name))
  if(tableone==TRUE) {
    tab = checkTrainTest(get(train_name),get(test_name))
    return(tab)
  }
}


######################### END train_test ###############


############################################################################
#####   TABLE 2                                                         ####
#####   Description: calculates the Odds/Hazard ratios and their        ####
#####     confidence intervals from a given model
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-03-09                                       ####
#####   Last Modified: 2018-04-16                                       ####
############################################################################

Table2 <- function(mod, rv=NULL,level=0.95, decimals=3) {
  alpha <- 1-level
  msm <- suppressMessages(summary(mod))
  if(rlang::has_name(msm,"coefficients")==TRUE) {
    msm <- msm$coefficients
  } else if(rlang::has_name(msm,"coef")==TRUE) {
    msm <- msm$coef
  }
  if("coxph" %in% class(mod)) {
    exp_coef <- msm[,1]
    dd <- suppressMessages(exp(stats::confint(mod, level=level)))
    dd1 <- round(dd[,1],decimals)
    dd2 <- round(dd[,2],decimals)
    p_value <- round(msm[,ncol(msm)],decimals)
  } else {
    ciz <- stats::qnorm(1-(alpha/2))
    exp_coef <- exp(msm[, 1])
    se_exp_coef <- msm[,2] * exp_coef
    dd1 <- round(exp_coef - ciz * se_exp_coef, decimals)
    dd2 <- round(exp_coef + ciz * se_exp_coef, decimals)
    exp_coef <- round(exp_coef, decimals)
    z<- abs((exp_coef-1)/se_exp_coef)
    p_value <- round(2*(1-stats::pnorm(z)), decimals)
  }
  tb <- data.frame(cbind(Estimate=exp_coef,'CI_lo'=dd1,'CI_hi'=dd2,'p value'=p_value))
  if (is.null(rv)==FALSE) {
    row.names(tb) <- rv
  }
  return(tb)
}

############################################################################
#####   TABLE2 WITH FORESTPLOT                                          ####
#####   Description: Generates a publication ready version of a model   ####
#####      risk table with a forestplot graph inside it                 ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2018-05-17                                       ####
############################################################################
Table2.forestplot <- function(mod, nr=NULL) {
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(opar))
  tryCatch({tbA <- Table2(mod)},
           error=function(cond) {
             message("This model type is not supported !")
             return(NA)
           })
  if(exists("tbA")) {
    ## max value for x axis
    xmax <- max(tbA[,3])
    if(xmax < 5) {
      #axis(1, seq(0,xmax,by=.5), cex.axis=.5)
      rh <- 12
    } else {
      rh <- 18
    }
    colnames(tbA) <- c("coef","ci_low","ci_high","p_value")
    j <- nrow(tbA)
    nm <- row.names(tbA)
    rowseq <- seq(nrow(tbA),1)
    graphics::par(mai=c(1,0,0,0),new=FALSE)
    graphics::plot(tbA$coef, rowseq, pch=15,
                   xlim=c(-10,rh), ylim=c(0,j+3),
                   xlab='', ylab='', yaxt='n', xaxt='n',
                   bty='n')
    for (i in 1:j) {
      graphics::abline(h=i-0.5,lwd=1, lty=3, col="gray")
    }
    graphics::par(new=TRUE)
    graphics::plot(tbA$coef, rowseq, pch=15,
                   xlim=c(-10,rh), ylim=c(0,j+3),
                   xlab='', ylab='', yaxt='n', xaxt='n',
                   bty='n')
    graphics::axis(1, seq(0,xmax,by=.5), cex.axis=.5)
    graphics::segments(1,-1,1,j, lty=3)
    graphics::segments(tbA$ci_low, rowseq, tbA$ci_hi, rowseq)
    graphics::mtext('Lower risk',1, line=2.5, at=0, cex=.5, font=2)
    graphics::mtext('Higher risk',1.5, line=2.5, at=2, cex=.5, font=2)
    if (is.null(nr)) {
      nr <- data.frame(vars=names(mod$coefficients))
      col2 <- as.character(gsub(x=nr$vars, pattern=paste(names(mod$xlevels),collapse="|"),replacement=" "))
      col3 <- data.frame(vars=as.character(NULL),col3=as.character(NULL))
      for(n in names(mod$xlevels)) {
        col3 <- rbind(col3, cbind(vars=paste(n,levels(mod$data[[n]])[2],sep=""),col3=n))
      }
      nr$col1 <- ifelse(nr$vars %nin% setdiff(names(mod$coefficients),names(mod$data)),as.character(nr$vars),'')
      nr$col2 <- ifelse(nr$vars %nin% col2, col2, " ")
      nr[1,"col1"] <- ifelse(nr[1,"vars"]=="(Intercept)","(Intercept)",nr[1,"vars"])
      nr$col1 <- ifelse(nr$vars %in% col3, col3, nr$col1)
      suppressWarnings(suppressMessages(nr <- dplyr::left_join(nr,col3)))
      nr$col1 <- ifelse(is.na(nr$col3)==TRUE,nr$col1,as.character(nr$col3))
      nr$col1 <- ifelse(grepl(":",nr$vars),nr$vars,nr$col1)
    } else {
      nr <- data.frame(vars=nr)
      if (length(nm)==nrow(nr)) {
        nr <- cbind(nm,nr)
        colnames(nr) <- c("vars","col1","col2")
      } else {
        return("The number of variables in the table of names (nr) you give is not equal to the number of variables in the model.
               Please check the names you entered in the table.")
      }
    }
    ### this part writes the variable titles
    graphics::text(-10,j+2, "Variables", cex=.75, font=2, pos=4)
    graphics::abline(h=j+1, col="gray", lwd=1.5, lty=1)
    graphics::text(-10,rowseq, nr[,2], cex=.75, pos=4, font=3)
    ### and this writes the categories for nominal variables
    graphics::text(-6,rowseq, nr[,3], cex=.75, pos=4)
    graphics::text(-3,j+2, "Odds Ratio (95% CI)", cex=.75, font=2, pos=4)
    t3 <- ifelse(!is.na(tbA$coef),
                 with(tbA, paste(format(coef,nsmall = 3,digits = 3),' (',format(ci_low,nsmall = 3,digits = 3),'-',format(ci_high,nsmall = 3,digits = 3),')',sep='')), '')
    graphics::text(xmax,rowseq, t3, cex=.75, pos=4, bg="lightgreen")
    graphics::text(xmax+5,j+2, "P Value", cex=.75, font=2, pos=4)
    t4 <- ifelse(!tbA$p_value==0, paste0(" ",format(tbA$p_value,nsmall = 3,digits = 3)), '<0.001')
    graphics::text(xmax+5,rowseq, t4, cex=.75, pos=4)
    graphics::box(which = "outer",col="darkgray",lwd=3)
  }
  graphics::par(mai=c(1,1,1,1),new=FALSE)
}

############################################################################
#####   CALCULATE CONFIDENCE INTERVALS FOR MEANS                        ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-08-24                                       ####
############################################################################
MeanCI <- function(x,round=3) {
  m <- mean(x,na.rm=TRUE)
  s <- stats::sd(x,na.rm=TRUE)
  ci <- 1.96 * (s/sqrt(length(x)))
  CImin <- m - ci
  CImax <- m + ci
  return(c(mean=round(m,round),CImin=round(CImin,round),CImax=round(CImax,round)))
}

############################################################################
#####   CALCULATE CONFIDENCE INTERVALS FOR PROPORTIONS                  ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-08-24                                       ####
############################################################################
PropCI <- function(x,round=3,multi=100,ref=2) {
  recode <- function(x,ref) {
    y <- x
    if (ref==2) {
      y[which(x==min(x))] <- 0
      y[which(x==max(x))] <- 1
    } else {
      y[which(x==max(x))] <- 0
      y[which(x==min(x))] <- 1
    }
    return(y)
  }
  if (is.factor(x)==TRUE && length(levels(x))==2) {
    p <- levels(x)[ref]
    y <- recode(as.numeric(x),ref)
  } else if (is.numeric(x)==TRUE && length(levels(factor(x)))==2) {
    p <- ifelse(ref==2,max(x),min(x))
    y <- recode(x,ref)
  } else if (length(levels(factor(x)))==2) {
    p <- levels(factor(x))[ref]
    y <- recode(as.numeric(factor(x)),ref)
  } else {
    return("The variable must be dichotomic")
  }
  freq <- (mean(y,na.rm=TRUE))
  CI <- 1.96 * sqrt((freq * (1-freq))/length(y))
  CImin <- (freq - CI)*multi
  CImax <- (freq + CI)*multi
  return(c(var=p,freq=round(freq*multi,round),CImin=round(CImin,round),CImax=round(CImax,round)))
}

############################################################################
#####   GENERATE A TABLE WITH VALIDITY TESTS                            ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-08-17                                       ####
############################################################################

################# Validity Test #########################################
#
#                    Observed
#                              +                  -
#          -----------------------------------------
# Predicted   +    TP                FP        |     PPV
#                                            a                   b         |  e (e1-e2)
#                                                                             |
#                                                             |
#                         -    FN       TN        |     NPV
#                                c        d         |  f (f1-f2)
#                      ----------------------------------------
#
#                        Sensitivity Specificity  |  Prevalence
#                          g (g1-g2)  h (h1-h2)   |  i (i1-i2)
#
#
#       Chi-square
#       Corrected Chi-square
#       Error: (FP+FN)/(TP+FP+FN+TN)
#       Accuracy: (TP+TN)/(TP+FP+FN+TN)
#       Precision: TP/(TP+FP)
#       Recall: TP/(TP+FN)
#
#       Harmonic mean of precision and recall (F1-Score):
#        f1-Score: 2 * (Precision * Recall)/(Precision + Recall)
#
#####################################################################

ValidityTest <- function (a, b, c, d, multi = 100, caption = "Validity of the Model/Screening")
{
  
  proportionCI <- function(p, n, multi = 100, prob = 0.95, dec = 2) {
    alpha <- ifelse(prob != 0.95,0.01,0.05)
    ci <- Hmisc::binconf(p,n,alpha=alpha,method="wilson")
    pci = paste(round(ci[1] * multi, dec), " (",
                round(ci[2] * multi, dec), "-",
                round(ci[3] * multi, dec), ")",
                sep = "")
    return(pci)
  }
  
  lr.ci <- function( a,b,c,d, sig.level=0.95 ) {
    ### Positive and negative likelihood ratios with their 95% CI...
    alpha <- 1 - sig.level
    spec <- d/(b+d)
    sens <- a/(a+c)
    lr.pos <- sens/(1 - spec)
    if ( a != 0 & b != 0 ) {
      sigma2 <- (1/a) - (1/(a+c)) + (1/b) - (1/(b+d))
      lower.pos <- lr.pos * exp(-stats::qnorm(1-(alpha/2))*sqrt(sigma2))
      upper.pos <- lr.pos * exp(stats::qnorm(1-(alpha/2))*sqrt(sigma2))
    } else if ( a == 0 & b == 0 ) {
      lower.pos <- 0
      upper.pos <- Inf
    } else if ( a == 0 & b != 0 ) {
      a.temp <- (1/2)
      spec.temp <- d/(b+d)
      sens.temp <- a.temp/(a+c)
      lr.pos.temp <- sens.temp/(1 - spec.temp)
      lower.pos <- 0
      sigma2 <- (1/a.temp) - (1/(a.temp+c)) + (1/b) - (1/(b+d))
      upper.pos <- lr.pos.temp * exp(stats::qnorm(1-(alpha/2))*sqrt(sigma2))
    } else if ( a != 0 & b == 0 ) {
      b.temp <- (1/2)
      spec.temp <- d/(b.temp+d)
      sens.temp <- a/(a+c)
      lr.pos.temp <- sens.temp/(1 - spec.temp)
      sigma2 <- (1/a) - (1/(a+c)) + (1/b.temp) - (1/(b.temp+d))
      lower.pos <- lr.pos.temp * exp(-stats::qnorm(1-(alpha/2))*sqrt(sigma2))
      upper.pos <- Inf
    } else if ( (a == (a+c)) & (b == (b+d)) ) {
      a.temp <- a - (1/2)
      b.temp <- b - (1/2)
      spec.temp <- d/(b.temp+d)
      sens.temp <- a.temp/(a+c)
      lr.pos.temp <- sens.temp/(1 - spec.temp)
      sigma2 <- (1/a.temp) - (1/(a.temp+c)) + (1/b.temp) - (1/(b.temp+d))
      lower.pos <- lr.pos.temp * exp(-stats::qnorm(1-(alpha/2))*sqrt(sigma2))
      upper.pos <- lr.pos.temp * exp(stats::qnorm(1-(alpha/2))*sqrt(sigma2))
    }
    lr.neg <- (1 - sens)/spec
    if ( c != 0 & d != 0 ) {
      sigma2 <- (1/c) - (1/(a+c)) + (1/d) - (1/(b+d))
      lower.neg <- lr.neg * exp(-stats::qnorm(1-(alpha/2))*sqrt(sigma2))
      upper.neg <- lr.neg * exp(stats::qnorm(1-(alpha/2))*sqrt(sigma2))
    } else if ( c == 0 & d == 0 ) {
      lower.neg<- 0
      upper.neg <- Inf
    } else if ( c == 0 & d != 0 ) {
      c.temp <- (1/2)
      spec.temp <- d/(b+d)
      sens.temp <- a/(a+c.temp)
      lr.neg.temp <- (1 - sens.temp)/spec.temp
      lower.neg <- 0
      sigma2 <- (1/c.temp) - (1/(a+c)) + (1/d) - (1/(b+d))
      upper.neg <- lr.neg.temp * exp(stats::qnorm(1-(alpha/2))*sqrt(sigma2))
    } else if ( c != 0 & d == 0 ) {
      d.temp <- (1/2)
      spec.temp <- d.temp/(b+d)
      sens.temp <- a/(a+c)
      lr.neg.temp <- (1 - sens.temp)/spec.temp
      sigma2 <- (1/c) - (1/(a+c)) + (1/d.temp) - (1/(b+d))
      lower.neg <- lr.neg.temp * exp(-stats::qnorm(1-(alpha/2))*sqrt(sigma2))
      upper.neg <- Inf
    } else if ( (c == (a+c)) & (d == (b+d)) ) {
      c.temp <- c - (1/2)
      d.temp <- d - (1/2)
      spec.temp <- d.temp/(b+d)
      sens.temp <- a/(a+c.temp)
      lr.neg.temp <- (1 - sens.temp)/spec.temp
      sigma2 <- (1/c.temp) - (1/(a+c)) + (1/d.temp) - (1/(b+d))
      lower.neg <- lr.neg.temp * exp(-stats::qnorm(1-(alpha/2))*sqrt(sigma2))
      upper.neg <- lr.neg.temp * exp(stats::qnorm(1-(alpha/2))*sqrt(sigma2))
    }
    list(
      lr.pos=lr.pos, lower.pos=lower.pos, upper.pos=upper.pos,
      lr.neg=lr.neg, lower.neg=lower.neg, upper.neg=upper.neg
    )
  }
  
  ppv <- proportionCI(a, a + b, multi)
  npv <- proportionCI(d, c + d, multi)
  sensit <- proportionCI(a, c + a, multi)
  specif <- proportionCI(d, b + d, multi)
  prev <- proportionCI(a + c, (a + b + c + d), multi)
  er <- proportionCI(b + c, (a + b + c + d), multi)
  acc <- proportionCI(a + d, (a + b + c + d), multi)
  prec <- proportionCI(a, (a + b), multi)
  recall <- proportionCI(a, (a + c), multi)
  f1 <- proportionCI(2 * ((a/(a + b)) * (a/(a + c))), ((a/(a + b)) + (a/(a + c))), multi)
  #Odds ratios
  odds <- ((a/c)/(b/d))
  oddsci <- 1.96 * sqrt((1/a)+(1/b)+(1/c)+(1/d))
  oddsratio <- paste(round(odds,2), " (", round(exp(log(odds)-oddsci),2), "-",round(exp(log(odds)+oddsci),2),")",sep="")
  #False positive rate = type I error= 1 - specificity
  fpr <- proportionCI(b, (d + b), multi)
  #False negative rate = type II error= 1 - sensitivity
  fnr <- proportionCI(c, (a + c), multi)
  #Likelihood ratio positive = sensitivity / (1 - specificity)
  # (a/(c+a) / b/(d+b))
  lr <- lr.ci(a,b,c,d,sig.level=0.95)
  plr1 <- paste(round(lr$lr.pos,2), " (", round(lr$lower.pos,2),"-",round(lr$upper.pos,2),")",sep="")
  #Likelihood ratio negative = (1 - sensitivity) / specificity
  # (c/(a+c) / d/(b+d))
  #nlr <- round((c*(b+d))/(d*(a+c)),2)
  #nlr <- ((c*(b+d))/(d*(a+c)))
  nlr1 <-  paste(round(lr$lr.neg,2), " (", round(lr$lower.neg,2),"-",round(lr$upper.neg,2),")",sep="")
  x <- matrix(c(a, b, c, d), byrow = TRUE, 2, 2)
  csq <- tryCatch({
    warning(stats::chisq.test(x))
  }, warning = function(w) {
    message("Using simulated p-value! - ", conditionMessage(w))
    stats::chisq.test(x, simulate.p.value = TRUE)
  })
  xsq <- round(csq$statistic, 2)
  pval <- round(csq$p.value, 2)
  vars <- cbind("", "Observed", "", "")
  vars <- rbind(vars, cbind("", "+", "-", ""))
  vars <- rbind(vars, cbind("Expected", "(TP)", "(FP)", "PPV"))
  vars <- rbind(vars, cbind("+", a, b, ppv))
  vars <- rbind(vars, cbind("", "(FN)", "(TN)", "NPV"))
  vars <- rbind(vars, cbind("-", c, d, npv))
  vars <- rbind(vars, cbind("", "Sensitivity", "Specificity",
                            "Prevalence"))
  vars <- rbind(vars, cbind("", sensit, specif, prev))
  vars <- rbind(vars, cbind("", "", "", ""))
  vars <- rbind(vars, cbind("Chi-square (p-value)", paste(xsq,
                                                          " (", pval, ")", sep = ""), "", ""))
  vars <- rbind(vars, cbind("Error", er, "", ""))
  vars <- rbind(vars, cbind("Accuracy", acc, "",
                            ""))
  vars <- rbind(vars, cbind("Precision", prec, "(Same as PPV)",
                            ""))
  vars <- rbind(vars, cbind("Recall", recall, "(Same as Sensitivity)", ""))
  vars <- rbind(vars, cbind("F1-Score", f1, "(Harmonic mean of",
                            "precision and recall)"))
  vars <- rbind(vars, cbind("Odds ratios", oddsratio, "", ""))
  vars <- rbind(vars, cbind("False positive rate", fpr, "(type I error)", ""))
  vars <- rbind(vars, cbind("False negative rate", fnr, "(type II error)", ""))
  vars <- rbind(vars, cbind("Positive Likelihood ratio", plr1, "", ""))
  vars <- rbind(vars, cbind("Negative Likelihood ratio", nlr1, "", ""))
  return(vars)
}

############################################################################
#####   Model Validity                                                  ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-12-01                                       ####
############################################################################
modelValidity <- function (data, model, class, train=FALSE, calib.graph=FALSE)
{
  if ("glm" %in% class(model) | "earth" %in% class(model)) {
    data$pred <- stats::predict(model, newdata = data, type = "response")
  }
  else {
    data$pred <- stats::predict(model, newdata = data, type = "prob")[,2]
  }
  data <- subset(data, is.na(data[["pred"]])==FALSE)
  roc1 <- pROC::roc(data[, class], as.numeric(data[["pred"]]))
  ### GiViTI calibration test
  if(train==FALSE) {
    src="external"
  } else {
    src="internal"
  }
  if(is.factor(data[,class])==TRUE) {data[,class] <- as.numeric(data[,class])-1}
  cb <- givitiR::givitiCalibrationBelt(o=data[,class],e=data[["pred"]],devel=src)
  if(calib.graph==TRUE) {
    graphics::plot(cb, main = "Model calibration", xlab = "Model predicted probability", ylab = "Observed outcome")
  }
  cb <- round(cb$p.value,3)
  ### Hoslem Lemeshow test
  hl <- ResourceSelection::hoslem.test(model$y, stats::fitted(model), g = 10)$p.value
  cm <- table(actual = data[, class], fitted = ifelse(data[["pred"]] >= 0.5, 1, 0))
  mmce <- 1 - (sum(diag(cm))/sum(cm))
  #d <- sjstats::cod(model)$cod
  vr <- MASS::stdres(model)
  if (is.factor(data[, class])==TRUE) {data[,class] <- as.numeric(data[, class])-1}
  acc <- ROSE::accuracy.meas(data[,class],data[["pred"]])
  srme <-sqrt((sum((data[, class] - data[["pred"]])^2,na.rm=TRUE))/nrow(data))
  vld <- cbind(auc = roc1$auc, cimin = pROC::ci(roc1)[1], cimax = pROC::ci(roc1)[3],
               SRME = srme,
               precision = acc$precision, recall = acc$recall, fscore = acc$F,
               NPV =  InformationValue::npv(data[, class], data[["pred"]]), mmce = mmce, Hosmer_Lemeshow = hl,GiViTI_calibration=cb)
  vld <- round(vld, 3)
  return(vld)
}

############################################################################
#####   Model Cutoffs                                                   ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2017-07-09                                       ####
############################################################################
getModelCutoffs <- function(pred, obs, div=10) {
  #pred <- dc <- NULL
  modValidity <- function(a,b,c,d,cutoff) {
    ppv <- (a/(a+b))*100
    npv <- (d/(c + d))*100
    sensit <- (a/(c + a))*100
    specif <- (d/(b + d))*100
    prev <- ((a + c)/(a + b + c + d))*100
    er <- ((b + c)/(a + b + c + d))*100
    acc <- ((a + d)/(a + b + c + d))*100
    prec <- (a/(a + b))*100
    recall <- (a/(a + c))*100
    lift <- ppv/prev
    f1 <- ((2 * ((a/(a + b)) * (a/(a + c))))/((a/(a + b)) + (a/(a + c))))*100
    return(cbind(cutoff=cutoff,TP=a,FP=b,FN=c,TN=d,sensitivity=sensit,specificity=specif,PPV=ppv,NPV=npv,accuracy=acc,error=er,prevalence=prev,lift=lift,precision=prec,recall=recall,F1_score=f1))
  }
  getQuintiles <- function(x,div=div) {
    cut(x, breaks=c(stats::quantile(x, probs = seq(0, 1, by = 1/div),na.rm = TRUE)),
        include.lowest=TRUE)
  }
  dc <- getQuintiles(pred,div=div)
  fl <- unique(levels(dc))
  res <- NULL
  for (i in 1:length(fl)) {
    # get the selected cutoff
    idx <- dc %nin% fl[i]
    pred1 <- rep(0,length(pred))
    pred1[idx] <- 1
    ####
    mn <- min(pred[dc==fl[i]])
    #tab1 <- table(obs,pred=ifelse(data[["pred"]] > mn,1,0))
    tab1 <- table(pred1,obs)
    res <- rbind(res, round(modValidity(tab1[4],tab1[3],tab1[2],tab1[1],mn),3))
  }
  return(res)
}

############################################################################
#####   AGE ADJUSTED RATES                                              ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2017-05-07                                       ####
############################################################################
age_adjusted <- function(dataset,outcome,age,agemin=0,agemax=130,source="who",alpha=0.05) {
  #utils::globalVariables(c("weight","n","weighted_pct","outcome1","wght","adj","res","pop"))
  weight <- n <- outcm1 <- wght <- pop <- adj <- res <- NULL
  ###### generate tables
  age_group <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                 "35-39","40-44","45-49","50-54","55-59","60-64",
                 "65-69","70-74","75-79","80-84","85-89","90-94",
                 "95-99","100+")
  
  age_min <- seq(0,100,5)
  age_max <- c(seq(4,99,5),130)
  who <- c(8860,8690,8600,8470,8220,7930,7610,7150,6590,6040,5370,
           4550,3720,2960,2210,1520,910,440,150,40,5)
  euro <- c(5000,5500,5500,5500,6000,6000,6500,7000,7000,7000,
            7000,6500,6000,5500,5000,4000,2500,1500,800,180,20)
  us <- c(20201362,20348657,20677194,22040343,21585999,21101849,
          19962099,20179642,20890964,22708591,22298125,19664805,
          16817924,12435263,9278166,7317795,5743327,3620459,
          1448366,371244,53364)
  age.adjust <- tibble::tibble(age_group, age_min, age_max, who, euro, us)
  weighted_pct <- function(dataset,outcome,age,source,agemin,agemax) {
    weighting <- age.adjust %>%
      dplyr::select_(~age_group, ~age_min, ~age_max, source)
    weighting <- weighting %>% dplyr::filter_(~age_min >= agemin, ~age_max <= agemax)
    ages <- tibble::tibble(age=seq(0,120,1))
    ages <- ages %>% dplyr::mutate(age_min = ifelse((age/10)-floor(age/10) < 0.5, floor(age/10)*10, (floor(age/10)*10)+5),
                                   age_max = ifelse((age/10)-floor(age/10) < 0.5, (floor(age/10)*10)+4, (floor(age/10)*10)+9))
    ages <- ages %>% dplyr::mutate(age_min=replace(age_min, age_min > 100,100),
                                   age_max=replace(age_max, age_max > 100, 130))
    ##### take the correct weighting
    tot <- sum(weighting[,source])
    weighting <- weighting %>%
      dplyr::mutate_(weight=source) %>%
      dplyr::mutate(weight=(weight/tot))
    weighting <- suppressMessages(dplyr::inner_join(weighting, ages))
    dataset[,"outcome"] <- ifelse(dataset[,outcome]==1,1,0)
    ### correct for age names to be able to do the joint
    dataset[,"age"] <- dataset[,age]
    unw <- (table(dataset[,"outcome"])/nrow(dataset))[2]
    d1 <- suppressMessages(dplyr::inner_join(dataset, weighting))
    #### we have yet calculated the weight.. use it!!!!
    d1 <- d1 %>%
      dplyr::select(age_group, weight, outcome)#, tot)
    d2 <- d1 %>%
      dplyr::group_by(age_group) %>%
      dplyr::select(age_group, weight, outcome) %>%
      dplyr::summarise(outcm1=sum(outcome),
                       wght=max(weight),
                       pop=n()) %>%
      dplyr::select(outcm1,wght,pop)
    d2$adj <- (d2$wght * d2$outcm1)/d2$pop
    wgt <- d2 %>%
      dplyr::summarise(res=sum(adj)) %>%
      dplyr::select(res) %>%
      as.numeric()
    return(cbind(unw,wgt))
  }
  t1 <- nrow(dataset)
  evnt1 <- table(dataset[,outcome])[2]
  res1 <- round(weighted_pct(dataset=dataset,outcome=outcome,age=age,source,agemin,agemax),5)
  res1 <- round(res1,4)
  unw1 <- round(Hmisc::binconf((t1*res1[1]),t1,alpha=alpha),4)
  wgt1 <- round(Hmisc::binconf((t1*res1[2]),t1,alpha=alpha),4)
  g1 <- list(Outcome=outcome,Population=t1,Events=evnt1,
             crude=list(rate=res1[1]*100.0,
                        CImin=unw1[2]*100.0,
                        CImax=unw1[3]*100.0),
             weighted=list(rate=res1[2]*100.0,
                           CImin=wgt1[2]*100.0,
                           CImax=wgt1[3]*100.0)
  )
  #g1 <- data.frame(Outcome=outcome,Population=t1,Events=evnt1,CrudeRate=res1[1]*100.0,
  #                 'CR[CImin]'=unw1[2]*100.0,'CR[CImax]'=unw1[3]*100.0,
  #                 WeightedRate=res1[2]*100.0,'WR0[CImin]'=wgt1[2]*100.0,
  #                 'WR[CImax]'=wgt1[3]*100.0)
  return(g1)
}

############################################################################
#####   GET THE MISSINGNESS OF A DATASET                                ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2017-05-07                                       ####
############################################################################
### search for the number & % of missinf
### then count the number of rows with complete data
getMissingness <- function(data, getRows=FALSE) {
  #utils::globalVariables(c("desc","na_count","na_cnt","rn","pred","dc"))
  desc <- na_count <- na_cnt <- rn <- pred <- dc <- NULL
  l <- nrow(data)
  vn <- names(data)
  ### copy the dataset and replace the NAs by 1 else 0
  nadf <- data
  cnt <- NULL
  miss <- function(x) return(sum(is.na(x) ))
  for(n in vn) {
    nadf[[n]] <- ifelse(is.na(nadf[[n]])==TRUE,1,0)
    cnt <- rbind(cnt, data.frame(n,sum(nadf[[n]])))
  }
  names(cnt) <- c("var","na_count")
  cnt$rate <- round((cnt$na_count / nrow(nadf))*100,1)
  ### now sum by column
  nadf$na_cnt <- 0
  nadf$na_cnt <- rowSums(nadf)
  ### order descending the count of mossings and leave only those with missings
  cnt <- cnt %>%
    dplyr::arrange(desc(na_count)) %>%
    dplyr::filter(na_count>0)
  #totmiss <- nadf %>% dplyr::filter(na_cnt==0) %>% dplyr::tally()
  totmiss <- nadf %>% dplyr::filter(na_cnt==0) %>% dplyr::summarise(n=n())
  idx <- NULL
  msg <- (paste("This dataset has ", as.character(totmiss), " (",as.character(round(totmiss/nrow(data)*100,1)),"%)" ," complete rows. Original data has ",nrow(data)," rows.",sep=""))
  ### check id needs to return the row indexes
  if(getRows==TRUE & totmiss != 0) {
    nadf$rn <- seq_len(nrow(data))
    idx <- nadf %>% dplyr::filter(na_cnt==0) %>% dplyr::select(rn)
  }
  message(list(head(cnt,n=10), msg))
  return(list(missingness=cnt, message=msg, rows=idx$rn))
}
####################### END MECHKAR ####################### 

####################### READ FILE ####################### 
Crimes_data_original <- read.csv("~/DataScience/project/Crimes_FF_2015.CSV")
summary(Crimes_data_original)
head(Crimes_data_original)
names(Crimes_data_original)

####################### SUPPORTING FUNCTIONS ####################### 
### OUTLIERMATRIX:
outlierMatrix <- function(data,threshold=1.5) {
  vn <- names(data)
  outdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    if(is.numeric(data[[v]])) {
      outlow <- quantile(data[[v]],probs = 0.25,na.rm = T) 
      outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
      irq_level <- (outhigh - outlow) * threshold
      outlow <- outlow - irq_level
      outhigh <- outhigh +  irq_level
      mv <- ifelse(data[[v]] < outlow | data[[v]] > outhigh, 1, 0)
      outdata[v] <- mv
    } else {
      mv <- rep(0,nrow(data))
    }
  }
  outdata$row1 <- NULL
  return(outdata)
}

### DROP_OUTLIERS:
drop_outliers <- function(data, data2){
  Q <- quantile(data, probs=c(.25, .75), na.rm = TRUE)
  iqr<-IQR(data, na.rm=TRUE)
  lower<-Q[1]-1.5*iqr
  higher<-Q[2]+1.5*iqr
  data2 <- data
  data2[data2 < lower | data2 > higher] <- NA
  return(data2)
}

### MISSINGMATRIX:
missingMatrix <- function(data) {
  vn <- names(data)
  missdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    mv <- ifelse(is.na(data[[v]]),1,0)
    missdata[v] <- mv
  }
  missdata$row1 <- NULL
  return(missdata)
}

### GETMISSINGNESS:
getMissingness <- function (data, getRows = FALSE) {
  require(dplyr)
  l <- nrow(data)
  vn <- names(data)
  nadf <- data
  cnt <- NULL
  miss <- function(x) return(sum(is.na(x)))
  for (n in vn) {
    nadf[[n]] <- ifelse(is.na(nadf[[n]]) == T, 1, 0)
    cnt <- rbind(cnt, data.frame(n, sum(nadf[[n]])))
  }
  names(cnt) <- c("var", "na.count")
  cnt$rate <- round((cnt$na.count/nrow(nadf)) * 100, 1)
  nadf$na.cnt <- 0
  nadf$na.cnt <- rowSums(nadf)
  cnt <- cnt %>% dplyr::arrange(desc(na.count)) %>% dplyr::filter(na.count > 
                                                                    0)
  totmiss <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::tally()
  idx <- NULL
  msg <- (paste("This dataset has ", as.character(totmiss), 
                " (", as.character(round(totmiss/nrow(data) * 100, 1)), 
                "%)", " complete rows. Original data has ", nrow(data), 
                " rows.", sep = ""))
  if (getRows == TRUE & totmiss != 0) {
    nadf$rn <- seq_len(nrow(data))
    idx <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::select(rn)
  }
  print(list(head(cnt, n = 10), msg))
  return(list(missingness = cnt, message = msg, rows = idx$rn))
}

### MAKEPLOT - WITH LA_CRIMES: 
makeplot <- function(data, title){
  plot(data, Crimes_data_full$LA_Crimes,pch=1, main=title)
  abline(lm(Crimes_data_full$LA_Crimes~data), col="red")
}

####################### ADJUST THE GENERAL TABLE - SET COLUMN TYPES TO NUMERIC, FACTOR AREA, NEIGHBORHOOD ####################### 
summary(Crimes_data_original$LA_Crimes)
summary(Crimes_data_original)
names(Crimes_data_original)
Cnames <- names(subset(Crimes_data_original, select = -c(ï..Area, Neighborhood)))
for (n in Cnames){
  Crimes_data_original[[n]] <- as.numeric(Crimes_data_original[[n]], na.rm = T)
}
Crimes_data_original$ï..Area <- factor(Crimes_data_original$ï..Area)
Crimes_data_original$Neighborhood <- factor(Crimes_data_original$Neighborhood)
summary(Crimes_data_original)
Crimes_data_original$drop <- 0
Crimes_data_original$drop[!is.na(Crimes_data_original$LA_Crimes)] <- 'no'
Crimes_data <- Crimes_data_original[Crimes_data_original$drop =='no',]
Crimes_data$drop<-NULL
summary(Crimes_data$LA_Crimes)

###### List the top 5 neighborhoods:
neighborhoods <- Crimes_data %>% group_by(Neighborhood) %>% summarise(LA_Crimes=sum(LA_Crimes))
neighborhoods <- neighborhoods %>% dplyr::arrange(desc(LA_Crimes))
neighborhoods

####################### EDA:
exploreData(Crimes_data)

####################### CHECK ROWS WITH A LOT OF MISSING VALUES ####################### 
## CREATE THE MATRIX AND CHECK MISSING AMOUNT:
Crimes_missing <- getMissingness(Crimes_data)
Crimes_missing$missingness
Crimes_missing$missingness$var
## SHOW MISSING MATRIX:
Crimes_na <- missingMatrix(Crimes_data)
options(repr.plot.width = 15, repr.plot.height = 80)
vis_miss(Crimes_data_original, warn_large_data = F)
## CHECK THE ROWS FOR MISSING %:
Crimes_na_sum <- Crimes_na
Crimes_na_sum$pct <- rowSums(Crimes_na)/ncol(Crimes_na)
Crimes_na_sum %>% group_by(pct) %>% tally
## REMOVE ROWS WITH OVER 30% MISSING:
Crimes_data$drop <- ifelse(Crimes_na_sum$pct >= 0.3, 1, 0)
table(Crimes_data$drop)
nrow(Crimes_data)
Crimes_data_no_missing_rows <- Crimes_data
Crimes_data_no_missing_rows <- Crimes_data_no_missing_rows %>% filter (Crimes_data_no_missing_rows$drop==0)
Crimes_data_no_missing_rows$drop <- NULL
## CHECKING THE RESULT:
nrow(Crimes_data_no_missing_rows)
Crimes_missing <- getMissingness(Crimes_data_no_missing_rows)
Crimes_missing$missingness
Crimes_missing$missingness$var
Crimes_na <- missingMatrix(Crimes_data_no_missing_rows)
options(repr.plot.width = 15, repr.plot.height = 80)
vis_miss(Crimes_data_no_missing_rows, warn_large_data = F)

exploreData(Crimes_data_no_missing_rows)

## CHECK IF MISSING AT RANDOM WITH KOLMOGOROV-SMIRNOV: IF P-VAL<0.05 THERE IS CHANGE. With (-): MISSING AT RANDOM.
## SINCE NO (+): THERE IS NO IMPACT OF THE MISSING AND WE CAN IMPUTATE ALL OF THEM.
miss_res <- NULL
for (m in Crimes_missing$missingness$var) {
  p <- list()
  for (n in Cnames) {
    if (n != m) {
      miss <- Crimes_data_no_missing_rows[[n]]
      non <- Crimes_data_no_missing_rows[[n]][which(Crimes_na[[n]]==0)]
      missnum <- length(miss) - length(non)
      pval <- suppressWarnings(ks.test(miss, non)$p.value)
      miss_res <- rbind(miss_res, cbind(var=n,missing=m, outliers_cnt=missnum, distribution_changed=ifelse(pval<0.05,"+","-")))
    }
  }
}
miss_res
miss_res <- data.frame(miss_res)
miss_res %>% filter(distribution_changed=="+")

### ANOTHER TEST WITH GLM: SAME CONSLUSION
miss_res2 <- NULL
for (m in Crimes_missing$missingness$var) {
  ff <- Crimes_data_no_missing_rows[,Cnames]
  ff[[m]] <- Crimes_na[[m]]
  mod <- glm(ff[[m]] ~.,data=ff,family="binomial")
  sm <- summary(mod)
  if(is.null(sm)==F) {
    sm2 <- data.frame(var=row.names(sm$coefficients),pvalue=sm$coefficients[,4])
    miss_res2 <- rbind(miss_res2, cbind(m,sm2))
  } else {
    print(sm)
  }
}
row.names(miss_res2) <- NULL
miss_res2
miss_res2 %>% filter(pvalue <0.05)


####################### CHECK THE OUTLIERS ####################### 
outmat <- outlierMatrix(Crimes_data_original)

## LIST OF ALL NUMERIC COLUMNS IN THE THREE TABLES:
numlist <- NULL
for(i in names(Crimes_data_original)) {
  if(is.numeric(Crimes_data_original[[i]])==TRUE) {
    numlist <- c(numlist, i)
  }  
}

## CHECK IF OUTLIERS MAKE A DIFFERENCE: (KOLMOGOROV-SMIRNOV)
# IF P-VAL < 0.05 MEANING THERE IS A DIFFERENCE
res1 <- NULL
for (n in numlist) {
  out <- Crimes_data_original[[n]]
  non <- Crimes_data_original[[n]][which(outmat[[n]]==0)]
  outnum <- length(out) - length(non)
  pval <- suppressWarnings(ks.test(out, non)$p.value)
  res1 <- rbind(res1, cbind(var=n, outliers_pct=outnum, distribution_changed=ifelse(pval<0.05,"+","-")))
}
res1
### CHECK IF OUTLIERS CREATE / KILL CORRELATION WITH THE TARGET:
res2 <- NULL
for (n in numlist) {
  out <- Crimes_data_original[[n]]
  non <- Crimes_data_original[[n]][which(outmat[[n]]==0)]
  Crimes_data_out <- Crimes_data_original$LA_Crimes
  Crimes_data_non <- Crimes_data_out[which(outmat[[n]]==0)]
  outdf <- data.frame(x_out=out,y_out=Crimes_data_out)
  nondf <- data.frame(x_non=non,y_non=Crimes_data_non)
  cr <- cocor(~ x_out + y_out | x_non + y_non, data=list(outdf,nondf))
  pval <- cr@fisher1925$p.value
  res2 <- rbind(res2, cbind(var=n, correlation_changed=ifelse(pval<0.05,"+","-")))
}
res2

out <- Crimes_data_original$Youth_in_Non.Family_Households
non <- Crimes_data_original$Youth_in_Non.Family_Households[which(outmat$Youth_in_Non.Family_Households==0)]
Crimes_data_out <- Crimes_data_original$LA_Crimes
Crimes_data_non <- Crimes_data_out[which(outmat$Youth_in_Non.Family_Households==0)]
outdf <- data.frame(x_out=out,y_out=Crimes_data_out)
nondf <- data.frame(x_non=non,y_non=Crimes_data_non)
cr <- cocor(~ x_out + y_out | x_non + y_non, data=list(outdf,nondf))
pval <- cr@fisher1925$p.value
res2 <- rbind(res2, cbind(var="Youth_in_Non.Family_Households", correlation_changed=ifelse(pval<0.05,"+","-")))

out <- Crimes_data_original$Youth_in_Single_Parent_Households
non <- Crimes_data_original$Youth_in_Single_Parent_Households[which(outmat$Youth_in_Single_Parent_Households==0)]
Crimes_data_out <- Crimes_data_original$LA_Crimes
Crimes_data_non <- Crimes_data_out[which(outmat$Youth_in_Single_Parent_Households==0)]
outdf <- data.frame(x_out=out,y_out=Crimes_data_out)
nondf <- data.frame(x_non=non,y_non=Crimes_data_non)
cr <- cocor(~ x_out + y_out | x_non + y_non, data=list(outdf,nondf))
pval <- cr@fisher1925$p.value
res2 <- rbind(res2, cbind(var="Youth_in_Single_Parent_Households", correlation_changed=ifelse(pval<0.05,"+","-")))
res2

### COMBINE BOTH TABLES TO DECIDE: LOOKS LIKE WE CAN DROP ALL OUTLIERS!!!
res <- inner_join(data.frame(res1),data.frame(res2),by="var")
res$drop <- ifelse(res$distribution_changed=="+" & res$correlation_changed == "+","No","Yes")
res
## ALL OUTLIERS CAN BE DROPPED EXCEPT LA_Crimes, Asian, Youth_in_Non.Family_Households


## DROPING OUTLIERS WHERE POSSIBLE:
Crimes_data_no_outliers <- Crimes_data_no_missing_rows
Crimes_data_no_outliers$Total_Population <- drop_outliers(Crimes_data_no_outliers$Total_Population, Crimes_data_no_outliers$Total_Population_No_Outs)
Crimes_data_no_outliers$Under_Age_18 <- drop_outliers(Crimes_data_no_outliers$Under_Age_18, Crimes_data_no_outliers$Under_Age_18_No_Outs)
Crimes_data_no_outliers$Ages_18.24 <- drop_outliers(Crimes_data_no_outliers$Ages_18.24, Crimes_data_no_outliers$Ages_18.24_No_Outs)
Crimes_data_no_outliers$Ages_25.34 <- drop_outliers(Crimes_data_no_outliers$Ages_25.34, Crimes_data_no_outliers$Ages_25.34_No_Outs)
Crimes_data_no_outliers$Ages_35.44 <- drop_outliers(Crimes_data_no_outliers$Ages_35.44, Crimes_data_no_outliers$Ages_35.44_No_Outs)
Crimes_data_no_outliers$Ages_45.54 <- drop_outliers(Crimes_data_no_outliers$Ages_45.54, Crimes_data_no_outliers$Ages_45.54_No_Outs)
Crimes_data_no_outliers$Ages_55.64 <- drop_outliers(Crimes_data_no_outliers$Ages_55.64, Crimes_data_no_outliers$Ages_55.64_No_Outs)
Crimes_data_no_outliers$Ages_65_._Older <- drop_outliers(Crimes_data_no_outliers$Ages_65_._Older, Crimes_data_no_outliers$Ages_65_._Older_No_Outs)
Crimes_data_no_outliers$Young_Adults_Enrolled_in_School <- drop_outliers(Crimes_data_no_outliers$Young_Adults_Enrolled_in_School, Crimes_data_no_outliers$Young_Adults_Enrolled_in_School_No_Outs)
Crimes_data_no_outliers$College_Graduation_Rate <- drop_outliers(Crimes_data_no_outliers$College_Graduation_Rate, Crimes_data_no_outliers$College_Graduation_Rate_No_Outs)
Crimes_data_no_outliers$Less_Than_High_School <- drop_outliers(Crimes_data_no_outliers$Less_Than_High_School, Crimes_data_no_outliers$Less_Than_High_School_No_Outs)
Crimes_data_no_outliers$Labor_Force_Participation_Rate <- drop_outliers(Crimes_data_no_outliers$Labor_Force_Participation_Rate, Crimes_data_no_outliers$Labor_Force_Participation_Rate_No_Outs)
Crimes_data_no_outliers$Unemployment_Rate <- drop_outliers(Crimes_data_no_outliers$Unemployment_Rate, Crimes_data_no_outliers$Unemployment_Rate_No_Outs)
Crimes_data_no_outliers$Homeownership_Rate <- drop_outliers(Crimes_data_no_outliers$Homeownership_Rate, Crimes_data_no_outliers$Homeownership_Rate_No_Outs)
Crimes_data_no_outliers$Renter_Rate <- drop_outliers(Crimes_data_no_outliers$Renter_Rate, Crimes_data_no_outliers$Renter_Rate_No_Outs)
Crimes_data_no_outliers$Immigrant_Citizen <- drop_outliers(Crimes_data_no_outliers$Immigrant_Citizen, Crimes_data_no_outliers$Immigrant_Citizen_No_Outs)
Crimes_data_no_outliers$Immigrant_Non.Citizen <- drop_outliers(Crimes_data_no_outliers$Immigrant_Non.Citizen, Crimes_data_no_outliers$Immigrant_Non.Citizen_No_Outs)
Crimes_data_no_outliers$Immigrant <- drop_outliers(Crimes_data_no_outliers$Immigrant, Crimes_data_no_outliers$Immigrant_No_Outs)
Crimes_data_no_outliers$Divorced_Separated <- drop_outliers(Crimes_data_no_outliers$Divorced_Separated, Crimes_data_no_outliers$Divorced_Separated_No_Outs)
Crimes_data_no_outliers$Married <- drop_outliers(Crimes_data_no_outliers$Married, Crimes_data_no_outliers$Married_No_Outs)
Crimes_data_no_outliers$Never_Married <- drop_outliers(Crimes_data_no_outliers$Never_Married, Crimes_data_no_outliers$Never_Married_No_Outs)
Crimes_data_no_outliers$Median_Household_Income <- drop_outliers(Crimes_data_no_outliers$Median_Household_Income, Crimes_data_no_outliers$Median_Household_Income_No_Outs)
Crimes_data_no_outliers$Opportunity_Youth <- drop_outliers(Crimes_data_no_outliers$Opportunity_Youth, Crimes_data_no_outliers$Opportunity_Youth_No_Outs)
Crimes_data_no_outliers$Homeless_count <- drop_outliers(Crimes_data_no_outliers$Homeless_count, Crimes_data_no_outliers$Homeless_count_No_Outs)
Crimes_data_no_outliers$Below_100_Poverty_Threshold <- drop_outliers(Crimes_data_no_outliers$Below_100_Poverty_Threshold, Crimes_data_no_outliers$Below_100_Poverty_Threshold_No_Outs)
Crimes_data_no_outliers$Below_200_Poverty_Threshold <- drop_outliers(Crimes_data_no_outliers$Below_200_Poverty_Threshold, Crimes_data_no_outliers$Below_200_Poverty_Threshold_No_Outs)
Crimes_data_no_outliers$White <- drop_outliers(Crimes_data_no_outliers$White, Crimes_data_no_outliers$White_No_Outs)
Crimes_data_no_outliers$American_Indian_Or_Native <- drop_outliers(Crimes_data_no_outliers$American_Indian_Or_Native, Crimes_data_no_outliers$American_Indian_Or_Native_No_Outs)
Crimes_data_no_outliers$Black <- drop_outliers(Crimes_data_no_outliers$Black, Crimes_data_no_outliers$Black_No_Outs)
Crimes_data_no_outliers$Hispanic <- drop_outliers(Crimes_data_no_outliers$Hispanic, Crimes_data_no_outliers$Hispanic_No_Outs)
Crimes_data_no_outliers$Other_Race <- drop_outliers(Crimes_data_no_outliers$Other_Race, Crimes_data_no_outliers$Other_Race_No_Outs)
Crimes_data_no_outliers$Two_or_More_Races <- drop_outliers(Crimes_data_no_outliers$Two_or_More_Races, Crimes_data_no_outliers$Two_or_More_Races_No_Outs)
Crimes_data_no_outliers$Median_Rent_Price <- drop_outliers(Crimes_data_no_outliers$Median_Rent_Price, Crimes_data_no_outliers$Median_Rent_Price_No_Outs)
Crimes_data_no_outliers$No_Vehicle_Households <- drop_outliers(Crimes_data_no_outliers$No_Vehicle_Households, Crimes_data_no_outliers$No_Vehicle_Households_No_Outs)
Crimes_data_no_outliers$Youth_in_Single_Parent_Households <- drop_outliers(Crimes_data_no_outliers$Youth_in_Single_Parent_Households, Crimes_data_no_outliers$Youth_in_Single_Parent_HouseholdsNo_Outs)
summary(Crimes_data_no_outliers)
exploreData(Crimes_data_no_outliers)

####################### CHECK MISSING VALUES AGAIN AFTER DROPING THE OUTLIERS:
## CREATE THE MATRIX AND CHECK MISSING AMOUNT:
Crimes_missing <- getMissingness(Crimes_data_no_outliers)
Crimes_missing$missingness
Crimes_missing$missingness$var
## SHOW MISSING MATRIX:
Crimes_na <- missingMatrix(Crimes_data_no_outliers)
options(repr.plot.width = 15, repr.plot.height = 80)
vis_miss(Crimes_data_no_outliers, warn_large_data = F)
## CHECK THE ROWS FOR MISSING %:
Crimes_na_sum <- Crimes_na
Crimes_na_sum$pct <- rowSums(Crimes_na)/ncol(Crimes_na)
Crimes_na_sum %>% group_by(pct) %>% tally
## REMOVE ROWS WITH OVER 30% MISSING:
Crimes_data_no_outliers$drop <- ifelse(Crimes_na_sum$pct >= 0.3, 1, 0)
table(Crimes_data_no_outliers$drop)
nrow(Crimes_data_no_outliers)
Crimes_data_no_missing_rows2 <- Crimes_data_no_outliers
Crimes_data_no_missing_rows2 <- Crimes_data_no_missing_rows2 %>% filter (Crimes_data_no_missing_rows2$drop==0)
Crimes_data_no_missing_rows2$drop <- NULL
## CHECKING THE RESULT:
nrow(Crimes_data_no_missing_rows2)
Crimes_missing <- getMissingness(Crimes_data_no_missing_rows2)
Crimes_missing$missingness
Crimes_missing$missingness$var
Crimes_na <- missingMatrix(Crimes_data_no_missing_rows2)
options(repr.plot.width = 15, repr.plot.height = 80)
vis_miss(Crimes_data_no_missing_rows2, warn_large_data = F)

## CHECK IF MISSING AT RANDOM WITH KOLMOGOROV-SMIRNOV: IF P-VAL<0.05 THERE IS CHANGE. With (-): MISSING AT RANDOM.
## SINCE NO (+): THERE IS NO IMPACT OF THE MISSING AND WE CAN IMPUTATE ALL OF THEM.
miss_res <- NULL
for (m in Crimes_missing$missingness$var) {
  p <- list()
  for (n in Cnames) {
    if (n != m) {
      miss <- Crimes_data_no_missing_rows2[[n]]
      non <- Crimes_data_no_missing_rows2[[n]][which(Crimes_na[[n]]==0)]
      missnum <- length(miss) - length(non)
      pval <- suppressWarnings(ks.test(miss, non)$p.value)
      miss_res <- rbind(miss_res, cbind(var=n,missing=m, outliers_cnt=missnum, distribution_changed=ifelse(pval<0.05,"+","-")))
    }
  }
}
miss_res
miss_res <- data.frame(miss_res)
miss_res %>% filter(distribution_changed=="+")

### IMPUTATION WITH KNN:
Crimes_data_full<-kNN(Crimes_data_no_missing_rows2, k=3)
names(Crimes_data_full)
Crimes_data_full<-Crimes_data_full[, !grepl("imp", names( Crimes_data_full))]
summary(Crimes_data_full)
exploreData(data=Crimes_data_full, y="LA_Crimes")

### ANALYZING EACH ITEM WITH THE TARGET - CORRELATION AND IMPACT:
##CORRELATION TABLE OF ALL WITH LA_CRIMES:
Cnames <- names(subset(Crimes_data_full, select = -c(ï..Area, Neighborhood, LA_Crimes)))
corrTable <- NULL
for (n in Cnames){
  a<-(cor.test(Crimes_data_full$LA_Crimes, Crimes_data_full[[n]], method = "spearman"))
  pval <- a$p.value
  rhoval <- a$estimate
  corrTable <- rbind(corrTable, data.frame(var=n, significant=ifelse(pval<0.05,"yes","no"), r=rhoval))
}
corrTable
corrTableRes <- corrTable %>% dplyr::arrange(desc(abs(r)))
corrTableRes

write_csv(corrTableRes,"~/DataScience/project/correlationWithLACrimes.csv")

####################### MULTIVARIATE ANALYSIS:
# For two or more items. numeric & category with one another.
# Looking for correlation and differences between the items
# In my case all tests are dependet

##CORRELATION OF ALL ITEMS:
options(repr.plot.width = 15, repr.plot.height = 12)
dfcormat <- cor(Crimes_data_full[,Cnames], use = "complete.obs",method = "spearman")
corrgram::corrgram(dfcormat)

# CREATE TABLE WITH ALL P-VALUES WITH EACH OTHER:
numlist <- NULL
for(i in names(Crimes_data_full)) {
  if(is.numeric(Crimes_data_full[[i]])==TRUE) {
    numlist <- c(numlist, i)
  }  
}

corrTableAll <- NULL
for (n in numlist){
  numlist <- numlist[-1]
  for(m in numlist){
    a<-(cor.test(Crimes_data_full[[n]], Crimes_data_full[[m]], method = "spearman"))
    pval <- a$p.value
    rhoval <- a$estimate
    corrTableAll <- rbind(corrTableAll, data.frame(var=n, var=m, rhoval=rhoval, significant=ifelse(pval<0.05,"yes","no")))
  }
}
corrTableAll <- corrTableAll %>% dplyr::filter(significant == "yes")%>% dplyr::filter(abs(rhoval) > 0.7 & abs(rhoval) < 0.99) %>% dplyr::arrange(desc(abs(rhoval)))
corrTableAll

write_csv(corrTableAll,"~/DataScience/project/CorrelationTable.csv")

####################### DATA ENRICHMENT ####################### 
Crimes_data_full$YoungVsOld = (Crimes_data_full$Under_Age_18 + Crimes_data_full$Ages_18.24) / (Crimes_data_full$Ages_55.64 + Crimes_data_full$Ages_65_._Older + 0.05)
Crimes_data_full$OpportunityVsYoung = Crimes_data_full$Opportunity_Youth / (Crimes_data_full$Under_Age_18 + Crimes_data_full$Ages_18.24 + 0.05)
Crimes_data_full$EnrolledVsYoung = Crimes_data_full$Young_Adults_Enrolled_in_School / (Crimes_data_full$Ages_18.24 + 0.05)
Crimes_data_full$NoHighSchoolVsCollege = Crimes_data_full$Less_Than_High_School / (Crimes_data_full$College_Graduation_Rate + 0.05)
Crimes_data_full$Singles = Crimes_data_full$Never_Married + Crimes_data_full$Divorced_Separated
Crimes_data_full$NonWhite = Crimes_data_full$American_Indian_Or_Native + Crimes_data_full$Asian + Crimes_data_full$Black + Crimes_data_full$Hispanic + Crimes_data_full$Other_Race + Crimes_data_full$Two_or_More_Races
Crimes_data_full$WhiteVsNonWhite = Crimes_data_full$White / (Crimes_data_full$NonWhite + 0.05)

summary(Crimes_data_full)
names(Crimes_data_full)

####################### FEATURE SELECTION ####################### 
set.seed(123)
# calculate correlation matrix
correlationMatrix <- cor(Crimes_data_full[,3:47])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
# print indexes of highly correlated attributes
print(highlyCorrelated)
names(Crimes_data_full)

Crimes_data_final <- Crimes_data_full
Crimes_data_final$Homeless_count<-NULL
Crimes_data_final$Renter_Rate<-NULL
Crimes_data_final$Homeownership_Rate<-NULL
Crimes_data_final$Opportunity_Youth<-NULL
Crimes_data_final$Young_Adults_Enrolled_in_School<-NULL
Crimes_data_final$Divorced_Separated<-NULL
Crimes_data_final$Asian<-NULL
Crimes_data_final$EnrolledVsYoung<-NULL
Crimes_data_final$Below_100_Poverty_Threshold<-NULL
Crimes_data_final$Ages_65_._Older<-NULL
Crimes_data_final$Immigrant<-NULL
Crimes_data_final$Ages_45.54<-NULL
Crimes_data_final$Labor_Force_Participation_Rate<-NULL
Crimes_data_final$Unemployment_Rate<-NULL
Crimes_data_final$NoHighSchoolVsCollege<-NULL
Crimes_data_final$YoungVsOld<-NULL
Crimes_data_final$Never_Married<-NULL

summary(Crimes_data_final)
names(Crimes_data_final)
exploreData((Crimes_data_final))

####################### DATA PARTITION ####################### 
vn <- setdiff(names(Crimes_data_final),c("ï..Area","Neighborhood"))
### SPLIT TO TRAIN, DEV, TEST, SEED WAS CHOSEN TO GET A BALANCED PARTITION
firstPart <- train_test(data = Crimes_data_final[,vn], train_name = "temp", test_name = "test",prop = 0.8, seed = 7, tableone=T)
firstPart %>% filter(pval < 0.05)
secondPart <- train_test(data = temp[,vn], train_name = "train", test_name = "dev",prop = 0.8, seed = 7, tableone=T)
secondPart %>% filter(pval < 0.05)
dim(Crimes_data_final)
dim(temp)
dim(test)
dim(train)
dim(dev)
rm(temp)
### LOOK AT THE GROUPS' DESTRIBUTION THAT IS SIMILAR AND IT IS A NICE DISTRIBUTION:
options(repr.plot.width = 10, repr.plot.height = 8)

ggplot() +
  geom_density(aes(x=train$LA_Crimes),color="red",alpha=0.3) +
  geom_density(aes(x=dev$LA_Crimes),color="blue",alpha=0.3) +
  geom_density(aes(x=test$LA_Crimes),color="green",alpha=0.3)

################################## RMSLE ##################################
rmsle <- function(y,y_hat) {
  err <- sqrt(sum((log(y_hat+1)-log(y+1))^2,na.rm=T)/length(y))
  return(err)
}

##################################Basic Models##################################
err_res <- NULL
set.seed(18)
# First model - linear regression:
mod1 <- lm(LA_Crimes ~., data=train)
summary(mod1)
pred1 <- predict(mod1, data=dev)
err_res <- rbind(err_res, data.frame(Name="Base Linear regression", Model="mod1", 
                                     RMSLE=rmsle(dev$LA_Crimes,pred1)))

# Second model - tree:
mod2 <- tree(LA_Crimes ~., data=train)
summary(mod2)
pred2 <- predict(mod2,newdata=dev)
err_res <- rbind(err_res, data.frame(Name="Decision Trees-tree", Model="mod2", 
                                     RMSLE=rmsle(dev$LA_Crimes, pred2)))

# Third model - rpart, again tree:
mod3 <- rpart(LA_Crimes ~., data=train)
mod3
pred3 <- predict(mod3,newdata=dev)
err_res <- rbind(err_res, data.frame(Name="Decision Trees-rpart", Model="mod3", 
                                     RMSLE=rmsle(dev$LA_Crimes, pred3)))

# Fourth model - random forest
mod4 <- randomForest(LA_Crimes ~., data=train)
mod4
pred4 <- predict(mod4, newdata=dev)
err_res <- rbind(err_res, data.frame(Name="RandomForest (RF)", Model="mod4", 
                                     RMSLE=rmsle(dev$LA_Crimes,pred4)))

# Fifth mode - ranger, still random forest:
mod5 <- ranger(LA_Crimes ~., data=train)
mod5
pred5 <- predict(mod5, data=dev)
err_res <- rbind(err_res, data.frame(Name="RandomForest (ranger)", Model="mod5", 
                                     RMSLE=rmsle(dev$LA_Crimes, pred5$predictions)))

#Sixth model - XGboost - BEST SO FAR
nm <- setdiff(names(train),"LA_Crimes")
X_train <- data.matrix(train[,nm])
y_train <- train$LA_Crimes
mod6 <- xgboost(data=X_train, label = y_train, max_depth=2, nrounds=100, eta=1, print_every_n = 100)
X_test <- data.matrix(dev[,nm])
y_test <- dev$LA_Crimes
pred6 <- predict(mod6, newdata=X_test)
err_res <- rbind(err_res, data.frame(Name="XGBoost", Model="mod6", 
                                     RMSLE=rmsle(dev$LA_Crimes,pred6)))

# Seventh model - KNN:
mod7 <- knn(train, dev, cl=train$LA_Crimes, k=5, prob=TRUE)
pred7 <- as.numeric(as.character(mod7))
err_res <- rbind(err_res, data.frame(Name="kNN", Model="mod7", 
                                     RMSLE=rmsle(dev$LA_Crimes, pred7)))

# Eight model - SVM:
mod8 <- svm(LA_Crimes ~., train, gamma=0.5)
pred8 <- predict(mod8, newdata=dev)
err_res <- rbind(err_res, data.frame(Name="SVM", Model="mod8", 
                                     RMSLE=rmsle(dev$LA_Crimes,pred8)))
err_res %>% arrange(RMSLE)

#### OPTIMIZINGL XGBOOST: RUN EARLIER, TO IDENTFY THE BEST XGBOOST MODEL
set.seed(18)
err_res<-NULL
nm <- setdiff(names(train),"LA_Crimes")
X_train <- data.matrix(train[,nm])
y_train <- train$LA_Crimes

max_depth <- c(1:4)
min_child_weight <- c(1:4)
subsample <- c(0.1,0.2,0.3,0.4)
nrounds <- c(10,20,30,40,50,60)
eta <- c(0.3,0.6,0.9,1)
for (i in max_depth){
  for (j in min_child_weight){
    for (k in subsample){
      for (l in nrounds){
        for (m in eta){
          set.seed(18)
          mod6 <- xgboost(data=X_train, label = y_train, max_depth=i, min_child_weight=j, subsample=k, nrounds=l, eta=m, print_every_n = 100)
          X_test <- data.matrix(dev[,nm])
          y_test <- dev$LA_Crimes
          pred6 <- predict(mod6, newdata=X_test)
          err_res <- rbind(err_res, data.frame(Name="XGBoost", Model="mod6", i, j, k, l, m,
                                               RMSLE=rmsle(dev$LA_Crimes,pred6)))
          
        }
      }
    }    
  }
}

err_res %>% arrange(RMSLE)
model_fine_tune<-err_res %>% arrange(RMSLE)
write_csv(model_fine_tune,"~/DataScience/project/model_fine_tune.csv")

#### THE ADJUSTED XGBOOST - THE BEST MODEL:
err_res<-NULL
nm <- setdiff(names(train),"LA_Crimes")
X_train <- data.matrix(train[,nm])
y_train <- train$LA_Crimes
set.seed(18)
mod6 <- xgboost(data=X_train, label = y_train, max_depth=1, min_child_weight=1, subsample=0.1, nrounds=50, eta=0.9, print_every_n = 100)
X_test <- data.matrix(dev[,nm])
y_test <- dev$LA_Crimes
pred6 <- predict(mod6, newdata=X_test)
err_res <- rbind(err_res, data.frame(Name="XGBoost", Model="mod6", 
                                     RMSLE=rmsle(dev$LA_Crimes,pred6)))

err_res

## RUNNING THE MODEL ON THE TEST:
X_test <- data.matrix(test[,nm])
y_test <- test$LA_Crimes
pred6 <- predict(mod6, newdata=X_test)
err_res <- rbind(err_res, data.frame(Name="XGBoost", Model="mod6", 
                                     RMSLE=rmsle(test$LA_Crimes,pred6)))
err_res

