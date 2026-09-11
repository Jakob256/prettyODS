#' Title
#'
#' @param sheet todo
#' @param data todo
#' @param row todo
#' @param col todo
#' @param style todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_writeCell <- function(sheet, data, row, col, style){
  if (missing(style)){style=ODS_createStyle()}
  if (!inherits(style, "ODSstyle")){stop("'style' must be a style!")}
  if (length(data)!=1){stop("data must have length 1. Consider using ODS_writeData")}

  .ODS_writeArray(sheet,data,row,col,style,skipNA = FALSE)
  invisible(sheet)
}


#' Title
#'
#' @param sheet todo
#' @param data todo todo
#' @param startRow todo
#' @param startCol todo
#' @param style todo
#' @param useColnames todo
#' @param styleColnames todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_writeData <- function(sheet, data, startRow=1, startCol=1, style, useColnames=TRUE, styleColnames){
  column = NULL # due to NSE notes in R CMD check
  if (missing(style        )){style        =ODS_createStyle()}
  if (missing(styleColnames)){styleColnames=ODS_createStyle()}

  if (is.data.frame(data)){ ## includes data.table and tibble
    if (useColnames){
      colNames=matrix(colnames(data),nrow=1)
      .ODS_writeArray(sheet,colNames,startRow,startCol,styleColnames)
    }
    for (i in seq_len(ncol(data))){
      column=matrix(data[[i]],ncol=1)
      .ODS_writeArray(sheet,column,startRow+useColnames,startCol+i-1,style)
    }
    return(invisible(sheet))
  }

  if (is.matrix(data)){
    if (useColnames & !is.null(colnames(data))){
      colNames=matrix(colnames(data),nrow=1)
      .ODS_writeArray(sheet,colNames,startRow,startCol,styleColnames)
      startRow=startRow+1
    }
    .ODS_writeArray(sheet,data,startRow,startCol,style)
    return(invisible(sheet))
  }

  if (is.atomic(data)){
    .ODS_writeArray(sheet,matrix(data,ncol=1),startRow,startCol,style)
    return(invisible(sheet))
  }

  stop("Data type not yet supported")
}


#' Title
#'
#' @param sheet todo
#' @param array todo todo
#' @param startRow todo
#' @param startCol todo
#' @param style todo
#' @param skipNA todo
#'
#' @returns todo
#'
#' @examples 1+1
.ODS_writeArray <- function(sheet, array, startRow, startCol, style, skipNA=FALSE){
  ## This is an internal function, hence, there will be no error checking.
  ## The array (all of the same type and style), will be added to the sheet.
  ## This is meant to be the single function, that dumps data into the sheet.

  if (!is.matrix(array)){array=matrix(array)}

  ## Handling the style, i.e. does it already exist?
  if (length(style)==0){style=list(font=NA)} ## otherwise we would introduce a bug
  sheet$stylesTable=rbind(sheet$stylesTable,style,fill=TRUE)

  keys=apply(sheet$stylesTable, 1, paste, collapse = "|*")
  styleNumber=match(utils::tail(keys,1), keys)

  if (styleNumber<length(keys)){## the style already existed
    sheet$stylesTable=sheet$stylesTable[-nrow(sheet$stylesTable)]
  }


  ## Dumping the data
  unwind=c(array)
  row=c(row(array))+startRow-1
  col=c(col(array))+startCol-1


  if (typeof(array)%in%c("integer","double")){
    df=data.table::data.table(row=row,column=col,type="float",data_string=NA,data_float=unwind,styleNumber=styleNumber)
  } else {
    unwind=as.character(unwind)
    df=data.table::data.table(row=row,column=col,type="string",data_string=unwind,data_float=NA,styleNumber=styleNumber)
  }

  if (skipNA){df=df[!is.na(unwind),]}

  sheet$cellsContent <- rbind(sheet$cellsContent, df)
  #invisible(sheet)
}




#' Title
#'
#' @param sheet todo
#' @param rows todo
#' @param cols todo
#' @param mergeCols todo
#' @param mergeRows todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_mergeCells <- function(sheet, rows, cols, mergeCols=TRUE, mergeRows=TRUE){
  error1 = (min(cols)==max(cols) & min(rows)==max(rows))
  error2 = (!mergeCols & min(rows)==max(rows))
  error3 = (!mergeRows & min(cols)==max(cols))
  error4 = (!mergeCols & !mergeRows)
  if (error1 | error2 | error3 | error4){stop("You cannot merge single cells")}


  overlapps <- function(mergedCells,news){
    for (i in seq_len(nrow(mergedCells))){
      for (j in seq_len(nrow(news))){
        new=news[j,]
        merged=mergedCells[i,]
        ## we calculate a possible intersection point. If it does not intersect, we are safe!

        INTERSECTROW=max(new["fromRow"],merged["fromRow"])
        INTERSECTCOL=max(new["fromColumn"],merged["fromColumn"])
        if (INTERSECTROW<=min(new["toRow"],merged["toRow"]) &
            INTERSECTCOL<=min(new["toColumn"],merged["toColumn"])){return(c(INTERSECTROW,INTERSECTCOL))}
      }
    }
    return(FALSE)
  }


  if ( mergeCols){fromColumn=min(cols); toColumn=max(cols)}
  if (!mergeCols){fromColumn=cols;      toColumn=cols}
  if ( mergeRows){fromRow=min(rows);    toRow=max(rows)}
  if (!mergeRows){fromRow=rows;         toRow=rows}

  news=cbind(fromRow,toRow,fromColumn,toColumn)

  result=overlapps(sheet$mergedCells,news)
  if (length(result)!=1){
    stop(paste0("Mergeconflict: Cell at row=",result[1]," and column=",result[2], " is already merged"))
  }

  sheet$mergedCells=rbind(sheet$mergedCells,news)
  invisible(sheet)
}


#' Title
#'
#' @param sheet todo
#' @param row todo
#' @param col todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_getStyle <- function(sheet,row,col){
  column = NULL # due to NSE notes in R CMD check (see vignette of datatable-importing):

  r=row
  cc=col
  styleNumber=sheet$cellsContent[row==r & column==cc,styleNumber]
  if (length(styleNumber)==0){
    warning("cell has no assigned style")
    return(ODS_createStyle())
  }

  styleNumber=utils::tail(styleNumber,1) ## if cell was overwritten, take last style
  style=as.list(sheet$stylesTable[styleNumber])
  style=style[!sapply(style, is.na)]
  class(style)="ODSstyle"
  return(style)
}


#' Title
#'
#' @param sheet todo
#' @param cols todo
#' @param width todo
#' @param unit todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_setColWidths <- function(sheet,cols,width=1.7,unit="cm"){
  ## auto is missing for now. Also NAs maybe
  sheet$colWidths[cols]=paste0(width,unit)
}


#' Title
#'
#' @param sheet todo
#' @param rows todo
#' @param height todo
#' @param unit todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_setRowHeights <- function(sheet,rows,height=15,unit="pt"){
  ## auto is missing for now. Also NAs maybe
  sheet$rowHeights[rows]=paste0(height,unit)
}
