#~~~~~~~~~~~~~~~~~~~~~~~~~
## 1. Small Functions ####
#~~~~~~~~~~~~~~~~~~~~~~~~~



#' get hex representation of a color
#'
#' @param colors todo
#'
#' @returns todo
#'
#' @examples prettyODS:::.col2hex(c("red","darkblue","#ff3300",NA))
.col2hex <- function(colors){
  rgb=grDevices::col2rgb(colors)
  hex=sprintf("#%02X%02X%02X", rgb[1,], rgb[2,], rgb[3,])
  hex[is.na(colors)]=NA
  return(hex)
}



# we want 1.65 to be rounded to 1.7 with digits=1. However
#   round(1.65,1)=1.6
#   sprintf("%.1f",1.65)="1.6"
#   .round2character(1.65,1)="1.7"
# hence this unusual form:

#' Title
#'
#' @param x todo
#' @param digits todo
#'
#' @returns todo
#'
#' @examples 1+1
.round2character <- function(x, digits){
  sprintf(paste0("%.", digits, "f"), x*(1+2^(-48)))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2. Defining Classes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~
### 2.1. ODSstyle ####
#~~~~~~~~~~~~~~~~~~~~~



#' Title
#'
#' @param font todo
#' @param size todo
#' @param color todo
#' @param bold todo
#' @param italic todo
#' @param underline todo
#' @param cellcolor todo
#' @param vAlign todo
#' @param hAlign todo
#' @param margin todo
#' @param wrap todo
#' @param rotate todo
#' @param digits todo
#'
#' @returns todo
#' @export
#'
#' @examples ODS_createStyle(font="Arial",bold=TRUE)
ODS_createStyle <- function(font=NULL, size=NULL, color=NULL, bold=NULL, italic=NULL,
                            underline=NULL, cellcolor=NULL, vAlign=NULL, hAlign=NULL,
                            margin=NULL, wrap=NULL, rotate=NULL, digits=NULL){

  style=as.list(environment())
  style=style[!sapply(style, is.null)]
  class(style)="ODSstyle"

  ## these throw an error, if the colors are not valid:
  .col2hex(color)
  .col2hex(cellcolor)


  temp=unlist(lapply(style,length))
  temp=temp[temp>1]
  if (length(temp)!=0){stop(paste(names(temp),collapse=", "),": must be a single value")}

  return(style)
}



#' Title
#'
#' @param x todo
#' @param ... todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
print.ODSstyle <- function(x, ...){
  cat("ODSstyle\n")
  for (name in names(x)){
    cat("  ",name,": ",x[[name]],"\n", sep = "")
  }
}



#' Title
#'
#' @param style1 todo
#' @param style2 todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_mergeStyles <- function(style1,style2){
  ## style1 has priority!
  ## i.e. fills empty slots of style1 with style2
  if (!inherits(style1, "ODSstyle")){stop("style1 must be an ODSstyle")}
  if (!inherits(style2, "ODSstyle")){stop("style2 must be an ODSstyle")}

  style <- style2
  for (name in names(style1)) {
    style[[name]] <- style1[[name]]
  }
  class(style) <- "ODSstyle"

  return(style)
}



#~~~~~~~~~~~~~~~~~~~~~
### 2.2. ODSsheet ####
#~~~~~~~~~~~~~~~~~~~~~

#' @importFrom R6 R6Class
SHEET <- R6Class("ODSsheet",
                 public = list(
                   sheetName=NA,
                   cellsContent = data.table::data.table(
                     row = integer(),
                     column = integer(),
                     type = character(),
                     data_string = character(),
                     data_float = numeric(),
                     styleNumber = integer()
                   ),
                   mergedCells = matrix(
                     nrow = 0, ncol = 4,
                     dimnames = list(NULL, c("fromRow", "toRow", "fromColumn", "toColumn"))
                   ),
                   colWidths=c(),
                   rowHeights=c(),
                   stylesTable = data.table::data.table(
                     font = character(),
                     size = integer(),
                     color= character(),
                     bold = logical(),
                     italic=logical(),
                     underline=logical(),
                     cellcolor=character(),
                     vAlign=character(),
                     hAlign=character(),
                     margin=numeric(),
                     wrap=logical(),
                     rotate=integer(),
                     digits=integer()
                   ),



                   cleanup = function(){
                     ## fixes cells, that were accessed repeatedly (hence overwritten)
                     ## we could also fix styles that aren't used, but this seems tricky
                     if (nrow(self$cellsContent)<=1){return()}
                     cell_id <- self$cellsContent[, paste(row, column)]
                     keep <- !duplicated(cell_id, fromLast = TRUE)
                     self$cellsContent <- self$cellsContent[keep,]
                     #invisible(self)
                   },


                   print = function(...) {
                     self$cleanup()
                     bold <- function(x) paste0("\033[1m", x, "\033[0m")
                     cat(bold("class:"),"       ODSsheet\n")
                     cat(bold("sheetName:   "),self$sheetName,"\n")

                     cat(bold("cellsContent:\n"))
                     if (nrow(self$cellsContent)!=0){
                       print(self$cellsContent)
                       cat("\n")
                     }

                     cat(bold("mergedCells:\n"))
                     if (nrow(self$mergedCells)!=0){
                       print(self$mergedCells)
                     }

                     cat(bold("colWidths:   "),self$colWidths,"\n")
                     cat(bold("rowHeights:  "),self$rowHeights,"\n")
                     cat(bold("styles:"),"\n")
                     for (i in seq_len(nrow(self$stylesTable))){
                       cat("style ",i,":\n",sep="")
                       temp=as.list(self$stylesTable[i,])
                       temp=temp[!sapply(temp, is.na)]
                       for (name in names(temp)){
                         cat("  ",name,": ",temp[[name]],"\n", sep = "")
                       }
                     }
                   },


                   nrow = function(...){
                     return(max(0,self$cellsContent$row))
                   },

                   ncol = function(...){
                     return(max(0,self$cellsContent$column))
                   },


                   View = function(...){
                     self$cleanup()
                     sheet = matrix(NA_character_,nrow=self$nrow(),ncol=self$ncol())


                     ## for displaying, we abuse the variable "cellsContent"
                     stylesTable=data.table::copy(self$stylesTable)[,styleNumber:=seq_len(.N)]
                     cellsContent=merge(self$cellsContent,stylesTable[,c("styleNumber","digits")])
                     cellsContent[type=="float" &  is.na(digits),data_string:=as.character(data_float)]
                     cellsContent[type=="float" & !is.na(digits),data_string:=.round2character(data_float,digits)]

                     sheet[cbind(cellsContent$row,cellsContent$column)]=cellsContent$data_string
                     sheet=data.table::data.table(sheet)
                     NAMES=LETTERS
                     while(self$ncol()>length(NAMES)){
                       NAMES=as.vector(outer(c("",LETTERS),NAMES,paste0))
                       NAMES=unique(NAMES)
                       NAMES=NAMES[order(nchar(NAMES),NAMES)]
                     }
                     colnames(sheet)=NAMES[seq_len(ncol(sheet))]
                     View(sheet)
                     return(invisible(sheet))
                   }
                 )
)




#' Title
#'
#' @param sheetName todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_createSheet = function(sheetName=NULL){
  if (is.null(sheetName)){sheetName=NA}
  SHEET = SHEET$new()
  SHEET$sheetName=sheetName
  return(SHEET)
}


#' Title
#'
#' @param x todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
dim.ODSsheet <- function(x){c(x$nrow(),x$ncol())}

#' Title
#'
#' @param sheet todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
preview <- function(sheet){return(invisible(sheet$View()))}





#~~~~~~~~~~~~~~~~~~~
## 3. Functions ####
#~~~~~~~~~~~~~~~~~~~

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


#~~~~~~~~~~~~~~~~~
## 4. Writing ####
#~~~~~~~~~~~~~~~~~



#' Title
#'
#' @param sheet todo
#' @param file todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_write <- function(sheet, file="file.ods"){

  # due to NSE notes in R CMD check (see vignette of datatable-importing):
  bold=cellcolor=color=column=data_float=data_string=font=NULL
  fromColumn=fromRow=hAlign=italic=margin=par1=par2=rotate=NULL
  size=styleName=styleNumber=toColumn=toRow=type=underline=vAlign=wrap=NULL

  if (!"ODSsheet" %in% class(sheet)){stop("'sheet' must be an ODSsheet")}

  #~~~~~~~~~~~~~~~~~~~~~~~~~
  ### Preparing the data ###
  #~~~~~~~~~~~~~~~~~~~~~~~~~

  sheet$cleanup()
  DEFAULTS=list(colWidth="1.7cm", rowHeight="15pt", font="Calibri", size=11,
                color="black", bold=FALSE, italic=FALSE, underline=FALSE,
                cellcolor="transparent", vAlign="bottom", hAlign="left",
                margin=0, wrap=FALSE, rotate=0, digits=NA)

  # 0.1 NA style slots  ####

  # We will create the table "AA_stylesTable"
  AA_stylesTable=data.table::copy(sheet$stylesTable)
  AA_stylesTable[,styleName:=paste0("style", seq_len(.N))]

  # Replace missing with default values:
  AA_stylesTable[,color:=.col2hex(color)]
  AA_stylesTable[,cellcolor:=.col2hex(cellcolor)]
  for (attribute in colnames(AA_stylesTable)) {
    AA_stylesTable[is.na(get(attribute)), (attribute) := DEFAULTS[[attribute]]]
  }
  AA_stylesTable[,size:=paste0(size,"pt")]
  AA_stylesTable[,margin:=paste0(margin,"cm")]


  # 0.2 cells information ####
  AA_cellsContent=data.table::copy(sheet$cellsContent)
  AA_cellsContent[,styleName:=paste0("style",styleNumber)]
  AA_cellsContent=merge(AA_cellsContent,AA_stylesTable[,c("styleName","digits")])

  # 0.3 special Cells  ####

  AA_specialCells= data.table::data.table(
    row = integer(),
    column = integer(),
    type = character(), ## currently "mergeStart" or "covered"
    par1 = integer(),
    par2 = integer()
  )

  ## 0.3.1: merged Cells ####

  mc=data.table::data.table(data.table::copy(sheet$mergedCells))
  mc[,row:=fromRow]
  mc[,column:=fromColumn]
  mc[,type:="mergeStart"]
  mc[,par1:=toRow-fromRow+1]       ## height
  mc[,par2:=toColumn-fromColumn+1] ## width
  mc[, c("fromRow", "toRow", "fromColumn", "toColumn") := NULL]
  AA_specialCells=rbind(AA_specialCells,mc,fill=T)

  ## 0.3.2: covered Cells ####

  mc=sheet$mergedCells
  for (i in seq_len(nrow(mc))){
    r=mc[i,"fromRow"]:mc[i,"toRow"]
    c=mc[i,"fromColumn"]:mc[i,"toColumn"]

    tmp=data.table::data.table(
      row=rep(r,each=length(c)),
      column=rep(c,times=length(r)),
      type="covered"
    )
    tmp=tmp[-1,]
    AA_specialCells=rbind(AA_specialCells,tmp,fill=TRUE)
  }

  # 0.4 col/row styles ####

  ## defining row and column styles
  colWidths=c(sheet$colWidths,NA) #not elegant, but works well
  colWidths[is.na(colWidths)]=DEFAULTS[["colWidth"]]
  AA_colStylesDef=matrix(nrow=length(unique(colWidths)),ncol=2,dimnames=list(NULL, c("colStyleName", "width")))
  AA_colStylesDef[,"colStyleName"]=paste0("co",seq_len(nrow(AA_colStylesDef)))
  AA_colStylesDef[,"width"]=unique(colWidths)
  lookup <- stats::setNames(AA_colStylesDef[, "colStyleName"], AA_colStylesDef[, "width"])
  AA_colStyle <- unname(lookup[colWidths])

  maxROW=max(AA_specialCells[,row],AA_cellsContent[,row],length(sheet$rowHeights),0) ## later redefined
  rowHeights=c(sheet$rowHeights,NA) #not elegant, but works well
  if (length(rowHeights)<maxROW){length(rowHeights)=maxROW}
  rowHeights[is.na(rowHeights)]=DEFAULTS[["rowHeight"]]
  AA_rowStylesDef=matrix(nrow=length(unique(rowHeights)),ncol=2,dimnames=list(NULL, c("rowStyleName", "height")))
  AA_rowStylesDef[,"rowStyleName"]=paste0("ro",seq_len(nrow(AA_rowStylesDef)))
  AA_rowStylesDef[,"height"]=unique(rowHeights)
  lookup <- stats::setNames(AA_rowStylesDef[, "rowStyleName"], AA_rowStylesDef[, "height"])
  AA_rowStyle <- unname(lookup[rowHeights])



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### From here on, we only access these "AA_..." variables ###
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (FALSE){## debug
    AA_cellsContent<<-AA_cellsContent
    AA_specialCells<<-AA_specialCells
    AA_stylesTable <<-AA_stylesTable
    AA_rowStyle<<-AA_rowStyle
    AA_colStyle<<-AA_colStyle
    AA_colStylesDef<<-AA_colStylesDef
    AA_rowStylesDef<<-AA_rowStylesDef
  }


  # 1.1 mimetype  ####
  {
    LOCAL_MIMETYPE="application/vnd.oasis.opendocument.spreadsheet"
  }


  # 1.2 manifest  ####
  {
    doc <- xml2::xml_new_root(
      "manifest:manifest",
      `xmlns:manifest` =
        "urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"
    )

    xml2::xml_add_child(
      doc,
      "manifest:file-entry",
      `manifest:full-path` = "/",
      `manifest:media-type` =
        "application/vnd.oasis.opendocument.spreadsheet"
    )

    xml2::xml_add_child(
      doc,
      "manifest:file-entry",
      `manifest:full-path` = "styles.xml",
      `manifest:media-type` = "text/xml"
    )

    xml2::xml_add_child(
      doc,
      "manifest:file-entry",
      `manifest:full-path` = "content.xml",
      `manifest:media-type` = "text/xml"
    )

    xml2::xml_add_child(
      doc,
      "manifest:file-entry",
      `manifest:full-path` = "meta.xml",
      `manifest:media-type` = "text/xml"
    )

    LOCAL_MANIFEST=doc
  }

  # 1.3 meta  ####
  {
    doc <- xml2::xml_new_root(
      "office:document-meta",
      `xmlns:office` = "urn:oasis:names:tc:opendocument:xmlns:office:1.0",
      `xmlns:meta`   = "urn:oasis:names:tc:opendocument:xmlns:meta:1.0"
    )

    xml2::xml_add_child(doc, "office:meta")

    LOCAL_META=doc
  }

  # 1.4 styles  ####
  ## currently, we only define the number styles
  {
    doc <- xml2::xml_new_root(
      "office:document-styles",
      `xmlns:table`  = "urn:oasis:names:tc:opendocument:xmlns:table:1.0",
      `xmlns:office` = "urn:oasis:names:tc:opendocument:xmlns:office:1.0",
      `xmlns:text`   = "urn:oasis:names:tc:opendocument:xmlns:text:1.0",
      `xmlns:style`  = "urn:oasis:names:tc:opendocument:xmlns:style:1.0",
      `xmlns:draw`   = "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0",
      `xmlns:fo`     = "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0",
      `xmlns:xlink`  = "http://www.w3.org/1999/xlink",
      `xmlns:dc`     = "http://purl.org/dc/elements/1.1/",
      `xmlns:number` = "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0",
      `xmlns:svg`    = "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0",
      `xmlns:of`     = "urn:oasis:names:tc:opendocument:xmlns:of:1.2",
      `office:version` = "1.4"
    )


    ffd <- xml2::xml_add_child(doc, "office:font-face-decls")
    xml2::xml_add_child(
      ffd,
      "style:font-face",
      `style:name`      = DEFAULTS[["font"]],
      `svg:font-family` = DEFAULTS[["font"]]
    )


    styles <- xml2::xml_add_child(doc, "office:styles")

    # Number style N0
    ns0 <- xml2::xml_add_child(
      styles,
      "number:number-style",
      `style:name` = "N0"
    )
    xml2::xml_add_child(
      ns0,
      "number:number",
      `number:min-integer-digits` = "1"
    )

    ## for each style, add the number style:
    for (i in seq_len(nrow(AA_stylesTable))){
      ns <- xml2::xml_add_child(
        styles,
        "number:number-style",
        `style:name` = paste0("N",i)
      )
      nsc <- xml2::xml_add_child(
        ns,
        "number:number",
        `number:min-integer-digits` = "1"
      )
      if (!is.na(AA_stylesTable[i,digits])){
        xml2::xml_set_attr(nsc, "number:min-decimal-places",AA_stylesTable[i,digits])
        xml2::xml_set_attr(nsc, "number:decimal-places",AA_stylesTable[i,digits])
      }
    }


    # Default table-cell style
    default_style <- xml2::xml_add_child(
      styles,
      "style:style",
      `style:name` = "Default",
      `style:family` = "table-cell",
      `style:data-style-name` = "N0"
    )

    xml2::xml_add_child(
      default_style,
      "style:table-cell-properties",
      `style:vertical-align` = "automatic",
      `fo:background-color` = "transparent"
    )

    xml2::xml_add_child(
      default_style,
      "style:text-properties",
      `fo:color`               =DEFAULTS[["color"]],
      `style:font-name`        =DEFAULTS[["font"]],
      `style:font-name-asian`  =DEFAULTS[["font"]],
      `style:font-name-complex`=DEFAULTS[["font"]],
      `fo:font-size`           =paste0(DEFAULTS[["size"]],"pt"),
      `style:font-size-asian`  =paste0(DEFAULTS[["size"]],"pt"),
      `style:font-size-complex`=paste0(DEFAULTS[["size"]],"pt")
    )

    # automatic-styles
    auto <- xml2::xml_add_child(doc, "office:automatic-styles")

    pm1 <- xml2::xml_add_child(
      auto,
      "style:page-layout",
      `style:name` = "pm1"
    )

    xml2::xml_add_child(
      pm1,
      "style:page-layout-properties",
      `fo:margin-top` = "0.3in",
      `fo:margin-bottom` = "0.3in",
      `fo:margin-left` = "0.7in",
      `fo:margin-right` = "0.7in",
      `style:table-centering` = "none",
      `style:print` = "objects charts drawings"
    )

    hs <- xml2::xml_add_child(pm1, "style:header-style")
    xml2::xml_add_child(
      hs,
      "style:header-footer-properties",
      `fo:min-height` = "0.45in",
      `fo:margin-left` = "0.7in",
      `fo:margin-right` = "0.7in",
      `fo:margin-bottom` = "0in"
    )

    fs <- xml2::xml_add_child(pm1, "style:footer-style")
    xml2::xml_add_child(
      fs,
      "style:header-footer-properties",
      `fo:min-height` = "0.45in",
      `fo:margin-left` = "0.7in",
      `fo:margin-right` = "0.7in",
      `fo:margin-top` = "0in"
    )

    # master-styles
    masters <- xml2::xml_add_child(doc, "office:master-styles")

    mp1 <- xml2::xml_add_child(
      masters,
      "style:master-page",
      `style:name` = "mp1",
      `style:page-layout-name` = "pm1"
    )

    xml2::xml_add_child(mp1, "style:header")
    xml2::xml_add_child(mp1, "style:header-left", `style:display` = "false")
    xml2::xml_add_child(mp1, "style:header-first")
    xml2::xml_add_child(mp1, "style:footer")
    xml2::xml_add_child(mp1, "style:footer-left", `style:display` = "false")
    xml2::xml_add_child(mp1, "style:footer-first")
    LOCAL_STYLES=doc
  }

  # 1.5 content  ####
  {
    doc <- xml2::xml_new_root(
      "office:document-content",
      `xmlns:table`  = "urn:oasis:names:tc:opendocument:xmlns:table:1.0",
      `xmlns:office` = "urn:oasis:names:tc:opendocument:xmlns:office:1.0",
      `xmlns:text`   = "urn:oasis:names:tc:opendocument:xmlns:text:1.0",
      `xmlns:style`  = "urn:oasis:names:tc:opendocument:xmlns:style:1.0",
      `xmlns:draw`   = "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0",
      `xmlns:fo`     = "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0",
      `xmlns:xlink`  = "http://www.w3.org/1999/xlink",
      `xmlns:dc`     = "http://purl.org/dc/elements/1.1/",
      `xmlns:number` = "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0",
      `xmlns:svg`    = "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0",
      `xmlns:of`     = "urn:oasis:names:tc:opendocument:xmlns:of:1.2",
      `office:version` = "1.4"
    )

    # <office:font-face-decls>
    ff <- xml2::xml_add_child(doc, "office:font-face-decls")
    xml2::xml_add_child(ff, "style:font-face",
                       `style:name`     =DEFAULTS[["font"]],
                       `svg:font-family`=DEFAULTS[["font"]]
    )

    # <office:automatic-styles>
    as <- xml2::xml_add_child(doc, "office:automatic-styles")

    # ce1 (table-cell)
    xml2::xml_add_child(as, "style:style",
                  `style:name` = "ce1",
                  `style:family` = "table-cell",
                  `style:parent-style-name` = "Default",
                  `style:data-style-name` = "N0"
    )


    # COLUMN STYLES ARE DEFINED:
    co0 <- xml2::xml_add_child(as, "style:style",
                         `style:name` = "co0",
                         `style:family` = "table-column")
    xml2::xml_add_child(co0, "style:table-column-properties",
                  `fo:break-before` = "auto",
                  `style:column-width` = DEFAULTS[["colWidth"]])

    for (i in seq_len(nrow(AA_colStylesDef))){
      col <- xml2::xml_add_child(as, "style:style",
                           `style:name` = AA_colStylesDef[i,"colStyleName"],
                           `style:family` = "table-column")
      xml2::xml_add_child(col, "style:table-column-properties",
                    `fo:break-before` = "auto",
                    `style:column-width` = AA_colStylesDef[i,"width"])
    }



    # ROW STYLES ARE DEFINED:
    ro0 <- xml2::xml_add_child(as, "style:style",
                         `style:name` = "ro0",
                         `style:family` = "table-row"
    )
    xml2::xml_add_child(ro0, "style:table-row-properties",
                  `fo:break-before` = "auto",
                  `style:row-height` = DEFAULTS[["rowHeight"]],
                  `style:use-optimal-row-height` = "true"
    )

    for (i in seq_len(nrow(AA_rowStylesDef))){
      row <- xml2::xml_add_child(as, "style:style",
                           `style:name` = AA_rowStylesDef[i,"rowStyleName"],
                           `style:family` = "table-row")
      node <- xml2::xml_add_child(row, "style:table-row-properties",
                            `fo:break-before` = "auto",
                            `style:row-height` = AA_rowStylesDef[i,"height"])
      ## xml2::xml_set_attr(node, "style:use-optimal-row-height","true") ## this is just an experiment
      ## there exists the attribute 'style:use-optimal-row-height="true"' ... is this relevant?
      ## so far, I didn't find a difference
    }


    ## MAIN MODIFICATION: ADDING STYLES
    for (i in seq_len(nrow(AA_stylesTable))){
      xxx<- xml2::xml_add_child(as, "style:style",
                          `style:name` = AA_stylesTable[i,styleName],
                          `style:family` = "table-cell",
                          `style:parent-style-name` = "Default",
                          `style:data-style-name` = paste0("N",i)
      )

      ## this node is not always necessary... lets see if i can just include it always...
      ## also, there would be style:repeat-content="false" when we deal with alignments... I dont know what that is
      node <-xml2::xml_add_child(xxx, "style:table-cell-properties",
                           `style:vertical-align` = AA_stylesTable[i,vAlign])

      if (AA_stylesTable[i,cellcolor]!="transparent"){xml2::xml_set_attr(node, "fo:background-color",AA_stylesTable[i,cellcolor])}
      if (AA_stylesTable[i,wrap]     ==TRUE         ){xml2::xml_set_attr(node, "fo:wrap-option","wrap")}
      if (AA_stylesTable[i,rotate]   !=0            ){xml2::xml_set_attr(node, "style:rotation-angle",AA_stylesTable[i,rotate])}

      ## this node is not always necessary...
      if (AA_stylesTable[i,hAlign]=="middle"){
        node <-xml2::xml_add_child(xxx, "style:paragraph-properties",
                             `fo:text-align` = "center")}
      if (AA_stylesTable[i,hAlign]=="left"){
        node <-xml2::xml_add_child(xxx, "style:paragraph-properties",
                             `fo:text-align` = "start",
                             `fo:margin-left`= AA_stylesTable[i,margin])}
      if (AA_stylesTable[i,hAlign]=="right"){
        node <-xml2::xml_add_child(xxx, "style:paragraph-properties",
                             `fo:text-align`  ="end",
                             `fo:margin-right`=AA_stylesTable[i,margin])}


      node <-xml2::xml_add_child(xxx, "style:text-properties",
                           `fo:color` = AA_stylesTable[i,color],
                           `style:font-name`         = AA_stylesTable[i,font],
                           `style:font-name-asian`   = AA_stylesTable[i,font],
                           `style:font-name-complex` = AA_stylesTable[i,font],
                           `style:font-size`         = AA_stylesTable[i,size],
                           `style:font-size-asian`   = AA_stylesTable[i,size],
                           `style:font-size-complex` = AA_stylesTable[i,size]
      )

      if (AA_stylesTable[i,bold]){
        xml2::xml_set_attr(node, "fo:font-weight",           "bold")
        xml2::xml_set_attr(node, "style:font-weight-asian",  "bold")
        xml2::xml_set_attr(node, "style:font-weight-complex","bold")
      }
      if (AA_stylesTable[i,italic]){
        xml2::xml_set_attr(node, "fo:font-style",           "italic")
        xml2::xml_set_attr(node, "style:font-style-asian",  "italic")
        xml2::xml_set_attr(node, "style:font-style-complex","italic")
      }
      if (AA_stylesTable[i,underline]){
        xml2::xml_set_attr(node, "style:text-underline-style", "solid")
        xml2::xml_set_attr(node, "style:text-underline-type", "single")
      }
    }




    # ta1 (table) + its properties
    ta1 <- xml2::xml_add_child(as, "style:style",
                         `style:name` = "ta1",
                         `style:family` = "table",
                         `style:master-page-name` = "mp1"
    )
    xml2::xml_add_child(ta1, "style:table-properties",
                  `table:display` = "true",
                  `style:writing-mode` = "lr-tb"
    )


    body <- xml2::xml_add_child(doc, "office:body")
    ss <- xml2::xml_add_child(body, "office:spreadsheet")

    # calculation-settings
    xml2::xml_add_child(ss, "table:calculation-settings",
                  `table:case-sensitive` = "false",
                  `table:search-criteria-must-apply-to-whole-cell` = "true",
                  `table:use-wildcards` = "true",
                  `table:use-regular-expressions` = "false",
                  `table:automatic-find-labels` = "false"
    )

    # <table:table table:name="Tabelle1" table:style-name="ta1">
    tbl <- xml2::xml_add_child(ss, "table:table",
                         `table:name` = ifelse(is.na(sheet$sheetName),"Tabellle1",sheet$sheetName),
                         `table:style-name` = "ta1"
    )

    # Define all columns (very inefficient for now)
    for (col in seq_along(AA_colStyle)){
      xml2::xml_add_child(tbl, "table:table-column",
                    `table:style-name` = AA_colStyle[col],
                    `table:default-cell-style-name` = "ce1")
    }
    xml2::xml_add_child(tbl, "table:table-column",
                  `table:style-name` = "co0",
                  `table:number-columns-repeated` = 2^14-length(AA_colStyle),
                  `table:default-cell-style-name` = "ce1")


    # Here we fill every cell:
    maxROW=max(AA_specialCells[,row],AA_cellsContent[,row],length(AA_rowStyle),0)

    for (rowNr in seq_len(maxROW)){
      ### <progress bar> ###
      if (rowNr==1){progress=-1}
      if (round(100*rowNr/maxROW) > progress){
        round(100*rowNr/maxROW)-> progress # :-)
        p=round(progress/5)
        cat("|",rep("=",p),">",rep(" ",40-2*p),"<",rep("=",p),"|  ",progress,"%\r",sep = "")
      }
      if (rowNr==maxROW){cat("|",rep("=",42),"|  100%\n",sep = "")}
      ### </progress bar> ###

      row <- xml2::xml_add_child(tbl, "table:table-row",`table:style-name` = AA_rowStyle[rowNr])

      maxCOL=max(AA_specialCells[row==rowNr,column],AA_cellsContent[row==rowNr,column],0)

      for (colNr in seq_len(maxCOL)){
        special    =AA_specialCells[row==rowNr & column==colNr,]
        cellContent=AA_cellsContent[row==rowNr & column==colNr,]

        if (nrow(special)==1){
          if (special$type=="covered"){ ## covered cell:
            xml2::xml_add_child(row, "table:covered-table-cell",
                          `table:number-columns-repeated` = "1")
            next
          }
        }


        if (nrow(cellContent)==0){ ## add empty cell
          cell <- xml2::xml_add_child(row, "table:table-cell",
                                `table:style-name` = "ce1")
        } else {
          cell <- xml2::xml_add_child(row, "table:table-cell",
                                `office:value-type`= cellContent[,type],
                                `table:style-name` = cellContent[,styleName])

          if (cellContent[,type]=="float"){
            xml2::xml_set_attr(cell, "office:value", as.character(cellContent[,data_float]))
            p <- xml2::xml_add_child(cell, "text:p")
            digits=cellContent$digits
            if (is.na(digits)){
              xml2::xml_set_text(p, as.character(cellContent[,data_float]))
            } else {
              xml2::xml_set_text(p, .round2character(cellContent[,data_float],digits))
            }
          }

          if (cellContent[,type]=="string"){
            p <- xml2::xml_add_child(cell, "text:p")
            xml2::xml_set_text(p, cellContent[,data_string])
          }


        }

        if (nrow(special)==1){
          if (special$type=="mergeStart"){
            xml2::xml_set_attr(cell, "table:number-rows-spanned"   ,special[,par1])
            xml2::xml_set_attr(cell, "table:number-columns-spanned",special[,par2])
          }
        }

      }

      # finish the row:
      xml2::xml_add_child(row, "table:table-cell",`table:number-columns-repeated` = 2^14-maxCOL)

    }


    # Remaining rows as empty
    row2 <- xml2::xml_add_child(tbl, "table:table-row",
                          `table:number-rows-repeated` = 2^20-maxROW,
                          `table:style-name` = "ro0"
    )
    xml2::xml_add_child(row2, "table:table-cell",
                  `table:number-columns-repeated` = 2^14
    )













    LOCAL_CONTENT=doc
    ## DEBUG:
    if (FALSE){cat("\n\n\n",as.character(LOCAL_STYLES, options = "format"))}
    if (FALSE){cat("\n\n\n",as.character(LOCAL_CONTENT, options = "format"))}
  }


  # 2.1 writing  ####
  {
    tempdir_ods=tempfile("prettyods_")
    dir.create(tempdir_ods)
    on.exit(unlink(tempdir_ods, recursive=TRUE), add=TRUE)
    dir.create(file.path(tempdir_ods,"META-INF"))

    xml2::write_xml(LOCAL_MANIFEST,file.path(tempdir_ods,"META-INF","manifest.xml"))
    xml2::write_xml(LOCAL_CONTENT ,file.path(tempdir_ods,"content.xml"))
    xml2::write_xml(LOCAL_META    ,file.path(tempdir_ods,"meta.xml"))
    write(          LOCAL_MIMETYPE,file.path(tempdir_ods,"mimetype"))
    xml2::write_xml(LOCAL_STYLES  ,file.path(tempdir_ods,"styles.xml"))


    ## 1. zipping the files
    temp_ods=tempfile(fileext=".ods")
    result=try({
      zip::zip(zipfile=temp_ods,files=list.files(tempdir_ods,recursive=T),
               include_directories=FALSE,recurse=TRUE,
               mode="cherry-pick",root=tempdir_ods)
    },silent=T)

    if (inherits(result,"try-error")){
      stop("I am very sorry, but something went wrong while creating the ODS file")
    }
    on.exit(unlink(temp_ods, recursive=TRUE), add=TRUE)



    ## 2. moving to the correct location
    result=suppressWarnings(file.copy(temp_ods,file,overwrite=TRUE))
    if (!result){
      stop(paste0("I am very sorry, but it seems that ",file," is currently open:("))
    }
  }
}
