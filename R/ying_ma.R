#' Batch effect correction
#' @description Batch effect correction
#' @param object a data frame
#' @param method raw, combat, limma, mnn, harmony, cca
#' @param plot true, false
#' @param meta_data meta data with batch aand group
#' @param id the id column for future development
#' @param batch the batch to correct
#' @param group the meaningful group
#' @importFrom sva ComBat
#' @importFrom limma removeBatchEffect
#' @importFrom batchelor fastMNN
#' @importFrom harmony HarmonyMatrix
#' @importFrom ggbiplot ggbiplot
#' @importFrom stats prcomp
#' @import ggplot2
#' @export
table_correct_batch <- function(object,method,plot,meta_data,id=NULL,batch,group){
  if(is.null(id)){
    id = row.names(meta_data)
  }else{
    id = meta_data[[id]]
  }
  pca <- prcomp(t(object),scale. = T)
  p1 <- NULL
  p2 <- NULL
  if(method == "raw"){
  }
  if(method == "combat"){
    object <- sva::ComBat(dat = object, batch=meta_data[[batch]], mod=NULL, par.prior=TRUE, prior.plots=FALSE)
    pca <- prcomp(t(object),scale. = T)
  }
  if(method == "limma"){
    object <- limma::removeBatchEffect(object, meta_data[[batch]])
    pca <- prcomp(t(object),scale. = T)
  }
  if(method == "mnn"){
    n = length(table(meta_data[[batch]]))
    types = names(table(meta_data[[batch]]))
    if(n==3){
      tab_1 <- object[,which(colnames(object) %in% rownames(meta_data[which(meta_data[[batch]] == types[1]),]))]
      tab_2 <- object[,which(colnames(object) %in% rownames(meta_data[which(meta_data[[batch]] == types[2]),]))]
      tab_3 <- object[,which(colnames(object) %in% rownames(meta_data[which(meta_data[[batch]] == types[3]),]))]
      tab_fastMNN_object <- fastMNN(tab_1,tab_2,tab_3)
    }
    object <- as.data.frame(tab_fastMNN_object@assays@data$reconstructed)
    pca <- prcomp(t(object),scale. = T)
  }
  if(method == "harmony"){
    # harmony edit in pca level, hense the object didn't change
    tab_harmony <- HarmonyMatrix(pca$x,meta_data,vars_use = batch,do_pca = F)
    pca <- prcomp(tab_harmony,scale. = T)
  }
  if(plot){
    p1 <- ggbiplot(pca,obs.scale = 1,var.scale = 1,groups = meta_data[[batch]],ellipse = T,circle = T,var.axes = F)+scale_color_discrete(name="")+theme(legend.direction = "horizontal",legend.position = "top")
    p2 <- ggbiplot(pca,obs.scale = 1,var.scale = 1,groups = meta_data[[group]],ellipse = T,circle = T,var.axes = F)+scale_color_discrete(name="")+theme(legend.direction = "horizontal",legend.position = "top")
  }
  list <- list(object,p1,p2)
  return(list)
}

## CCA
#tab_cca_object <- CreateSeuratObject(counts = tab,meta.data = group,project = "cca",min.cells = 0,min.features = 0)
#tab_cca_list <- SplitObject(tab_cca_object, split.by = "batch")
#for (i in 1:length(tab_cca_list)) {
#  tab_cca_list[[i]] <- NormalizeData(tab_cca_list[[i]], verbose = FALSE)
#  tab_cca_list[[i]] <- FindVariableFeatures(tab_cca_list[[i]], selection.method = "vst", nfeatures = 2000,
#                                             verbose = FALSE)
#}
#tab_cca_anchors <- FindIntegrationAnchors(object.list = tab_cca_list, dims = 1:7,nn.method = "annoy")
#tab_cca_integrated <- IntegrateData(anchorset = tab_cca_list, dims = 1:30)
