#' Calculate the urban vulnerability index for census data
#'
#'
#' @param CC A matrix or data.frame whose rows sum to one or one hundred.
#' @return A matrix composed of the log ratios values.
#' @examples
#' C = matrix(1:20,4)/rowSums(matrix(1:20,4))
#' logratio(C)
#' @export
reduction_dimension = function(ipc){

  ipc.N <- ipc%>%group_by(Cod_bairro)%>%mutate(id=paste0(Nome_do_bairro,"_",1:n()))%>%
  as.data.frame()

DD <- ipc.N%>%`rownames<-`(ipc.N$id)%>%dplyr::select(-c(Cod_bairro,ipc,id))


CC.M <- DD%>%dplyr::select(-Nome_do_bairro)%>% # tira a coluna da especie
  scale() %>%                 # normaliza os dados, mean zero and unit variance
  prcomp() ->                 # faz PCA
  pca


dados <- ipc.N%>%dplyr::select(c(Nome_do_bairro,ipc))%>%
  dplyr::mutate(pca=CC.M$x[,1]%>%as.numeric(),pca.ipc = (pca-min(pca))/(max(pca) - min(pca)) )%>%
  dplyr::select(-pca)%>%`colnames<-`(c("NOME","ipc","pca.ipc"))%>%
  dplyr::group_by(NOME)%>%
  summarise_at(c("pca.ipc","ipc"),mean)
return(dados)
}
