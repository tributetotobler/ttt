#' @title ttt_flowmapper_types: Compute gross and net flows
#' @description Compute gross (symmetric) and net (antisymetric) flows from initial asymetric matrix
#' @param tab is the input flow dataset
#' @param origin the place of origin code
#' @param destination the place of destination code
#' @param fij the flow value between origin and destination places
#' @param format specify the flow dataset format, "M " for square matrix or "L" for long [i,j,data]
#' @param type enter the flowtype : "gross" or "net". See Details.
#' @param lowup for extracting the lower or upper triangular sub-portion of the gross matrix. See Details.
#' @param net for extracting the "positive" or the "negative" flow values of the net matrix
#' @details 
#' Type of flow are:\cr 
#' - type = "gross" for building the gross matrix - as bilateral volum\cr
#' - type = "net" for building the net matrix - as bilateral balance\cr
#' Lowup part of the matrix are:\cr
#' - lowup ="up" for triangular part above the main diagonal \cr
#' - lowup = "low" for triangular part below the main diagonal\cr
#' @import dplyr
#' @import cartograflow
#' @importFrom rlang .data
#' @examples
#' #Example 1 : building gross flows
#' gross_up<-ttt_flowmapper_types(data, origin="i",destination="j",fij="fij", format="L", type="gross",lowup="up")\cr
#' #Example 2 : building net flows
#' net_positive <-ttt_flowmapper_types(data,origin="i",destination="j",fij="fij",format="L",type="net",net="positive")\cr 
#' @references  Waldo R. Tobler, 1979, A geographical flow mapping program, Geographical Analysis, n°13, Vol. 1, pp. 1-20
#' @export


ttt_flowmapper_types <- function(tab,origin=NULL,destination=NULL,fij=NULL,format,type ,lowup, net){
  
    if (format == "L"){
    
                      Fij<-Fij<-Fji<-NULL 
                      minFij<-Fij<-Fji<-NULL 
                      
                      f1 <- tab %>% select(origin,destination,fij)
                      names(f1) <- c("i", "j", "Fij")
                      
                      f2<-tab %>% select(destination,origin,fij)
                      names(f2) <- c("i", "j", "Fji")
                      
                      tabflow <- merge(f1, f2, by = c("i", "j"), all.X = TRUE, all.Y = TRUE)
                      
                      tabflow<- tabflow %>% 
                                mutate(FSij = .data$Fij + .data$Fji) %>% 
                                mutate(FBij = .data$Fji - .data$Fij)
                      
            
                      if (missing(type)) {message("You must choose a flow type: gross or net")}
    
                      if (type == "gross"){
                                    
                                    flow_gross <- tabflow %>% select(.data$i,.data$j,.data$FSij)
                                    
                                    tab_up<-cartograflow::flowtabmat(flow_gross,matlist = "M")
                                    temp_up<-lower.tri(tab_up, diag = FALSE)
                                    
                                    tab_low<-cartograflow::flowtabmat(flow_gross,matlist = "M")
                                    temp_low<-upper.tri(tab_low,diag=FALSE)
                                    
                                    nbi<-dim(tab_up)[1]
                                    nbj<-dim(tab_up)[2]
                        
                                    for (i in 1:nbi){
                                      for (j in 1:nbj){
                                        if (temp_up[i,j] == TRUE){tab_up[i,j]<-0 }
                                        if (temp_low[i,j] == TRUE){tab_low[i,j]<-0 }
                                        }}
                          
                                    tabflow_low<-cartograflow::flowtabmat(tab_low,matlist = "L")
                                    tabflow_up<-cartograflow::flowtabmat(tab_up,matlist = "L")
                     
                                    if(missing(lowup)){return(flow_gross)}
                                    
                                    if (lowup == "up"){
                                      return(tabflow_up)}
                                    
                                    if (lowup == "low"){
                                      return(tabflow_low)}    
                          }
      
                   
                   if (type == "net") {
                                           
                                  tabflow_net <- tabflow %>% select(.data$i,.data$j,.data$FBij)
                                
                                  if (missing(net)){return(tabflow_net)}
                     
                                  if (net == "negative"){
                                     tabflow_net <- tabflow_net %>% filter(.data$FBij<0)
                                     return(tabflow_net)}
                     
                                  if (net == "positive") {
                                     tabflow_net <- tabflow_net %>% filter(.data$FBij>=0)
                                     return(tabflow_net)}
                   }
}}