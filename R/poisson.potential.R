# Poisson potential
#
# 
# Compute the attractivity potential on a grid from balance value on polygon following the method of Tobler.
# TTT equivalent of the potflow program of Waldo Tobler
# 'POTENTIAL FIELD FROM DATA GIVEN BY POLYGONS CONVERTED
# 'TO A LATTICE USING THE POLYGRID PROGRAM.
# 'COMPUTED AS THE SOLUTION TO THE POISSON EQUATION
# 'WITH BOUNDARY CONDITION DZ/DN=0.
# 'SUMMER 1978, REVISED, CONVERTED TO BASIC, SPRING 1994. Rev 6/8/98
#' \code{poisson.potential} returns an sf
#'
#' @param xsf an sf data.frame of polygons
#' @param varname name of the feature where the balance of each polygons are stored 
#' @param method resolution method for poisson equation either jacobi or solve
#' @param nb_it number of iteration for jacobi resolution
#' @param cellsize size of the 
#' @return an sf data.frame with
#' @examples
#' @export
poisson.potential <- function(xsf,varname="delta",method="jacobi",nb_it = 1000,cellsize=sqrt((sf::st_bbox(xsf)[3]-sf::st_bbox(xsf)[1])*(sf::st_bbox(xsf)[4]-sf::st_bbox(xsf)[2])/2500)) {

  # grid construction
  cat("Building grid\n")
  # ! warning make grid numbering start in bottom right corner and by rows
  nc = ceiling((sf::st_bbox(xsf)[3]-sf::st_bbox(xsf)[1])/cellsize)
  nr = ceiling((sf::st_bbox(xsf)[4]-sf::st_bbox(xsf)[2])/cellsize)
  #grid = sf::st_make_grid(xsf,cellsize = cellsize,n=c(nc,nr))
  grid = sf::st_make_grid(offset =sf::st_bbox(xsf)[1:2],cellsize = cellsize,n=c(nc,nr),crs=sf::st_crs(xsf))
  
  g_size = length(grid)

  # find neigboors indices
  ijp=ij(1:g_size,nr,nc)
  up_nei=cbind(ijp[,1]+1,ijp[,2])
  up_nei[up_nei[,1]>nr,]=NA
  up_ind = iv(up_nei,nr,nc)


  left_nei=cbind(ijp[,1],ijp[,2]-1)
  left_nei[left_nei[,2]<1, ]=NA
  left_ind = iv(left_nei,nr,nc)

  bottom_nei=cbind(ijp[,1]-1,ijp[,2])
  bottom_nei[bottom_nei[,1]<1,]=NA
  bottom_ind = iv(bottom_nei,nr,nc)

  right_nei=cbind(ijp[,1],ijp[,2]+1)
  right_nei[right_nei[,2]>nc,]=NA
  right_ind = iv(right_nei,nr,nc)

  neib = cbind(up_ind,left_ind,bottom_ind,right_ind)

  #check
  # i=150
  # plot(grid)
  # plot(grid[i],add=TRUE,col='green')
  # plot(grid[neib[i,]],add=TRUE,col='red')
  # ! check and handdle multiple polygons inside grid units .... 
  # affectation and grid checking
  net = sf::st_centroid(grid)
  # st_is_within_distance allow for points on boundary
  #affect = unlist(lapply(sf::st_is_within_distance(net,xsf,cellsize/10),function(polid){ifelse(length(polid)<1,NA,polid[1])}))
  affect = unlist(lapply(sf::st_within(net,xsf),function(polid){ifelse(length(polid)<1,NA,polid[1])}))
  # remove lone pixels
  ind_in=which(!is.na(affect))
  neib_in = neib[ind_in,]
  neib_in[apply(neib_in,2,function(col){is.na(affect[col])})]=NA
  nb_prune=0
  # revoir la condition (na(left) & na(right)) | (na(up) & na(bottom)) 
  while(sum(is.disconnected(neib_in))>0){
    if(nb_prune==0){
      cat("Cleanning borders\n")
    }
    nb_prune=nb_prune+1
    affect[ind_in[is.disconnected(neib_in)]]=NA
    ind_in=which(!is.na(affect))
    neib_in = neib[ind_in,]
    neib_in[apply(neib_in,2,function(col){is.na(affect[col])})]=NA
  }
  # update grid
  grid_in=grid[ind_in]
  ij_in = ijp[ind_in,]
  # update neigboors indices in cleansed grid
  neib_in=apply(neib_in,2,function(col){match(col,ind_in)})

  cat(paste0("Grid is build, it contains ",length(grid_in)," cells\n"))
  #check
  #i=1
  #plot(grid_in)
  #plot(grid_in[i],add=TRUE,col='green')
  #plot(grid_in[neib_in[i,]],add=TRUE,col='red')

  # build weights
  left_w = ifelse(is.na(neib_in[,"right_ind"]),-2,-1)
  right_w = ifelse(is.na(neib_in[,"left_ind"]),-2,-1)
  up_w = ifelse(is.na(neib_in[,"bottom_ind"]),-2,-1)
  bottom_w = ifelse(is.na(neib_in[,"up_ind"]),-2,-1)
  weights = cbind(up_w,left_w,bottom_w,right_w)
  weights[is.na(neib_in)]=NA
  # consitency check
  # all(rowSums(weights,na.rm = TRUE)==-4)

  # build matrix W
  # reecrire avec des rbind cbind
  ind_sys = lapply(1:length(grid_in),function(cnode){data.frame(i=cnode,j=neib_in[cnode,],w=weights[cnode,])})
  ind_W = do.call(rbind,ind_sys)
  ind_W = ind_W[!is.na(ind_W$j),]
  W = Matrix::sparseMatrix(ind_W[,"i"],ind_W[,"j"],x=ind_W[,"w"],dims=c(length(grid_in),length(grid_in)))
  # image(W)
  diag(W)=4


  # build vector b
  affect_in = affect[!is.na(affect)]
  # must find a better way !
  tot = table(affect_in)
  tot_v = rep(0,length(unique(affect_in)))
  tot_v[as.numeric(names(tot))]=tot
  vals = xsf[[varname]]
  b   = as.vector(vals[affect_in]/tot_v[affect_in])
  cat("Grid weighting performed\n")
  
  # different components (islands) ? keep the biggest one ! this is a continuous model
  G = igraph::graph_from_adjacency_matrix(abs(W+Matrix::t(W)))
  comp=igraph::components(G)
  if(comp$no>1){
    cat(paste0("Islands were found, keeping the biggest connected component only with ",comp$csize[1]," cells!\n"))
    W = W[comp$membership==1,comp$membership==1]
    b = b[comp$membership==1]
    grid_in = grid_in[comp$membership==1]
    # ! attention je dois mettre a jour nieb_in
    neib_in=neib_in[comp$membership==1,]
    ind_in=which(comp$membership==1)
    neib_in=apply(neib_in,2,function(col){match(col,ind_in)})
    ij_in=ij_in[comp$membership==1,]
  }
  
  
  cat("Solving poisson equations\n")
  if(method=="jacobi"){
    # solve by jacobi
    att = as.vector(rep(1,length(grid_in)))
    R=W
    diag(R)=0
    D=Matrix::sparseMatrix(1:length(grid_in),1:length(grid_in),x=1/4)
    for(i in 1:nb_it){
      att = D%*%(b-R%*%att)
      err = 0.5*sum((W%*%att-b)^2)/length(b)
      # handle constraints
      att = att-sum(att)/length(att)
      cat(paste0("mse: ",err,"\n"))
    }
  }
  if(method=="solve"){

    # solve
    ij = Matrix::which(W!=0,arr.ind=TRUE)
    wei = W[ij]
    Ncells = length(grid_in)
    ij = rbind(ij,cbind(c(rep(Ncells+1,Ncells),1:Ncells),c(1:Ncells,rep(Ncells+1,Ncells))))
    wei = c(wei,rep(1,Ncells*2))
    Wf = Matrix::sparseMatrix(ij[,1],ij[,2],x=wei,dims=c(Ncells+1,Ncells+1))
    bf=c(b,0)
    attf = Matrix::solve(Wf,bf)
    att = attf[1:Ncells]
    #att = att -sum(att)
  }
  
  cat("Post-processing\n")
  # results
  grid_in.sf=sf::st_sf(grid_in)
  grid_in.sf$i=ij_in[,1]
  grid_in.sf$j=ij_in[,2]
  grid_in.sf$attractivity = as.numeric(att)
  grid_in.sf$residuals = as.numeric(b-W%*%att)
  grid_in.sf$b = b

  #plot(grid_in.sf %>% select(attractivity))
  #plot(grid_in.sf %>% select(b))

  # compute the field
  grid_in.sf$dx = 0
  idx_ok = !is.na(neib_in[,"left_ind"]) & !is.na(neib_in[,"right_ind"])
  grid_in.sf$dx[idx_ok] = (grid_in.sf$attractivity[neib_in[idx_ok,"right_ind"]]-grid_in.sf$attractivity[neib_in[idx_ok,"left_ind"]])/2

  grid_in.sf$dy = 0
  idy_ok =  !is.na(neib_in[,"up_ind"]) & !is.na(neib_in[,"bottom_ind"])
  grid_in.sf$dy[idy_ok] = (grid_in.sf$attractivity[neib_in[idy_ok,"up_ind"]]-grid_in.sf$attractivity[neib_in[idy_ok,"bottom_ind"]])/2

  # extract x,y normalize dx,dy fro flows mapping
  xy = sf::st_coordinates(sf::st_centroid(grid_in))
  grid_in.sf$x=xy[,1]
  grid_in.sf$y=xy[,2]
  #grid_in.sf$dy = grid_in.sf$dy/max(abs(grid_in.sf$dy))*cellsize
  #grid_in.sf$dx = grid_in.sf$dx/max(abs(grid_in.sf$dx))*cellsize
  grid_in.sf
}

# utils functions to find neighboors indices on grid
# ii -> (i,j) i=1,j=1 coin gauche bas
ij=function(iv,nr,nc){
  cbind((iv-1)%/%nc+1,(iv-1)%%nc+1)
}
# (i,j) -> ii
iv=function(ij,nr,nc){
  ij[,2]+(ij[,1]-1)*nc
}

is.disconnected=function(nei){
  (is.na(nei[,1])&is.na(nei[,3])) | (is.na(nei[,2])&is.na(nei[,4]))
}


# utils to show the flows
#' @export
poisson.flows = function(sf,normfact=0.1){
  bb = st_bbox(sf)
  width = bb[3]-bb[1]
  height = bb[4]-bb[2]
  l = min(width,height)
  lw = sqrt(sf$dx^2+sf$dy^2)
  lw = lw/max(lw)
  arrows(sf$x,sf$y,sf$x+sf$dx*normfact,sf$y+sf$dy*normfact,length = 0.03,lwd=lw)
}

# utils to convert potential to raster
#' @export
rasterFromPotential = function(pot){
  att = rasterFromXYZ(cbind(pot$x,pot$y,pot$attractivity),crs=sf:st_crs(pot)) 
}

#' @export
balance = function(OD){
  if(nrow(OD)!=ncol(OD)){
    stop("OD matrix must be square")
  }else{
    balance = colSums(OD)-rowSums(OD)
  }
  balance
}

#' @export
#' winds(pot,nbparticules = 1000,lifespan = 50,resolution = 0.2,scalefact = 0.1)
winds =function(pot, lifespan=5,scalefact=0.1,linewidth=1,nbparticules=500,resolution=sf::st_bbox(pot[1,])[3]-sf::st_bbox(pot[1,])[1]){
  inputs=list(data=pot,params=list(
    lifespan=lifespan,scalefact=scalefact,linewidth=linewidth,nbparticules=nbparticules,resolution=resolution          
    ))
  r2d3(inputs,script = "./d3/winds.js",container = "canvas",d3_version = 5)
}
  





