# points24: calculate 24 points by +-*/ and () only with 4 numbers ranging from 0 to 13
# dedicated to Danmin and other small friends, they like 24 points
# by Hua Tan on 9/4/2018 @8181
# 
# copyright (c), Hua Tan, warm.tan@gmail.com

library(combinat)

pts24 <- function(vec){
  l <- length(vec)
  op <- c("+","-","*","/")
  if(l<=2){
    x <- vec[1]
    y <- vec[2]
    res <- signif(c(x+y,x-y,x*y,x/y,y-x,y/x),10)
    ex <- paste0("(",x,op,y,")")
    ex[5] <- paste0("(",y,"-",x,")")
    ex[6] <- paste0("(",y,"/",x,")")
    res2 <- list(val=res,ex=ex)
    # print(res)
  }else{
    x <- vec[1]
    vec <- vec[2:l]
    yy <- pts24(vec)
    y <- yy[[1]] #value
    z <- yy[[2]] #expression
    res <- signif(c(x+y,x-y,x*y,x/y,y-x,y/x),10)
    ex <- unlist(lapply(op,function(i){paste0("(",x,i,z,")")}))
    ex <- c(ex,unlist(lapply(c("-","/"),function(i){
      paste0("(",z,i,x,")")
    })))
    res2 <- list(val=res,ex=ex)
    # print(res)
  }
  return(res2)
}

filterpts24 <- function(res2,pts=24){
  val <- res2$val
  ex <- res2$ex
  idx <- abs(val-pts)<1e-3
  if(sum(idx)>0){
    res2$val <- val[idx]
    res2$ex <- unlist(lapply(ex[idx],function(x){
      l <- nchar(x)
      substr(x,2,l-1)
    }))
  }else{
    res2 <- NULL
  }
  return(res2)
}

finalpts24 <- function(vec,pts=24,verbose=T){
  print("input: ")
  print(vec)
  
  allvec <- permn(vec)
  allvec <- allvec[!duplicated(allvec)]
  allres <- lapply(allvec,function(x){
    res2 <- pts24(x)
    res2 <- filterpts24(res2,pts)
  })
  l <- unlist(lapply(allres,length))
  allres <- allres[l>0]
  if(length(allres)>0){
    allres <- allres[!duplicated(allres)]
    allres <- lapply(allres,function(x){
      x$val <- round(x$val)
      x})
    names(allres) <- seq(1,length(allres))
    print(paste0("# hit: ",length(allres)))
    if(verbose){
      print(unlist(allres))
    }
  }else{
    print(paste0("EMPTY, ",paste(letters[c(9,12,15,22,5,4,13)],collapse = "")))
  }
  return(allres)
}
vec <- c(1,5,5,5)
vec <- c(4,4,10,10)
vec <- c(3,3,5,5)
vec <- c(3,3,4,4)
vec <- c(3,3,7,7)
vec <- c(1,2,7,7)
vec <- c(1,4,5,6)
vec <- c(3,3,8,8) 
vec <- c(5,6,7,9)
vec <- sample(seq(0,13),4)
vec <- c(1,9,8,3)
vec <- c(9,6,12,11)
vec <- c(1,11,11,13)
vec <- c(1,11,13,13)
vec <- c(1,11,12,12)
allres <- finalpts24(vec,24)


# # ###-----
# # vec <- c(1,5,5,5)
# # vec <- c(4,4,10,10)
# # vec <- c(3,3,7,7)
# # vec <- c(1,2,7,7)
# # vec <- c(2,2,2,9)
# # vec <- c(2,7,8,9)
# # vec <- c(6,9,9,10) 
# # vec <- c(2,5,5,10)
# # vec <- c(1,4,5,6)
# # vec <- c(3,3,8,8) 
# # #################################
# N <- 13
# counter <- 1
# allcmb <- matrix(NA,13^4,4)
# for(i in 1:N){
#   for (j in 1:N){
#     for (k in 1:N){
#       for (l in 1:N){
#         allcmb[counter,] <- c(i,j,k,l)
#         counter <- counter+1
#       }
#     }
#   }
# } #28561*4
# allcmbord <- apply(allcmb,1,function(x){
#   sort(x)
# })#4*28561
# allcmbord <- t(allcmbord) #28561*4
# allcmbrd <- allcmbord[!duplicated(allcmbord),] #1820*4
# ##--
# allres_list <- apply(allcmbrd,1,function(x){finalpts24(x,24,F)})
# l <- unlist(lapply(allres_list,length))
# only1 <- allres_list[l==1] #162
# only1_tab <- allcmbrd[l==1,]
# ex <- unlist(lapply(only1,function(x){
#   x[[1]]$ex[[1]]
# }))
# only1_tab_ <- data.frame(only1_tab,stringsAsFactors = F)
# only1_tab_$ex <- ex
# write.table(only1_tab_,file = "pts24_only1_tab_.txt",sep = " ",quote = F,row.names = F)
# write.table(only1_tab,file = "pts24_only1_tab.txt",sep = " ",quote = F,row.names = F)
# # N <- choose(13,4)+3*choose(13,3)+3*choose(13,2)+choose(13,1)
# # # 1820

