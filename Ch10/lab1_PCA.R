row.names(USArrests)
names(USArrests)

apply(X = USArrests, MARGIN = 2, FUN = mean)
apply(X = USArrests, MARGIN = 2, FUN = var)

pr.out <- prcomp(x = USArrests, scale. = TRUE)

names(pr.out)

pr.out$scale
pr.out$center
pr.out$rotation
biplot(x = pr.out, scale = 0)

pr.var <- pr.out$sdev^2
pve <- pr.var / sum(pr.var)

plot(pve, xlab = "Principal Components", ylab = "Proportion of Variance Explained", 
     ylim = c(0,1), type = "b")


plot(cumsum(pve), xlab = "Principal Components", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0,1), type = "b")
