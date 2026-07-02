source("src/lib/distance.R")
source("src/lib/myPBSmapping.R")

shp <- importShapefile("data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
polydata <- attr(shp, 'PolyData')

cents <- calcCentroid(shp, rollup=2)
areas <- calcArea(shp, rollup=2)
centroids <- cents %>% left_join(areas, by=c('PID', 'SID')) %>% group_by(PID) %>%
    dplyr::summarize(X=X[which.max(area)], Y=Y[which.max(area)])

centroids$show <- F
for (PID in order(polydata$POP_EST, decreasing=T)) {
    dists <- gcd.slc(centroids$X[PID], centroids$Y[PID], centroids$X[centroids$show], centroids$Y[centroids$show])
    if (all(dists > 600))
        centroids$show[PID] <- T
}
centroids$show[centroids$X < -176] <- F
centroids$show[centroids$X > 176] <- F
centroids$show[centroids$Y < -50] <- F
centroids$show[centroids$Y > 65] <- F

shpl <- importShapefile("data/regions/ne_10m_land/ne_10m_land.shp")
