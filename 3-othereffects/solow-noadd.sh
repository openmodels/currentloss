for ii in {1..10}
do
    Rscript solow-noadd.R
    rclone copy -uP ../../data "udel:Research/Current Losses/data"
done
