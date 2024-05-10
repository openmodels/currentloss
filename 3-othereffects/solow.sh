for ii in {1..10}
do
    Rscript solow.R
    rclone copy -uP ../../data "udel:Research/Current Losses/data"
done

