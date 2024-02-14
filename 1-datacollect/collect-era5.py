import xarray as xr
import geopandas as gpd
import xagg as xa
import cdsapi

do_popweight = True

c = cdsapi.Client()

weightmap0 = None
weightmap1 = None
cols2keep = ['poly_idx', 'time', 'ADMIN', 'ADM0_A3', 't2m', 't2mmin', 't2mmax']

for year in range(1940, 2024):
    print(year)
    c.retrieve(
        'reanalysis-era5-single-levels',
        {
            'product_type': 'reanalysis',
            'variable': '2m_temperature',
            'year': str(year),
            'month': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
            ],
            'day': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
                '13', '14', '15',
                '16', '17', '18',
                '19', '20', '21',
                '22', '23', '24',
                '25', '26', '27',
                '28', '29', '30',
                '31',
            ],
            'time': [
                '00:00', '03:00', '06:00',
                '09:00', '12:00', '15:00',
                '18:00', '21:00',
            ],
            'format': 'netcdf',
        },
        'download.nc')
    
    ## Get max, min, mean
    with xr.open_dataset("download.nc") as ds:
        dsday = ds.resample(time='1D').mean()
        dsday['t2mmin'] = ds.t2m.resample(time='1D').min()
        dsday['t2mmax'] = ds.t2m.resample(time='1D').max()
        # dsday.to_netcdf("era5-t2m-" + str(year) + ".nc4")
        
        if weightmap0 is None:
            gdf_regions = gpd.read_file("../../data/regions/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
            if do_popweight:
                ds_pop = xr.open_dataset('../../data/regions/gl_gpwv3_pcount_90_ascii_25/glp90ag.asc')
                weightmap0 = xa.pixel_overlaps(dsday, gdf_regions, weights=ds_pop.band_data[0, :, :], subset_bbox=False)
            else:
                weightmap0 = xa.pixel_overlaps(dsday, gdf_regions, subset_bbox=False)

        aggregated0 = xa.aggregate(dsday, weightmap0)
        aggregated0.to_dataframe()[:, cols2keep].to_csv("era5-t2m-" + str(year) + "-adm0.csv")
        
        if weightmap1 is None:
            gdf_regions = gpd.read_file("../../data/regions/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")
            if do_popweight:
                ds_pop = xr.open_dataset('../../data/regions/gl_gpwv3_pcount_90_ascii_25/glp90ag.asc')
                weightmap1 = xa.pixel_overlaps(dsday, gdf_regions, weights=ds_pop.band_data[0, :, :], subset_bbox=False)
            else:
                weightmap1 = xa.pixel_overlaps(dsday, gdf_regions, subset_bbox=False)
        
        aggregated1 = xa.aggregate(dsday, weightmap1)
        aggregated1.to_dataframe()[:, cols2keep].to_csv("era5-t2m-" + str(year) + "-adm1.csv")

