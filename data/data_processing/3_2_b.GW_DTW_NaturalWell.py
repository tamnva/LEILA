import numpy as np
import pandas as pd
from dtw import *
import scipy
from datetime import timedelta
from multiprocessing import Pool
from functools import partial
import geopandas as gpd
import os

import matplotlib.pyplot as plt


def na_interpolation(series, method="linear", spline_order=3, 
                     maxgap=np.inf, rule=2):
    import pandas as pd
    import numpy as np
    from scipy.interpolate import interp1d, UnivariateSpline, PchipInterpolator

    if not isinstance(series, pd.Series):
        raise TypeError("Input must be a pandas Series")

    if series.isna().sum() == 0:
        return series.copy()

    # Step 1: Identify maxgap-allowed blocks
    is_na = series.isna()
    group = (~is_na).cumsum()
    gap_sizes = is_na.groupby(group).sum()

    # Build a mask of NaNs that are allowed to be interpolated
    mask_allowed = is_na.copy()
    for k, v in gap_sizes.items():
        if v > maxgap:
            mask_allowed[group == k] = False

    # Step 2: Replace disallowed NaNs with sentinel
    tmp_series = series.copy()
    tmp_series[~mask_allowed & is_na] = -99999999  # placeholder

    # Step 3: Convert index to numeric
    xi = series.index.view("int64")
    x = tmp_series[~tmp_series.isin([np.nan, -99999999])].index.view("int64")
    y = tmp_series[~tmp_series.isin([np.nan, -99999999])].values

    if len(x) < 2:
        return series.copy()

    # Step 4: Choose interpolation function
    if method == "linear":
        fill = "extrapolate" if rule == 2 else (np.nan, np.nan)
        f = interp1d(x, y, kind='linear', bounds_error=False, fill_value=fill)
    elif method == "spline":
        f = UnivariateSpline(x, y, k=spline_order, s=0)
    elif method == "pchip":
        f = PchipInterpolator(x, y, extrapolate=(rule == 2))
    else:
        raise ValueError(
            "Unsupported method. Choose 'linear', 'spline', or 'pchip'."
            )

    interpolated = pd.Series(f(xi), index=series.index)
    result = series.copy()
    result[mask_allowed] = interpolated[mask_allowed]

    return result

def DTW_Test(GW_TS1, GW_TS2, step_pattern, keep_internals, distance_only):
    
    if ((~GW_TS1.isna()).sum() < 3 * 52) | ((~GW_TS2.isna()).sum() < 3 * 52):
        return 0, 0
    
    GW_TS1_zscore = pd.Series(scipy.stats.zscore(
        GW_TS1.interpolate(method='linear'), 
        nan_policy='omit').astype('float32'),
        index=GW_TS1.index)
    
    GW_TS2_zscore = pd.Series(scipy.stats.zscore(
        GW_TS2.interpolate(method='linear'), 
        nan_policy='omit').astype('float32'),
        index=GW_TS2.index)
    
    if ((~np.isnan(GW_TS1_zscore)).sum() == 0) | (
            (~np.isnan(GW_TS2_zscore)).sum() == 0):
        return 0, 0
    
    startdate = np.max([GW_TS1.index[0], GW_TS2.index[0]]) - timedelta(days=28)
    enddate = np.min([GW_TS1.index[-1], GW_TS2.index[-1]]) + timedelta(days=28)
    if enddate >= startdate + timedelta(days=10 * 365):
        
        GW_TS_concat = pd.concat([GW_TS1_zscore, GW_TS2_zscore], 
                                 keys=['TS1', 'TS2'], axis=1)
        GW_TS_concat = GW_TS_concat.loc[startdate:enddate]
        GW_TS_concat.dropna(axis=0, how='all', inplace=True)

        GW_TS_concat1 = GW_TS_concat['TS1'].dropna()
        GW_TS_concat1_idx = GW_TS_concat1.index
        GW_TS_concat1_value = GW_TS_concat1.to_numpy()
        GW_TS_concat2 = GW_TS_concat['TS2'].dropna()
        GW_TS_concat2_value = GW_TS_concat2.to_numpy()

        alignment = dtw(GW_TS_concat1_value, GW_TS_concat2_value,
                        keep_internals=keep_internals, distance_only=distance_only,
                        dist_method="Euclidean", step_pattern=step_pattern,
                        window_type=sakoeChibaWindow, window_args={"window_size": 365})
        return GW_TS_concat1_idx, alignment
    else:
        return 0, 0

def DTW_Neighbouring_Wells(row, route, step_pattern, 
                           keep_internals, distance_only):
    
    UFZ_ID = row[1]['point_UFZ.ID']
    
    UFZ_ID_fn = UFZ_ID.replace("/", "@")
    
    GW_TS1 = pd.read_csv(route + UFZ_ID_fn + ".txt", engine='python', sep=';',
                         index_col=0, parse_dates=[0], encoding="latin1")  # Load the data of evaluated GW
    GW_TS1['GW.int'] = round(na_interpolation(
        GW_TS1['GW.obs'], method="linear", maxgap=40, rule=1), 2)
    
    neighbor_UFZ_ID = row[1]['neighbor_UFZ.ID']
    neighbor_UFZ_ID_fn = neighbor_UFZ_ID.replace("/", "@")
    
    GW_TS2 = pd.read_csv(route + neighbor_UFZ_ID_fn + ".txt", 
                         engine='python', sep=';',
                         index_col=0, parse_dates=[0], 
                         encoding="latin1")  # Load the data of evaluated GW
    
    GW_TS2['GW.int'] = round(na_interpolation(
        GW_TS2['GW.obs'], method="linear", maxgap=40, rule=1), 2)
    
    GW_TS1_idx, alignment = DTW_Test(
        GW_TS1['GW.int'], GW_TS2['GW.int'], step_pattern, 
        keep_internals, distance_only)

    if alignment:
        return alignment.normalizedDistance
    else:
        return 99

if __name__ == "__main__":
    
    out_dir = "S:/pyvenv/results/"
    data_dir = "Y:/Gruppen/gw-data/GW-data-QC/GWL/GWTS_Monthly_DTW_QC/"
    gw_meta_dir = "Y:/Gruppen/gw-data/GW-data-QC/GWL/gw_meta_monthly_geo_qc.txt"
    
    GW_meta = pd.read_csv(gw_meta_dir, engine='python', sep=';', encoding="latin1")
    GW_meta_gdf = gpd.GeoDataFrame(
        GW_meta, 
        geometry=gpd.points_from_xy(GW_meta.x_EPSG25832, GW_meta.y_EPSG25832),
        crs="EPSG:25832"
        )
    
    print("Finding neighbours...")
    # Tam modify using distance from Zhenyu (before GW_meta_gdf.buffer(10000))
    GW_meta_gdf.loc[:, 'buffer']  = GW_meta_gdf.buffer(10000)
    #bl_list = ['B', 'BR', 'BW', 'BY', 'H', 'MV', 'NRW', 'NS', 'RP', 'S', 
    #           'SA', 'SAAR', 'SH', 'TH'] 
    #buffer_list = [1500, 4000, 1500, 10000, 2500, 6500, 500, 3000, 1500, 
    #               2000, 4000, 10000, 4500, 3500]
    
    #buffer_dict = dict(zip(bl_list, buffer_list))
    
    #GW_meta_gdf["buffer_radius"] = GW_meta_gdf["BL"].map(buffer_dict)
    #GW_meta_gdf.loc[:, 'buffer']  = GW_meta_gdf.buffer(GW_meta_gdf["buffer_radius"])
    
    
    
    # Extract polygon geometry from 'buffer' column
    GW_meta_buffer = GW_meta_gdf.loc[GW_meta_gdf['natstat'] == True, 
                                     ['UFZ.ID', 'buffer']] 
    
    # Set 'buffer' as the active geometry column
    GW_meta_buffer.set_geometry('buffer', inplace=True) 
    
    # Extract point geometry from 'geometry' column
    GW_meta_points = GW_meta_gdf[['UFZ.ID', 'geometry']] 
    
    # Set 'geometry' as the active geometry column
    GW_meta_points.set_geometry('geometry', inplace=True) 

    # Step 2: Perform a spatial join between the buffered points and the original points
    neighbors = gpd.sjoin(GW_meta_buffer, GW_meta_points, predicate='intersects')

    # Step 3: Clean up the results to get indices of the neighbors
    # 'index_right' gives the index of neighbors from the original GeoDataFrame
    neighbors = neighbors.reset_index() # Reset index to get the original index in a column
    neighbors = neighbors.rename(
        columns={'index': 'point_index', 
                 'UFZ.ID_left': 'point_UFZ.ID',
                 'index_right': 'neighbor_index', 
                 'UFZ.ID_right': 'neighbor_UFZ.ID'})
    
    # remove self-joins
    neighbors = neighbors[neighbors['point_index'] != 
                          neighbors['neighbor_index']].reset_index()
    neighbors = neighbors.drop(['buffer'], axis=1)


    print("Calculating DTW...")

    DTW_Neighbouring_Wells_partial = partial(DTW_Neighbouring_Wells, 
                                             route=data_dir,
                                             step_pattern='symmetric2', 
                                             keep_internals=False, 
                                             distance_only=True)
    # Process in batches to save memory
    batch_size = 25  # adjust based on your memory capacity
    num_batches = len(neighbors) // batch_size + 1
    cores = 25

    #for batch in [142, 144, 147, 1529, 1531, 1544]:
    for batch in range(num_batches):
        results = []
        print("{}/{}".format(batch, num_batches))
        batch_neighbors = neighbors.iloc[(batch * batch_size):((batch + 1) * batch_size)]
        with Pool(cores) as pool:
            batch_results = pool.imap(DTW_Neighbouring_Wells_partial, batch_neighbors.iterrows())
            results.extend(batch_results)
            pool.close()
            pool.join()
        batch_neighbors['DTW_dist'] = results
        batch_neighbors.to_csv(out_dir + str(batch) + '.txt', sep=';', index=False)
     
    print("***********DONE***********")




