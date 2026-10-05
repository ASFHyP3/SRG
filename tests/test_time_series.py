from srg import time_series


def test_create_time_series_product_name():
    granule_names = [
        'S1A_IW_RAW__0SDV_001_003_054532_06A2F8_8276.zip',
        'S1A_IW_RAW__0SDV_004_005_054882_06AF26_2CE5.zip',
        'S1A_IW_RAW__0SDV_010_020_055057_06B527_1346.zip',
    ]
    bounds = [-100.0, 45.0, -90.0, 50.0]
    name = time_series.create_time_series_product_name(granule_names, bounds)
    assert name.startswith('S1_SRG_SBAS_35_W100_0_N45_0_W090_0_N50_0_001_010')

    bounds = [101.5123, -34.333, 56.866, -25.8897]
    name = time_series.create_time_series_product_name(granule_names, bounds)
    assert name.startswith('S1_SRG_SBAS_35_E101_5_S34_3_E056_9_S25_9_001_010')


def test_get_size_from_dem(tmp_path):
    rsc_content = """
    WIDTH          1235
    FILE_LENGTH    873
    X_FIRST        -124.41472222
    Y_FIRST        39.52388889
    X_STEP         0.0027777778
    Y_STEP         -0.0027777778
    X_UNIT        degrees
    Y_UNIT        degrees
    Z_OFFSET      0
    Z_SCALE       1
    PROJECTION    LL
    xstart         1
    ystart         1
    xsize          12357
    ysize          8731"""

    rsc_path = tmp_path / 'elevation.dem.rsc'
    with open(rsc_path, 'w') as rsc_file:
        rsc_file.write(rsc_content.strip())
    dem_width, dem_height = time_series.get_size_from_dem(dem_path=rsc_path)
    assert (dem_width, dem_height) == (1235, 873)
