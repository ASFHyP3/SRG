import os
import argparse
import numpy as np
from osgeo import gdal, osr

# TODO Functionalize this 👀

parser = argparse.ArgumentParser(description='Convert a .geo file into a tiff')
parser.add_argument("geo_path", type=str)
parser.add_argument("rsc_path", type=str)
parser.add_argument("filename", type=str)
args = parser.parse_args()

geo_path = vars(args)["geo_path"]
rsc_path = vars(args)["rsc_path"]
filename = vars(args)["filename"]

HOME = os.environ['PROC_HOME']

strip_ext = geo_path.split('.')[0].split('/')
filename = HOME + "/output/" + filename

# Gather the RSC Data
print("Gathering RSC Data...")
rsc = open(rsc_path)
lines = rsc.readlines()
info = []
for line in lines:
    for word in line.split():
        if word[0].isnumeric() or word[1].isnumeric():
            info.append(word)
WIDTH       =      int(info[0])
FILE_LENGTH =      int(info[1])
X_FIRST     = np.float(info[2])
Y_FIRST     = np.float(info[3])
X_STEP      = np.float(info[4])
Y_STEP      = np.float(info[5])
rsc.close()

# Calculate transform for the projection
print("Calculating Geotransform...")
X_LAST = X_FIRST + (WIDTH * X_STEP) 
Y_LAST = Y_FIRST + (FILE_LENGTH * Y_STEP)

X_RES = (X_LAST - X_FIRST) / float(WIDTH)
Y_RES = (Y_FIRST - Y_LAST) / float(FILE_LENGTH)

geotransform = (X_FIRST, X_RES, 0, Y_LAST, 0, -Y_RES)
print("Geotransform: ", geotransform)

# Split the complex and real parts and make bands out of them
print("Spliting Complex and Real Parts...")
band1 = np.fromfile(geo_path, dtype=np.float)
band2 = band1.copy()

r = np.arange(WIDTH * FILE_LENGTH)
iz = np.where(r % 2 == 0)
qz = np.where(r % 2 != 0)
    
band1[qz] = 0   
band2[iz] = 0

del r, iz, qz 

band1 = band1.reshape((FILE_LENGTH, WIDTH))
band2 = band2.reshape((FILE_LENGTH, WIDTH))

amp = np.sqrt(np.square(band1)+np.square(band2))

# Create The File
print("Creating Geotiff Driver...")
driver = gdal.GetDriverByName("GTiff")
outdata = driver.Create(filename, WIDTH, FILE_LENGTH, 3, gdal.GDT_Float64)

# Set The Projection to WGS84 (EPSG: 4326)
outdata.SetGeoTransform(geotransform)
srs = osr.SpatialReference()
srs.ImportFromEPSG(4326)
outdata.SetProjection(srs.ExportToWkt())

# Write the Data to the Raster Bands
print("Writing Complex Band...")
print("Band 1 Shape: ", band1.shape)
outband = outdata.GetRasterBand(1)
outband.WriteArray(band1)

print("Writing Real Band...")
print("Band 2 Shape: ", band2.shape)
outband = outdata.GetRasterBand(2)
outband.WriteArray(band2)

print("Writing Amplitude Band...")
print("Band 3 Shape: ", amp.shape)
outband = outdata.GetRasterBand(3)
outband.WriteArray(amp)

print("Saving...")
outdata.FlushCache()
outdata = None

