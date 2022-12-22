import pandas as pd
import glob
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits import mplot3d
import netCDF4
import time
from datetime import datetime
import os


if __name__ == "__main__":

    print('''
---------------------------------------------
           Post-Processing System
  Script started at {}
---------------------------------------------
    '''.format(datetime.now()))

    file_path = "../../bin/swf_*.nc4"

    files = sorted(glob.glob(file_path))

    if ( len(files) == 0 ):
        print("No files found!")
        print("")
        quit() 

    print("  - Found {} files".format(len(files)))
    print("")

    for i in files:
       
        start = time.time()
        output_name = ".".join(i.split("/")[-1].split(".")[0:2]) + ".nc4"

        print("  - Processing {}...".format(output_name))

        if os.path.exists(output_name):
            print("  File {} already exists.".format(output_name))
            continue

        nc = netCDF4.Dataset(i,"r")

        xc = nc.variables['Xc'][:]
        yc = nc.variables['Yc'][:]

        
        xm = int((len(xc)/2) - 1)
        ym = int((len(yc)/2) - 1)
       
        X, Y = np.meshgrid(xc,yc)

        height = nc.variables['height'][:][:] - 1000

        fig = plt.figure()
        ax = fig.add_subplot(111, projection='3d')

# Plot a sin curve using the x and y axes.
        
        ax.plot(xc, height[xm][:], zs = 0, zdir='x', color="blue", label='curve in (x,y)')

        ax.plot(yc, height[:][ym], zs = int(np.max(yc)) + 0.5 , zdir='y', color="blue", label='curve in (x,y)')
        ax.set_zlim3d(-2,2)
        surf = ax.plot_surface(X, Y, height, cmap="OrRd", alpha=0.9,
                     linewidth=0, antialiased=True, vmin = -1, vmax = 1)

        fig.colorbar(surf, shrink=0.5, aspect=5)
    
        ax.set_xlabel('X')
        ax.set_ylabel('Y')

        plt.savefig("{}.png".format(output_name),dpi=320,bbox_inches='tight', pad_inches=0)
        
        plt.close()

        end = time.time()
        print("  Done. Time elapsed = {} seconds".format(round(end-start,2)))


    print("")
    print("  Script ended at {}".format(datetime.now()))
    print("=============================================")
    print()