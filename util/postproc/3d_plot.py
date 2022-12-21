import pandas as pd
import glob
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits import mplot3d
import netCDF4
import time
from datetime import datetime


if __name__ == "__main__":

    print('''
---------------------------------------------
           Post-Processing System
  Script started at {}
---------------------------------------------
    '''.format(datetime.now()))

    file_path = "../../src/swf_*.nc4"

    files = sorted(glob.glob(file_path))

    print("  - Found {} files".format(len(files)))
    print("")

    for i in files:
       
        start = time.time()
        output_name = i.split("/")[-1].split(".")[0]

        print("  - Processing {}.nc4...".format(output_name))

        nc = netCDF4.Dataset(i,"r")

        xc = nc.variables['Xc'][:]
        yc = nc.variables['Yc'][:]

        X, Y = np.meshgrid(xc,yc)

        height = nc.variables['height'][:]

        fig = plt.figure()
        ax = fig.add_subplot(111, projection='3d')

        ax.set_zlim3d(-4,4)
        surf = ax.plot_surface(X, Y, height, cmap="OrRd",
                     linewidth=0, antialiased=True, vmin = -4, vmax = 4)
        #ax.view_init(elev=90, azim=0)
        fig.colorbar(surf, shrink=0.5, aspect=5)
    

        plt.savefig("{}.png".format(output_name),dpi=320,bbox_inches='tight', pad_inches=0)
        
        plt.close()

        end = time.time()
        print("  Done. Time elapsed = {} seconds".format(round(end-start,2)))

        quit()

    print("")
    print()