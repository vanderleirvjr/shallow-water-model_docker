
FROM intel/oneapi-hpckit:latest

RUN apt-get -y update

ADD ./util/docker/system_requirements.txt /system_requirements.txt 

RUN apt-get install -y --no-install-recommends  $(cat /system_requirements.txt)

RUN groupadd -r container && useradd -r -g container container
RUN mkdir /home/container && chown container:container /home/container

#  Add new user docker to sudo group
RUN adduser container sudo

# Ensure sudo group users are not 
# asked for a password when using 
# sudo command by ammending sudoers file
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

WORKDIR /home/container

RUN  cd /usr/local && wget --no-check-certificate  https://sourceforge.net/projects/lmod/files/lua-5.1.4.9.tar.bz2 && tar -jxf lua-5.1.4.9.tar.bz2 && cd lua-5.1.4.9 && ./configure --prefix=/usr/local && make && make install 

RUN mkdir /usr/share/lmod/lmod/modulefiles/intel 
ADD ./util/docker/modulefiles/intel/* /usr/share/lmod/lmod/modulefiles/intel/.

RUN mkdir /usr/share/lmod/lmod/modulefiles/gnu 
ADD ./util/docker/modulefiles/gnu/* /usr/share/lmod/lmod/modulefiles/gnu/.

# ARG LD_LIBRARY_PATH="/usr/local/intel/lib:${LD_LIBRARY_PATH}"
# ARG PATH="/usr/local/intel/bin:${PATH}"

# RUN mkdir -p /usr/local/intel
# RUN  cd /usr/local/intel &&  wget --no-check-certificate  https://zlib.net/zlib-1.2.13.tar.gz &&  tar -zxf zlib-1.2.13.tar.gz &&  cd zlib-1.2.13 &&  CXX=icpc CC=icc FC=ifort ./configure --prefix=/usr/local/intel &&  make &&  make install &&  cd /usr/local/intel &&  rm -rf zlib-1.2.13.tar.gz 
# RUN  cd /usr/local/intel &&  wget --no-check-certificate  https://hdf-wordpress-1.s3.amazonaws.com/wp-content/uploads/manual/HDF5/HDF5_1_12_2/source/hdf5-1.12.2.tar &&  tar -xf hdf5-1.12.2.tar &&  cd hdf5-1.12.2 &&  CXX=icpc CC=icc FC=ifort  CPPFLAGS="-I/usr/local/intel/include" LDFLAGS="-L/usr/local/intel/lib" ./configure --prefix=/usr/local/intel  --with-zlib=/usr/local/intel --enable-hl &&  make -j 4 &&  make install &&  cd /usr/local/intel &&  rm -rf hdf5-1.12.2.tar 
# RUN  cd /usr/local/intel &&  wget --no-check-certificate  https://downloads.unidata.ucar.edu/netcdf-c/4.9.0/netcdf-c-4.9.0.tar.gz &&  tar -zxf netcdf-c-4.9.0.tar.gz &&  cd netcdf-c-4.9.0 &&  CXX=icpc CC=icc FC=ifort CPPFLAGS="-I/usr/local/intel/include" LDFLAGS="-L/usr/local/intel/lib" ./configure --prefix=/usr/local/intel --enable-fortran=yes &&  make -j 4 &&  make install &&  cd /usr/local/intel &&  rm -rf netcdf-c-4.9.0.tar.gz 
# RUN  cd /usr/local/intel &&  wget --no-check-certificate https://downloads.unidata.ucar.edu/netcdf-fortran/4.5.3/netcdf-fortran-4.5.3.tar.gz &&  tar -zxvf netcdf-fortran-4.5.3.tar.gz &&  cd netcdf-fortran-4.5.3 &&  CXX=icpc CC=icc FC=ifort  CPPFLAGS="-I/usr/local/intel/include" LDFLAGS="-L/usr/local/intel/lib" ./configure --prefix=/usr/local/intel &&  make -j 4 &&  make install &&  cd /usr/local/intel &&  rm -rf netcdf-fortran-4.5.3.tar.gz

# ARG LD_LIBRARY_PATH="/usr/local/gnu/lib:${LD_LIBRARY_PATH}"
# ARG PATH="/usr/local/gnu/bin:${PATH}"

# RUN mkdir -p /usr/local/gnu
# RUN  cd /usr/local/gnu &&  wget --no-check-certificate  https://zlib.net/zlib-1.2.13.tar.gz &&  tar -zxf zlib-1.2.13.tar.gz &&  cd zlib-1.2.13 &&  CXX=g++ CC=gcc FC=gfortran ./configure --prefix=/usr/local/gnu &&  make &&  make install &&  cd /usr/local/gnu &&  rm -rf zlib-1.2.13.tar.gz 
# RUN  cd /usr/local/gnu &&  wget --no-check-certificate  https://hdf-wordpress-1.s3.amazonaws.com/wp-content/uploads/manual/HDF5/HDF5_1_12_2/source/hdf5-1.12.2.tar &&  tar -xf hdf5-1.12.2.tar &&  cd hdf5-1.12.2 &&  CXX=g++ CC=gcc FC=gfortran  CPPFLAGS="-I/usr/local/gnu/include" LDFLAGS="-L/usr/local/gnu/lib" ./configure --prefix=/usr/local/gnu  --with-zlib=/usr/local/gnu --enable-hl &&  make -j 4 &&  make install &&  cd /usr/local/gnu &&  rm -rf hdf5-1.12.2.tar 
# RUN  cd /usr/local/gnu &&  wget --no-check-certificate  https://downloads.unidata.ucar.edu/netcdf-c/4.9.0/netcdf-c-4.9.0.tar.gz &&  tar -zxf netcdf-c-4.9.0.tar.gz &&  cd netcdf-c-4.9.0 &&  CXX=g++ CC=gcc FC=gfortran CPPFLAGS="-I/usr/local/gnu/include" LDFLAGS="-L/usr/local/gnu/lib" ./configure --prefix=/usr/local/gnu --enable-fortran=yes &&  make -j 4 &&  make install &&  cd /usr/local/gnu &&  rm -rf netcdf-c-4.9.0.tar.gz 
# RUN  cd /usr/local/gnu &&  wget --no-check-certificate https://downloads.unidata.ucar.edu/netcdf-fortran/4.5.3/netcdf-fortran-4.5.3.tar.gz &&  tar -zxvf netcdf-fortran-4.5.3.tar.gz &&  cd netcdf-fortran-4.5.3 &&  CXX=g++ CC=gcc FC=gfortran  CPPFLAGS="-I/usr/local/gnu/include" LDFLAGS="-L/usr/local/gnu/lib" ./configure --prefix=/usr/local/gnu &&  make -j 4 &&  make install &&  cd /usr/local/gnu &&  rm -rf netcdf-fortran-4.5.3.tar.gz

USER container:container


