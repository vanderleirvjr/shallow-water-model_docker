
FROM intel/oneapi-hpckit:latest

RUN apt-get -y update

ARG DEBIAN_FRONTEND=noninteractive

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

RUN mkdir -p /usr/local/modulefiles/compilers/intel /usr/local/modulefiles/default/intel/netcdf /usr/local/modulefiles/default/intel/zlib /usr/local/modulefiles/default/intel/hdf5
ADD ./util/docker/modulefiles/intel/* /usr/local/modulefiles/compilers/intel/.
ADD ./util/docker/modulefiles/netcdf/intel/* /usr/local/modulefiles/default/intel/netcdf/.
ADD ./util/docker/modulefiles/zlib/intel/* /usr/local/modulefiles/default/intel/zlib/.
ADD ./util/docker/modulefiles/hdf5/intel/* /usr/local/modulefiles/default/intel/hdf5/.

RUN mkdir -p /usr/local/modulefiles/compilers/gnu /usr/local/modulefiles/default/gnu/netcdf /usr/local/modulefiles/default/gnu/zlib /usr/local/modulefiles/default/gnu/hdf5
ADD ./util/docker/modulefiles/gnu/* /usr/local/modulefiles/compilers/gnu/.
ADD ./util/docker/modulefiles/netcdf/gnu/* /usr/local/modulefiles/default/gnu/netcdf/.
ADD ./util/docker/modulefiles/zlib/gnu/* /usr/local/modulefiles/default/gnu/zlib/.
ADD ./util/docker/modulefiles/hdf5/gnu/* /usr/local/modulefiles/default/gnu/hdf5/.

RUN  mkdir -p /usr/local/default/intel/zlib && cd /usr/local/default/intel/zlib &&  wget --no-check-certificate  https://zlib.net/zlib-1.2.13.tar.gz &&  tar -zxf zlib-1.2.13.tar.gz &&  cd zlib-1.2.13 &&  CXX=icpc CC=icc FC=ifort ./configure --prefix=/usr/local/default/intel/zlib &&  make &&  make install &&  cd /usr/local/default/intel/zlib &&  rm -rf zlib-1.2.13.tar.gz 
ARG  LD_LIBRARY_PATH="/usr/local/default/intel/netcdf/lib:/usr/local/default/intel/zlib/lib:/usr/local/default/intel/hdf5/lib:${LD_LIBRARY_PATH}"
RUN  mkdir -p /usr/local/default/intel/hdf5 && cd /usr/local/default/intel/hdf5 &&  wget --no-check-certificate  https://hdf-wordpress-1.s3.amazonaws.com/wp-content/uploads/manual/HDF5/HDF5_1_12_2/source/hdf5-1.12.2.tar &&  tar -xf hdf5-1.12.2.tar &&  cd hdf5-1.12.2 &&  CXX=icpc CC=icc FC=ifort  CPPFLAGS="-I/usr/local/default/intel/zlib/include" LDFLAGS="-L/usr/local/default/intel/zlib/lib" ./configure --prefix=/usr/local/default/intel/hdf5  --with-zlib=/usr/local/default/intel/zlib --enable-hl &&  make -j 4 &&  make install &&  cd /usr/local/default/intel/hdf5 &&  rm -rf hdf5-1.12.2.tar 
ARG  LD_LIBRARY_PATH="/usr/local/default/intel/netcdf/lib:/usr/local/default/intel/zlib/lib:/usr/local/default/intel/hdf5/lib:${LD_LIBRARY_PATH}"
RUN  mkdir -p /usr/local/default/intel/netcdf && cd /usr/local/default/intel/netcdf  &&  wget --no-check-certificate  https://downloads.unidata.ucar.edu/netcdf-c/4.9.0/netcdf-c-4.9.0.tar.gz &&  tar -zxf netcdf-c-4.9.0.tar.gz &&  cd netcdf-c-4.9.0 &&  CXX=icpc CC=icc FC=ifort CPPFLAGS="-I/usr/local/default/intel/zlib/include -I/usr/local/default/intel/hdf5/include" LDFLAGS="-L/usr/local/default/intel/zlib/lib -L/usr/local/default/intel/hdf5/lib" ./configure --prefix=/usr/local/default/intel/netcdf --enable-fortran=yes &&  make -j 4 &&  make install &&  cd /usr/local/default/intel/netcdf &&  rm -rf netcdf-c-4.9.0.tar.gz 
RUN  mkdir -p /usr/local/default/intel/netcdf && cd /usr/local/default/intel/netcdf  &&  wget --no-check-certificate https://downloads.unidata.ucar.edu/netcdf-fortran/4.5.3/netcdf-fortran-4.5.3.tar.gz &&  tar -zxvf netcdf-fortran-4.5.3.tar.gz &&  cd netcdf-fortran-4.5.3 &&  CXX=icpc CC=icc FC=ifort CPPFLAGS="-I/usr/local/default/intel/netcdf/include -I/usr/local/default/intel/zlib/include -I/usr/local/default/intel/hdf5/include" LDFLAGS="-L/usr/local/default/intel/netcdf/lib -L/usr/local/default/intel/zlib/lib -L/usr/local/default/intel/hdf5/lib" ./configure --prefix=/usr/local/default/intel/netcdf &&  make -j 4 &&  make install &&  cd /usr/local/default/intel/netcdf &&  rm -rf netcdf-fortran-4.5.3.tar.gz

RUN  mkdir -p /usr/local/default/gnu/zlib && cd /usr/local/default/gnu/zlib &&  wget --no-check-certificate  https://zlib.net/zlib-1.2.13.tar.gz &&  tar -zxf zlib-1.2.13.tar.gz &&  cd zlib-1.2.13 &&  CXX=g++ CC=gcc FC=gfortran ./configure --prefix=/usr/local/default/gnu/zlib &&  make &&  make install &&  cd /usr/local/default/gnu/zlib &&  rm -rf zlib-1.2.13.tar.gz 
ARG  LD_LIBRARY_PATH="/usr/local/default/gnu/netcdf/lib:/usr/local/default/gnu/zlib/lib:/usr/local/default/gnu/hdf5/lib:${LD_LIBRARY_PATH}"
RUN  mkdir -p /usr/local/default/gnu/hdf5 && cd /usr/local/default/gnu/hdf5 &&  wget --no-check-certificate  https://hdf-wordpress-1.s3.amazonaws.com/wp-content/uploads/manual/HDF5/HDF5_1_12_2/source/hdf5-1.12.2.tar &&  tar -xf hdf5-1.12.2.tar &&  cd hdf5-1.12.2 &&  CXX=g++ CC=gcc FC=gfortran  CPPFLAGS="-I/usr/local/default/gnu/zlib/include" LDFLAGS="-L/usr/local/default/gnu/zlib/lib" ./configure --prefix=/usr/local/default/gnu/hdf5  --with-zlib=/usr/local/default/gnu/zlib --enable-hl &&  make -j 4 &&  make install &&  cd /usr/local/default/gnu/hdf5 &&  rm -rf hdf5-1.12.2.tar 
ARG  LD_LIBRARY_PATH="/usr/local/default/gnu/netcdf/lib:/usr/local/default/gnu/zlib/lib:/usr/local/default/gnu/hdf5/lib:${LD_LIBRARY_PATH}"
RUN  mkdir -p /usr/local/default/gnu/netcdf && cd /usr/local/default/gnu/netcdf  &&  wget --no-check-certificate  https://downloads.unidata.ucar.edu/netcdf-c/4.9.0/netcdf-c-4.9.0.tar.gz &&  tar -zxf netcdf-c-4.9.0.tar.gz &&  cd netcdf-c-4.9.0 &&  CXX=g++ CC=gcc FC=gfortran CPPFLAGS="-I/usr/local/default/gnu/zlib/include -I/usr/local/default/gnu/hdf5/include" LDFLAGS="-L/usr/local/default/gnu/zlib/lib -L/usr/local/default/gnu/hdf5/lib" ./configure --prefix=/usr/local/default/gnu/netcdf --enable-fortran=yes &&  make -j 4 &&  make install &&  cd /usr/local/default/gnu/netcdf &&  rm -rf netcdf-c-4.9.0.tar.gz 
RUN  mkdir -p /usr/local/default/gnu/netcdf && cd /usr/local/default/gnu/netcdf  &&  wget --no-check-certificate https://downloads.unidata.ucar.edu/netcdf-fortran/4.5.3/netcdf-fortran-4.5.3.tar.gz &&  tar -zxvf netcdf-fortran-4.5.3.tar.gz &&  cd netcdf-fortran-4.5.3 &&  CXX=g++ CC=gcc FC=gfortran CPPFLAGS="-I/usr/local/default/gnu/netcdf/include -I/usr/local/default/gnu/zlib/include -I/usr/local/default/gnu/hdf5/include" LDFLAGS="-L/usr/local/default/gnu/netcdf/lib -L/usr/local/default/gnu/zlib/lib -L/usr/local/default/gnu/hdf5/lib" ./configure --prefix=/usr/local/default/gnu/netcdf &&  make -j 4 &&  make install &&  cd /usr/local/default/gnu/netcdf &&  rm -rf netcdf-fortran-4.5.3.tar.gz

RUN wget --no-check-certificate  https://repo.anaconda.com/archive/Anaconda3-2022.10-Linux-x86_64.sh && /bin/bash Anaconda3-2022.10-Linux-x86_64.sh -b -p /opt/conda

USER container:container

RUN /opt/conda/bin/pip install netcdf4
RUN /opt/conda/bin/pip install matplotlib 

