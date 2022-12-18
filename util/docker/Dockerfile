
FROM intel/oneapi-hpckit:latest

RUN apt-get -y update

ADD ./util/docker/system_requirements.txt /system_requirements.txt 
ADD ./util/docker/entrypoint.sh /entrypoint.sh

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

RUN  echo "export LD_LIBRARY_PATH=/usr/local/lib:${LD_LIBRARY_PATH}" >> .bashrc &&  echo "export PATH=/usr/local/bin:${PATH}" >> .bashrc 
RUN  cd /usr/local &&  wget --no-check-certificate  https://zlib.net/zlib-1.2.13.tar.gz &&  tar -zxf zlib-1.2.13.tar.gz &&  cd zlib-1.2.13 &&  CXX=icpc CC=icc FC=ifort ./configure --prefix=/usr/local &&  make &&  make install &&  cd /usr/local &&  rm -rf zlib-1.2.13.tar.gz 
# RUN  cd /usr/local &&  wget --no-check-certificate  https://hdf-wordpress-1.s3.amazonaws.com/wp-content/uploads/manual/HDF5/HDF5_1_12_2/source/hdf5-1.12.2.tar &&  tar -xf hdf5-1.12.2.tar &&  cd hdf5-1.12.2 &&  CXX=icpc CC=icc FC=ifort  CPPFLAGS="-I/usr/local/include" LDFLAGS="-L/usr/local/lib" ./configure --prefix=/usr/local  --with-zlib=/usr/local &&  make -j 4 &&  make install &&  cd /usr/local &&  rm -rf hdf5-1.12.2.tar 
# RUN  cd /usr/local &&  wget --no-check-certificate  https://downloads.unidata.ucar.edu/netcdf-c/4.9.0/netcdf-c-4.9.0.tar.gz &&  tar -zxf netcdf-c-4.9.0.tar.gz &&  cd netcdf-c-4.9.0 &&  CXX=icpc CC=icc FC=ifort CPPFLAGS="-I/usr/local/include" LDFLAGS="-L/usr/local/lib" ./configure --prefix=/usr/local  --with-zlib=/usr/local --with-hdf5=/usr/local --enable-fortran=yes --enable-cxx-4=yes &&  make -j 4 &&  make install &&  cd /usr/local &&  rm -rf netcdf-c-4.9.0.tar.gz 
# RUN  cd /usr/local &&  wget --no-check-certificate https://downloads.unidata.ucar.edu/netcdf-fortran/4.5.3/netcdf-fortran-4.5.3.tar.gz &&  tar -zxvf netcdf-fortran-4.5.3.tar.gz &&  cd netcdf-fortran-4.5.3 &&  export LD_LIBRARY_PATH=/usr/local/lib &&  CXX=icpc CC=icc FC=ifort  CPPFLAGS="-I/usr/local/include" LDFLAGS="-L/usr/local/lib" ./configure --prefix=/usr/local &&  make -j 4 &&  make install &&  cd /usr/local &&  rm -rf netcdf-fortran-4.5.3.tar.gz

USER container:container

CMD ["/entrypoint.sh"]

