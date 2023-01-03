
check_container () {
    if command -v docker &> /dev/null; then
      HASH_ID=`docker ps | grep intel-gnu | grep intel/oneapi-hpckit | cut -d' ' -f1`
      echo $HASH_ID
    else
      echo ""
    fi



}

build_makefile() {

    input="./arch/Makefile.ac"
    if [ ! -z ${1} ]; then
      if [ ${1} == "mpiintel" ]; then 
          input="./arch/Makefile_mpi.ac"
      fi
    fi
  
    echo "include ../config.in" > ./src/Makefile

    IFS=$'\n'
    for line in $(cat $input); do
       echo $line >> ./src/Makefile
    done

}

build_container() {

    IMAGE=`docker images | grep intel/oneapi-hpckit`

    if [ ! -z $IMAGE ]; then
        while true; do

    echo

    container=$(check_container)
    if [ -z $container ]; then 
        read -p "  Found image called intel/oneapi-hpckit. Do you want to rebuild the image? [Y/n] " opt
    else 
      opt="n"
    fi
    echo
    
    case $opt in
       "Y")
          docker build -f ./util/docker/Dockerfile -t intel/oneapi-hpckit:1.0 .
          break
          ;;
      "n")
          break
          ;;
      *)
         echo "ERROR: Option $opt not valid. Try again..."
         echo   
     ;;
    esac

        done
    else 
        docker build -f ./util/docker/Dockerfile -t intel/oneapi-hpckit:1.0 .
    fi

    ROOT_DIR=`pwd`

if [ $COMP_NAME == "mpiintel" ]; then

cat << EOF > ./bin/run

#!/bin/bash

USER=\`whoami\`

if [ \${USER} != 'container' ]; then
  echo
  echo "  ERROR: Connect to the container first."
  echo
  exit
fi

if [ -z \${1} ]; then 
    PROC=2
else
    PROC=\${1}
fi

cd \${HOME}/bin

mpirun -np \$PROC ../src/swf.exe

EOF


else 

cat << EOF > ./bin/run

#!/bin/bash

USER=\`whoami\`

if [ \${USER} != 'container' ]; then
  echo
  echo "  ERROR: Connect to the container first."
  echo
  exit
fi

cd \${HOME}/bin

../src/swf.exe

EOF

fi

chmod 755 ./bin/run

cat << EOF > ./bin/connect

#!/bin/bash

HASH_ID=\`docker ps | grep intel-gnu | grep intel/oneapi-hpckit | cut -d' ' -f1\`

if [ ! -z \${HASH} ]; then
  echo
  echo "  ERROR: Cannot connect to the container."
  echo
  exit
fi

docker exec -it \${HASH_ID} bash

EOF

chmod 755 ./bin/connect

YML_FILE="${ROOT_DIR}/docker-compose.yml"
cat << EOF > ./bin/start

#!/bin/bash

CHECK=`which docker`

if [ ! -f \${CHECK} ]; then
  echo
  echo "  ERROR: Cannot start the container."
  echo
  exit
fi

docker-compose -f ${YML_FILE} up -d

EOF

chmod 755 ./bin/start

cat << EOF > ./bin/stop

#!/bin/bash

HASH_ID=\`docker ps | grep intel-gnu | grep intel/oneapi-hpckit | cut -d' ' -f1\`

if [ ! -z \${HASH} ]; then
  echo
  echo "  ERROR: The container is not running."
  echo
  exit
fi

docker stop \${HASH_ID}

EOF

chmod 755 ./bin/stop

}

build_config() {

    if [ ${1}  == "intel" ]; then
      COMP_NAME="intel"
    elif [ ${1} == "mpiintel" ]; then 
      COMP_NAME="mpiintel"
    elif [ ${1} == "gnu" ]; then 
      COMP_NAME="gnu"
    fi

    DATE=`date`
    write=false
    input="./arch/configure.ac"
    echo "#-----------------------------------------" > config.in
    echo "# config file generated at $DATE" >> config.in
    while IFS= read -r line
    do

        if [[ "$line" == *"//"* ]] && [[ $write == true ]] ; then
            write=false
            break
        fi

        if [[ "$line" == *"$COMP_NAME"* ]] || [[ $write == true ]]; then
            echo "$line" >> config.in
            write=true
        fi

    done < "$input"

}