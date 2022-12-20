
build_makefile() {

    input="./arch/Makefile.ac"
    echo "FC = $FC" > ./src/Makefile
    echo "LDFLAGS = $LDFLAGS" >> ./src/Makefile
    echo  "CPPFLAGS = $CPPFLAGS" >> ./src/Makefile
    echo  "FFLAGS = $FFLAGS" >> ./src/Makefile

    IFS=$'\n'
    for line in $(cat $input); do
       echo $line >> ./src/Makefile
    done

}

build_container() {

    IMAGE=`docker images | grep intel/oneapi-hpckit`

    opt="n"
    if [ ! -z $IMAGE ]; then
      read -p "Found image called intel/oneapi-hpckit. Do you want to build another image? [Y/n] " opt
    fi

    if [ $opt == "Y" ]; then 
        docker build -f ./util/docker/Dockerfile -t intel/oneapi-hpckit:1.0 .
    fi

    ROOT_DIR=`pwd`

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


}

build_config() {

    if [ ${1}  == "intel" ]; then
      COMP_NAME="intel"
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