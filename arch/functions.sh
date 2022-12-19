
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

    docker build -f ./util/docker/Dockerfile -t intel/oneapi-hpckit:1.0 .

    ROOT_DIR=`pwd`

cat << EOF > docker-compose.yml
version: "3.9"
   
services:

  main:
    image: intel/oneapi-hpckit:1.0
    container_name: intel-gnu
    ports:
      - "7777:9000"
    volumes:
      - "${ROOT_DIR}:/home/container"

EOF

cat << EOF > ./bin/connect

#!/bin/bash

HASH_ID=\`docker ps | grep intel-gnu | grep ntel/oneapi-hpckit | cut -d' ' -f1\`

docker exec -it \${HASH_ID} bash

EOF

    YML_FILE="${ROOT_DIR}/docker-compose.yml"
    echo "docker-compose -f ${YML_FILE} up -d" > ./bin/start 

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