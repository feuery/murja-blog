#!/bin/env bash

if [ ! -f ./swagger-codegen-cli.jar ]; then
    echo "swagger-codegen-cli.jar not found. wgetting..."
    wget https://repo1.maven.org/maven2/io/swagger/swagger-codegen-cli/2.4.21/swagger-codegen-cli-2.4.21.jar -O swagger-codegen-cli.jar

fi

rm -rf murja-client/

java -jar ./swagger-codegen-cli.jar generate -i http://localhost:3000/swagger.json -l bash -o ./murja-client/

