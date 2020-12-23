#!/usr/bin/env bash

method=$1
murja_url=$2
input_file=$3
cookie_jar=./cookies

# echo curling $payload to $murja_url

case $method in
    'GET')
	# echo curl -s -X GET --cookie-jar "$cookie_jar" --header 'Accept: application/json' "$murja_url"
	result=$(curl -s -X GET --cookie-jar "$cookie_jar" -b "$cookie_jar" --header 'Accept: application/json' "$murja_url");;
    'POST')
	result=$(curl -s -X POST --cookie-jar "$cookie_jar" -b "$cookie_jar" --header 'Content-Type: application/json' --header 'Accept: application/json' -d @$input_file "$murja_url");;
    'DELETE')
	result=$(curl -s -X DELETE --cookie-jar "$cookie_jar" -b "$cookie_jar" --header 'Accept: application/json' "$murja_url");;
    'PUT')
	result=$(curl -s -X PUT --cookie-jar "$cookie_jar" -b "$cookie_jar" --header 'Content-Type: application/json' --header 'Accept: application/json' -d @$input_file "$murja_url");;
    *)
	echo lolwat?;;
esac
echo $result
