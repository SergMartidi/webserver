#!/bin/sh
# email must be a properly formatted email address a@b.c
# password must be at least 6 chars

echo 'no data'
OUTPUT=`curl -s -b cookiejar -c cookiejar -i -H "Content-Type: application/json" -X POST  http://localhost:8080/register`
echo $OUTPUT
TOKEN=`echo "${OUTPUT}" | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'} | tr -d '"'`
curl -b cookiejar -i -H "Content-Type: application/json" -X GET http://localhost:8080/register?token=$TOKEN

echo 'no fname'
OUTPUT=`curl -s -b cookiejar -c cookiejar -i -H "Content-Type: application/json" -X POST -d '{"email":"x@xyz.com", "lname":"Lastname","pass":"xyzXYZ"}' http://localhost:8080/register`
echo $OUTPUT
TOKEN=`echo "${OUTPUT}" | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'} | tr -d '"'`
curl -b cookiejar -i -H "Content-Type: application/json" -X GET http://localhost:8080/register?token=$TOKEN

echo 'data is good'
OUTPUT=`curl -s -b cookiejar -c cookiejar -i -H "Content-Type: application/json" -X POST -d '{"email":"x@xyz.com","fname":"Firstname","lname":"Lastname","pass":"xyzXYZ"}' http://localhost:8080/register`
echo $OUTPUT
TOKEN=`echo "${OUTPUT}" | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'} | tr -d '"'`
echo "TOKEN:${TOKEN}"

#echo 'no token from cookie'
#curl -i -H "Content-Type: application/json" -X GET http://localhost:8080/register?token=$TOKEN
echo
echo 'normal request'
curl -b cookiejar -i -H "Content-Type: application/json" -X GET http://localhost:8080/register?token=$TOKEN
echo
