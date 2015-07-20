#/bin/sh

sbt +package

cp target/scala-2.*/*.jar mvn/

