sbt clean
sbt package
sbt caseClassGenerator/assembly
cp caseClassGenerator/target/scala-2.13/caseClassGenerator-assembly-0.1.0-SNAPSHOT.jar ~/Projects/internalProjects/assets/workspace/tel-tal-api-rust/lib
cp target/scala-3.7.0/crassus_3-0.1.0-SNAPSHOT.jar ~/Projects/internalProjects/assets/workspace/tel-tal-api-rust/lib
