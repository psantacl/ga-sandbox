JAR_WITH_DEP=@target.directory@/@artifact.id@-@version@-jar-with-dependencies.jar
CLASSPATH="$JAR_WITH_DEP"

java -server \
    -Xdebug \
    -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8885 \
    -cp "$CLASSPATH" \
    com.github.psantacl.ga_sandbox.einstein_main \
    "$@"
