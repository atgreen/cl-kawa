JAVA_HOME ?= /home/linuxbrew/.linuxbrew/opt/openjdk@8/libexec/jre
KAWA_JAR = libs/kawa-3.1.1.jar

check: $(KAWA_JAR)
	LDK_CLASSPATH=$(KAWA_JAR) JAVA_HOME=$(JAVA_HOME) sbcl --load test.lisp

$(KAWA_JAR):
	mvn dependency:copy-dependencies -DoutputDirectory=./libs
