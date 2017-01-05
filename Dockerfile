FROM 1science/sbt

RUN id
RUN apk --no-cache add ca-certificates && update-ca-certificates
RUN apk --no-cache add ca-certificates && update-ca-certificates
RUN ls -l /
RUN ls -l /home
RUN id
RUN mkdir -p /app && mkdir -p /home/root/.sbt 
RUN echo $PATH
RUN  echo "[repositories]" > /home/root/.sbt/repositories \
  && echo "local" >> /home/root/.sbt/repositories \
  && echo "taobao: http://maven.aliyun.com/nexus/content/groups/public/" >> /home/root/.sbt/repositories \
  && echo "jboss: http://repository.jboss.com/maven2/" >> /home/root/.sbt/repositories \
  && echo "typesafe: https://repo.typesafe.com/typesafe/ivy-releases/, [organization]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[type]s/[artifact](-[classifier]).[ext], bootOnly  " >> /home/root/.sbt/repositories \
  && echo "sonatype-oss-releases" >>  /home/root/.sbt/repositories
RUN cat  /home/root/.sbt/repositories
ADD . /app
WORKDIR /app
RUN sbt test
#RUN sbt docker:publishLocal
RUN sbt package:dist
RUN unzip target/universal/dumframer-1.0.zip
EXPOSE 9000
ENTRYPOINT ["./dumframer-1.0/bin/dumframer"]
