FROM buildo/scala-sbt-alpine

WORKDIR /usr/cpa

COPY . /usr/cpa

RUN sbt assembly

WORKDIR /usr/cpa/workdir


ENTRYPOINT ["java", "-jar", "/usr/cpa/target/scala-2.12/SDT_summaries_app.jar"]
