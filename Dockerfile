FROM openjdk:8-alpine

COPY target/uberjar/org-clock-stats.jar /org-clock-stats/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/org-clock-stats/app.jar"]
