FROM quay.io/wildfly/wildfly

ARG DATABASE_URL
ARG DB_USERNAME
ARG DB_PASSWORD
ARG WAR_PATH

ENV DATABASE_URL=${DATABASE_URL}
ENV DB_USERNAME=${DB_USERNAME}
ENV DB_PASSWORD=${DB_PASSWORD}

ADD customization /opt/jboss/wildfly/customization/
COPY modules/ /opt/jboss/wildfly/modules/

USER root

RUN chmod +x /opt/jboss/wildfly/customization/execute.sh

USER jboss

RUN /opt/jboss/wildfly/customization/execute.sh

COPY ${WAR_PATH} /opt/jboss/wildfly/standalone/deployments/

CMD ["/opt/jboss/wildfly/bin/standalone.sh", "-b", "0.0.0.0", "-bmanagement", "0.0.0.0"]