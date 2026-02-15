<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %> <%@ page contentType="text/html;charset=UTF-8" %>
<%@taglib uri="WEB-INF/mytags.tld" prefix="app" %>
<jsp:useBean id="hitData" class="web.model.HitData" scope="session" />
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <meta name="author" content="Линейский Аким Евгеньевич" />
    <meta name="group" content="P3215" />
    <meta name="variant" content="73024" />
    <title>Web-prog Lab-2</title>

    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" />
    <link rel="stylesheet" href="./resources/css/styles.css" />
    <link rel="stylesheet" href="./resources/css/toasts.css" />
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
    <script src="./resources/js/toaster.js"></script>
    <script src="./resources/js/validator.js"></script>
    <script src="./resources/js/graph.js"></script>
    <script src="./resources/js/main.js"></script>
  </head>

  <body>
    <div class="notifications"></div>
    <header>
      <h1>Web - lab 2. var - 73024</h1>
      <h2>Линейский Аким Евгеньевич, группа P3215</h2>
    </header>

    <main class="main-container">
      <div class="main-row">
        <div class="form-cell card">
          <app:formValidator formId="formCoords" action="./controller" method="GET">
            <app:field name="x" label="X">
              <app:rule type="required" />
              <app:rule type="range" value="-5,3" />
            </app:field>

            <app:field name="y" label="Y">
              <app:rule type="required" />
              <app:rule type="maxLength" value="6" />
              <app:rule type="pattern" value="^-?\d+([.,]\d*)?$" />

              <app:rule type="minValue" value="-5" />
              <app:rule type="maxValue" value="3" />
            </app:field>

            <app:field name="r" label="R">
              <app:rule type="required" />
              <app:rule type="range" value="1,5" />
            </app:field>

            <app:field name="submit" label="Submit"> </app:field>
          </app:formValidator>
        </div>
        <div class="graph-cell">
          <canvas id="graph" class="card" width="500" height="500"></canvas>
        </div>
      </div>
    </main>
    <script>
      window.points = [
        <c:forEach var="point" items="${hitData.getDataList()}" varStatus="status">
          {x: ${point.x()}, y: ${point.y()}, hit: ${point.hit()}}
          <c:if test="${!status.last}">,</c:if>
        </c:forEach>
      ];
    </script>
  </body>
</html>
