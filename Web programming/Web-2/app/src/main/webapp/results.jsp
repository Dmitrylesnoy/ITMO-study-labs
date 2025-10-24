<%@ page contentType="text/html;charset=UTF-8" language="java" %> <%@ page import="java.time.format.DateTimeFormatter"
%> <%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
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
  </head>

  <body>
    <header>
      <h1>Web - lab 2. var - 73024</h1>
      <h2>Линейский Аким Евгеньевич, группа P3215</h2>
    </header>

    <main class="main-container">
      <div class="main-row">
        <button type="button" onclick="window.location.href='./index.jsp'">Go to form</button>
      </div>

      <div class="main-row">
        <div class="table-cell">
          <div class="table">
            <table id="tableData">
              <thead>
                <tr>
                  <th>X</th>
                  <th>Y</th>
                  <th>R</th>
                  <th>Hit</th>
                  <th>Calc time</th>
                  <th>Current time</th>
                </tr>
              </thead>
              <tbody id="output">
                <c:set var="dateFormatter" value="<%= DateTimeFormatter.ofPattern(\"yyyy-MM-dd HH:mm:ss\") %>" />
                <c:forEach var="point" items="${hitData.getReversedDataList()}">
                  <c:set var="hitStr" value="${point.hit() ? 'Yes' : 'No'}" />
                  <c:set var="rowClass" value="${point.hit() ? 'hit-yes' : 'hit-no'}" />
                  <tr class="${rowClass}">
                    <td>${point.x()}</td>
                    <td>${point.y()}</td>
                    <td>${point.r()}</td>
                    <td>${hitStr}</td>
                    <td>${point.execTime()} ns</td>
                    <td>${dateFormatter.format(point.date())}</td>
                  </tr>
                </c:forEach>
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </main>
  </body>
</html>
