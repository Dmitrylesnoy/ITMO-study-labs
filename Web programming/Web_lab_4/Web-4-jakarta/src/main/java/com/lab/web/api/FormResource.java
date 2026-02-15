
package com.lab.web.api;

import java.net.URI;
import java.time.LocalDateTime;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.lab.web.api.records.Point;
import com.lab.web.data.HitDataBean;
import com.lab.web.data.PointData;
import com.lab.web.utils.RateLimiter;
import com.lab.web.utils.auth.UserVetification;
import com.lab.web.utils.PointValidator;

import jakarta.inject.Inject;
import jakarta.security.auth.message.AuthException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.HeaderParam;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.UriInfo;
import jakarta.ws.rs.ext.Provider;

@Provider
@Path("/form")
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class FormResource {
    @Context
    private UriInfo context;

    @Context
    private HttpServletRequest httpRequest;

    @Inject
    private HitDataBean hitDataBean;

    @Inject
    private UserVetification userVetification;

    private static final Logger logger = Logger.getLogger(FormResource.class.getName());

    private static final Cache<Point, PointData> cache = Caffeine.newBuilder().expireAfterWrite(5, TimeUnit.MINUTES)
            .maximumSize(100).build();

    private ObjectWriter ow = new ObjectMapper().registerModule(new JavaTimeModule()).writer()
            .withDefaultPrettyPrinter();

    @GET
    public Response getData(@HeaderParam("Authorization") String token) {
        try {
            userVetification.checkUserByToken(token);

            if (!RateLimiter.tryFormConsume(httpRequest, 1)) {
                return Response.status(Response.Status.TOO_MANY_REQUESTS).entity("{\"error\": \"Too many requests \"}")
                        .type(MediaType.APPLICATION_JSON).build();
            }

            logger.info("success data fetch");
            String jsonResponce = ow
                    .writeValueAsString(hitDataBean.getData(userVetification.getUserIDbyToken(token)));

            return Response.ok().entity(jsonResponce)
                    .type(MediaType.APPLICATION_JSON).build();
        } catch (AuthException e) {
            URI logoutUri = context.getBaseUriBuilder().path("user").path("logout").build();
            return Response.temporaryRedirect(logoutUri).build();
        } catch (Exception e) {
            return Response.status(Response.Status.BAD_REQUEST)
                    .entity("{\"error\": \"" + e.getMessage() + "\"}")
                    .type(MediaType.APPLICATION_JSON)
                    .build();
        }
    }

    @POST
    public Response postForm(@HeaderParam("Authorization") String token, Point pointBean) {
        try {
            userVetification.checkUserByToken(token);

            if (!RateLimiter.tryFormConsume(httpRequest, 1)) {
                return Response.status(Response.Status.TOO_MANY_REQUESTS)
                        .entity("{\"error\": \"Too many requests for form\"}")
                        .type(MediaType.APPLICATION_JSON).build();
            }

            PointData cachedPoint = cache.getIfPresent(pointBean);
            if (cachedPoint == null) {
                logger.info("point did not found in cache " + pointBean.toString());
                cachedPoint = PointValidator.fillPoint(
                        String.valueOf(pointBean.x()),
                        String.valueOf(pointBean.y()),
                        String.valueOf(pointBean.r()),
                        !pointBean.graphFlag());
                cache.put(pointBean, cachedPoint);
            } else {
                logger.info("Get point from cache " + cachedPoint.toString());
            }

            Long userId = userVetification.getUserIDbyToken(token);
            PointData newPoint = new PointData(cachedPoint.getX(), cachedPoint.getY(), cachedPoint.getR(),
                    cachedPoint.isHit(), cachedPoint.getExecTime(), LocalDateTime.now(), userId);
            hitDataBean.addPoint(newPoint);

            String jsonResponce = ow.writeValueAsString(newPoint);
            logger.info("Processed point: graph " + pointBean.graphFlag() + " User: " + userId + " " + jsonResponce);
            return Response.ok()
                    .entity(jsonResponce)
                    .type(MediaType.APPLICATION_JSON)
                    .build();
        } catch (AuthException e) {
            URI logoutUri = context.getBaseUriBuilder().path("user").path("logout").build();
            return Response.temporaryRedirect(logoutUri).build();
        } catch (Exception e) {
            return Response.status(Response.Status.BAD_REQUEST)
                    .entity("{\"error\": \"" + e.getMessage() + "\"}")
                    .type(MediaType.APPLICATION_JSON)
                    .build();
        }
    }
}
