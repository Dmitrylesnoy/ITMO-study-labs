package com.lab.web.utils;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;

import io.github.bucket4j.Bucket;
import jakarta.servlet.http.HttpServletRequest;

public class RateLimiter {

    private static final Cache<String, Bucket> loginCache = Caffeine.newBuilder()
            .expireAfterAccess(1, TimeUnit.HOURS)
            .maximumSize(10000)
            .build();

    private static final Cache<String, Bucket> formCache = Caffeine.newBuilder()
            .expireAfterAccess(1, TimeUnit.HOURS)
            .maximumSize(10000)
            .build();

    private RateLimiter() {
    }

    public static Bucket getFormBucket(HttpServletRequest request) {
        String ip = request.getRemoteAddr();
        return formCache.get(ip, k -> Bucket.builder()
                .addLimit(limit -> limit.capacity(30).refillGreedy(10, Duration.ofMinutes(1)))
                .build());
    }

    public static boolean tryFormConsume(HttpServletRequest request, long cost) {
        Bucket bucket = getFormBucket(request);
        return bucket.tryConsume(cost);
    }

    public static Bucket getLoginBucket(HttpServletRequest request) {
        String ip = request.getRemoteAddr();
        return loginCache.get(ip, k -> Bucket.builder()
                .addLimit(limit -> limit.capacity(5).refillGreedy(5, Duration.ofMinutes(1)))
                .build());
    }

    public static boolean tryLoginConsume(HttpServletRequest request, long cost) {
        Bucket bucket = getLoginBucket(request);
        return bucket.tryConsume(cost);
    }
}
