package web.model;

import java.time.LocalDateTime;

public record Point(Float x, Float y, Integer r, Boolean hit, Float execTime, LocalDateTime date) {

}