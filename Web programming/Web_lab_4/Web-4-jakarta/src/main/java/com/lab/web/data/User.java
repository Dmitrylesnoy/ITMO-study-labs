package com.lab.web.data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

@Entity
@Table(name = "web_users")
public record User(
        @Id @GeneratedValue(strategy = GenerationType.IDENTITY) Long id,
        @Column(name = "username", unique = true, nullable = false) String username,
        @Column(name = "password", nullable = false) String password) {
}
