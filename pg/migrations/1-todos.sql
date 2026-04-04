-- liquibase formatted sql

-- changeset dev:1
CREATE TABLE IF NOT EXISTS todos (
      id BIGSERIAL PRIMARY KEY,
      title TEXT NOT NULL,
      completed BOOLEAN NOT NULL DEFAULT FALSE
)
