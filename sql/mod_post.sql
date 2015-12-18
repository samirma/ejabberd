DROP table posts;

DROP table comments;

DROP table rates;

CREATE TABLE posts (
    id SERIAL UNIQUE,
    username text NOT NULL,
    post text NOT NULL,
    location geometry NULL,
    rate integer NOT NULL DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_posts ON posts USING btree (username);


CREATE TABLE comments
(
  id serial,
  post_id integer NOT NULL,
  username text NOT NULL,
  commentary text NOT NULL,
  location geometry NULL,
  rate integer NOT NULL DEFAULT 0,
  created_at TIMESTAMP NOT NULL DEFAULT now()
);


CREATE TABLE rates
(
  id serial,
  post_id integer,
  comment_id integer,
  username text NOT NULL,
  rate integer NOT NULL DEFAULT 0,
  created_at TIMESTAMP NOT NULL DEFAULT now()
);


CREATE TABLE hidden_posts
(
  id serial,
  post_id integer,
  comment_id integer,
  hidden_from_user text NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 

CREATE TABLE hidden_comments
(
  id serial,
  comment_id integer,
  hidden_from_user text NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 

CREATE TABLE wifi_locations
(
  id serial,
  wifi_name text,
  location geometry,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 


CREATE TABLE wifi_locations
(
  id serial,
  wifi_name text,
  location geometry,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 


CREATE TABLE wifi_posts
(
  wifi_locations_id integer,
  post_id integer,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 
