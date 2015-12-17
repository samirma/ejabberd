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
