DROP TABLE public.posts;

DROP TABLE public.comments;

DROP TABLE public.rates;

DROP TABLE public.wifi_posts;
DROP TABLE public.hidden_posts;
DROP TABLE public.hidden_comments;
DROP TABLE public.wifi_locations;


CREATE TABLE public.posts (
    id SERIAL UNIQUE,
    username text NOT NULL,
    post text NOT NULL,
    location geometry NULL,
    rate integer NOT NULL DEFAULT 0,
    rates_count integer NOT NULL DEFAULT 0,
    views_count integer NOT NULL DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_posts ON posts USING btree (username);


CREATE TABLE public.comments
(
  id serial,
  post_id integer NOT NULL,
  username text NOT NULL,
  commentary text NOT NULL,
  location geometry NULL,
  rate integer NOT NULL DEFAULT 0,
  rates_count integer NOT NULL DEFAULT 0,
  created_at TIMESTAMP NOT NULL DEFAULT now()
);


CREATE TABLE public.rates
(
  id serial,
  post_id integer,
  comment_id integer,
  username text NOT NULL,
  rate integer NOT NULL DEFAULT 0,
  created_at TIMESTAMP NOT NULL DEFAULT now()
);


CREATE TABLE public.hidden_posts
(
  id serial,
  hidden_id integer,
  hidden_from_user text NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 

CREATE TABLE public.hidden_comments
(
  id serial,
  hidden_id integer,
  hidden_from_user text NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 

CREATE TABLE public.wifi_locations
(
  id serial,
  wifi_name text,
  location geometry,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 



CREATE TABLE public.wifi_posts
(
  wifi_locations_id integer,
  post_id integer,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 


CREATE TABLE public.registed_users
(
  username text,
  phone_id integer,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 


CREATE TABLE public.user_phone
(
  id serial,
  phone text,
  register_code text,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 

CREATE TABLE public.user_details
(
  id serial,
  gender text,
  username text NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 


CREATE TABLE public.user_preferences
(
  id serial,
  preference_id integer,
  username text NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 


CREATE TABLE public.preferences
(
  id serial,
  preference text,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 



