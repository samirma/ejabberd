DROP TABLE public.posts;

DROP TABLE public.comments;

DROP TABLE public.rates;

DROP TABLE public.wifi_posts;
DROP TABLE public.hidden_posts;
DROP TABLE public.hidden_comments;
DROP TABLE public.wifi_localizations;

DROP TABLE public.registed_users;
DROP TABLE public.user_phone;
DROP TABLE public.user_details;
DROP TABLE public.user_preferences;
DROP TABLE public.preferences;


CREATE TABLE public.posts (
    id SERIAL UNIQUE,
    username text NOT NULL,
    post text NOT NULL,
    localization geometry NULL,
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
  localization geometry NULL,
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

CREATE TABLE public.wifi_localizations
(
  id serial,
  wifi_name text,
  localization geometry,
  created_at TIMESTAMP NOT NULL DEFAULT now()
); 



CREATE TABLE public.wifi_posts
(
  wifi_localizations_id integer,
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


INSERT INTO "public".preferences (preference) 
	VALUES ('Sports')
INSERT INTO "public".preferences (preference) 
	VALUES ('Star War')
INSERT INTO "public".preferences (preference) 
	VALUES ('Music')


