CREATE TABLE posts (
    username text NOT NULL,
    post text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_posts ON posts USING btree (username);
