create table books (
  id serial primary key,
  title text not null,
  created_at timestamp with time zone not null,
  read boolean default false,
  unique (title)
);

create table users (
  id serial primary key,
  name varchar(128) not null,
  created_at timestamp with time zone not null,
  unique (name)
);

create table votes (
  book_id integer not null references books(id),
  user_id integer not null references users(id),
  created_at timestamp with time zone not null,
  unique (book_id, user_id)
);
