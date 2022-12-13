create table article
(
    id         serial       not null
        constraint article_pk
            primary key,
    name       varchar(300) not null,
    annotation varchar(3000)
);

create table file
(
    id         serial       not null
        constraint file_pk
            primary key,
    name       varchar(300) not null,
    path       varchar(300) not null,
    article_id integer
        constraint file_article_id_fk
            references article
);

create table "user"
(
    id   serial       not null
        constraint user_pk
            primary key,
    name varchar(200) not null
);

create unique index user_name_uindex
    on "user" (name);

create table article_feedback
(
    id         serial        not null
        constraint article_feedback_pk
            primary key,
    article_id integer
        constraint article_feedback_article_id_fk
            references article,
    user_id    integer
        constraint article_feedback_user_id_fk
            references "user",
    text       varchar(1000) not null
);

create table article_view
(
    id         serial not null
        constraint article_view_pk
            primary key,
    article_id integer
        constraint article_view_article_id_fk
            references article,
    user_id    integer
        constraint article_view_user_id_fk
            references "user"
);